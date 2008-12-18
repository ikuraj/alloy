/* Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import static edu.mit.csail.sdg.alloy4.OurConsole.style;

/** Graphical syntax-highlighting StyledDocument.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

class OurSyntaxDocument extends DefaultStyledDocument {

   /** This silences javac's warning about missing serialVersionUID. */
   private static final long serialVersionUID = 0;

   /** The "comment mode" at the start of each line (0 = no comment) (1 = block comment) (2 = javadoc comment) (-1 = unknown) */
   private final List<Integer> comments = new ArrayList<Integer>();

   /** Whether syntax highlighting is currently enabled or not. */
   private boolean enabled = true;

   /** The current font name is. */
   private String fontName = "Monospaced";

   /** The current font size. */
   private int fontSize = 14;

   /** The current tab size. */
   private int tabSize = 4;

   /** The character style for regular text. */
   private final MutableAttributeSet styleNormal = style(fontName, fontSize, false, Color.BLACK, 0);

   /** The character style for symbols. */
   private final MutableAttributeSet styleSymbol = style(fontName, fontSize, true, Color.BLACK, 0);

   /** The character style for integer constants. */
   private final MutableAttributeSet styleNumber = style(fontName, fontSize, true, new Color(168, 10, 10), 0);

   /** The character style for keywords. */
   private final MutableAttributeSet styleKeyword = style(fontName, fontSize, true, new Color(30, 30, 168), 0);

   /** The character style for string literals. */
   private final MutableAttributeSet styleString = style(fontName, fontSize, false, new Color(168, 10, 168), 0);

   /** The character style for up-to-end-of-line-style comment. */
   private final MutableAttributeSet styleComment = style(fontName, fontSize, false, new Color(10, 148, 10), 0);

   /** The character style for non-javadoc-style block comment. */
   private final MutableAttributeSet styleBlockComment = style(fontName, fontSize, false, new Color(10, 148, 10), 0);

   /** The character style for javadoc-style block comment. */
   private final MutableAttributeSet styleJavadocComment = style(fontName, fontSize, true, new Color(10, 148, 10), 0);

   /** The list containing all the above styles. */
   private final ConstList<MutableAttributeSet> all = Util.asList(
         styleNormal, styleSymbol, styleNumber, styleKeyword, styleString, styleComment, styleBlockComment, styleJavadocComment);

   /** The paragraph style for indentation. */
   private final MutableAttributeSet tabset = new SimpleAttributeSet();

   /** This stores the currently recognized set of reserved keywords. */
   private static final String[] keywords = new String[] {"abstract", "all", "and", "as", "assert", "but", "check", "disj",
      "disjoint", "else", "enum", "exactly", "exh", "exhaustive", "expect", "extends", "fact", "for", "fun", "iden",
      "iff", "implies", "in", "Int", "int", "let", "lone", "module", "no", "none", "not", "one", "open", "or", "part",
      "partition", "pred", "private", "run", "seq", "set", "sig", "some", "String", "sum", "this", "univ"
   };

   /** Returns true if array[start .. start+len-1] matches one of the reserved keyword. */
   private static final boolean do_keyword(String array, int start, int len) {
      if (len >= 2 && len <= 10) for(int i = keywords.length - 1; i >= 0; i--) {
         String str = keywords[i];
         if (str.length()==len) for(int j=0; ;j++) if (j==len) return true; else if (str.charAt(j) != array.charAt(start+j)) break;
      }
      return false;
   }

   /** Returns true if "c" can be in the start or middle or end of an identifier. */
   private static final boolean do_iden(char c) {
      return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$' || (c>='0' && c<='9') || c=='_' || c=='\'' || c=='\"';
   }

   /** Constructor. */
   public OurSyntaxDocument() {
      putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");
      tabSize++;
      do_setFont(fontName, fontSize, tabSize - 1); // force recomputation of the tab size
   }

   /** Enables or disables syntax highlighting. */
   public final void do_enableSyntax (boolean flag) {
      if (enabled == flag) return; else { enabled = flag;  comments.clear(); }
      if (flag) do_reapplyAll(); else setCharacterAttributes(0, getLength(), styleNormal, false);
   }

   /** Return the number of lines represented by the given text (where partial line counts as a line).
    * <p> For example: count("")==1, count("x")==1, count("x\n")==2, and count("x\ny")==2
    */
   public final int do_getLineCount() {
      String txt = toString();
      for(int ans=1, i=0; ; i++) if (i >= txt.length()) return ans; else if (txt.charAt(i)=='\n') ans++;
   }

   /** Return the starting offset of the given line (If "line" argument is too large, it will return the last line's starting offset)
    * <p> For example: given "ab\ncd\n", start(0)==0, start(1)==3, start(2...)==6.  Same thing when given "ab\ncd\ne".
    */
   public final int do_getLineStartOffset(int line) {
      String txt = toString();
      for(int ans=0, i=0, y=0; ; i++) if (y>=line || i==txt.length()) return ans; else if (txt.charAt(i)=='\n') { ans=i+1; y++; }
   }

   /** Return the line number that the offset is in (If "offset" argument is too large, it will just return do_getLineCount()).
    * <p> For example: given "ab\ncd\n", offset(0..2)==0, offset(3..5)==1, offset(6..)==2.  Same thing when given "ab\ncd\ne".
    */
   public final int do_getLineOfOffset(int offset) {
      String txt = toString();
      for(int n=txt.length(), ans=0, i=0; ; i++) if (i>=offset || i>=n) return ans; else if (txt.charAt(i)=='\n') ans++;
   }

   /** This method is called by Swing to insert a String into this document.
    * We intentionally ignore "attr" and instead use our own coloring.
    */
   @Override public void insertString(int offset, String string, AttributeSet attr) throws BadLocationException {
      if (string.indexOf('\r')>=0) string = Util.convertLineBreak(string); // we don't want '\r'
      if (!enabled) { super.insertString(offset, string, styleNormal); return; }
      int startLine = getDefaultRootElement().getElementIndex(offset);
      for(int i = 0; i < string.length(); i++) { // For each inserted '\n' we need to shift the values in "comments" array down
         if (string.charAt(i)=='\n') { if (startLine < comments.size()-1) comments.add(startLine+1, -1); }
      }
      super.insertString(offset, string, styleNormal);
      try { do_update(startLine); } catch(Exception ex) { comments.clear(); }
   }

   /** This method is called by Swing to delete text from this document. */
   @Override public void remove(int offset, int length) throws BadLocationException {
      if (!enabled) { super.remove(offset, length); return; }
      int i = 0, startLine = getDefaultRootElement().getElementIndex(offset);
      for(String oldText = toString(); i<length; i++) { // For each deleted '\n' we need to shift the values in "comments" array up
         if (oldText.charAt(offset+i)=='\n') if (startLine < comments.size()-1) comments.remove(startLine+1);
      }
      super.remove(offset, length);
      try { do_update(startLine); } catch(Exception ex) { comments.clear(); }
   }

   /** This method is called by Swing to replace text in this document. */
   @Override public void replace(int offset, int length, String string, AttributeSet attrs) throws BadLocationException {
      if (length > 0) this.remove(offset, length);
      if (string != null && string.length() > 0) this.insertString(offset, string, attrs);
   }

   /** Reapply styles assuming line "i" has just been modified */
   private final void do_update(int i) throws BadLocationException  {
      String content = toString();
      int lineCount = do_getLineCount();
      int comment = i<comments.size() ? comments.get(i) : -1;
      comment = do_reapply(comment<0 ? do_getStartingStyleOfLine(content, i) : comment, content, i);
      for (i++; i < lineCount; i++) { // update each subsequent line until it already starts with its expected comment mode
         if (i>=comments.size() || comments.get(i)!=comment) comment = do_reapply(comment, content, i); else break;
      }
   }

   /** Reparse the given line and apply appropriate styles to it. */
   private final int do_reapply(int comment, final String txt, final int line) {
      while (line >= comments.size()) comments.add(-1); // enlarge array if needed
      comments.set(line, comment);                      // record the fact that this line starts with the given comment mode
      for(int n=txt.length(), i=getDefaultRootElement().getElement(line).getStartOffset(); i < n; i++) {
         char c = txt.charAt(i);
         if (c=='\n') break;
         if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
            int d = txt.indexOf('\n', i);
            setCharacterAttributes(i, d<0 ? (n-i) : (d-i), styleComment, false);
            break;
         } else if ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               || (comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
            setCharacterAttributes(i, 3, styleJavadocComment, false); i=i+2; comment=2;
         } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
            setCharacterAttributes(i, 2, styleBlockComment, false); i++; comment=1;
         } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
            setCharacterAttributes(i, 2, comment==1 ? styleBlockComment : styleJavadocComment, false); i++; comment=0;
         } else if (comment>0) {
            int oldi = i++;
            while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='*') i++;
            setCharacterAttributes(oldi, i-oldi, comment==1 ? styleBlockComment : styleJavadocComment, false);
            i--;
         } else if (c=='\"') {
            int oldi = i++;
            while(i<n) {
               if (txt.charAt(i)=='\n') break;
               if (txt.charAt(i)=='\"') {i++; break;}
               if (txt.charAt(i)=='\\' && i+1<n && txt.charAt(i+1)!='\n') i=i+2; else i++;
            }
            setCharacterAttributes(oldi, i-oldi, styleString, false);
            i--;
         } else if (do_iden(c)) {
            int oldi = i++;
            while(i<n && do_iden(txt.charAt(i))) i++;
            AttributeSet s=styleNormal; if (c>='0' && c<='9') s=styleNumber; else if (do_keyword(txt, oldi, i-oldi)) s=styleKeyword;
            setCharacterAttributes(oldi, i-oldi, s, false);
            i--;
         } else {
            int oldi = i++;
            while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='-' && txt.charAt(i)!='/' && !do_iden(txt.charAt(i))) i++;
            setCharacterAttributes(oldi, i-oldi, styleSymbol, false);
            i--;
         }
      }
      return comment;
   }

   /** Reparse the entire text to determine the appropriate comment mode at the start of the given line. */
   private static final int do_getStartingStyleOfLine(final String txt, final int line) {
      if (line<=0) return 0;
      int comment=0, y=0, n=txt.length();
      for(int i=0; i<n; i++) {
         char c = txt.charAt(i);
         if (c=='\n') {
            y++; if (y==line) return comment;
         } else if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
            i = txt.indexOf('\n', i);
            if (i<0) break; else i--;
         } else if ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               || (comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
            i=i+2; comment=2;
         } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') { i++; comment=1;
         } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') { i++; comment=0; }
      }
      return comment;
   }

   /** Reapply the appropriate style to the entire document. */
   private final void do_reapplyAll() {
      setCharacterAttributes(0, getLength(), styleNormal, true);
      comments.clear();
      String content = toString();
      for(int comment = 0, i = 0, n = do_getLineCount(); i < n; i++)  comment = do_reapply(comment, content, i);
   }

   /** Changes the font and tabsize for the document. */
   public final void do_setFont(String fontName, int fontSize, int tabSize) {
      if (tabSize < 1) tabSize = 1; else if (tabSize > 100) tabSize = 100;
      if (fontName.equals(this.fontName) && fontSize == this.fontSize && tabSize == this.tabSize) return;
      this.fontName = fontName;
      this.fontSize = fontSize;
      this.tabSize = tabSize;
      for(MutableAttributeSet s: all) { StyleConstants.setFontFamily(s, fontName);  StyleConstants.setFontSize(s, fontSize); }
      do_reapplyAll();
      BufferedImage im = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB); // this is used to derive the tab width
      int gap = tabSize * im.createGraphics().getFontMetrics(new Font(fontName, Font.PLAIN, fontSize)).charWidth('X');
      TabStop[] pos = new TabStop[100];
      for(int i=0; i<100; i++) { pos[i] = new TabStop(i*gap + gap); }
      StyleConstants.setTabSet(tabset, new TabSet(pos));
      setParagraphAttributes(0, getLength(), tabset, false);
   }

   /** Overriden to return the full text of the document.
    * @return the entire text
    */
   @Override public String toString() {
      try { return getText(0, getLength()); } catch(BadLocationException ex) { return ""; }
   }
}
