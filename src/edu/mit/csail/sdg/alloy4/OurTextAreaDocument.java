/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

/**
 * Graphical syntax-highlighting implementation of StyledDocument.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

final class OurTextAreaDocument extends DefaultStyledDocument {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The root element. */
    private final Element root;

    /** The various style to use when displaying text in the text area. */
    private final Style styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleSymbol;

    /** Stores the "comment mode" at the start of each line: 0 means no comment, 1 means block comment, 2 means javadoc block comment, -1 means unknown. */
    private final List<Integer> comments = new ArrayList<Integer>();

    /** Whether syntax highlighting is currently enabled or not. */
    private boolean enabled = true;

    /** Caches the most recent font. */
    private Font mostRecentFont = null;

    /** Caches the most recent tab size. */
    private int tabSize = 4;

    /** Caches the list of TAB positions. */
    private SimpleAttributeSet tabAttribute = null;

    /** This stores the currently recognized set of keywords. */
    private final String[] keywords = new String[] {"abstract", "all", "and", "as", "assert", "but", "check", "disj",
      "disjoint", "else", "enum", "exactly", "exh", "exhaustive", "expect", "extends", "fact", "for", "fun", "iden",
      "iff", "implies", "in", "Int", "int", "let", "lone", "module", "no", "none", "not", "one", "open", "or", "part",
      "partition", "pred", "private", "run", "seq", "set", "sig", "some", "sum", "this", "univ"
    };

    /** Returns true if "c" can be the start of an identifier. */
    private static boolean myIdenStart(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$'; }

    /** Returns true if "c" can be in the middle or the end of an identifier. */
    private static boolean myIden(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9') || c=='$' || c=='\'' || c=='\"'; }

    /** Returns true if c[start..start+len-1] matches one of the reserved keyword. */
    private boolean myIsKeyword(String array, int start, int len) {
       again:
       for(int i=keywords.length-1; i>=0; i--) {
          String str=keywords[i];
          if (str.length() != len) continue;
          for(int j=0; j<len; j++) if (str.charAt(j)!=array.charAt(start+j)) continue again;
          return true;
       }
       return false;
    }

    /** Enables or disables syntax highlighting. */
    void mySyntaxHighlighting (JTextPane pane, boolean flag) {
        if (enabled == flag) return;
        enabled = flag;
        comments.clear();
        if (flag) { myReapplyAll(); return; }
        setCharacterAttributes(0, getLength(), styleNormal, true);
        pane.getInputAttributes().removeAttribute(pane.getInputAttributes());
        pane.getInputAttributes().addAttributes(styleNormal);
    }

    /** Construct a new OurTextAreaDocument object. */
    public OurTextAreaDocument() {
        root = getDefaultRootElement();
        putProperty( DefaultEditorKit.EndOfLineStringProperty, "\n" );
        styleNormal = addStyle("N", null);
        StyleConstants.setBold      (styleNormal, false);
        StyleConstants.setForeground(styleNormal, Color.BLACK);
        StyleConstants.setFontFamily(styleNormal, "Monospaced");
        StyleConstants.setFontSize  (styleNormal, 14);
        StyleConstants.setBold      (styleSymbol         = addStyle("S", styleNormal), true);
        StyleConstants.setForeground(styleKeyword        = addStyle("K", styleSymbol), new Color(30, 30, 168));
        StyleConstants.setForeground(styleNumber         = addStyle("9", styleSymbol), new Color(168, 10, 10));
        StyleConstants.setForeground(styleComment        = addStyle("0", styleNormal), new Color(10, 148, 10));
        StyleConstants.setForeground(styleBlockComment   = addStyle("1", styleNormal), new Color(10, 148, 10));
        StyleConstants.setForeground(styleJavadocComment = addStyle("2", styleSymbol), new Color(10, 148, 10));
    }

    /** {@inheritDoc} */
    @Override public void insertString(int offset, String string, AttributeSet attr) throws BadLocationException {
        if (string.indexOf('\r')>=0) string = Util.convertLineBreak(string);
        if (!enabled) { super.insertString(offset, string, attr); return; }
        int startLine = root.getElementIndex(offset), lineCount = 1;
        for(int i=0; i<string.length(); i++) {
            // given that we are about to insert line breaks into the document, we need to shift the values in this.comments array
            if (string.charAt(i)=='\n') { lineCount++; if (startLine <= comments.size()-1) comments.add(startLine+1, -1); }
        }
        super.insertString(offset, string, attr);
        myUpdate((startLine<comments.size() ? comments.get(startLine) : -1), startLine, lineCount);
    }

    /** {@inheritDoc} */
    @Override public void remove(int offset, int length) throws BadLocationException {
        if (!enabled) { super.remove(offset, length); return; }
        String oldText = myText();
        int startLine = root.getElementIndex(offset);
        for(int i=0; i<length; i++) {
            // given that we are about to delete line breaks from the document, we need to shift the values in this.comments array
            if (oldText.charAt(i)=='\n') if (startLine < comments.size()-1) comments.remove(startLine+1);
        }
        super.remove(offset, length);
        myUpdate((startLine<comments.size() ? comments.get(startLine) : -1), startLine, 1);
    }

    /** Returns the entire text. */
    private String myText() {
        try { return getText(0, getLength()); } catch(BadLocationException ex) { return ""; }
    }

    /** Returns the number of lines represented by the given text. */
    private int myGetLineCount(String content) {
        int lineCount = 1;
        for(int i=0; i<content.length(); i++) if (content.charAt(i)=='\n') lineCount++;
        return lineCount;
    }

    /**
     * Apply appropriate styles based on the new changes.
     * @param comment - the comment mode at the start of the given line (-1 if unknown)
     * @param startLine - the line where changes begin
     * @param numLines - the number of lines directly affected; we still have to check beyond it to see if more styling are needed
     */
    private void myUpdate(int comment, int startLine, int numLines) throws BadLocationException  {
        String content = myText();
        int i, lineCount = myGetLineCount(content);
        if (comment<0) comment = myGetStartingStyleOfLine(content, startLine);
        // for each line, we apply the appropriate styles
        for (i = startLine; i < startLine+numLines; i++) { comment = myReapply(comment, content, i); }
        // afterwards, we need to apply styles to each subsequent line until we find that it already starts with the same comment mode as before
        for (; i < lineCount; i++) { if (i>=comments.size() || comments.get(i)!=comment) comment = myReapply(comment, content, i); else break; }
    }

    /** Reparse the entire text to determine the appropriate comment mode at the start of the given line. */
    private static int myGetStartingStyleOfLine(final String txt, final int line) {
        if (line<=0) return 0;
        int comment=0, y=0, n=txt.length();
        for(int i=0; i<n; i++) {
            char c = txt.charAt(i);
            if (c=='\n') {
                y++; if (y==line) return comment;
            } else if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
                i = txt.indexOf('\n', i);
                if (i<0) break; else i--;
            } else if
                ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               ||(comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
                i=i+2; comment=2;
            } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
                i++; comment=1;
            } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
                i++; comment=0;
            }
        }
        return comment;
    }

    /** Parse the given line and apply appropriate styles to it. */
    private int myReapply(int comment, final String txt, final int line) {
        // enlarge the comments array as appropriate
        while (line>=comments.size()) comments.add(-1);
        // record the fact that this line starts with the given comment mode
        comments.set(line, comment);
        // now, go over it character-by-character until we reach end of file or end of line (whichever comes first)
        for(int n=txt.length(), i=root.getElement(line).getStartOffset(); i < n; i++) {
            char c = txt.charAt(i);
            if (c=='\r' || c=='\n') break;
            if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
                int d = txt.indexOf('\n', i);
                setCharacterAttributes(i, d<0 ? (n-i) : (d-i), styleComment, true);
                break;
            } else if
                ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               ||(comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
                setCharacterAttributes(i, 3, styleJavadocComment, true); i=i+2; comment=2;
            } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
                setCharacterAttributes(i, 2, styleBlockComment, true); i++; comment=1;
            } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
                setCharacterAttributes(i, 2, comment==1 ? styleBlockComment : styleJavadocComment, true); i++; comment=0;
            } else if (comment>0) {
                int oldi=i; i++; while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='*') i++;
                setCharacterAttributes(oldi, i-oldi, comment==1 ? styleBlockComment : styleJavadocComment, true);
                i--;
            } else if ((c>='0' && c<='9') || myIdenStart(c)) {
                int oldi=i; i++; while(i<n && myIden(txt.charAt(i))) i++;
                Style s=styleNormal; if (c>='0' && c<='9') s=styleNumber; else if (myIsKeyword(txt, oldi, i-oldi)) s=styleKeyword;
                setCharacterAttributes(oldi, i-oldi, s, true);
                i--;
            } else {
                int oldi=i; i++; while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='-' && txt.charAt(i)!='/' && !myIden(txt.charAt(i))) i++;
                setCharacterAttributes(oldi, i-oldi, styleSymbol, true);
                i--;
            }
        }
        return comment;
    }

    /** Reapply the appropriate style to the entire document. */
    void myReapplyAll() {
        String content = myText();
        int lineCount = myGetLineCount(content);
        comments.clear();
        for(int comment=0, i=0; i<lineCount; i++)  comment = myReapply(comment, content, i);
    }

    /** Changes the font for the document. */
    void mySetFont(JTextPane pane, Font font) {
        if (mostRecentFont == font) return; else mostRecentFont = font;
        for(Style s: new Style[]{styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleSymbol}) {
            StyleConstants.setFontFamily(s, font.getFamily());
            StyleConstants.setFontSize(s, font.getSize());
        }
        tabAttribute = null; // forces the recomputation of the tab positions based on the new font
        mySetTabSize(pane, tabSize);
        myReapplyAll();
    }

    /** Changes the tab size for the document. */
    void mySetTabSize(JTextPane pane, int tab) {
        if (tab<1) tab=1; else if (tab>100) tab=100;
        if (tabAttribute!=null && this.tabSize==tab) return; else this.tabSize = tab;
        int gap = (tab * pane.getFontMetrics(mostRecentFont).charWidth('X') * 2) / 2;
        tabAttribute = new SimpleAttributeSet();
        final TabStop[] pos = new TabStop[100];
        for(int i=0; i<100; i++) { pos[i] = new TabStop(i*gap+gap); }
        final TabSet tabSet = new TabSet(pos);
        StyleConstants.setTabSet(tabAttribute, tabSet);
        setParagraphAttributes(0, getLength(), tabAttribute, false);
    }
}
