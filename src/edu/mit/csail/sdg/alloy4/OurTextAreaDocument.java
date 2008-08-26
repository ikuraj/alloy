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

    /** This records an insertion or deletion. */
    private static final class OurTextAreaAction {

        /** True if this was an insertion; false if this was a deletion. */
        private boolean insert;

        /** The text being inserted or deleted. */
        private String text;

        /** The offset where the insertion or deletion occurred. */
        private int offset;

        /** Construct a new OurTextAreaAction object. */
        private OurTextAreaAction(boolean insert, String text, int offset) {
            this.insert = insert;
            this.text = text;
            this.offset = offset;
        }
    }

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The root element. */
    private final Element root;

    /** The various styles to use when displaying text in the text area. */
    private final Style
      styleNormal = addStyle("-", null),
      styleNumber = addStyle("0", null),
      styleKeyword = addStyle("k", null),
      styleComment = addStyle("c", null),
      styleString = addStyle("s", null),
      styleBlockComment = addStyle("b", null),
      styleJavadocComment = addStyle("j", null),
      styleSymbol = addStyle("+", null);

    /** Stores the "comment mode" at the start of each line: 0 means no comment, 1 means block comment, 2 means javadoc block comment, -1 means unknown. */
    private final List<Integer> comments = new ArrayList<Integer>();

    /** The list of undo's. */
    private final List<OurTextAreaAction> undos = new ArrayList<OurTextAreaAction>();

    /** The number of items in the undo list that are actual undone. */
    private int undo = 0;

    /** Whether syntax highlighting is currently enabled or not. */
    private boolean enabled = true;

    /** The maximum number of UNDO actions we want to keep. */
    private static final int MAXUNDO = 100;

    /** Caches the most recent font family. */
    private String fontFamily = "Monospaced";

    /** Caches the most recent font size. */
    private int fontSize = 14;

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
    private static boolean do_start(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$'; }

    /** Returns true if "c" can be in the middle or the end of an identifier. */
    private static boolean do_iden(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$' || (c>='0' && c<='9') || c=='_' || c=='\'' || c=='\"'; }

    /** Returns true if array[start..start+len-1] matches one of the reserved keyword. */
    private boolean do_keyword(String array, int start, int len) {
       again:
       for(int i=keywords.length-1; i>=0; i--) {
          String str = keywords[i];
          if (str.length() != len) continue;
          for(int j=0; j<len; j++) if (str.charAt(j)!=array.charAt(start+j)) continue again;
          return true;
       }
       return false;
    }

    /** Enables or disables syntax highlighting. */
    void do_syntaxHighlighting (JTextPane pane, boolean flag) {
        if (enabled == flag) return;
        enabled = flag;
        comments.clear();
        if (flag) { do_reapplyAll(); return; }
        setCharacterAttributes(0, getLength(), styleNormal, false);
    }

    /** Construct a new OurTextAreaDocument object. */
    public OurTextAreaDocument() {
        root = getDefaultRootElement();
        putProperty(DefaultEditorKit.EndOfLineStringProperty, "\n");
        for(Style s: new Style[]{styleNormal, styleNumber, styleKeyword, styleString, styleComment, styleBlockComment, styleJavadocComment, styleSymbol}) {
            StyleConstants.setBold      (s, false);
            StyleConstants.setForeground(s, Color.BLACK);
            StyleConstants.setFontFamily(s, "Monospaced");
            StyleConstants.setFontSize  (s, 14);
        }
        StyleConstants.setBold      (styleSymbol, true);
        StyleConstants.setForeground(styleString, new Color(168, 10, 168));
        StyleConstants.setForeground(styleComment, new Color(10, 148, 10));
        StyleConstants.setForeground(styleBlockComment, new Color(10, 148, 10));
        StyleConstants.setBold      (styleKeyword, true);
        StyleConstants.setBold      (styleNumber, true);
        StyleConstants.setBold      (styleJavadocComment, true);
        StyleConstants.setForeground(styleKeyword, new Color(30, 30, 168));
        StyleConstants.setForeground(styleNumber, new Color(168, 10, 10));
        StyleConstants.setForeground(styleJavadocComment, new Color(10, 148, 10));
    }

    /** Performs the insertString() operation without touching the Undo/Redo history. */
    private void do_insert(int offset, String string) throws BadLocationException {
        if (!enabled) { super.insertString(offset, string, styleNormal); return; }
        int startLine = root.getElementIndex(offset), lineCount = 1;
        for(int i=0; i<string.length(); i++) {
            // given that we are about to insert line breaks into the document, we need to shift the values in this.comments array
            if (string.charAt(i)=='\n') { lineCount++; if (startLine <= comments.size()-1) comments.add(startLine+1, -1); }
        }
        super.insertString(offset, string, styleNormal);
        try {
            do_update((startLine<comments.size() ? comments.get(startLine) : -1), startLine, lineCount);
        } catch(Exception ex) {
            comments.clear(); // syntax highlighting is not crucial, but if error occurred, let's clear the cache so we'll recompute the highlighting next time
        }
    }

    /** Performs the remove() operation without touching the Undo/Redo history. */
    private void do_remove(int offset, int length) throws BadLocationException {
        if (!enabled) { super.remove(offset, length); return; }
        String oldText = do_text();
        int startLine = root.getElementIndex(offset);
        for(int i=0; i<length; i++) {
            // given that we are about to delete line breaks from the document, we need to shift the values in this.comments array
            if (oldText.charAt(offset+i)=='\n') if (startLine < comments.size()-1) comments.remove(startLine+1);
        }
        super.remove(offset, length);
        try {
            do_update((startLine<comments.size() ? comments.get(startLine) : -1), startLine, 1);
        } catch(Exception ex) {
            comments.clear(); // syntax highlighting is not crucial, but if error occurred, let's clear the cache so we'll recompute the highlighting next time
        }
    }

    /** This method is called by Swing to insert a String into this document. */
    @Override public void insertString(int offset, String string, AttributeSet attr) throws BadLocationException {
        // make sure we don't insert the '\r' character
        if (string.indexOf('\r')>=0) string = Util.convertLineBreak(string);
        // don't perform trivial actions
        if (string.length() == 0) return;
        // clear all the "potential redo's"
        while(undo>0) { undos.remove(undos.size()-1); undo--; }
        // perform the actual insertion
        do_insert(offset, string);
        // if the last action and this action can be merged, then merge them
        OurTextAreaAction act = (undos.size()>0 ? undos.get(undos.size()-1) : null);
        if (act!=null && act.insert && act.offset==offset-act.text.length()) { act.text=act.text+string; return; }
        // if there are already MAX undo items in the undo cache, then evict the earliest action from the UNDO cache
        if (undos.size()>=MAXUNDO) undos.remove(0);
        undos.add(new OurTextAreaAction(true, string, offset));
    }

    /** This method is called by Swing to delete text from this document. */
    @Override public void remove(int offset, int length) throws BadLocationException {
        // don't perform trivial actions
        if (length==0) return;
        // clear all the "potential redo's"
        while(undo>0) { undos.remove(undos.size()-1); undo--; }
        String string = do_text().substring(offset, offset+length);
        // perform the actual removal
        do_remove(offset, length);
        // if the last action and this action can be merged, then merge them
        OurTextAreaAction act = (undos.size()>0 ? undos.get(undos.size()-1) : null);
        if (act!=null && !act.insert && act.offset==offset) { act.text=act.text+string; return; }
        if (act!=null && !act.insert && act.offset==offset+length) { act.offset=offset; act.text=string+act.text; return; }
        // if there are already MAX undo items in the undo cache, then evict the earliest action from the UNDO cache
        if (undos.size()>=MAXUNDO) undos.remove(0);
        undos.add(new OurTextAreaAction(false, string, offset));
    }

    /** Clear the undo history. */
    public void do_clearUndo() { undos.clear(); undo=0; }

    /** Returns true if we can perform undo right now. */
    public boolean do_canUndo() { return undo < undos.size(); }

    /** Returns true if we can perform redo right now. */
    public boolean do_canRedo() { return undo > 0 ; }

    /** Perform undo if possible, and return the new caret position (-1 if no undo is possible) */
    public int do_undo() {
        int n = undos.size();
        if (undo >= n) return -1;
        undo++;
        OurTextAreaAction act = undos.get(n-undo);
        try {
            if (act.insert) { do_remove(act.offset, act.text.length()); return act.offset; }
            else { do_insert(act.offset, act.text); return act.offset + act.text.length(); }
        } catch(Exception ex) {
            comments.clear();
            return -1;
        }
    }

    /** Perform redo if possible, and return the new caret position (-1 if no redo is possible) */
    public int do_redo() {
        int n = undos.size();
        if (undo == 0) return -1;
        OurTextAreaAction act = undos.get(n-undo);
        undo--;
        try {
            if (act.insert) { do_insert(act.offset, act.text); return act.offset + act.text.length(); }
            else { do_remove(act.offset, act.text.length()); return act.offset; }
        } catch(Exception ex) {
            comments.clear();
            return -1;
        }
    }

    /** Returns the entire text. */
    private String do_text() {
        try { return getText(0, getLength()); } catch(BadLocationException ex) { return ""; }
    }

    /** Returns the number of lines represented by the given text. */
    private int do_getLineCount(String content) {
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
    private void do_update(int comment, int startLine, int numLines) throws BadLocationException  {
        String content = do_text();
        int i, lineCount = do_getLineCount(content);
        if (comment<0) comment = do_getStartingStyleOfLine(content, startLine);
        // for each line, we apply the appropriate styles
        for (i = startLine; i < startLine+numLines; i++) { comment = do_reapply(comment, content, i); }
        // afterwards, we need to apply styles to each subsequent line until we find that it already starts with the same comment mode as before
        for (; i < lineCount; i++) { if (i>=comments.size() || comments.get(i)!=comment) comment = do_reapply(comment, content, i); else break; }
    }

    /** Reparse the entire text to determine the appropriate comment mode at the start of the given line. */
    private static int do_getStartingStyleOfLine(final String txt, final int line) {
        if (line<=0) return 0;
        int comment=0, y=0, n=txt.length();
        for(int i=0; i<n; i++) {
            char c = txt.charAt(i);
            if (c=='\n') {
                y++; if (y==line) return comment;
            } else if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
                i=txt.indexOf('\n', i);
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
    private int do_reapply(int comment, final String txt, final int line) {
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
                setCharacterAttributes(i, d<0 ? (n-i) : (d-i), styleComment, false);
                break;
            } else if
                ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               ||(comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
                setCharacterAttributes(i, 3, styleJavadocComment, false); i=i+2; comment=2;
            } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
                setCharacterAttributes(i, 2, styleBlockComment, false); i++; comment=1;
            } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
                setCharacterAttributes(i, 2, comment==1 ? styleBlockComment : styleJavadocComment, false); i++; comment=0;
            } else if (comment>0) {
                int oldi=i; i++; while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='*') i++;
                setCharacterAttributes(oldi, i-oldi, comment==1 ? styleBlockComment : styleJavadocComment, false);
                i--;
            } else if (c=='\"') {
                int oldi=i; i++;
                while(i<n) {
                  if (txt.charAt(i)=='\r' || txt.charAt(i)=='\n') break;
                  if (txt.charAt(i)=='\"') {i++; break;}
                  if (txt.charAt(i)=='\\' && i+1<n && txt.charAt(i+1)!='\r' && txt.charAt(i+1)!='\n') i=i+2; else i=i+1;
                }
                setCharacterAttributes(oldi, i-oldi, styleString, false);
                i--;
            } else if ((c>='0' && c<='9') || do_start(c)) {
                int oldi=i; i++; while(i<n && do_iden(txt.charAt(i))) i++;
                Style s=styleNormal; if (c>='0' && c<='9') s=styleNumber; else if (do_keyword(txt, oldi, i-oldi)) s=styleKeyword;
                setCharacterAttributes(oldi, i-oldi, s, false);
                i--;
            } else {
                int oldi=i; i++; while(i<n && txt.charAt(i)!='\n' && txt.charAt(i)!='-' && txt.charAt(i)!='/' && !do_iden(txt.charAt(i))) i++;
                setCharacterAttributes(oldi, i-oldi, styleSymbol, false);
                i--;
            }
        }
        return comment;
    }

    /** Reapply the appropriate style to the entire document. */
    void do_reapplyAll() {
        String content = do_text();
        int lineCount = do_getLineCount(content);
        comments.clear();
        for(int comment=0, i=0; i<lineCount; i++)  comment = do_reapply(comment, content, i);
    }

    /** Changes the font for the document. */
    void do_setFont(JTextPane pane, String fontFamily, int fontSize) {
        if (fontFamily.equals(this.fontFamily) && fontSize==this.fontSize) return;
        this.fontFamily = fontFamily;
        this.fontSize = fontSize;
        for(Style s: new Style[]{styleNormal, styleNumber, styleKeyword, styleString, styleComment, styleBlockComment, styleJavadocComment, styleSymbol}) {
            StyleConstants.setFontFamily(s, fontFamily);
            StyleConstants.setFontSize(s, fontSize);
        }
        tabAttribute = null; // forces the recomputation of the tab positions based on the new font
        do_setTabSize(pane, tabSize);
        do_reapplyAll();
    }

    /** Changes the tab size for the document. */
    void do_setTabSize(JTextPane pane, int tab) {
        if (tab<1) tab=1; else if (tab>100) tab=100;
        if (tabAttribute!=null && this.tabSize==tab) return; else this.tabSize = tab;
        int gap = tab * pane.getFontMetrics(new Font(fontFamily, Font.PLAIN, fontSize)).charWidth('X');
        tabAttribute = new SimpleAttributeSet();
        final TabStop[] pos = new TabStop[100];
        for(int i=0; i<100; i++) { pos[i] = new TabStop(i*gap+gap); }
        StyleConstants.setTabSet(tabAttribute, new TabSet(pos));
        setParagraphAttributes(0, getLength(), tabAttribute, false);
    }
}
