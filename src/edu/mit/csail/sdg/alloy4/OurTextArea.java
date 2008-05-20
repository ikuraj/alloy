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

import java.util.ArrayList;
import java.util.List;
import java.awt.Font;
import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.undo.UndoableEdit;

/**
 * Graphical syntax-highlighting editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

public final class OurTextArea extends JTextPane {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The styled document being displayed. */
    private final OurTextAreaDocument doc;

    /** Stores the list of UndoableEditListeners registered with this widget. */
    private final List<UndoableEditListener> undoListeners = new ArrayList<UndoableEditListener>();

    /** Add the given listener to the list of UndoableEditListeners registered with this widget. */
    public void myAddUndoableEditListener(UndoableEditListener listener) {
        for(int i=0; i<undoListeners.size(); i++) if (undoListeners.get(i)==listener) return;
        undoListeners.add(listener);
    }

    /** Implements the core traversal logic in getLineStartOffset, getLineCount, and getLineOfOffset. */
    private static int myHelper(String text, int action, int target) throws BadLocationException {
        final int n=text.length();
        if (action==3) if (target<0 || target>text.length()) throw new BadLocationException("", target);
        for(int i=0, line=0; i<=n; line++) {
            // invariant #1:  line == the number of lines we've seen already before text[i]
            // invariant #2:  i==0 or text[i-1]=='\n'
            int j = (i>=n) ? n : text.indexOf('\n',i); if (j<0) j=n; // offset of the end of this line
            if (action==1 && line==target) return i;
            if (action==2 && j==n) return line+1;
            if (action==3 && target>=i && target<=j) return line;
            i=j+1;
        }
        throw new BadLocationException("", target);
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineStartOffset(int) getLineStartOffset} method. */
    public int getLineStartOffset(int line) throws BadLocationException { return myHelper(getText(), 1, line); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineCount() getLineCount} method. */
    public int getLineCount() { try {return myHelper(getText(), 2, 0);} catch(BadLocationException ex) {return 0;} }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineOfOffset(int) getLineOfOffset} method. */
    public int getLineOfOffset(int offset) throws BadLocationException { return myHelper(getText(), 3, offset); }

    /** Returns true if we can perform undo right now. */
    public boolean myCanUndo() { return doc.myCanUndo(); }

    /** Returns true if we can perform redo right now. */
    public boolean myCanRedo() { return doc.myCanRedo(); }

    /** Perform undo if possible. */
    public void myUndo() { int i = doc.myUndo(); if (i>=0 && i<=getText().length()) setCaretPosition(i); }

    /** Perform redo if possible. */
    public void myRedo() { int i = doc.myRedo(); if (i>=0 && i<=getText().length()) setCaretPosition(i); }

    /** Clear the undo history. */
    public void myClearUndo() { doc.myClearUndo(); }

    /** Apply the italic style to the given part of the document. */
    public void myItalic(int start, int len) { doc.myItalic(start, len); }

    /** Clear all existing italic style. */
    public void myClearItalic() { doc.myClearItalic(); }

    /** Constructs a text area widget. */
    public OurTextArea(boolean syntaxHighlighting, String text, String fontFamily, int fontSize, int tabSize) {
        super();
        // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
        // 30000 is a good number; value higher than about 32768 will cause errors.
        // Also, it tells the JTextPane to use our syntax-highlighting document object.
        final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();
        setEditorKit(new StyledEditorKit() {
          private static final long serialVersionUID = 1L;
          public Document createDefaultDocument() { return new OurTextAreaDocument(); }
          @Override public final ViewFactory getViewFactory() {
             return new ViewFactory() {
                public final View create(Element x) {
                   if (!AbstractDocument.SectionElementName.equals(x.getName())) return defaultFactory.create(x);
                   return new BoxView(x, View.Y_AXIS) {
                      @Override public final float getMinimumSpan(int axis) { return super.getPreferredSpan(axis); }
                      @Override public final void layout(int width,int height) { try {super.layout(30000, height);} catch(Throwable ex) {} }
                   };
                }
             };
          }
        });
        doc = (OurTextAreaDocument) getStyledDocument();
        doc.mySyntaxHighlighting(this, syntaxHighlighting);
        doc.mySetFont(this, new Font(fontFamily, Font.PLAIN, fontSize));
        doc.mySetTabSize(this, tabSize);
        if (text.length()>0) setText(text);
        myClearUndo();
        getDocument().addDocumentListener(new DocumentListener() {
           public void changedUpdate(DocumentEvent e) { }
           public void insertUpdate(DocumentEvent e) { removeUpdate(e); }
           public void removeUpdate(DocumentEvent e) {
              if (e instanceof UndoableEdit && e.getLength()>0) {
                 UndoableEditEvent event = new UndoableEditEvent(this, (UndoableEdit)e);
                 for(UndoableEditListener listener: undoListeners) listener.undoableEditHappened(event);
              }
           }
        });
    }

    /** Changes the text of the document. */
    @Override public void setText(String text) {
        if (text.indexOf('\r')>=0) text = Util.convertLineBreak(text);
        super.setText(text);
        setCaretPosition(0);
    }

    /** Changes the font for the document. */
    @Override public void setFont(Font font) { if (doc!=null) doc.mySetFont(this, font); }

    /** Changes the tab size for the document. */
    public void setTabSize(int tab) { if (doc!=null) doc.mySetTabSize(this, tab); }

    /** Enables or disables syntax highlighting. */
    public void mySyntaxHighlighting(boolean flag) { doc.mySyntaxHighlighting(this, flag); }
}
