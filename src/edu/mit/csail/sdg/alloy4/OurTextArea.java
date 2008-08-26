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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.border.EmptyBorder;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

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

    /** Constructs a text area widget. */
    public OurTextArea(boolean syntaxHighlighting, String text, String fontFamily, int fontSize, int tabSize) {
        super();
        OurAntiAlias.register(this);
        // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
        // 30000 is a good number; value higher than about 32768 will cause errors.
        // Also, it tells the JTextPane to use our syntax-highlighting document object.
        final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();
        setEditorKit(new StyledEditorKit() {
          private static final long serialVersionUID = 1L;
          @Override public Document createDefaultDocument() { return new OurTextAreaDocument(); }
          @Override public final ViewFactory getViewFactory() {
             return new ViewFactory() {
                public final View create(Element x) {
                   if (!AbstractDocument.SectionElementName.equals(x.getName())) return defaultFactory.create(x);
                   return new BoxView(x, View.Y_AXIS) {
                      @Override public final float getMinimumSpan(int axis) { return super.getPreferredSpan(axis); }
                      @Override public final void layout(int width, int height) { try {super.layout(30000, height);} catch(Throwable ex) {} }
                   };
                }
             };
          }
        });
        doc = (OurTextAreaDocument) getStyledDocument();
        doc.do_syntaxHighlighting(this, syntaxHighlighting);
        doc.do_setFont(this, fontFamily, fontSize);
        doc.do_setTabSize(this, tabSize);
        if (text.length()>0) { setText(text); setCaretPosition(0); }
        doc.do_clearUndo();
        setBackground(Color.WHITE);
        setBorder(new EmptyBorder(1, 1, 1, 1));
        getActionMap().put("alloy_copy", new AbstractAction("alloy_copy") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) { copy(); }
        });
        getActionMap().put("alloy_cut", new AbstractAction("alloy_cut") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) { cut(); }
        });
        getActionMap().put("alloy_paste", new AbstractAction("alloy_paste") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) { paste(); }
        });
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK), "alloy_copy");
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_MASK), "alloy_cut");
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_V, InputEvent.CTRL_MASK), "alloy_paste");
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.CTRL_MASK), "alloy_copy");
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.SHIFT_MASK), "alloy_paste");
        getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.SHIFT_MASK), "alloy_cut");
    }

    /** Changes the font for the document. */
    public void do_setFont(String fontFamily, int fontSize) { if (doc!=null) doc.do_setFont(this, fontFamily, fontSize); }

    /** Changes the tab size for the document. */
    public void do_setTabSize(int tab) { if (doc!=null) doc.do_setTabSize(this, tab); }

    /** Enables or disables syntax highlighting. */
    public void do_syntaxHighlighting(boolean flag) { if (doc!=null) doc.do_syntaxHighlighting(this, flag); }

    /** Implements the core traversal logic in getLineStartOffset, getLineCount, and getLineOfOffset. */
    private static int do_helper(String text, int action, int target) throws BadLocationException {
        final int n = text.length();
        if (action==3) if (target<0 || target>text.length()) throw new BadLocationException("", target);
        for(int i=0, line=0; i<=n; line++) {
            // loop invariant #1:  line == the number of lines we've seen already before text[i]
            // loop invariant #2:  i==0 or text[i-1]=='\n'
            int j = (i>=n) ? n : text.indexOf('\n',i); if (j<0) j=n; // offset of the end of this line
            if (action==1 && line==target) return i;
            if (action==2 && j==n) return line+1;
            if (action==3 && target>=i && target<=j) return line;
            i=j+1;
        }
        throw new BadLocationException("", target);
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineStartOffset(int) getLineStartOffset} method. */
    public int do_getLineStartOffset(int line) throws BadLocationException { return do_helper(getText(), 1, line); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineCount() getLineCount} method. */
    public int do_getLineCount() { try {return do_helper(getText(), 2, 0);} catch(BadLocationException ex) {return 0;} }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineOfOffset(int) getLineOfOffset} method. */
    public int do_getLineOfOffset(int offset) throws BadLocationException { return do_helper(getText(), 3, offset); }

    /** Returns true if we can perform undo right now. */
    public boolean do_canUndo() { return doc.do_canUndo(); }

    /** Returns true if we can perform redo right now. */
    public boolean do_canRedo() { return doc.do_canRedo(); }

    /** Perform undo if possible. */
    public void do_undo() { int i = doc.do_undo(); if (i>=0 && i<=getText().length()) setCaretPosition(i); }

    /** Perform redo if possible. */
    public void do_redo() { int i = doc.do_redo(); if (i>=0 && i<=getText().length()) setCaretPosition(i); }

    /** Clear the undo history. */
    public void do_clearUndo() { doc.do_clearUndo(); }

    /** This method is called by Swing to draw this component. */
    @Override public void paint(Graphics gr) {
        if (OurAntiAlias.antiAlias() && gr instanceof Graphics2D) {
            ((Graphics2D)gr).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        super.paint(gr);
    }
}
