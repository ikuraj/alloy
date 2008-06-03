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
import java.awt.Dimension;
import java.awt.Event;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 * Graphical input/output prompt.
 *
 * <p> This class's constructor takes a Computer object, then constructs a JScrollPane
 * in which the user can type commands, and the output from the Computer object will be displayed.
 * This interactive prompt supports UP and DOWN arrow command histories and basic copy/cut/paste editing.
 *
 * <p> For each user input, if the Computer object returns a String, it is displayed in blue.
 * But if the Computer object throws an exception, the exception will be displayed in red.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurConsole extends JScrollPane {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The style for default text. */
    private final SimpleAttributeSet plain;

    /** The style for bold text. */
    private final SimpleAttributeSet bold;

    /** The style for successful result. */
    private final SimpleAttributeSet blue;

    /** The style for failed result. */
    private final SimpleAttributeSet red;

    /**
     * The number of characters before the horizontal divider bar.
     * (The interactive console is composed of a JTextPane which contains 0 or more input/output pairs, followed
     * by horizontal divider bar, followed by an embedded sub-JTextPane (where the user can type in the next input))
     */
    private int len = 0;

    /** The main JTextPane containing 0 or more input/output pairs, followed by a horizontal bar, followed by this.sub */
    private final JTextPane main = do_makeTextPane(false, 5, 5, 5);

    /** The sub JTextPane where the user can type in the next command. */
    private final JTextPane sub;

    /** The history of all commands entered so far, plus an extra String representing the user's next command. */
    private final List<String> history = new ArrayList<String>(Arrays.asList(""));

    /** The position in this.history that is currently showing. */
    private int browse = 0;

    /** This helper method constructs a JTextPane with the given settings. */
    private JTextPane do_makeTextPane(boolean editable, int topMargin, int bottomMargin, int otherMargin) {
        JTextPane x = OurUtil.make(new JTextPane(), Color.BLACK, Color.WHITE, new Font("Verdana", Font.PLAIN, 16));
        x.setEditable(editable);
        x.setAlignmentX(0);
        x.setAlignmentY(0);
        x.setCaretPosition(0);
        x.setMargin(new Insets(topMargin, otherMargin, bottomMargin, otherMargin));
        return x;
    }

    /** This helper method enables cut/copy/paste using ctrl-{c,v,x,insert} and shift-{insert,delete} for this.main and this.sub */
    private void do_cutCopyPaste() {
        // Have to make sure only one of {input, output} has an active selection, or else it may confuse the user
        final Caret subCaret = sub.getCaret(), mainCaret = main.getCaret();
        subCaret.addChangeListener(new ChangeListener() {
           public void stateChanged(ChangeEvent e) {
              if (mainCaret.getMark() != mainCaret.getDot()) mainCaret.setDot(mainCaret.getDot());
           }
        });
        mainCaret.addChangeListener(new ChangeListener() {
           public void stateChanged(ChangeEvent e) {
              if (subCaret.getMark() != subCaret.getDot()) subCaret.setDot(subCaret.getDot());
           }
        });
        // now, create the 3 actions
        AbstractAction alloy_paste = new AbstractAction("alloy_paste") {
            private static final long serialVersionUID = 1L;
            public void actionPerformed(ActionEvent ev) { sub.paste(); }
         };
        AbstractAction alloy_copy = new AbstractAction("alloy_copy") {
           private static final long serialVersionUID = 1L;
           public void actionPerformed(ActionEvent ev) { if (sub.getSelectionStart()!=sub.getSelectionEnd()) sub.copy(); else main.copy(); }
        };
        AbstractAction alloy_cut = new AbstractAction("alloy_cut") {
           private static final long serialVersionUID = 1L;
           public void actionPerformed(ActionEvent ev) { if (sub.getSelectionStart()!=sub.getSelectionEnd()) sub.cut(); else main.copy(); }
        };
        // create the keyboard associations: ctrl-{c,v,x,insert} and shift-{insert,delete}
        for(int i=0; i<=1; i++) {
           InputMap  inputMap  = (i==0) ? sub.getInputMap()  : main.getInputMap();
           ActionMap actionMap = (i==0) ? sub.getActionMap() : main.getActionMap();
           actionMap.put("alloy_paste", alloy_paste);
           actionMap.put("alloy_copy",  alloy_copy);
           actionMap.put("alloy_cut",   alloy_cut);
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, Event.CTRL_MASK), "alloy_paste");
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, Event.CTRL_MASK), "alloy_copy");
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, Event.CTRL_MASK), "alloy_cut");
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, Event.SHIFT_MASK), "alloy_paste");
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, Event.CTRL_MASK),  "alloy_copy");
           inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, Event.SHIFT_MASK), "alloy_cut");
        }
    }

    /** Performs "page up" in the JScrollPane. */
    private void do_pageup() {
        JScrollBar b = getVerticalScrollBar();
        b.setValue(b.getValue() - 200);
    }

    /** Performs "page down" in the JScrollPane. */
    private void do_pagedown() {
        JScrollBar b = getVerticalScrollBar();
        b.setValue(b.getValue() + 200);
    }

    /** Insert the given text into the given location and with the given style; if "where" is -1, then we append the text. */
    private void do_add(int where, String text, AttributeSet style) {
        StyledDocument doc = main.getStyledDocument();
        try { doc.insertString(where >= 0 ? where : doc.getLength(), text, style); } catch(BadLocationException ex) { }
    }

    /** This method processes a user command. */
    private void do_command(Computer computer, String cmd) {
        cmd = cmd.trim();
        if (cmd.length()<=0) return;
        StyledDocument doc = main.getStyledDocument();
        if (history.size()>=2 && cmd.equals(history.get(history.size()-2))) {
           history.set(history.size()-1, "");
        } else {
           history.set(history.size()-1, cmd);
           history.add("");
        }
        browse = history.size()-1;
        // display the command
        int old = doc.getLength();
        do_add(len, cmd+"\n\n", plain);
        len = doc.getLength() - old + len;
        // perform the computation
        boolean bad = false;
        try { cmd=computer.compute(cmd); } catch(Throwable ex) { cmd=ex.toString(); bad=true; }
        int savePosition = len;
        // display the outcome
        old = doc.getLength();
        do_add(len, cmd.trim()+"\n\n", (bad ? red : blue));
        len = doc.getLength() - old + len;
        // indent the outcome
        main.setSelectionStart(savePosition+1);
        main.setSelectionEnd(len);
        main.setParagraphAttributes(blue, false);
        // redraw then scroll to the bottom
        invalidate(); repaint(); validate();
        sub.scrollRectToVisible(new Rectangle(0, sub.getY(), 1, sub.getHeight()));
        do_pagedown(); // need to do this after the validate() so that the scrollbar knows the new limit
    }

    /**
     * Construct a JScrollPane that allows the user to interactively type in commands and see replies.
     *
     * @param computer - this object is used to evaluate the user input
     *
     * @param initialMessages - this is a list of String and Boolean; each String is printed to the screen as is,
     * and Boolean.TRUE will turn subsequent text bold, and Boolean.FALSE will turn subsequent text non-bold.
     */
    public OurConsole(final Computer computer, final Object... initialMessages) {
        super(VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
        setViewportView(main);
        StyledDocument doc = main.getStyledDocument();
        // construct the various styles
        StyleConstants.setFontFamily(plain = new SimpleAttributeSet(), "Verdana"); StyleConstants.setFontSize(plain, 14);
        StyleConstants.setBold(bold = new SimpleAttributeSet(plain), true);
        StyleConstants.setForeground(blue = new SimpleAttributeSet(plain), Color.BLUE); StyleConstants.setLeftIndent(blue, 15);
        StyleConstants.setForeground(red = new SimpleAttributeSet(plain), Color.RED); StyleConstants.setLeftIndent(red, 15);
        // show the initial message
        SimpleAttributeSet st = plain;
        for(Object x: initialMessages) {
           if (x instanceof Boolean) st = ((Boolean)x) ? bold : plain;
           if (x instanceof String)  do_add(-1, (String)x, st);
        }
        do_add(-1, "\n", plain); // we must add a linebreak to ensure that subsequent text belong to a "different paragraph"
        // insert the divider and the sub JTextPane
        sub = do_makeTextPane(true, 10, 10, 0);
        final JPanel divider = new JPanel(); divider.setBackground(Color.LIGHT_GRAY); divider.setPreferredSize(new Dimension(1,1));
        final Style dividerStyle = doc.addStyle("divider", null); StyleConstants.setComponent(dividerStyle, divider);
        final Style inputStyle   = doc.addStyle("input",   null); StyleConstants.setComponent(inputStyle, sub);
        len = doc.getLength();
        do_add(-1, "x\n", dividerStyle);
        do_add(-1, "x\n", inputStyle);
        // enable cut+copy+paste
        do_cutCopyPaste();
        // configure so that, upon receiving focus, we automatically focus and scroll to the sub-JTextPane
        FocusListener focus = new FocusListener() {
           public void focusGained(FocusEvent e) {
              sub.requestFocusInWindow();
              sub.scrollRectToVisible(new Rectangle(0, sub.getY(), 1, sub.getHeight()));
           }
           public void focusLost(FocusEvent e) { }
        };
        addFocusListener(focus);
        sub.addFocusListener(focus);
        main.addFocusListener(focus);
        // configure so that mouse clicks in the main JTextPane will immediately transfer focus to the sub JTextPane
        main.addMouseListener(new MouseAdapter() {
           public void mousePressed(MouseEvent e) { sub.requestFocusInWindow(); }
           public void mouseClicked(MouseEvent e) { sub.requestFocusInWindow(); }
        });
        // configure the behavior for PAGE_UP, PAGE_DOWN, UP, DOWN, TAB, and ENTER
        sub.addKeyListener(new KeyListener() {
           public void keyTyped(KeyEvent e) {
              if (e.getKeyChar()=='\t') { e.consume(); }
              if (e.getKeyChar()=='\n') { e.consume(); String cmd = sub.getText(); sub.setText(""); do_command(computer, cmd); }
           }
           public void keyPressed(KeyEvent e) {
              if (e.getKeyCode()==KeyEvent.VK_ENTER || e.getKeyCode()==KeyEvent.VK_TAB) e.consume();
              if (e.getKeyCode()==KeyEvent.VK_PAGE_UP) { e.consume(); do_pageup(); }
              if (e.getKeyCode()==KeyEvent.VK_PAGE_DOWN) { e.consume(); do_pagedown(); }
              if (e.getKeyCode()==KeyEvent.VK_UP) {
                 e.consume();
                 if (browse==history.size()-1) { history.set(browse, sub.getText()); }
                 if (browse>0 && browse-1<history.size()) { browse--; sub.setText(history.get(browse)); }
              }
              if (e.getKeyCode()==KeyEvent.VK_DOWN) {
                 e.consume();
                 if (browse<history.size()-1) { browse++; sub.setText(history.get(browse)); }
              }
           }
           public void keyReleased(KeyEvent e) {
              if (e.getKeyCode()==KeyEvent.VK_ENTER || e.getKeyCode()==KeyEvent.VK_TAB) e.consume();
           }
        });
    }
}
