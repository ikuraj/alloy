package edu.mit.csail.sdg.alloy4.helper;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import edu.mit.csail.sdg.alloy4util.Func1;
import edu.mit.csail.sdg.alloy4util.OurUtil;
import edu.mit.csail.sdg.alloy4util.Util;

/**
 * This logger will log the messages into a JTextPane.
 *
 * <p/> Its constructor and its public methods can be called by any thread,
 *      though some of its private methods can be called only by the AWT thread.
 *
 * <p/><b>Thread Safety:</b>  Safe (as long as the AWT thread never blocks on other threads)
 *
 * @author Felix Chang
 */

public final class LogToJTextPane extends Log {

    /**
     * The JTextPane object that will display the log.
     * Since it's a GUI object, only the AWT thread may call its methods.
     */
    private JTextPane log;

    /**
     * The styles to use when writing regular messages, bold messages, and red messages.
     * Since these are GUI objects, only the AWT thread may call its methods.
     */
    private Style styleRegular, styleBold, styleRed;

    /**
     * The color to use as the background of the JTextPane.
     * Since it's a GUI object, only the AWT thread may call its methods.
     */
    private final Color background;

    /**
     * The method to call when user clicks on one of the hyperlink in the JTextPane;
     * Func1's specification insists that only the AWT thread may call its methods.
     */
    private final Func1 action;

    /**
     * The current length of the log (not counting any "red" error message at the end of the log).
     * For simplicity, we insist that only the AWT thread may read and write this field.
     */
    private int lastSize=0;

    /**
     * Constructs a new JTextPane logger, and put it inside an existing JScrollPane.
     * @param parent - the JScrollPane to insert the JTextPane into
     * @param background - the background color to use for the JTextPane
     * @param regular - the color to use for regular messages
     * @param bold - the color to use for bold messages
     * @param red - the color to use for red messages
     * @param action - the function to call when users click on a hyperlink message
     * (as required by Func1's specification, we guarantee we will only call action.run() from the AWT thread).
     */
    public LogToJTextPane(final JScrollPane parent,
            Color background, final Color regular, final Color bold, final Color red, Func1 action) {
        this.background=background;
        this.action=action;
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { initialize(parent,regular,bold,red); }
            });
            return;
        }
        initialize(parent,regular,bold,red);
    }

    /**
     * Helper method that actually initializes the various GUI objects;
     * This method can be called only by the AWT thread.
     */
    private void initialize(JScrollPane parent, Color regular, Color bold, Color red) {
        log=new JTextPane();
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(background);
        log.setEditable(false);
        log.setFocusable(false);
        log.setFont(OurUtil.getFont());
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        styleRegular=doc.addStyle("regular", old);
        StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        StyleConstants.setForeground(styleBold,bold);
        styleRed=doc.addStyle("red", styleBold);
        StyleConstants.setForeground(styleRed,red);
        parent.setViewportView(log);
        parent.setBackground(background);
    }

    /** Print a text message into the log window using the provided style. */
    private void log(final String msg, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { log(msg,style); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        try { doc.insertString(doc.getLength(), msg, style); }
        catch (BadLocationException e) { Util.harmless("log()",e); }
        log.setCaretPosition(doc.getLength());
        if (style!=styleRed) lastSize=doc.getLength();
    }


    /** Write "msg" in regular style. */
    @Override public void log(String msg) { log(msg,styleRegular); }

    /** Write "msg" in bold style. */
    @Override public void logBold(String msg) { log(msg,styleBold); }

    /** Write "msg" in red style. */
    public void logRed(String msg) { log(msg,styleRed); }

    /** Write msg1 into the log window using the red style, then write msg2 using the default style. */
    public void log(final String msg1, final String msg2) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { log(msg1,msg2); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        try {
            doc.insertString(doc.getLength(),msg1,styleRed);
            doc.insertString(doc.getLength(),msg2,styleRegular);
        }
        catch (BadLocationException e) {Util.harmless("log()",e);}
        log.setCaretPosition(doc.getLength());
    }

    /** Write a clickable button into the log window. */
    @Override public void logLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JButton b=new JButton("Click here to see the result");
        if (Util.onMac()) b.setBackground(background);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont());
        b.setForeground(Color.BLUE);
        b.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { action.run(link); }
        });
        StyleConstants.setComponent(s,b);
        log(" ",s);
        lastSize=doc.getLength();
    }

    /** Write a clickable text link into the log window. */
    public void logTextLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logTextLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        final JLabel b=new JLabel("[Click here to see the result]");
        final Color linkColor=new Color(0.3f, 0.3f, 0.9f), hoverColor=new Color(.9f, .3f, .3f);
        b.setAlignmentY(0.8f);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont().deriveFont(Font.BOLD));
        b.setForeground(linkColor);
        b.addMouseListener(new MouseListener(){
            public final void mouseClicked(MouseEvent e) { action.run(link); }
            public final void mousePressed(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { b.setForeground(hoverColor); }
            public final void mouseExited(MouseEvent e) { b.setForeground(linkColor); }
        });
        StyleConstants.setComponent(s, b);
        log(" ",s);
        lastSize=doc.getLength();
    }

    /** Erase the entire log. */
    public void clear() {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { clear(); } });
            return;
        }
        log.setText("");
        lastSize=0;
    }

    /**
     * Helper method that removes any messages writtin in "red" style.
     * This method can be called only from the AWT thread.
     */
    private void clearError() {
        // Since this class always removes "red" messages prior to writing anything,
        // that means if there are any red messages, they will always be at the end of the JTextPane.
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=lastSize) return;
        try {doc.remove(lastSize,n-lastSize);}
        catch (BadLocationException e) {Util.harmless("log",e);}
    }

    /** This method does nothing, since changes to a JTextPane will always show up automatically. */
    @Override public void flush() { }
}
