package edu.mit.csail.sdg.alloy4.helper;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import edu.mit.csail.sdg.alloy4util.Func0;
import edu.mit.csail.sdg.alloy4util.Func1;
import edu.mit.csail.sdg.alloy4util.OurUtil;
import edu.mit.csail.sdg.alloy4util.Util;

/**
 * This logger will log the messages into a JTextPane.
 *
 * <p/> Its constructor and its public methods can be called by any thread.
 *
 * <p/><b>Thread Safety:</b>  Safe (as long as the AWT thread never blocks on other threads)
 *
 * @author Felix Chang
 */

public final class LogToJTextPane extends Log {

    /** The newly created JTextPane object that will display the log. */
    private JTextPane log;

    /** The style to use when writing regular messages. */
    private Style styleRegular;

    /** The style to use when writing bold messages. */
    private Style styleBold;

    /** The style to use when writing red messages. */
    private Style styleRed;

    /** This stores the JLabels used for displaying the hyperlinks. */
    private List<JLabel> links=new ArrayList<JLabel>();

    /** The color to use as the background of the JTextPane. */
    private Color background;

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
     * The current font size.
     * For simplicity, we insist that only the AWT thread may read and write this field.
     */
    private int fontSize;

    /** Set the font size. */
    public void setFontSize(final int fontSize) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { setFontSize(fontSize); } });
            return;
        }
        // Changes the settings for future writes into the log
        this.fontSize=fontSize;
        log.setFont(OurUtil.getFont(fontSize));
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setFontSize(styleBold, fontSize);
        StyleConstants.setFontSize(styleRed, fontSize);
        // Changes all existing text
        StyledDocument doc=log.getStyledDocument();
        Style temp=doc.addStyle("temp", null);
        StyleConstants.setFontFamily(temp, OurUtil.getFontName());
        StyleConstants.setFontSize(temp, fontSize);
        doc.setCharacterAttributes(0, doc.getLength(), temp, false);
        // Changes all existing hyperlinks
        Font newFont=new Font(OurUtil.getFontName(), Font.BOLD, fontSize);
        for(JLabel x:links) x.setFont(newFont);
    }

    /** Set the background color. */
    public void setBackground(final Color newBackground) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { setBackground(newBackground); } });
            return;
        }
        background=newBackground;
        log.setBackground(background);
    }

    /**
     * Constructs a new JTextPane logger, and put it inside an existing JScrollPane.
     * @param parent - the JScrollPane to insert the JTextPane into
     * @param fontSize - the font size to start with
     * @param background - the background color to use for the JTextPane
     * @param regular - the color to use for regular messages
     * @param red - the color to use for red messages
     * @param focusAction - the function to call when this log window gets the focus
     * @param clickAction - the function to call when users click on a hyperlink message
     * (as required by Func1's specification, we guarantee we will only call action.run() from the AWT thread).
     */
    public LogToJTextPane(final JScrollPane parent, final int fontSize,
            Color background, final Color regular, final Color red,
            final Func0 focusAction,
            final Func1 clickAction) {
        this.background=background;
        this.action=clickAction;
        this.fontSize=fontSize;
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { initialize(parent,regular,red,focusAction); }
            });
            return;
        }
        initialize(parent,regular,red,focusAction);
    }

    /** This customized EditorKit forces the JTextPane to use our customized StyledViewFactory. */
    private static final class OurEditorKit extends StyledEditorKit {
        /** This suppresses javac's warning about missing serialVersionUID. */
        private static final long serialVersionUID = 1L;
        /** This method returns our customized ViewFactory rather than the default ViewFactory. */
        @Override public ViewFactory getViewFactory() { return new OurViewFactory(); }
    }

    /**
     * This customized StyledViewFactory gives prevents line-wrapping up to 30000 pixels;
     * value higher than about 32768 gives an error.
     */
    private static class OurViewFactory implements ViewFactory {
        /** This stores a default ViewFactory that will handle the requests we don't care about. */
        private static final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();
        /** This implementation prevents line-wrapping up to 30000 pixels wide. */
        public View create(Element x) {
            String name=x.getName();
            if (name!=null && name.equals(AbstractDocument.SectionElementName)) {
                return new BoxView(x, View.Y_AXIS) {
                    @Override public void layout(int width, int height) { super.layout(30000,height); }
                    @Override public float getMinimumSpan(int axis) { return super.getPreferredSpan(axis); }
                };
            }
            return defaultFactory.create(x);
        }
    }

    /**
     * Helper method that actually initializes the various GUI objects;
     * This method can be called only by the AWT thread.
     */
    private void initialize(JScrollPane parent, Color regular, Color red, final Func0 focusAction) {
        log=new JTextPane();
        log.setEditorKit(new OurEditorKit()); // This customized EditorKit prevents line-wrapping.
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(background);
        log.setEditable(false);
        log.setFont(OurUtil.getFont(fontSize));
        log.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { focusAction.run(); }
            public final void focusLost(FocusEvent e) { }
        });
        StyledDocument doc=log.getStyledDocument();
        styleRegular=doc.addStyle("regular", null);
        StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        StyleConstants.setForeground(styleBold, regular);
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

    /** Write a horizontal separator into the log window. */
    public void logDivider() {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logDivider(); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JPanel jp = new JPanel();
        jp.setBackground(Color.LIGHT_GRAY);
        jp.setPreferredSize(new Dimension(300,1)); // 300 is arbitrary, since it will auto-stretch
        StyleConstants.setComponent(s, jp);
        log(".",s);
        log("\n\n",styleRegular);
        lastSize=doc.getLength();
    }

    /** Write a clickable link into the log window. */
    @Override public void logLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        final JLabel b=new JLabel(label);
        final Color linkColor=new Color(0.3f, 0.3f, 0.9f), hoverColor=new Color(.9f, .3f, .3f);
        b.setAlignmentY(0.8f);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont(fontSize).deriveFont(Font.BOLD));
        b.setForeground(linkColor);
        b.addMouseListener(new MouseListener(){
            public final void mouseClicked(MouseEvent e) { action.run(link); }
            public final void mousePressed(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { b.setForeground(hoverColor); }
            public final void mouseExited(MouseEvent e) { b.setForeground(linkColor); }
        });
        StyleConstants.setComponent(s, b);
        links.add(b);
        log(" ",s);
        lastSize=doc.getLength();
    }

    private static final class IntegerBox {
        private int value;
        public IntegerBox() {value=0;}
        public synchronized int getValue() { return value; }
        public synchronized void setValue(int newValue) { value=newValue; }
    }

    /** Query the current length of the log. */
    public void getLength(final IntegerBox answer) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { getLength(answer); } });
            return;
        }
        int length=log.getStyledDocument().getLength();
        answer.setValue(length);
    }

    /** Query the current length of the log. */
    @Override public int getLength() {
        IntegerBox box=new IntegerBox();
        getLength(box);
        return box.getValue();
    }

    /** Truncate the log to the given length; if the log is smaller than the number given, then nothing happens. */
    @Override public void setLength(final int newLength) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { setLength(newLength); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=newLength) return;
        try {doc.remove(newLength,n-newLength);}
        catch (BadLocationException e) {Util.harmless("log",e);}
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

    /** Must only be accessed by the AWT thread. */
    private String label="Visualize";

    /** Change the label on all the hyperlinks, and remember it as the default label for the next hyperlink. */
    public void changeLinkLabel(final String label) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { changeLinkLabel(label); } });
            return;
        }
        for(JLabel x:links) x.setText(label);
        this.label=label;
    }

    /** Removes any messages writtin in "red" style. */
    public void clearError() {
        // Since this class always removes "red" messages prior to writing anything,
        // that means if there are any red messages, they will always be at the end of the JTextPane.
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { clearError(); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=lastSize) return;
        try {doc.remove(lastSize,n-lastSize);}
        catch (BadLocationException e) {Util.harmless("log",e);}
    }

    /** This method does nothing, since changes to a JTextPane will always show up automatically. */
    @Override public void flush() { }

    /** This method copies the currently selected text in the log (if any) into the clipboard. */
    public void copy() {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { copy(); } });
            return;
        }
        log.copy();
    }
}
