package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
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

import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;

/**
 * This logger writes the messages into a JTextPane.
 *
 * <p/> Its constructor and its public methods can be called by any thread.
 *
 * <p/><b>Thread Safety:</b>  Safe
 * (as long as we follow the important general rule that the AWT thread must never block on other threads)
 */

public final class LogToJTextPane extends Log {

    /**
     * Mutable; this class holds a mutable integer.
     * <p/><b>Thread Safety:</b> Safe
     */
    private static final class IntegerBox {
        /** The integer value. */
        private int value;
        /** Constructs an IntegerBox with an initial value of 0. */
        public IntegerBox() {value=0;}
        /** Reads the current value. */
        public synchronized int getValue() {return value;}
        /** Sets the current value. */
        public synchronized void setValue(int newValue) {value=newValue;}
    }

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

    /**
     * When the window gains focus, we'll call handler.run(ev_focus);
     * When a hyperlink is clicked, we'll call handler.run(evs_click, linkURL);
     */
    private final MultiRunnable handler;

    /** When the window gains focus, we'll call handler.run(ev_focus). */
    private final int ev_focus;

    /** When a hyperlink is clicked, we'll call handler.run(evs_click, linkURL). */
    private final int evs_click;

    /** The current length of the log, not counting any "red" error message at the end of the log. */
    private int lastSize=0;

    /** The current font size. */
    private int fontSize;

    /** The color to use for hyperlinks. */
    private static final Color linkColor=new Color(0.3f, 0.3f, 0.9f);

    /** The color to use for a hyperlink when the mouse is hovering over it. */
    private static final Color hoverColor=new Color(.9f, .3f, .3f);

    /** This stores a default ViewFactory that will handle the View requests that we don't care about. */
    private static final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();

    /**
     * Constructs a new JTextPane logger, and put it inside an existing JScrollPane.
     * @param parent - the JScrollPane to insert the JTextPane into
     * @param fontSize - the font size to start with
     * @param background - the background color to use for the JTextPane
     * @param regular - the color to use for regular messages
     * @param red - the color to use for red messages
     * @param focusAction - the function to call when this log panel gets the focus
     * (as required by Func0's specification, we guarantee we will only call focusAction.run() from the AWT thread).
     * @param clickAction - the function to call when users click on a hyperlink message
     * (as required by Func1's specification, we guarantee we will only call clickAction.run() from the AWT thread).
     */
    public LogToJTextPane(final JScrollPane parent, final int fontSize,
            final Color background, final Color regular, final Color red,
            final MultiRunnable handler, final int ev_focus, final int evs_click) {
        this.handler=handler;
        this.ev_focus=ev_focus;
        this.evs_click=evs_click;
        this.fontSize=fontSize;
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { initialize(parent,background,regular,red); }
            });
            return;
        }
        initialize(parent,background,regular,red);
    }

    /**
     * Helper method that actually initializes the various GUI objects;
     * This method can be called only by the AWT thread.
     */
    private void initialize(JScrollPane parent,Color background,Color regular,Color red) {
        log=new JTextPane();
        // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
        // 30000 is a good number; value higher than about 32768 will cause errors.
        log.setEditorKit(new StyledEditorKit() {
            private static final long serialVersionUID = 1L;
            @Override public final ViewFactory getViewFactory() { return new ViewFactory() {
                public final View create(Element x) {
                    if (!AbstractDocument.SectionElementName.equals(x.getName())) return defaultFactory.create(x);
                    return new BoxView(x, View.Y_AXIS) {
                        @Override public final float getMinimumSpan(int axis) { return super.getPreferredSpan(axis); }
                        @Override public final void layout(int width,int height) { super.layout(30000,height); }
                    };
                }
            };}
        });
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(background);
        log.setEditable(false);
        log.setFont(OurUtil.getFont(fontSize));
        log.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { handler.run(ev_focus); }
            public final void focusLost(FocusEvent e) { }
        });
        StyledDocument doc=log.getStyledDocument();
        styleRegular=doc.addStyle("regular", null);
        StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        styleRed=doc.addStyle("red", styleBold);
        StyleConstants.setForeground(styleRed,red);
        parent.setViewportView(log);
        parent.setBackground(background);
    }

    /** Write a horizontal separator into the log window. */
    public void logDivider() {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { logDivider(); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style dividerStyle=doc.addStyle("bar", styleRegular);
        JPanel jpanel=new JPanel();
        jpanel.setBackground(Color.LIGHT_GRAY);
        jpanel.setPreferredSize(new Dimension(300,1)); // 300 is arbitrary, since it will auto-stretch
        StyleConstants.setComponent(dividerStyle, jpanel);
        log(".",dividerStyle); // Any character here would do; the "." will instead be replaced by the JPanel
        log("\n\n",styleRegular);
        log.setCaretPosition(doc.getLength());
        lastSize=doc.getLength();
    }

    /** Write a clickable link into the log window. */
    @Override public void logLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { logLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style linkStyle=doc.addStyle("link", styleRegular);
        final JLabel label=new JLabel("Visualize");
        label.setAlignmentY(0.8f);
        label.setMaximumSize(label.getPreferredSize());
        label.setFont(OurUtil.getFont(fontSize).deriveFont(Font.BOLD));
        label.setForeground(linkColor);
        label.addMouseListener(new MouseListener(){
            public final void mouseClicked(MouseEvent e) { handler.run(evs_click, link); }
            public final void mousePressed(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { label.setForeground(hoverColor); }
            public final void mouseExited(MouseEvent e) { label.setForeground(linkColor); }
        });
        StyleConstants.setComponent(linkStyle, label);
        links.add(label);
        log(".",linkStyle); // Any character here would do; the "." will instead be replaced by the JLabel
        log.setCaretPosition(doc.getLength());
        lastSize=doc.getLength();
    }

    /** Write "msg" in regular style. */
    @Override public void log(String msg) { log(msg,styleRegular); }

    /** Write "msg" in bold style. */
    @Override public void logBold(String msg) { log(msg,styleBold); }

    /** Write "msg" in red style. */
    public void logRed(String msg) {
        StringBuilder sb=new StringBuilder();
        while(msg.length()>0) {
            int i=msg.indexOf('\n');
            if (i>=0) { linewrap(sb, msg.substring(0,i)); sb.append('\n'); msg=msg.substring(i+1); }
            else { linewrap(sb,msg); break; }
        }
        log(sb.toString(), styleRed);
    }

    /** Write "msg" using the provided style. */
    private void log(final String msg, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { log(msg,style); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        try { doc.insertString(doc.getLength(), msg, style); }
        catch (BadLocationException e) { Util.harmless("Should not happen",e); }
        log.setCaretPosition(doc.getLength());
        if (style!=styleRed) lastSize=doc.getLength();
    }

    /** Try to wrap the input to about 50 characters per line; however, if a token is too longer, we won't break it. */
    public void linewrap(StringBuilder sb, String msg) {
        StringTokenizer tokenizer=new StringTokenizer(msg,"\r\n\t ");
        int max=50;
        int now=0;
        while(tokenizer.hasMoreTokens()) {
            String x=tokenizer.nextToken();
            if (now+1+x.length() > max) { if (now>0) sb.append('\n'); sb.append(x); now=x.length(); }
            else { if (now>0) {now++; sb.append(' ');} sb.append(x); now=now+x.length(); }
        }
    }

    /** Set the font size. */
    public void setFontSize(final int fontSize) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { setFontSize(fontSize); } });
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
        for(JLabel link:links) link.setFont(newFont);
    }

    /** Set the background color. */
    public void setBackground(final Color background) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { setBackground(background); } });
            return;
        }
        log.setBackground(background);
    }

    /** Query the current length of the log, and store it into the IntegerBox object. */
    private void getLength(final IntegerBox answer) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { getLength(answer); } });
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

    /** Truncate the log to the given length; if the log is shorter than the number given, then nothing happens. */
    @Override public void setLength(final int newLength) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { setLength(newLength); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=newLength) return;
        try {doc.remove(newLength, n-newLength);}
        catch (BadLocationException e) {Util.harmless("This shouldn't happen",e);}
        if (lastSize>doc.getLength()) lastSize=doc.getLength();
    }

    /** Removes any messages writtin in "red" style. */
    public void clearError() {
        // Since this class always removes "red" messages prior to writing anything,
        // that means if there are any red messages, they will always be at the end of the JTextPane.
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { clearError(); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=lastSize) return;
        try {doc.remove(lastSize,n-lastSize);}
        catch (BadLocationException e) {Util.harmless("This shouldn't happen",e);}
    }

    /** This method copies the currently selected text in the log (if any) into the clipboard. */
    public void copy() {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public void run() { copy(); } });
            return;
        }
        log.copy();
    }

    /** This method does nothing, since changes to a JTextPane will always show up automatically. */
    @Override public void flush() { }
}
