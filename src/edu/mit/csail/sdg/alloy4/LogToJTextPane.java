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
 * This logger writes messages into a JTextPane.
 *
 * <p> Its constructor and its public methods can be called by any thread.
 *
 * <p><b>Thread Safety:</b>  Safe
 * (as long as the rest of the code follows the important
 * general rule that the AWT event thread must never block on other threads)
 */

public final class LogToJTextPane extends Log {

    /**
     * This field buffers previous calls to log() so that we can write them out later in a single Swing call
     * (If there is nothing buffered, this field can be an empty list or even null).
     *
     * <p> This field must be synchronized by LogToJTextPane.this
     */
    private List<String> batch = null;

    /** The newly created JTextPane object that will display the log. */
    private JTextPane log;

    /** The style to use when writing regular messages. */
    private Style styleRegular;

    /** The style to use when writing bold messages. */
    private Style styleBold;

    /** The style to use when writing red messages. */
    private Style styleRed;

    /** This stores the JLabels used for displaying hyperlinks. */
    private final List<JLabel> links = new ArrayList<JLabel>();

    /**
     * When the window gains focus, we'll call handler.run(focus);
     * When a hyperlink is clicked, we'll call handler.run(click, linkURL).
     */
    private final MultiRunnable handler;

    /** When the window gains focus, we'll call handler.run(focus). */
    private final int focus;

    /** When a hyperlink is clicked, we'll call handler.run(click, URL). */
    private final int click;

    /** The current length of the log, not counting any "red" error message at the end of the log. */
    private int lastSize=0;

    /** The current font name. */
    private String fontName;

    /** The current font size. */
    private int fontSize;

    /** The color to use for hyperlinks when the mouse is not hovering over it. */
    private static final Color linkColor = new Color(0.3f, 0.3f, 0.9f);

    /** The color to use for a hyperlink when the mouse is hovering over it. */
    private static final Color hoverColor = new Color(0.9f, 0.3f, 0.3f);

    /** This stores a default ViewFactory that will handle the View requests that we don't care to override. */
    private static final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();

    /**
     * Constructs a new JTextPane logger, and put it inside an existing JScrollPane.
     * @param parent - the existing JScrollPane to insert the JTextPane into
     * @param fontName - the font family to use
     * @param fontSize - the font size to use
     * @param background - the background color to use
     * @param regular - the color to use for regular messages
     * @param red - the color to use for red messages
     * @param handler - the function to call when this log panel gets the focus or a hyperlink is clicked
     * @param focus - When the window gains focus, we'll call handler.run(focus)
     * @param click - when a hyperlink is clicked, we'll call handler.run(click, linkURL)
     */
    public LogToJTextPane(
            final JScrollPane parent, String fontName, int fontSize,
            final Color background, final Color regular, final Color red,
            final MultiRunnable handler, final int focus, final int click) {
        this.handler=handler;
        this.focus=focus;
        this.click=click;
        this.fontName=fontName;
        this.fontSize=fontSize;
        // We must make sure the GUI calls are done from the AWT event thread.
        if (SwingUtilities.isEventDispatchThread()) {
            initialize(parent, background, regular, red);
            return;
        }
        OurUtil.invokeAndWait(new Runnable() {
            public final void run() {
                initialize(parent, background, regular, red);
            }
        });
    }

    /**
     * Helper method that actually initializes the various GUI objects;
     * This method can be called only by the AWT event thread.
     */
    private void initialize(JScrollPane parent, Color background, Color regular, Color red) {
        log=new JTextPane();
        // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
        // 30000 is a good number; value higher than about 32768 will cause errors.
        log.setEditorKit(new StyledEditorKit() {
            private static final long serialVersionUID = 1L;
            @Override public final ViewFactory getViewFactory() {
                return new ViewFactory() {
                    public final View create(Element x) {
                        if (!AbstractDocument.SectionElementName.equals(x.getName())) {
                            return defaultFactory.create(x);
                        }
                        return new BoxView(x, View.Y_AXIS) {
                            @Override public final float getMinimumSpan(int axis) {
                                return super.getPreferredSpan(axis);
                            }
                            @Override public final void layout(int width,int height) {
                                super.layout(30000, height);
                            }
                        };
                    }
                };
            }
        });
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(background);
        log.setEditable(false);
        log.setFont(new Font(fontName, Font.PLAIN, fontSize));
        log.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { handler.run(focus); }
            public final void focusLost(FocusEvent e) { }
        });
        StyledDocument doc=log.getStyledDocument();
        styleRegular=doc.addStyle("regular", null);
        StyleConstants.setFontFamily(styleRegular, fontName);
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        styleRed=doc.addStyle("red", styleBold);
        StyleConstants.setForeground(styleRed, red);
        parent.setViewportView(log);
        parent.setBackground(background);
    }

    /** Write a horizontal separator into the log window. */
    public void logDivider() {
        handle("logDivider");
    }

    /** Write a clickable link into the log window. */
    @Override public void logLink(String link, String linkDestination) {
        handle("logLink", 0, null, link, linkDestination, null);
    }

    /** Write "msg" in regular style. */
    @Override public synchronized void log(String msg) {
        if (batch==null) {
            batch=new ArrayList<String>();
        }
        batch.add(msg);
    }

    /** Write "msg" in bold style. */
    @Override public void logBold(String msg) {
        handle("log", 0, null, msg, "", styleBold);
    }

    /** Write "msg" in red style (with automatic line wrap). */
    public void logRed (String msg) {
        StringBuilder sb=new StringBuilder();
        while (msg.length()>0) {
            int i = msg.indexOf('\n');
            if (i>=0) {
                linewrap(sb, msg.substring(0,i));
                sb.append('\n');
                msg=msg.substring(i+1);
            } else {
                linewrap(sb, msg);
                break;
            }
        }
        handle("log", 0, null, sb.toString(), "", styleRed);
    }

    /** Write "msg" in regular style (with automatic line wrap). */
    public void logIndented (String msg) {
        StringBuilder sb=new StringBuilder();
        while(msg.length()>0) {
            int i = msg.indexOf('\n');
            if (i>=0) {
                linewrap(sb, msg.substring(0,i));
                sb.append('\n');
                msg=msg.substring(i+1);
            } else {
                linewrap(sb, msg);
                break;
            }
        }
        handle("log", 0, null, sb.toString(), "", styleRegular);
    }

    /** Try to wrap the input to about 60 characters per line; however, if a token is too long, we won't break it. */
    private static void linewrap(StringBuilder sb, String msg) {
        StringTokenizer tokenizer=new StringTokenizer(msg,"\r\n\t ");
        final int max=60;
        int now=0;
        while(tokenizer.hasMoreTokens()) {
            String x=tokenizer.nextToken();
            if (now+1+x.length() > max) {
                if (now>0) {
                    sb.append('\n');
                }
                sb.append(x);
                now=x.length();
            } else {
                if (now>0) {
                    now++;
                    sb.append(' ');
                }
                sb.append(x);
                now=now+x.length();
            }
        }
    }

    /** Set the font name. */
    public void setFontName(String fontName) {
        handle("setFontName",0,null,fontName,"",null);
    }

    /** Set the font size. */
    public void setFontSize(int fontSize) {
        handle("setFontSize",fontSize);
    }

    /** Set the background color. */
    public void setBackground(Color background) {
        handle("setBackground",0,background,"","",null);
    }

    /** Query the current length of the log. */
    @Override public int getLength() {
        if (SwingUtilities.isEventDispatchThread()) {
            handle("flush");
            return log.getStyledDocument().getLength();
        }
        final IntegerBox box=new IntegerBox(0);
        OurUtil.invokeAndWait(new Runnable() {
            public final void run() {
                handle("flush");
                int length = log.getStyledDocument().getLength();
                synchronized(box) {
                    box.set(length);
                }
            }
        });
        synchronized(box) {
            return box.get();
        }
    }

    /** Truncate the log to the given length; if the log is shorter than the number given, then nothing happens. */
    @Override public void setLength(int newLength) {
        handle("setLength",newLength);
    }

    /** Removes any messages writtin in "red" style. */
    public void clearError() {
        handle("clearError");
    }

    /** This method copies the currently selected text in the log (if any) into the clipboard. */
    public void copy() {
        handle("copy");
    }

    /** Commits all outstanding writes (if the messages are buffered). */
    @Override public void flush() {
        handle("flush");
    }

    /** Handles the given message (and switch to AWT event thread if the current thread != the AWT event thread). */
    private void handle
    (final String op, final int arg, final Color color, final String text, final String text2, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public void run() {
                    handle(op,arg,color,text,text2,style);
                }
            });
            return;
        }
        List<String> batch=null;
        synchronized(this) {
            batch=this.batch;
            this.batch=null;
        }
        if (batch!=null) {
            for(String msg: batch) {
                handle("log", 0, null, msg, "", styleRegular);
            }
        }
        if (op=="setLength") {
            StyledDocument doc=log.getStyledDocument();
            int n=doc.getLength();
            if (n<=arg) return;
            try {
                doc.remove(arg, n-arg);
            } catch (BadLocationException e) {
                // Harmless
            }
            if (lastSize>doc.getLength()) {
                lastSize=doc.getLength();
            }
        }
        if (op=="clearError") {
            // Since this class always removes "red" messages prior to writing anything,
            // that means if there are any red messages, they will always be at the end of the JTextPane.
            StyledDocument doc=log.getStyledDocument();
            int n=doc.getLength();
            if (n<=lastSize) return;
            try {
                doc.remove(lastSize, n-lastSize);
            } catch (BadLocationException e) {
                // Harmless
            }
        }
        if (op=="setFontSize" || op=="setFontName") {
            // Changes the settings for future writes into the log
            if (op == "setFontSize") {
                this.fontSize = arg;
            } else {
                this.fontName = text;
            }
            log.setFont(new Font(fontName, Font.PLAIN, fontSize));
            StyleConstants.setFontSize(styleRegular, fontSize);
            StyleConstants.setFontSize(styleBold, fontSize);
            StyleConstants.setFontSize(styleRed, fontSize);
            // Changes all existing text
            StyledDocument doc=log.getStyledDocument();
            Style temp=doc.addStyle("temp", null);
            StyleConstants.setFontFamily(temp, fontName);
            StyleConstants.setFontSize(temp, fontSize);
            doc.setCharacterAttributes(0, doc.getLength(), temp, false);
            // Changes all existing hyperlinks
            Font newFont = new Font(fontName, Font.BOLD, fontSize);
            for(JLabel link: links) {
                link.setFont(newFont);
            }
        }
        if (op=="logLink") {
            clearError();
            StyledDocument doc=log.getStyledDocument();
            Style linkStyle=doc.addStyle("link", styleRegular);
            final JLabel label=new JLabel(text);
            label.setAlignmentY(0.8f);
            label.setMaximumSize(label.getPreferredSize());
            label.setFont(new Font(fontName, Font.BOLD, fontSize));
            label.setForeground(linkColor);
            label.addMouseListener(new MouseListener(){
                public final void mousePressed(MouseEvent e) { handler.run(click, text2); }
                public final void mouseClicked(MouseEvent e) { }
                public final void mouseReleased(MouseEvent e) { }
                public final void mouseEntered(MouseEvent e) { label.setForeground(hoverColor); }
                public final void mouseExited(MouseEvent e) { label.setForeground(linkColor); }
            });
            StyleConstants.setComponent(linkStyle, label);
            links.add(label);
            handle("log", 0, null, ".", "", linkStyle); // Any character would do; the "." will be replaced by the JLabel
            log.setCaretPosition(doc.getLength());
            lastSize = doc.getLength();
        }
        if (op=="logDivider") {
            clearError();
            StyledDocument doc = log.getStyledDocument();
            Style dividerStyle = doc.addStyle("bar", styleRegular);
            JPanel jpanel = new JPanel();
            jpanel.setBackground(Color.LIGHT_GRAY);
            jpanel.setPreferredSize(new Dimension(300,1)); // 300 is arbitrary, since it will auto-stretch
            StyleConstants.setComponent(dividerStyle, jpanel);
            handle("log", 0, null, ".","",dividerStyle); // Any character would do; "." will be replaced by the JPanel
            handle("log", 0, null, "\n\n","", styleRegular);
            log.setCaretPosition(doc.getLength());
            lastSize = doc.getLength();
        }
        if (op=="log") {
            clearError();
            int i=text.lastIndexOf('\n'), j=text.lastIndexOf('\r');
            if (i>=0 && i<j) {
                i=j;
            }
            StyledDocument doc=log.getStyledDocument();
            try {
                if (i<0) {
                    doc.insertString(doc.getLength(), text, style);
                } else {
                    // Performs intelligent caret positioning
                    doc.insertString(doc.getLength(), text.substring(0,i+1), style);
                    log.setCaretPosition(doc.getLength());
                    if (i<text.length()-1) {
                        doc.insertString(doc.getLength(), text.substring(i+1), style);
                    }
                }
            } catch (BadLocationException e) {
                // Harmless
            }
            if (style!=styleRed) {
                lastSize=doc.getLength();
            }
        }
        if (op=="copy") {
            log.copy();
        }
        if (op=="setBackground") {
            log.setBackground(color);
        }
    }

    /** Convenience method for calling handle() */
    private void handle(final String op) {
        handle(op,0,null,"","",null);
    }

    /** Convenience method for calling handle() */
    private void handle(final String op, final int arg) {
        handle(op,arg,null,"","",null);
    }
}
