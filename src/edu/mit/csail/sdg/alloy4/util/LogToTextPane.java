package edu.mit.csail.sdg.alloy4.util;

import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;
import edu.mit.csail.sdg.alloy4util.Util;

/**
 * This logger will append the messages into an existing JTextPane.
 *
 * @author Felix Chang
 */

public final class LogToTextPane extends Log {

    /** The JTextPane object to send the messages to. */
    private final JTextPane pane;

    /** The style to use when printing default messages. */
    private final Style defaultStyle;

    /** The style to use when printing bold messages. */
    private final Style boldStyle;

    /** An internal buffer, used to cache the messages. */
    private final StringBuilder buffer=new StringBuilder();

    /** If buffer.length()!=0, then this is the style to use to write the buffer out. */
    private Style latestStyle=null;

    /**
     * Creates a logger that appends messages into an existing JTextPane object.
     *
     * @param pane - the JTextPane to send the messages to
     * @param defaultStyle - the style to use when printing default messages
     * @param boldStyle - the style to use when printing bold messages
     */
    public LogToTextPane(JTextPane pane, Style defaultStyle, Style boldStyle) {
        this.pane=pane;
        this.defaultStyle=defaultStyle;
        this.boldStyle=boldStyle;
    }

    /** Writes msg into the log. */
    @Override public void log(final String msg) {
        if (latestStyle==defaultStyle) { buffer.append(msg); return; }
        flush();
        latestStyle=defaultStyle;
        buffer.append(msg);
    }

    /** Writes msg into the log in a bold style. */
    @Override public void logBold(final String msg) {
        if (latestStyle==boldStyle) { buffer.append(msg); return; }
        flush();
        latestStyle=boldStyle;
        buffer.append(msg);
    }

    /** Flushes the buffered text (if any) to the JTextPane. */
    @Override public void flush() {
        if (buffer.length()==0) return;
        final String content=buffer.toString();
        final Style style=(latestStyle==boldStyle ? boldStyle : defaultStyle);
        buffer.setLength(0);
        if (SwingUtilities.isEventDispatchThread()) { realFlush(pane,content,style); return; }
        try {
            SwingUtilities.invokeAndWait(new Runnable() {
                public final void run() { realFlush(pane,content,style); }
            });
        } catch (Exception e) {
            // Should not happen. Added "Util.harmless()" to silence FindBugs's warning about ignoring exception
            Util.harmless("LogToTextPane.flush()", e);
        }
    }

    /**
     * This method performs the actual GUI operation.
     * Note: this method must only be called from the AWT Event Dispatch Thread.
     * Also, since this method might be executed from code other than the main data thread,
     * we made sure this method doesn't access any fields, just final handles to GUI or String objects.
     */
    private static void realFlush(final JTextPane pane, final String content, final Style style) {
        StyledDocument doc=pane.getStyledDocument();
        try {
            doc.insertString(doc.getLength(), content, style);
        } catch (BadLocationException e) {
            // Should not happen. Added "Util.harmless()" to silence FindBugs's warning about ignoring exception
            Util.harmless("LogToTextPane.realFlush()",e);
        }
        pane.setCaretPosition(doc.getLength());
    }
}
