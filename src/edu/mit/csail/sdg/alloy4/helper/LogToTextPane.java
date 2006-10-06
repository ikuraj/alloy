package edu.mit.csail.sdg.alloy4.helper;

import java.lang.reflect.InvocationTargetException;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;
import edu.mit.csail.sdg.alloy4util.Util;

/**
 * This logger will append the messages into an existing JTextPane.
 *
 * <p/><b>Thread Safety:</b>  Safe (Though it could deadlock if the AWT thread somehow gets blocked by other threads)
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

    /** An internal buffer used to cache messages. */
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
    @Override public synchronized void log(final String msg) {
        if (latestStyle==defaultStyle) { buffer.append(msg); return; }
        flush();
        latestStyle=defaultStyle;
        buffer.append(msg);
    }

    /** Writes msg into the log in a bold style. */
    @Override public synchronized void logBold(final String msg) {
        if (latestStyle==boldStyle) { buffer.append(msg); return; }
        flush();
        latestStyle=boldStyle;
        buffer.append(msg);
    }

    /** Flushes the buffered text (if any) to the JTextPane. */
    @Override public synchronized void flush() {
        if (buffer.length()==0) return;
        String content=buffer.toString();
        Style style=(latestStyle==boldStyle ? boldStyle : defaultStyle);
        buffer.setLength(0);
        realFlush(pane,content,style);
    }

    /**
     * This method performs the actual GUI operation.
     * Since this method may switch threads to use the AWT thread,
     * we made sure this method only takes in final references to GUI objects and String objects.
     */
    private static void realFlush(final JTextPane pane, final String content, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            // GUI methods must only be called by the event dispatch thread
            try {
                SwingUtilities.invokeAndWait(new Runnable() {
                    public final void run() { realFlush(pane,content,style); }
                });
            } catch(InterruptedException ex) {
                // Should not happen. Util.harmless() method could choose to log this occurrence.
                Util.harmless("LogToTextPane.realFlush()", ex);
            } catch(InvocationTargetException ex) {
                // Should not happen. Util.harmless() method could choose to log this occurrence.
                Util.harmless("LogToTextPane.realFlush()", ex);
            }
            return;
        }
        StyledDocument doc=pane.getStyledDocument();
        try {
            doc.insertString(doc.getLength(), content, style);
        } catch (BadLocationException ex) {
            // Should not happen. Util.harmless() method could choose to log this occurrence.
            Util.harmless("LogToTextPane.realFlush()", ex);
        }
        pane.setCaretPosition(doc.getLength());
    }
}
