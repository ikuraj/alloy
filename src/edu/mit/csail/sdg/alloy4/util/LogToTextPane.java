package edu.mit.csail.sdg.alloy4.util;

import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

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
    	if (!SwingUtilities.isEventDispatchThread()) {
    		try {
				SwingUtilities.invokeAndWait(new Runnable() {
				    public void run() { log(msg); }
				});
			} catch (Exception e) {
				return; // If this happens, so be it.
			}
			return;
    	}
        StyledDocument doc=pane.getStyledDocument();
        try {
            doc.insertString(doc.getLength(), msg, defaultStyle);
        } catch (BadLocationException e) {
        	return; // Should not happen
        }
        pane.setCaretPosition(doc.getLength());
    }

    /** Writes msg into the log in a bold style. */
    @Override public void logBold(final String msg) {
    	if (!SwingUtilities.isEventDispatchThread()) {
    		try {
				SwingUtilities.invokeAndWait(new Runnable() {
				    public void run() { logBold(msg); }
				});
			} catch (Exception e) {
				return; // If this happened, so be it.
			}
			return;
    	}
        StyledDocument doc=pane.getStyledDocument();
        try {
            doc.insertString(doc.getLength(), msg, boldStyle);
        } catch (BadLocationException e) {
        	return; // Should not happen
        }
        pane.setCaretPosition(doc.getLength());
    }

    /** This method does nothing (since JTextPane buffering, if any, is beyond our control). */
    @Override public void flush() { }
}
