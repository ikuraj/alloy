package edu.mit.csail.sdg.alloy4;

/**
 * This logger writes messages into an internal StringBuilder.
 *
 * Since the output is plain String, logBold() and logLink() simply call log().
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public class LogToStringBuilder extends Log {

    /** The internal StringBuilder. */
    private final StringBuilder sb = new StringBuilder();

    /** Constructs a logger that logs into an internal StringBuilder. */
    public LogToStringBuilder() { }

    /**
     * This method writes msg into the log.
     */
    @Override public synchronized void log(String msg) {
        sb.append(msg);
    }

    /**
     * This implementation simply calls log(msg).
     */
    @Override public synchronized void logBold(String msg) {
        sb.append(msg);
    }

    /**
     * This implementation simply calls log(msg).
     */
    @Override public synchronized void logLink(String msg, String linkDestination) {
        sb.append(msg);
    }

    /**
     * This implementation does nothing.
     */
    @Override public void flush() { }

    /**
     * This method returns the current size of the log.
     */
    @Override public synchronized int getLength() {
        return sb.length();
    }

    /**
     * This method truncates the log if it's longer than the given length.
     */
    @Override public synchronized void setLength(int newLength) {
        if (sb.length() > newLength) {
            sb.setLength(newLength);
        }
    }

    /**
     * This method retrieves the current content of the log.
     */
    @Override public synchronized String toString() {
        return sb.toString();
    }
}
