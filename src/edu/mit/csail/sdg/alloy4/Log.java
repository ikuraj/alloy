package edu.mit.csail.sdg.alloy4;

/**
 * This logger is the superclass of all loggers; it ignores all calls and simply does nothing.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 */

public class Log {

    /** Constructs a logger that ignores all calls and simply logs nothing. */
    public Log() { }

    /**
     * This method is supposed to write msg into the log
     * (but this default implementation does nothing).
     */
    public void log(String msg) { }

    /**
     * This method is supposed to write msg into the log in a bold style (if possible)
     * (but this default implementation does nothing).
     */
    public void logBold(String msg) { }

    /**
     * This method is supposed to write msg into the log as a clickable link (if possible)
     * (but this default implementation does nothing).
     */
    public void logLink(String msg) { }

    /**
     * This method is supposed to commit all outstanding writes (if the logger is buffered)
     * (but this default implementation does nothing).
     */
    public void flush() { }

    /**
     * This method is supposed to return the current size of the log
     * (but this default implementation always returns 0).
     */
    public int getLength() { return 0; }

    /**
     * This method is supposed to truncate the log if it's longer than the given length
     * (but this default implementation does nothing).
     */
    public void setLength(int newLength) { }
}
