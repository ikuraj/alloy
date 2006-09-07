package edu.mit.csail.sdg.alloy4.util;

/**
 * This default logger ignores all calls and simply does nothing.
 *
 * @author Felix Chang
 */

public class Log {

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
     * This method is supposed to commit all outstanding writes (if the logger is buffered)
     * (but this default implementation does nothing).
     */
    public void flush() { }
}
