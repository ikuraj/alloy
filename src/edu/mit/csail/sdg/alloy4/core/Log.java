package edu.mit.csail.sdg.alloy4.core;

/**
 * This inteface defines what a logger can do.
 *
 * @author Felix Chang
 */

public interface Log {

    /** Writes x into the log. */
    public void log(String x);

    /** Writes x into the log in a bold style (if possible) */
    public void logBold(String x);

    /** Commits all outstanding writes (if the logger is buffered) */
    public void flush();
}
