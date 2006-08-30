package edu.mit.csail.sdg.alloy4.core;

/**
 * This is a minimum implementation of a log.
 *
 * <p/> In particular, this implementation does nothing: writes to it are ignored,
 * and there is no way to read the log, and therefore every method does nothing.
 *
 * <p/> Client code should subclass Logger to satisfy their logging needs.
 *
 * @author Felix Chang
 */

public class Log {
	
	/** Adds x into the log. */
    public void log(String x) {}
    
    /** Adds x into the log in a bold style */ 
    public void logBold(String x) {}
    
    /** Commits all outstanding writes (if the logger is buffered) */
    public void flush() {}
}
