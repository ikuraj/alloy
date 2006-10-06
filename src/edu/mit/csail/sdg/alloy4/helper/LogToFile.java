package edu.mit.csail.sdg.alloy4.helper;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * This logger will log the messages into a new text file
 * (which will be overwritten if it exists).
 *
 * Since the output is plain text, the logBold() and log() methods are the same.
 *
 * @author Felix Chang
 */

public final class LogToFile extends Log {

    /** The PrintWriter object for the output file (null if the log has been closed). */
    private PrintWriter file;

    /**
     * Creates a logger that logs to "filename" (which will be overwritten if it exists).
     * @param filename - the filename to log to
     * @throws IOException - if the file could not be opened for some reason
     */
    public LogToFile(String filename) throws IOException {
        try { file=new PrintWriter(filename); }
        catch(FileNotFoundException ex) { throw new IOException(ex.getMessage()); }
    }

    /** Writes msg into the log. */
    @Override public void log(String x) {
        if (file!=null) file.print(x);
    }

    /** Writes msg into the log (just like log() since text files don't support bold styles). */
    @Override public void logBold(String x) {
        if (file!=null) file.print(x);
    }

    /** Commits all outstanding writes (if the logger is buffered). */
    @Override public void flush() {
        if (file!=null) file.flush();
    }

    /**
     * This method flushes then closes the file
     * (after this, further calls to this logger will be ignored).
     */
    public void close() {
        if (file!=null) { file.flush(); file.close(); file=null; }
    }
}
