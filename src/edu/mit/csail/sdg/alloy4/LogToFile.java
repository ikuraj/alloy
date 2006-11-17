package edu.mit.csail.sdg.alloy4;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;

/**
 * This logger writes the messages using UTF-8 into a file
 * (which will be overwritten if it exists).
 *
 * Since the output is plain text, logBold() and logLink() simply call log().
 *
 * <p/><b>Thread Safety:</b>  Safe.
 */

public final class LogToFile extends Log {

    /** The RandomAccessFile object for the output file. */
    private final RandomAccessFile file;

    /** Records whether a failure has occurred or not. */
    private IOException error=null;

    /** Records the current length of the file. */
    private int length=0;

    /**
     * Creates a logger that logs to "filename" (which will be overwritten if it exists).
     * @param filename - the filename to log to
     * @throws IOException - if the file could not be opened for some reason
     */
    public LogToFile(String filename) throws IOException {
        file=new RandomAccessFile(filename,"rw");
        file.setLength(0);
    }

    /** Writes msg into the log. */
    @Override public synchronized void log(String msg) {
        if (error!=null) return;
        try {
            byte[] bytes=msg.getBytes("UTF-8");
            int newlength=length+bytes.length;
            if (newlength<0) {error=new IOException("File length overflow."); return;}
            length=newlength;
            try {file.write(bytes);} catch(IOException ex) {error=ex;}
        } catch (UnsupportedEncodingException e) {
            error=new IOException("UTF8 encoding error: "+e.getMessage());
        }
    }

    /** Simply calls log(msg) since text files don't support bold styles. */
    @Override public synchronized void logBold(String msg) { log(msg); }

    /** Simply calls log(msg) since text files don't support hyperlinks. */
    @Override public synchronized void logLink(String msg, String linkDestination) { log(msg); }

    /** Commits all outstanding writes (if the logger is buffered). */
    @Override public synchronized void flush() { }

    /** Returns the current length of document. */
    @Override public synchronized int getLength() { return length; }

    /** Truncate the file if it is longer than the given length. */
    @Override public synchronized void setLength(int newLength) {
        if (error==null) {
            try { if (length>newLength) { file.setLength(newLength); length=newLength; } }
            catch(IOException ex) { error=ex; }
        }
    }
}
