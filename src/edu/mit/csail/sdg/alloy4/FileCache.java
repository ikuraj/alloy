package edu.mit.csail.sdg.alloy4;

import java.io.IOException;
import java.io.Reader;

/** Implements a file-to-content cache. */

public interface FileCache {

    /** True if this cache contains value for the given filename. */
    public boolean contains(String filename);

    /** If the file was already cached, return the cached value; else return null. */
    public Reader readFile(String filename) throws IOException, Err;
}
