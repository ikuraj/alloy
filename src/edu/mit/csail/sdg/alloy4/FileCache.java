package edu.mit.csail.sdg.alloy4;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.LinkedHashMap;
import java.util.Map;

/** Mutable; this class caches all the files you read. */

public final class FileCache {

    /** This caches the content of each file. */
    private final Map<String,String> file2content=new LinkedHashMap<String,String>();

    /** Constructs a cache with no initial cached content. */
    public FileCache() { }
    
    /** Explicitly cache the content of a file. */
    public void add(String filename, String content) { file2content.put(filename,content); }

    /** If the file was already cached, return the cached value; Otherwise read it, cache it, then return it. */
    public Reader readFile(String filename) throws IOException {
        filename=Util.canon(filename);
        String value=file2content.get(filename);
        while(value==null) {
            FileInputStream fis=null;
            InputStreamReader isr=null;
            int now=0, max=0;
            char[] buf;
            try {
                fis=new FileInputStream(filename);
                isr=new InputStreamReader(fis,"ISO8859_1");
                buf=new char[max];
                while(true) {
                   if (max<=now) {
                        long fn=(new File(filename)).length();
                        int max2=((int)fn)+16;
                        if (max2<16 || ((long)(max2-16))!=fn) throw new IOException("File too big to fit in memory");
                        char[] buf2=new char[max2];
                        if (now>0) System.arraycopy(buf, 0, buf2, 0, now);
                        buf=buf2;
                        max=max2;
                   }
                   int n=isr.read(buf, now, max-now);
                   if (n<=0) break;
                   now+=n;
                }
            } catch(IOException ex) {
                if (isr!=null) { try {isr.close();} catch(IOException ex2) {} }
                if (fis!=null) { try {fis.close();} catch(IOException ex2) {} }
                throw ex;
            }
            value=new String(buf, 0, now);
            file2content.put(filename, value);
        }
        return new StringReader(value);
    }

    /** Clears all currently cached results. */
    public void clearCache() { file2content.clear(); }
}
