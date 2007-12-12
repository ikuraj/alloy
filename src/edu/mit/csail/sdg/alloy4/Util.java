/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Locale;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.prefs.Preferences;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/**
 * This provides useful static methods for I/O and XML operations.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Util {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private Util() { }

    /**
     * This reads and writes String-valued Java persistent preferences.
     * <p><b>Thread Safety:</b>  Safe.
     */
    public static final class StringPref {
        /** The id associated with this preference. */
        private final String id;
        /** The default value for this preference. */
        private final String defaultValue;
        /** Constructs a new StringPref object with the given id. */
        public StringPref (String id) {this.id=id; this.defaultValue="";}
        /** Constructs a new StringPref object with the given id and the given default value. */
        public StringPref (String id, String defaultValue) {this.id=id; this.defaultValue=defaultValue;}
        /** Sets the value for this preference. */
        public void set (String value) { Preferences.userNodeForPackage(Util.class).put(id,value); }
        /** Reads the value for this preference; if not set or is empty, we return the default value. */
        public String get () {
            String ans=Preferences.userNodeForPackage(Util.class).get(id,"");
            return (ans==null || ans.length()==0) ? defaultValue : ans;
        }
    }

    /**
     * This reads and writes boolean-valued Java persistent preferences.
     * <p><b>Thread Safety:</b>  Safe.
     */
    public static final class BooleanPref {
        /** The id associated with this preference. */
        private final String id;
        /** Constructurs a new BooleanPref object with the given id. */
        public BooleanPref (String id) { this.id=id; }
        /** Sets the value for this preference. */
        public void set (boolean value) { Preferences.userNodeForPackage(Util.class).put(id, value?"y":""); }
        /** Reads the value for this preference; if not set, we return false. */
        public boolean get () { return Preferences.userNodeForPackage(Util.class).get(id,"").length()>0; }
    }

    /**
     * This reads and writes integer-valued Java persistent preferences.
     * <p><b>Thread Safety:</b>  Safe.
     */
    public static final class IntPref {
        /** The id associated with this preference. */
        private final String id;
        /** The minimum value for this preference. */
        private final int min;
        /** The maximum value for this preference. */
        private final int max;
        /** The default value for this preference. */
        private final int def;
        /** If min>n, we return min; else if n>max, we return max; otherwise we return n. */
        private int bound (int n) { return n<min ? min : (n>max? max : n); }
        /** Constructs a new IntPref object with the given id; you must ensure max >= min, but def does not have to be between min..max */
        public IntPref (String id, int min, int def, int max) {this.id=id; this.min=min; this.def=def; this.max=max;}
        /** Sets the value for this preference. */
        public void set (int value) { Preferences.userNodeForPackage(Util.class).putInt(id,bound(value)); }
        /** Reads the value for this preference; if not set, we return the default value. */
        public int get () {
            int n;
            String t=Preferences.userNodeForPackage(Util.class).get(id,"");
            if (t==null || t.length()==0) return def;
            try { n=Integer.parseInt(t); } catch(NumberFormatException ex) { return def; }
            return bound(n);
        }
    }

    /** Copy the input list, append "element" to it, then return the result as a unmodifiable list. */
    public static<T> ConstList<T> append(List<T> list, T element) {
        TempList<T> ans=new TempList<T>(list.size()+1);
        ans.addAll(list);
        ans.add(element);
        return ans.makeConst();
    }

    /** Copy the input list, prepend "element" to it, then return the result as a unmodifiable list. */
    public static<T> ConstList<T> prepend(List<T> list, T element) {
        TempList<T> ans=new TempList<T>(list.size()+1);
        ans.add(element);
        ans.addAll(list);
        return ans.makeConst();
    }

    /** Copy the input list, remove a single instance of "element" if exists, then return the result as a unmodifiable list. */
    public static<T> ConstList<T> remove(List<T> list, T element) {
        for(int i=0, n=list.size(); i<n; i++) {
            T x=list.get(i);
            if (x==element || (x!=null && x.equals(element))) {
                TempList<T> newlist=new TempList<T>(list.size()-1);
                for(int j=0; j<n; j++) { if (i!=j) newlist.add(list.get(j)); }
                return newlist.makeConst();
            }
        }
        return ConstList.make(list);
    }

    /** Returns an unmodifiable List with same elements as the array. */
    public static<T> ConstList<T> asList(T... array) {
        TempList<T> ans = new TempList<T>(array.length);
        for(int i=0; i<array.length; i++) { ans.add(array[i]); }
        return ans.makeConst();
    }

    /** Return an iterable whose iterator is a read-only iterator that first iterate over Collection1, and then Collection2. */
    public static<E> Iterable<E> fastJoin(final Iterable<E> collection1, final Iterable<E> collection2) {
       return new Iterable<E>() {
          public Iterator<E> iterator() {
             return new Iterator<E> () {
                private Iterator<E> a=collection1.iterator(), b=collection2.iterator();
                public boolean hasNext() {
                   if (a!=null) { if (a.hasNext()) return true; a=null; }
                   if (b!=null) { if (b.hasNext()) return true; b=null; }
                   return false;
                }
                public E next() {
                   if (a!=null) { if (a.hasNext()) return a.next(); a=null; }
                   if (b!=null) { if (b.hasNext()) return b.next(); b=null; }
                   throw new NoSuchElementException();
                }
                public void remove() { throw new UnsupportedOperationException(); }
             };
          }
       };
    }

    /** Helper method that converts Windows/Mac/Unix linebreaks into "\n" */
    public static String convertLineBreak(String input) {
        return input.replace("\r\n","\n").replace('\r','\n');
    }

    /**
     * Attempt to close the file/stream/reader/writer and return true if and only if we successfully closed it.
     * (If object==null, we return true right away)
     */
    public static boolean close(Closeable object) {
        if (object==null) return true;
        boolean ans=true;
        try {
            if (object instanceof PrintStream && ((PrintStream)object).checkError()) ans=false;
            if (object instanceof PrintWriter && ((PrintWriter)object).checkError()) ans=false;
            object.close();
            return ans;
        } catch(Throwable ex) {
            return false;
        }
    }

    /** This synchronized field stores the current "default directory" which is used by the FileOpen and FileSave dialogs. */
    private static String currentDirectory = canon(System.getProperty("user.home"));

    /** Modifies the current "default directory" which is used by the FileOpen and FileSave dialogs. */
    public synchronized static void setCurrentDirectory(File newDirectory) {
        if (newDirectory==null) // this can actually happen
            currentDirectory = canon(System.getProperty("user.home"));
        else
            currentDirectory = canon(newDirectory.getAbsolutePath());
    }

    /** Returns the current "default directory" which is used by the FileOpen and FileSave dialogs. */
    public synchronized static String getCurrentDirectory() {
        return currentDirectory;
    }

    /** Read the file then return the file size and a byte[] array (the array could be slightly bigger than file size) */
    private static Pair<byte[],Integer> readEntireFile(boolean fromJar, String filename) throws FileNotFoundException, IOException {
        InputStream fis=null;
        int now=0, max=4096;
        if (!fromJar) {
            long maxL=new File(filename).length();
            max=(int)maxL;
            if ((long)max != maxL) throw new IOException("File too big to fit in memory");
        }
        byte[] buf;
        try {
            buf=new byte[max];
            fis = fromJar ? Util.class.getClassLoader().getResourceAsStream(filename) : new FileInputStream(filename);
            if (fis==null) throw new FileNotFoundException("File \""+filename+"\" cannot be found");
            while(true) {
                if (now >= max) {
                    max=now+4096;
                    if (max<now) throw new IOException("File too big to fit in memory"); // since the new array size cannot fit in an "int"
                    byte[] buf2=new byte[max];
                    if (now>0) System.arraycopy(buf, 0, buf2, 0, now);
                    buf=buf2;
                }
                int r = fis.read(buf, now, max-now);
                if (r<0) break;
                now = now + r;
            }
        } catch(OutOfMemoryError ex) {
            System.gc();
            throw new IOException("There is insufficient memory.");
        } finally {
            close(fis);
        }
        return new Pair<byte[],Integer>(buf,now);
    }

    /** Read everything into a String; throws IOException if an error occurred. */
    public static String readAll(boolean fromJar, String filename) throws FileNotFoundException, IOException {
        final CodingErrorAction r=CodingErrorAction.REPORT;
        final Pair<byte[],Integer> p=readEntireFile(fromJar,filename);
        ByteBuffer bbuf;
        String ans="";
        try {
            // We first try UTF-8;
            bbuf=ByteBuffer.wrap(p.a, 0, p.b);
            ans=Charset.forName("UTF-8").newDecoder().onMalformedInput(r).onUnmappableCharacter(r).decode(bbuf).toString();
        } catch(CharacterCodingException ex) {
            try {
                // if that fails, we try using the platform's default charset
                bbuf=ByteBuffer.wrap(p.a, 0, p.b);
                ans=Charset.defaultCharset().newDecoder().onMalformedInput(r).onUnmappableCharacter(r).decode(bbuf).toString();
            } catch(CharacterCodingException ex2) {
                // if that also fails, we try using "ISO-8859-1" which should always succeed but may map characters wrong
                bbuf=ByteBuffer.wrap(p.a, 0, p.b);
                ans=Charset.forName("ISO-8859-1").newDecoder().onMalformedInput(r).onUnmappableCharacter(r).decode(bbuf).toString();
            }
        }
        return ans;
    }

    /** Read everything into a String; throws IOException if an error occurred. */
    public static String readAll(String filename) throws FileNotFoundException, IOException { return readAll(false,filename); }

    /** Open then overwrite the file with the given content; throws Err if an error occurred. */
    public static void writeAll(String filename, String content) throws Err {
        final FileOutputStream fos;
        try {
            fos=new FileOutputStream(filename);
        } catch(IOException ex) {
            throw new ErrorFatal("Cannot write to the file "+filename);
        }
        // Convert the line break into the UNIX line break
        content = convertLineBreak(content);
        // If the last line does not have a LINEBREAK, add it
        if (content.length()>0 && content.charAt(content.length()-1)!='\n') content=content+"\n";
        // Now, convert the line break into the local platform's line break, then write it to the file
        try {
            final String NL=System.getProperty("line.separator");
            fos.write(content.replace("\n",NL).getBytes("UTF-8"));
            fos.close();
        } catch(IOException ex) {
            close(fos);
            throw new ErrorFatal("Cannot write to the file "+filename);
        }
    }

    /**
     * Returns the canonical absolute path for a file.
     * If an IO error occurred, or if the file doesn't exist yet,
     * we will at least return a noncanonical but absolute path for it.
     * <p> Note: if filename=="", we return "".
     */
    public static final String canon(String filename) {
        if (filename==null || filename.length()==0) return "";
        File file=new File(filename);
        String answer;
        try {
            answer=file.getCanonicalPath();
        } catch(IOException ex) {
            // Hopefully this shouldn't happen
            answer=file.getAbsolutePath();
        }
        return answer;
    }

    /**
     * Sorts two strings for optimum module order; we guarantee slashComparator(a,b)==0 iff a.equals(b).
     * <br> (1) First of all, the builtin names "extend" and "in" are sorted ahead of other names
     * <br> (2) Else, if one string has fewer '/' than the other, then it is considered smaller.
     * <br> (3) Else, if one string starts with "this/", then it is considered smaller
     * <br> (4) Else, we compare them lexically without case-sensitivity.
     * <br> (5) Finally, we compare them lexically with case-sensitivity.
     */
    public static final Comparator<String> slashComparator = new Comparator<String>() {
        public final int compare(String a, String b) {
            if (a==null) return (b==null)?0:-1;
            if (b==null) return 1;
            if (a.equals("extends")) return a.equals(b) ? 0 : -1;
            if (b.equals("extends")) return 1;
            if (a.equals("in")) return a.equals(b) ? 0 : -1;
            if (b.equals("in")) return 1;
            int acount=0, bcount=0;
            for(int i=0; i<a.length(); i++) { if (a.charAt(i)=='/') acount++; }
            for(int i=0; i<b.length(); i++) { if (b.charAt(i)=='/') bcount++; }
            if (acount!=bcount) return (acount<bcount)?-1:1;
            if (a.startsWith("this/")) {
                if (!b.startsWith("this/")) return -1;
            } else if (b.startsWith("this/")) {
                return 1;
            }
            int result = a.compareToIgnoreCase(b);
            return result!=0 ? result : a.compareTo(b);
        }
    };

    /**
     * Copy the given file from JAR into the destination file; if the destination file exists, we then do nothing.
     * Returns true iff a file was created and written.
     */
    private static synchronized boolean copy(String sourcename, String destname) {
        File destfileobj=new File(destname);
        if (destfileobj.isFile() && destfileobj.length()>0) return false;
        boolean result=true;
        InputStream res=null;
        InputStream in=null;
        FileOutputStream out=null;
        try {
            res=Util.class.getClassLoader().getResourceAsStream(sourcename);
            if (res==null) return false; // This means the file is not relevant for this setup, so we don't pop up a fatal dialog
            in=new BufferedInputStream(res);
            out=new FileOutputStream(destname);
            byte[] b=new byte[16384];
            while(true) {
                int numRead = in.read(b);
                if (numRead < 0) break;
                if (numRead > 0) out.write(b, 0, numRead);
            }
        } catch (IOException e) {
            result=false;
        }
        if (!close(out)) result=false;
        if (!close(in)) result=false;
        if (!close(res)) result=false;
        if (!result) OurDialog.fatal(null,"Error occurred in creating the file \""+destname+"\"");
        return true;
    }

    /**
     * Copy the list of files from JAR into the destination directory,
     * then set the correct permissions on them if possible.
     *
     * @param executable - if true, we will attempt to set the file's "executable" permission (failure to do this is ignored)
     * @param keepPath - if true, the full path will be created for the destination file
     * @param destdir - the destination directory
     * @param names - the files to copy from the JAR
     */
    public static synchronized void copy(boolean executable, boolean keepPath, String destdir, String... names) {
        String[] args=new String[names.length+2];
        args[0]="/bin/chmod"; // This does not work on Windows, but the "executable" bit is not needed on Windows anyway.
        args[1]=(executable ? "700" : "600"); // 700 means read+write+executable; 600 means read+write.
        int j=2;
        for(int i=0; i<names.length; i++) {
            String name=names[i];
            String destname=name;
            if (!keepPath) { int ii=destname.lastIndexOf('/'); if (ii>=0) destname=destname.substring(ii+1); }
            destname=(destdir+'/'+destname).replace('/', File.separatorChar);
            int last=destname.lastIndexOf(File.separatorChar);
            new File(destname.substring(0,last+1)).mkdirs(); // Error will be caught later by the file copy
            if (copy(name, destname)) { args[j]=destname; j++; }
        }
        if (onWindows() || j<=2) return;
        String[] realargs=new String[j];
        for(int i=0; i<j; i++) realargs[i]=args[i];
        try {
            Runtime.getRuntime().exec(realargs).waitFor();
        } catch (Throwable ex) {
            // We only intend to make a best effort
        }
    }

    /**
     * Write a String into a PrintWriter, and encode special characters using XML-specific encoding.
     *
     * <p>
     * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
     * into "&amp;lt;" "&amp;gt;" "&amp;amp;" "&amp;apos;" and "&amp;quot;" and turns any characters
     * outside of the 32..126 range into the "&amp;#xHHHH;" encoding
     * (where HHHH is the 4 digit lowercase hexadecimal representation of the character value).
     *
     * @param out - the PrintWriter to write into
     * @param str - the String to write out
     */
    public static void encodeXML(PrintWriter out, String str) {
        int n=str.length();
        for(int i=0; i<n; i++) {
            char c=str.charAt(i);
            if (c=='<') { out.write("&lt;"); continue; }
            if (c=='>') { out.write("&gt;"); continue; }
            if (c=='&') { out.write("&amp;"); continue; }
            if (c=='\'') { out.write("&apos;"); continue; }
            if (c=='\"') { out.write("&quot;"); continue; }
            if (c>=32 && c<=126) { out.write(c); continue; }
            out.write("&#x");
            String v=Integer.toString((int)c, 16);
            for(int j=v.length(); j<4; j++) out.write('0');
            out.write(v);
            out.write(';');
        }
    }

    /**
     * Write a list of Strings into a PrintWriter, where strs[2n] are written as-is, and strs[2n+1] are XML-encoded.
     *
     * <p> For example, if you call encodeXML(out, A, B, C, D, E), it is equivalent to the following:
     * <br> out.print(A);
     * <br> out.encodeXML(B);
     * <br> out.print(C);
     * <br> out.encodeXML(D);
     * <br> out.print(E);
     * <br> In other words, it writes the even entries as-is, and writes the odd entries using XML encoding.
     *
     * @param out - the PrintWriter to write into
     * @param strs - the list of Strings to write out
     */
    public static void encodeXMLs(PrintWriter out, String... strs) {
        for(int i=0; i<strs.length; i++) {
            if ((i%2)==0) out.print(strs[i]); else encodeXML(out,strs[i]);
        }
    }

    /**
     * Finds the first occurrence of <b>small</b> within <b>big</b>.
     * @param big - the String that we want to perform the search on
     * @param small - the pattern we are looking forward
     * @param start - the offset within "big" to start (for example: 0 means to start from the beginning of "big")
     * @param forward - true if the search should go forward; false if it should go backwards
     * @param caseSensitive - true if the search should be done in a case-sensitive manner
     *
     * @return 0 or greater if found, -1 if not found (Note: if small=="", then we always return -1)
     */
    public static int indexOf(String big, String small, int start, boolean forward, boolean caseSensitive) {
        int len=big.length(), slen=small.length();
        if (slen==0) return -1;
        while(start>=0 && start<len) {
            for(int i=0 ; ; i++) {
                if (i>=slen) return start;
                if (start+i>=len) break;
                int b=big.charAt(start+i), s=small.charAt(i);
                if (!caseSensitive && b>='A' && b<='Z') b=(b-'A')+'a';
                if (!caseSensitive && s>='A' && s<='Z') s=(s-'A')+'a';
                if (b!=s) break;
            }
            if (forward) start++; else start--;
        }
        return -1;
    }

    /** Returns true iff running on Windows **/
    public static boolean onWindows() {
        return System.getProperty("os.name").toLowerCase(Locale.US).startsWith("windows");
    };

    /** Returns true iff running on Mac OS X. **/
    public static boolean onMac() {
        return System.getProperty("mrj.version")!=null || System.getProperty("os.name").toLowerCase(Locale.US).startsWith("mac ");
    }

    /** Returns the substring after the last "/" */
    public static String tail(String string) { int i=string.lastIndexOf('/'); return (i<0) ? string : string.substring(i+1); }
}
