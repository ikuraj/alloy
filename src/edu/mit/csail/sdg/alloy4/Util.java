package edu.mit.csail.sdg.alloy4;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * This provides useful static methods for I/O and XML operations.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Util {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private Util() { }

    /** Copy the input list, append "element" to it, then return the result as a unmodifiable list. */
    public static<T> List<T> add(List<T> list, T element) {
        list=new ArrayList<T>(list);
        list.add(element);
        return Collections.unmodifiableList(list);
    }

    /** Copy the input list, remove a single instance of "element", then return the result as a unmodifiable list. */
    public static<T> List<T> remove(List<T> list, T element) {
        list=new ArrayList<T>(list);
        list.remove(element);
        return Collections.unmodifiableList(list);
    }

    /** Read everything into a String; throws IOException if an error occurred. */
    public static String readAll(String filename) throws IOException {
        Pair<char[],Integer> p = readEntireFile(filename);
        char[] a = p.a;
        int b = p.b;
        while(b>0 && a[b-1]>=0 && a[b-1]<=32) {
            b--; // Remove trailing whitespace
        }
        if (b==0) {
            return "";
        }
        if (b<a.length) {
            a[b]='\n';
            return new String(a,0,b+1);
        }
        return (new String(a,0,b))+"\n";
    }

    /** Open then overwrite the file with the given content; throws IOException if an error occurred. */
    public static void writeAll(String filename, String content) throws IOException {
        FileWriter fw=new FileWriter(filename);
        BufferedWriter bw=new BufferedWriter(fw);
        PrintWriter out=new PrintWriter(bw);
        BufferedReader rd=new BufferedReader(new StringReader(content));
        while(true) {
            try {
                String line=rd.readLine();
                if (line==null) {
                    break;
                }
                out.println(line);
            } catch(IOException ex) {
                out.close();
                out=null;
                break;
            }
        }
        if (out!=null) {
            out.flush();
            out.close();
        }
        bw.close();
        fw.close();
        rd.close();
    }

    /**
     * Returns the canonical absolute path for a file.
     * If an IO error occurred, or if the file doesn't exist yet,
     * we will at least return a noncanonical but absolute path for it.
     * <p> Note: if filename=="", we return "".
     */
    public static final String canon(String filename) {
      if (filename.length()==0) {
          return filename;
      }
      File file=new File(filename);
      String answer;
      try {
          answer=file.getCanonicalPath();
      } catch(IOException ex) {
          answer=file.getAbsolutePath();
      }
      return answer;
    }

    /**
     * Sorts two strings for optimum module order; we guarantee slashCompartor(a,b)==0 iff a.equals(b).
     * <br> (1) First of all, the builtin names "extend" and "in" are sorted ahead of other names
     * <br> (2) Else, if one string has fewer '/' than the other, then it is considered smaller.
     * <br> (3) Else, if one string starts with "this/", then it is considered smaller
     * <br> (4) Else, we compare them lexically without case-sensitivity.
     * <br> (5) Else, we compare them lexically with case-sensitivity.
     * <br>
     */
    public static final Comparator<String> slashComparator = new Comparator<String>() {
        public final int compare(String a, String b) {
            if (a==null) {
                return (b==null)?0:-1;
            }
            if (b==null) {
                return 1;
            }
            if (a.equals("extends")) {
                return a.equals(b) ? 0 : -1;
            }
            if (b.equals("extends")) {
                return 1;
            }
            if (a.equals("in")) {
                return a.equals(b) ? 0 : -1;
            }
            if (b.equals("in")) {
                return 1;
            }
            int acount=0, bcount=0;
            for(int i=0; i<a.length(); i++) {
                if (a.charAt(i)=='/') {
                    acount++;
                }
            }
            for(int i=0; i<b.length(); i++) {
                if (b.charAt(i)=='/') {
                    bcount++;
                }
            }
            if (acount!=bcount) {
                return (acount<bcount)?-1:1;
            }
            if (a.startsWith("this/")) {
                if (!b.startsWith("this/")) {
                    return -1;
                }
            } else if (b.startsWith("this/")) {
                return 1;
            }
            int result = a.compareToIgnoreCase(b);
            return result!=0 ? result : a.compareTo(b);
        }
    };

    /**
     * Copy the list of files from JAR into the destination directory,
     * then set the correct permissions on them if possible.
     *
     * @param keepPath - if true, the directory of each file will be created
     * @param deleteOnExit - if true, each file will be deleted on exit
     */
    public static synchronized void copy(boolean executable, boolean keepPath, boolean deleteOnExit, String destdir, String... names) {
        String[] args=new String[names.length+2];
        args[0]="chmod"; // This does not work on Windows, but the "executable" bit is not needed on Windows anyway.
        args[1]=(executable ? "700" : "600"); // 700 means read+write+executable; 600 means read+write.
        int j=2;
        for(int i=0; i<names.length; i++) {
            String name=names[i];
            String destname=name;
            if (!keepPath) { int ii=destname.lastIndexOf('/'); if (ii>=0) destname=destname.substring(ii+1); }
            destname=(destdir + File.separatorChar + destname).replace('/', File.separatorChar);
            int last=destname.lastIndexOf(File.separatorChar);
            new File(destname.substring(0,last+1)).mkdirs(); // Error will be caught later by the file copy
            if (deleteOnExit) new File(destname).deleteOnExit();
            if (copy(name, destname)) { args[j]=destname; j++; }
        }
        if (onWindows() || j<=2) return;
        String[] realargs=new String[j];
        for(int i=0; i<j; i++) realargs[i]=args[i];
        try {
            Runtime.getRuntime().exec(realargs).waitFor();
        } catch (Exception ex) {
            // We only intend to make a best effort
        }
    }

    /**
     * Copy the given file from JAR into the destination file; if the destination file exists, we then do nothing.
     * Returns true iff a file was created and written.
     */
    private static synchronized boolean copy(String sourcename, String destname) {
        File destfileobj=new File(destname);
        if (destfileobj.isFile() && destfileobj.length()>0) return false;
        InputStream resStream=Util.class.getClassLoader().getResourceAsStream(sourcename);
        if (resStream==null) return false;
        boolean result=true;
        InputStream binStream=new BufferedInputStream(resStream);
        FileOutputStream tmpFileOutputStream=null;
        try {
            tmpFileOutputStream=new FileOutputStream(destname);
        } catch (FileNotFoundException e) {
            result=false;
        }
        if (tmpFileOutputStream!=null) {
            try {
                byte[] b = new byte[16384];
                while(true) {
                    int numRead = binStream.read(b);
                    if (numRead == -1) break;
                    if (numRead > 0) tmpFileOutputStream.write(b, 0, numRead);
                }
            } catch(IOException e) { result=false; }
            try { tmpFileOutputStream.close(); } catch(IOException ex) { result=false; }
        }
        try { binStream.close(); } catch(IOException ex) { result=false; }
        try { resStream.close(); } catch(IOException ex) { result=false; }
        if (!result) OurDialog.fatal(null,"Error occurred in creating the file \""+destname+"\"");
        return true;
    }

    /**
     * Return the given text file from JAR as a String (or null if the file doesn't exist)
     */
    public static synchronized String readTextFromJAR(String sourcename) {
        InputStream resStream=Util.class.getClassLoader().getResourceAsStream(sourcename);
        if (resStream==null) return null;
        StringBuilder result = new StringBuilder();
        InputStream binStream=new BufferedInputStream(resStream);
        try {
            byte[] b = new byte[16384];
            while(true) {
                int numRead = binStream.read(b);
                if (numRead == -1) break;
                if (numRead > 0) for(int i=0;i<numRead;i++) result.append((char)(b[i]));
            }
        } catch(IOException e) { result=null; }
        try { binStream.close(); } catch(IOException ex) { result=null; }
        try { resStream.close(); } catch(IOException ex) { result=null; }
        return result==null ? null : result.toString();
    }

    /** Appends "st", "nd", "rd", "th"... as appropriate; (for example, 21 becomes 21st, 22 becomes 22nd...) */
    public static String th(int n) {
        if (n==1) return "First";
        if (n==2) return "Second";
        if (n==3) return "Third";
        if (n>0 && (n%10)==1 && (n%100)!=11) return n+"st";
        if (n>0 && (n%10)==2 && (n%100)!=12) return n+"nd";
        if (n>0 && (n%10)==3 && (n%100)!=13) return n+"rd";
        return n+"th";
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
            if (c>=32 && c<127) { out.write(c); continue; }
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
            for(int i=0;; i++) {
                if (i>=slen) return start;
                char b=big.charAt(start+i), s=small.charAt(i);
                if (b==s) continue;
                if (!caseSensitive && b>='A' && b<='Z') b=Character.toLowerCase(b);
                if (!caseSensitive && s>='A' && s<='Z') s=Character.toLowerCase(s);
                if (b!=s) break;
            }
            if (forward) start++; else start--;
        }
        return -1;
    }

    /** Read the file then return file size and a char[] array (the array could be slightly bigger than file size) */
    private static Pair<char[],Integer> readEntireFile(String filename) throws IOException {
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
                    if (max2<=now) throw new IOException("File shrunk during reading.");
                    char[] buf2=new char[max2];
                    if (now>0) System.arraycopy(buf, 0, buf2, 0, now);
                    buf=buf2;
                    max=max2;
               }
               int n=isr.read(buf, now, max-now);
               if (n<=0) break;
               now+=n;
            }
            return new Pair<char[],Integer>(buf,now);
        } finally {
            if (isr!=null) { try {isr.close();} catch(IOException ex2) {} }
            if (fis!=null) { try {fis.close();} catch(IOException ex2) {} }
        }
    }

    /** Returns true iff running on Windows **/
    public static boolean onWindows() { return System.getProperty("os.name").toLowerCase().startsWith("windows"); };

    /** Returns true iff running on Mac OS X. **/
    public static boolean onMac() { return System.getProperty("mrj.version")!=null; }
}
