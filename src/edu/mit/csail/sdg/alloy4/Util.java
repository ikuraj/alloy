package edu.mit.csail.sdg.alloy4;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.StringReader;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Comparator;
import java.util.Date;
import java.util.Random;

/**
 * This class provides useful static methods for I/O and XML operations.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Util {

    /** This constructor is private, since this utility class never needs to be instantiated. */
    private Util() { }

    /** This variable caches the value of the system's file separator. */
    private static final String fs=System.getProperty("file.separator");

    /** Read everything into a String; throws IOException if an error occurred. */
    public static String readAll(String filename) throws IOException {
        String error=null;
        StringBuilder sb=new StringBuilder();
        FileReader fr=new FileReader(filename);
        BufferedReader br=new BufferedReader(fr);
        try {
            while(true) { String s=br.readLine(); if (s==null) break; sb.append(s); sb.append('\n'); }
        } catch(IOException ex) {
            error=ex.getMessage();
        }
        try { br.close(); } catch(IOException ex) { if (error==null) error=ex.getMessage(); }
        try { fr.close(); } catch(IOException ex) { if (error==null) error=ex.getMessage(); }
        if (error!=null) throw new IOException(error);
        return sb.toString();
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
                if (line==null) break;
                out.println(line);
            } catch(IOException ex) {
                out.close();
                out=null;
                break;
            }
        }
        if (out!=null) { out.flush(); out.close(); }
        bw.close();
        fw.close();
        rd.close();
    }

    /**
     * Returns the canonical absolute path for a file.
     * If an IO error occurred, or if the file doesn't exist yet,
     * we will at least return a noncanonical but absolute path for it.
     */
    public static final String canon(String filename) {
      File file=new File(filename);
      String answer;
      try { answer=file.getCanonicalPath(); } catch(IOException ex) { answer=file.getAbsolutePath(); }
      return answer;
    }

    /**
     * Sorts two strings for optimum module order; we guarantee slashCompartor(a,b)==0 iff a.equals(b).
     * <br> (1) First of all, the builtin names "extend" and "in" are sorted ahead of other names
     * <br> (2) Else, if one string has fewer '/' than the other, then it is considered smaller.
     * <br> (3) Else, we compare them lexically without case-sensitivity.
     * <br> (4) Else, we compare them lexically with case-sensitivity.
     * <br>
     */
    public static final Comparator<String> slashComparator = new Comparator<String>() {
        public final int compare(String a, String b) {
            if (a==null) return (b==null)?0:-1;
            if (b==null) return 1;
            if (a.equals("extends")) { if (a.equals(b)) return 0; return -1; }
            if (b.equals("extends")) return 1;
            if (a.equals("in")) { if (a.equals(b)) return 0; return -1; }
            if (b.equals("in")) return 1;
            int acount=0, bcount=0;
            for(int i=0; i<a.length(); i++) if (a.charAt(i)=='/') acount++;
            for(int i=0; i<b.length(); i++) if (b.charAt(i)=='/') bcount++;
            if (acount!=bcount) return (acount<bcount)?-1:1;
            int result = a.compareToIgnoreCase(b);
            return result!=0 ? result : a.compareTo(b);
        }
    };

    /** This variable caches the result of alloyHome() function call. */
    private static String alloyHome=null;

    /** Find a temporary directory to store Alloy files; it's guaranteed to be a canonical absolute path. */
    public static synchronized String alloyHome() {
        if (alloyHome!=null) return alloyHome;
        String temp=System.getProperty("java.io.tmpdir");
        if (temp==null || temp.length()==0)
            OurDialog.fatal(null,"Error. JVM need to specify a temporary directory using java.io.tmpdir property.");
        String username=System.getProperty("user.name");
        File tempfile=new File(temp+fs+"alloy4tmp6-"+(username==null?"":username));
        tempfile.mkdirs();
        String ans=canon(tempfile.getPath());
        if (!tempfile.isDirectory()) {
            OurDialog.fatal(null, "Error. Cannot create the temporary directory "+ans);
        }
        if (!onWindows()) {
            String[] args={"chmod", "700", ans};
            try {Runtime.getRuntime().exec(args).waitFor();}
            catch (Exception ex) {harmless("Util.alloyHome()", ex);} // We only intend to make a best effort.
        }
        return alloyHome=ans;
    }

    /**
     * Copy the list of files from JAR into the destination directory,
     * then set the correct permissions on them if possible.
     */
    public static synchronized void copy(boolean executable, String destdir, String... names) {
        String[] args=new String[names.length+2];
        args[0]="chmod"; // This does not work on Windows, but the "executable" bit is not needed on Windows anyway.
        args[1]=(executable ? "700" : "600"); // 700 means read+write+executable; 600 means read+write.
        int j=2;
        for(int i=0; i<names.length; i++) {
            String name=names[i];
            String destname=(destdir + File.separatorChar + name).replace('/', File.separatorChar);
            int last=destname.lastIndexOf(File.separatorChar);
            new File(destname.substring(0,last+1)).mkdirs(); // Error will be caught later by the file copy
            if (copy(name, destname)) { args[j]=destname; j++; }
        }
        if (onWindows() || j<=2) return;
        String[] realargs=new String[j];
        for(int i=0; i<j; i++) realargs[i]=args[i];
        try {Runtime.getRuntime().exec(realargs).waitFor();}
        catch (Exception ex) {harmless("chmod",ex);} // We only intend to make a best effort
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

    /** Returns true iff running on Windows **/
    public static boolean onWindows() { return System.getProperty("os.name").toLowerCase().startsWith("windows"); };

    /** Returns true iff running on Mac OS X. **/
    public static boolean onMac() { return System.getProperty("mrj.version")!=null; }

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
     * This method is called when a harmless or impossible exception occurred;
     * now it does nothing, but we could choose to log the occurrence instead.
     */
    public static void harmless(String message, Exception ex) { }

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
     * Create an empty temporary directory for use, designate it "deleteOnExit", then return it.
     * It is guaranteed to be a canonical absolute path.
     */
    public static String maketemp() {
        Random r=new Random(new Date().getTime());
        while(true) {
            int i=r.nextInt(1000000);
            String dest = Util.alloyHome()+fs+"tmp"+fs+i;
            File f=new File(dest);
            if (f.exists()) continue;
            f.mkdirs();
            f.deleteOnExit();
            if (f.isDirectory()) return canon(dest);
        }
    }

    /** Lock then overwrite the given file. */
    public static synchronized void lockThenWrite(String filename, String content) throws IOException {
        RandomAccessFile raf;
        raf = new RandomAccessFile(new File(filename),"rw"); // If this line fails, IOException will be thrown
        FileChannel fc = raf.getChannel(); // This line does not throw IOException
        try {
            // If any code in this block throws IOException, it will go to the catch clause for clean up
            @SuppressWarnings("unused")
            FileLock lock=fc.lock();
            raf.setLength(0);
            raf.write(content.getBytes("UTF-8"));
            raf.close();
        } catch(IOException ex) {
            // Here, we make a final attempt to close the file, before forwarding the IOException to outside
            // (Since open file descriptors is a scarce resource... furthermore, if it was locked, then
            // it will prevent other processes from accessing this)
            try { raf.close(); } catch(IOException ex2) { harmless("close",ex2); }
            throw ex;
        }
    }

    /** Open (create if does not exist), lock, read up to 30000 bytes, then truncate the given file. */
    public static synchronized String lockThenReadThenErase(String filename) throws IOException {
        RandomAccessFile raf;
        raf = new RandomAccessFile(new File(filename),"rw"); // If this line fails, IOException will be thrown
        FileChannel fc = raf.getChannel(); // This line does not throw IOException
        byte[] buffer = new byte[30000];
        int length = 0;
        try {
            // If any code in this block throws IOException, it will go to the catch clause for clean up
            @SuppressWarnings("unused")
            FileLock lock=fc.lock();
            while(length<30000) {
                int c=raf.read();
                if (c<0) break;
                buffer[length]=(byte)c;
                length++;
            }
            raf.seek(0);
            raf.setLength(0);
            raf.close();
            return new String(buffer,0,length,"UTF-8");
        } catch(IOException ex) {
            // Here, we make a final attempt to close the file, before forwarding the IOException to outside
            // (Since open file descriptors is a scarce resource... furthermore, if it was locked, then
            // it will prevent other processes from accessing this)
            try { raf.close(); } catch(IOException ex2) { harmless("close",ex2); }
            throw ex;
        }
    }
}
