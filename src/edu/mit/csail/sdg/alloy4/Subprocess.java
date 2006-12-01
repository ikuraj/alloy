package edu.mit.csail.sdg.alloy4;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * This class provides a convenience wrapper around a Process object.
 *
 * <p>  To launch a subprocess, simply write Subprocess x=new Subprocess(args);
 * <br> The subprocess will run concurrently with your current JVM.
 *
 * <p>  When you call x.waitFor(), you will wait until the process terminates and get the return value;
 * <br> subsequent x.waitFor() calls will always return -1, and
 * <br> subsequent x.getOutput() will return either the program output, or an error message beginning with "Error"
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Subprocess {

    /** The actual subprocess (null if the subprocess did not even start successfully). */
    private Process process=null;

    /** This field will store the process termination status. */
    private String result="";

    /** This field will store the result of sending stdin input to the program. */
    private String stdin=null;

    /** This field will store the stdout output from the program. */
    private String stdout=null;

    /** This field will store the stderr output from the program. */
    private String stderr=null;

    /**
     * Synchronized method that reads the output from the process
     * (Note: you must call waitFor() first)
     */
    public synchronized String getOutput() { return result; }

    /** Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess. */
    public Subprocess(String[] commandline, String input) {
        try {
            process=Runtime.getRuntime().exec(commandline);
        } catch (IOException ex) {
            process=null;
            result="Error: " + ex.getMessage();
            return;
        }
        Thread thread0=new Thread(new InPipe(process.getOutputStream(), input));
        Thread thread1=new Thread(new OutPipe(process.getInputStream(), !true));
        Thread thread2=new Thread(new OutPipe(process.getErrorStream(), !false));
        thread0.start();
        thread1.start();
        thread2.start();
    }

    /** Wait for the process to finish and return its exit code (if we detected an error, we will return -1). */
    public int waitFor() {
        Process p;
        synchronized(this) {p=process;}
        try {
            if (p==null) return -1;
            int i=0, n=p.waitFor();
            synchronized(this) {process=null;}
            while(true) {
                synchronized(this) {if (stdin!=null && stdout!=null && stderr!=null) break;}
                i++; if (i>10) synchronized(this) { result="Error: Timeout from the process.\n"; return -1; }
                Thread.sleep(500);
            }
            synchronized(this) {
                stdin=stdin.trim(); if (stdin.length()>0) stdin=stdin+"\n";
                stdout=stdout.trim(); if (stdout.length()>0) stdout=stdout+"\n";
                result=stdin+stdout+stderr.trim();
                if (stdin.startsWith("Error")) return -1;
                if (stdout.startsWith("Error")) return -1;
                if (stderr.startsWith("Error")) return -1;
            }
            return n;
        }
        catch (InterruptedException e) {
            synchronized(this) {process=null; result="Error: "+e.getMessage();}
            p.destroy();
            return -1;
        }
    }

    /** Helper class that shuttles text into the subprocess. */
    private final class InPipe implements Runnable {
        /** The output stream to write the text into. */
        private final OutputStream stream;
        /** The text to write. */
        private final String text;
        /** Constructs the object that shuttles bytes from "text" into the output stream "stream". */
        public InPipe(OutputStream stream, String text) { this.stream=stream; this.text=text; }
        /** This is the main run method that does the copying. */
        public void run() {
            String result="";
            try {
                byte[] bytes=text.getBytes("UTF-8");
                for(int i=0, n=bytes.length; i<n;) {
                    int len=((n-i)>1024) ? 1024 : (n-i); // Pick a small number to avoid platform-dependent overflows
                    stream.write(bytes,i,len);
                    i=i+len;
                }
                stream.flush();
            } catch(IOException ex) {
                result="Error: Input stream failure.";
            }
            try {stream.close();} catch(IOException ex) {result="Error: Input stream failure.";}
            synchronized(Subprocess.this) {stdin=result;}
        }
    }

    /** Helper class that shuttles output from the subprocess into a StringBuilder. */
    private final class OutPipe implements Runnable {
        /** The input of the pipe comes from this InputStream. */
        private final InputStream input;
        /** True if this one captures stdout; false if this one captures stderr. */
        private final boolean isStdout;
        /** Constructor that constructs a new pipe. */
        public OutPipe(InputStream input, boolean isStdout) {
            this.input=input;
            this.isStdout=isStdout;
        }
        /** The run method that keeps reading from InputStream into StringBuilder until the input stream is closed. */
        public void run() {
            StringBuilder output=new StringBuilder();
            String text="";
            byte[] buffer=new byte[8192];
            try {
                while(true) {
                    int n=input.read(buffer);
                    if (n<=0) break;
                    for(int i=0;i<n;i++) output.append((char)(buffer[i]));
                }
            } catch (IOException ex) { text="Error: "+ex.getMessage()+"\n"; }
            try { input.close(); } catch(IOException ex) { if (text.length()==0) text="Error: "+ex.getMessage()+"\n"; }
            if (text.length()==0) text=output.toString();
            synchronized(Subprocess.this) { if (isStdout) stdout=text; else stderr=text; }
        }
    }
}
