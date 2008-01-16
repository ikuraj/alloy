/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.io.InputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Timer;
import java.util.TimerTask;

/**
 * This provides a convenience wrapper around a Process object.
 *
 * <p>  To launch a subprocess, simply write Subprocess x=new Subprocess(0,args);
 * <br> The subprocess will run concurrently with your current JVM.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Subprocess {

    // Invariant: process==null => (stdout!=null && stderr!=null)

    /** The actual subprocess (null if the subprocess never started, or if we detected it has terminated already) */
    private Process process = null;

    /** This field will store the stdout output from the program. */
    private String stdout = null;

    /** This field will store the stderr output from the program. */
    private String stderr = null;

    /** If >= 0, that means we expect and demand it to be the process exit code. */
    private final int expect;

    /** Timer used to schedule a timeout for the process. */
    private static final Timer stopper = new Timer();

    /**
     * Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess.
     * @param timeLimit - if timeLimit>0, we will attempt to terminate the process after that many milliseconds have passed
     * @param commandline - the command line
     */
    public Subprocess(long timeLimit, String[] commandline) { this(timeLimit, commandline, -1, ""); }

    /**
     * Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess.
     * @param timeLimit - if timeLimit>0, we will attempt to terminate the process after that many milliseconds have passed
     * @param commandline - the command line
     * @param expectedExitCode - if expectedExitCode>=0, we will expect it to be the process's exit code
     */
    public Subprocess(final long timeLimit, String[] commandline, int expectedExitCode) { this(timeLimit, commandline, expectedExitCode, ""); }

    /**
     * Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess.
     * @param timeLimit - if timeLimit>0, we will attempt to terminate the process after that many milliseconds have passed
     * @param commandline - the command line
     * @param expectedExitCode - if expectedExitCode>=0, we will expect it to be the process's exit code
     * @param input - if input.length()>0, we will write it to the subprocess as initial input
     * (note: the caller thread will wait until the entire input has been fed to the subprocess)
     */
    public Subprocess(final long timeLimit, String[] commandline, int expectedExitCode, String input) {
        final Process p;
        expect = expectedExitCode;
        try {
            p = Runtime.getRuntime().exec(commandline);
            process = p;
        } catch (Throwable ex) {
            process = null;
            stdout = "";
            stderr = "Error: " + ex.getMessage();
            return;
        }
        Thread thread1 = new Thread(new OutPipe(p.getInputStream(), true));
        Thread thread2 = new Thread(new OutPipe(p.getErrorStream(), false));
        thread1.start();
        thread2.start();
        if (timeLimit>0) {
            TimerTask stoptask=new TimerTask() {
                public void run() {
                    Process p;
                    synchronized(Subprocess.this) {
                        if (process==null) return;
                        p = process;
                        process = null;
                        stdout = "";
                        stderr = "Error: time out after "+timeLimit+" milliseconds.";
                    }
                    p.destroy();
                }
            };
            synchronized(Subprocess.class) { stopper.schedule(stoptask, timeLimit); }
        }
        byte[] bytes = new byte[0];
        if (input!=null && input.length()>0) {
            try {bytes=input.getBytes("UTF-8");} catch(UnsupportedEncodingException ex) {bytes=new byte[0];} // Should not happen
            for(int i=0, n=bytes.length; i<n;) {
                int len=((n-i)>1024) ? 1024 : (n-i); // Pick a small number to avoid overflow on some OS
                try {p.getOutputStream().write(bytes,i,len);} catch(IOException ex) {bytes=null;break;}
                i=i+len;
            }
        }
        try {p.getOutputStream().close();} catch(IOException ex) {bytes=null;}
        if (bytes==null) {
            synchronized(this) { process=null; stdout=""; stderr="Error: Input Timeout"; }
            p.destroy();
        }
    }

    /** Wait for the process to finish and return its standard output */
    public String getStandardOutput() {
        Process p;
        int n;
        synchronized(this) { p=process; if (p==null) return stdout; }
        try {
            n=p.waitFor();
        } catch (InterruptedException e) {
            synchronized(this) { process=null; stdout=""; stderr="Error: Thread Interrupted"; }
            p.destroy();
            return "";
        }
        for(int i=0; ;i++) {
            synchronized(this) {
                if (stdout!=null && stderr!=null) {
                    process=null;
                    if (expect>=0 && expect!=n && stderr.length()==0) stderr="Error: Exit code="+n;
                    return stdout;
                }
                if (i>10) {
                    process=null;
                    stdout="";
                    stderr="Error: Thread Timeout";
                    return "";
                }
            }
            try {Thread.sleep(500);} catch(InterruptedException ex) {}
        }
    }

    /** Wait for the process to finish and return its standard output + standard error */
    public String getStandardOutputAndError() {
        String a=getStandardOutput().trim(), b;
        synchronized(this) { b=stderr; }
        b=b.trim();
        if (a.length()!=0 && b.length()!=0) return a+"\n"+b; else return a+b;
    }

    /** Helper class that shuttles output from the subprocess into a StringBuilder. */
    private final class OutPipe implements Runnable {
        /** The input of the pipe comes from this InputStream. */
        private final InputStream input;
        /** True if this is stdout; false if this is stderr. */
        private final boolean isStdout;
        /** Constructor that constructs a new pipe. */
        public OutPipe(InputStream input, boolean isStdout) {
            this.input=input;
            this.isStdout=isStdout;
        }
        /** The run method that keeps reading from InputStream into StringBuilder until the input stream is closed. */
        public void run() {
            String text;
            try {
                StringBuilder output=new StringBuilder();
                byte[] buffer=new byte[8192];
                while(true) {
                    int n=input.read(buffer);
                    if (n<0) break;
                    for(int i=0; i<n; i++) { output.append((char)(buffer[i])); }
                }
                text=output.toString();
            } catch (Throwable ex) {
                text="Error: "+ex.getMessage();
            }
            Util.close(input);
            synchronized(Subprocess.this) {
                if (isStdout) { if (stdout==null) stdout=text; } else { if (stderr==null) stderr=text; }
            }
        }
    }
}
