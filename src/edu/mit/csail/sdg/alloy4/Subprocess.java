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

import java.io.IOException;
import java.io.InputStream;
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
    private Process process=null;

    /** This field will store the stdout output from the program. */
    private String stdout=null;

    /** This field will store the stderr output from the program. */
    private String stderr=null;

    /** If >= 0, that means we expect and demand it to be the process exit code. */
    private final int expect;

    /** Timer used to schedule a timeout for the process. */
    private static final Timer stopper=new Timer();

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
     * @param input - if input.length()>0, we will write it to the process as initial input
     */
    public Subprocess(final long timeLimit, String[] commandline, int expectedExitCode, String input) {
        final Process p;
        this.expect=expectedExitCode;
        try {
            p=Runtime.getRuntime().exec(commandline);
            process=p;
        } catch (Throwable ex) {
            process=null;
            stdout="";
            stderr="Error: " + ex.getMessage();
            return;
        }
        Thread thread1=new Thread(new OutPipe(p.getInputStream(), true));
        Thread thread2=new Thread(new OutPipe(p.getErrorStream(), false));
        thread1.start();
        thread2.start();
        if (timeLimit>0) {
            TimerTask stoptask=new TimerTask() {
                @Override public final void run() {
                    Process p;
                    synchronized(Subprocess.this) {
                        if (process==null) return;
                        p=process;
                        process=null;
                        stdout="";
                        stderr="Error: time out after "+timeLimit+" milliseconds.";
                    }
                    p.destroy();
                }
            };
            synchronized(Subprocess.class) { stopper.schedule(stoptask,timeLimit); }
        }
        if (input!=null && input.length()>0) {
            byte[] bytes=null;
            try {bytes=input.getBytes("UTF-8");} catch(UnsupportedEncodingException ex) {} // Should not happen
            for(int i=0, n=bytes.length; i<n;) {
                int len=((n-i)>1024) ? 1024 : (n-i); // Pick a small number to avoid platform-dependent overflows
                try {p.getOutputStream().write(bytes,i,len);} catch(IOException ex) {bytes=null;break;}
                i=i+len;
            }
            try {p.getOutputStream().close();} catch(IOException ex) {bytes=null;}
            if (bytes==null) {
                synchronized(this) { process=null; stdout=""; stderr="Error: Input Timeout"; }
                p.destroy();
            }
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
