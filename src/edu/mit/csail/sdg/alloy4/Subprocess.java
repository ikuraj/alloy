package edu.mit.csail.sdg.alloy4;

import java.io.InputStream;
import java.io.IOException;

/**
 * This convenience wrapper around a Process automatically deals with input and output to the process.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @author Felix Chang
 */

public final class Subprocess {

    /** The actual subprocess (null if the process did not even start successfully) */
    private Process process=null;

    /** This field will store the output from the program. */
    private String output="";

    /** This field records whether the process has ended or not. */
    private boolean stopped=false;

    /** Synchronized method that saves the final output from the process into the "output" field. */
    private synchronized void setOutput(String finalOutput) {
        output=finalOutput;
        stopped=true;
    }

    /** Synchronized method that saves the final output from the process into the "output" field if stopped==false. */
    private synchronized void setOutputIfNotStopped(String finalOutput) {
        if (!stopped) {output=finalOutput; stopped=true;}
    }

    /** Synchronized method that reads the output from the process
     * (Note: you must call waitFor() or terminate() or both first.) */
    public synchronized String getOutput() { return output; }

    /** Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess. */
    public Subprocess(String[] commandline) {
        try {
            process=Runtime.getRuntime().exec(commandline);
        } catch (IOException ex) {
            process=null;
            setOutput("Error:\n" + ex.getMessage());
            return;
        }
        Thread thread1=new Thread(new Subpipe(process.getInputStream(), new StringBuilder()));
        Thread thread2=new Thread(new Subpipe(process.getErrorStream(), null));
        thread1.start();
        thread2.start();
    }

    /** Forcibly terminate the process. */
    public void terminate() {
        Process p;
        synchronized(Subprocess.this) {p=process; process=null; setOutput("Error!\nProcess forcibly terminated!\n");}
        if (p!=null) p.destroy();
    }

    /** Wait for the process to finish and return its exit code (if we detected an error, we will return -1). */
    public int waitFor() {
        Process p;
        synchronized(Subprocess.this) {p=process;}
        try {
            if (p==null) return -1;
            int i=0, n=p.waitFor();
            synchronized(Subprocess.this) {process=null;}
            while(true) {
                synchronized(Subprocess.this) {if (stopped) break;}
                i++; if (i>10) { setOutput("Error!\nTimeout from the process!\n"); return -1; }
                Thread.sleep(500);
            }
            return getOutput().startsWith("Error!") ? -1 : n;
        }
        catch (InterruptedException e) {
            terminate();
            setOutput("Error!\n"+e.getMessage());
            return -1;
        }
    }

    /** Helper class that shuttles bytes from the subprocess into a StringBuilder. */
    private class Subpipe implements Runnable {
        /** The input of the pipe comes from this InputStream. */
        private final InputStream input;
        /** The output of the pipe goes into this StringBuilder (null if we want to discard the output) */
        private final StringBuilder output;
        /** Constructor that constructs a new pipe. */
        public Subpipe(InputStream input, StringBuilder output) { this.input=input; this.output=output; }
        /** The run method that keeps reading from InputStream into StringBuilder until the input stream is closed. */
        public final void run() {
            String error="";
            byte[] buffer=new byte[8192];
            try {
                while(true) {
                    int n=input.read(buffer);
                    if (n<=0) break;
                    if (output!=null) for(int i=0;i<n;i++) output.append((char)(buffer[i]));
                }
            } catch (IOException ex) { error=ex.getMessage()+"\n"; }
            try { input.close(); } catch(IOException ex) { error=error+ex.getMessage()+"\n"; }
            if (output!=null) setOutputIfNotStopped(error.length()>0 ? ("Error!\n"+error) : output.toString());
        }
    }
}
