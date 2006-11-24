package edu.mit.csail.sdg.alloy4;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * This class provides a convenience wrapper around a Process object.
 *
 * <p>  To launch a subprocess, simply write Subprocess x=new Subprocess(args);
 * <br> The subprocess will run concurrently with your current JVM.
 * <br> When you are ready to deal with the subprocess, you can do one of two things:
 *
 * <p>  (1) if you call x.terminate(), the subprocess will be terminated;
 * <br> subsequent x.waitFor() will always return -1 (indicating error), and
 * <br> subsequent x.getOutput() will always return "Error: Process forcibly terminated."
 *
 * <p>  (2) if you call x.waitFor(), you will wait until the process terminates and get the return value;
 * <br> subsequent x.waitFor() calls will always return -1, and
 * <br> subsequent x.getOutput() will return either the program output, or an error message beginning with "Error"
 *
 * <p>  If you have one thread waiting on waitFor(), you are allowed to call terminate() from other threads.
 * <br> When that happens, the process will be terminated, and the waiting thread will awaken
 * <br> (with -1 as the return code).
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Subprocess {

    /** The actual subprocess (null if the subprocess did not even start successfully). */
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

    /**
     * Synchronized method that reads the output from the process
     * (Note: you must call waitFor() or terminate() or both first)
     */
    public synchronized String getOutput() { return output; }

    /** Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess. */
    public Subprocess(String[] commandline) {
        try {
            process=Runtime.getRuntime().exec(commandline);
        } catch (IOException ex) {
            process=null;
            setOutput("Error: " + ex.getMessage());
            return;
        }
        Thread thread1=new Thread(new Subpipe(process.getInputStream(), new StringBuilder()));
        Thread thread2=new Thread(new Subpipe(process.getErrorStream(), null));
        thread1.start();
        thread2.start();
    }

    /** Executes the given command line, and returns a "Subprocess" object that allows us to query the subprocess. */
    public Subprocess(String[] commandline, String input) {
        try {
            process=Runtime.getRuntime().exec(commandline);
        } catch (IOException ex) {
            process=null;
            setOutput("Error: " + ex.getMessage());
            return;
        }
        Thread thread0=new Thread(new SubinputPipe(process.getOutputStream(), input));
        Thread thread1=new Thread(new Subpipe(process.getInputStream(), new StringBuilder()));
        Thread thread2=new Thread(new Subpipe(process.getErrorStream(), null));
        thread0.start();
        thread1.start();
        thread2.start();
    }

    /** Forcibly terminate the process. */
    public void terminate() {
        Process p;
        synchronized(Subprocess.this) {p=process; process=null; setOutput("Error: Process forcibly terminated.\n");}
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
                i++; if (i>10) { setOutput("Error: Timeout from the process.\n"); return -1; }
                Thread.sleep(500);
            }
            return getOutput().startsWith("Error") ? -1 : n;
        }
        catch (InterruptedException e) {
            synchronized(Subprocess.this) {process=null;}
            setOutput("Error: "+e.getMessage());
            p.destroy();
            return -1;
        }
    }

    /** Helper class that shuttles text into the subprocess. */
    private class SubinputPipe implements Runnable {
        /** The output stream to write the text into. */
        private final OutputStream stream;
        /** The text to write. */
        private final String text;
        /** Constructs the object that shuttles bytes from "text" into the output stream "stream". */
        public SubinputPipe(OutputStream stream, String text) { this.stream=stream; this.text=text; }
        /** This is the main run method that does the copying. */
        public void run() {
            try {
                byte[] bytes=text.getBytes("UTF-8");
                for(int i=0, n=bytes.length; i<n;) {
                    int len=((n-i)>1024) ? 1024 : (n-i); // Pick a small number to avoid platform-dependent overflows
                    stream.write(bytes,i,len);
                    i=i+len;
                }
                stream.flush();
            } catch(IOException ex) {
                setOutputIfNotStopped("Error: Input stream failure.");
            }
            try {stream.close();} catch(IOException ex) {setOutputIfNotStopped("Error: Input stream failure.");}
        }
    }

    /** Helper class that shuttles output from the subprocess into a StringBuilder. */
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
            } catch (IOException ex) { error=ex.getMessage(); }
            try { input.close(); } catch(IOException ex) { if (error.length()>0) error=ex.getMessage(); }
            if (output!=null) setOutputIfNotStopped(error.length()>0 ? ("Error: "+error+"\n") : output.toString());
        }
    }
}
