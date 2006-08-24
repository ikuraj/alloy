/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;

/**
 * An embellishment of {@link Process}, providing some added functionality, including:
 * <ul>
 *  <li>Extracting the process binary from the .jar as a resource and storing it
 *      in the temporary directory.  (This is done only once per run.)</li>
 *  <li>Non-blocking test of whether the process is still running</li>
 *  <li>Convenience routines for writing to the stdin of the process and reading
 *      from the stdout of the process </li>
 *  <li>Automatic setup of buffers for the process's standard I/O streams</li>
 *  </ul>
 *
 * <em>Note</em>: {@link ProcessInfo} does <em>not</em> consume stderr output --
 * so processes run with the help of this class must not write to stderr,
 * or they will hang.
 */
@SuppressWarnings("unchecked")
public class ProcessInfo {
    /** The {@link Process} we're running. */
    private Process _process;

    /** stdin stream of the process. */
    private OutputStream _processStdin;

    /** stdout stream of the process */
    private InputStream _processStdout;

    /** A {@link PrintWriter} for the stdin of a process. */
    private PrintWriter _processStdinWriter;

    /** A {@link LineNumberReader} for the stdout of a process. */
    private LineNumberReader _processStdoutReader;

    /** The name of the process we're running.  Used for producing
    debug messages. */
    private String _processName;


	/** unit test */
	public static void main(String[] args) {
		
	}
	
    /////////////
    // Methods //
    /////////////

    /**
     * Start a process given the name of the executable, and construct
     * a {@link ProcessInfo} object for monitoring it.
     * <p>
     * If the {@link Params global option} DEVEL.bindir is non-empty, we look
     * for the binary file in the given directory.  Otherwise, we
     * extract the resource from the jar.  Specifying bindir lets you
     * store the binaries for a given platform once, and not have to
     * extract them every time Alloy is run.
     */
    public ProcessInfo(String processName_, final String[] processArgs_, final File dir, boolean processIsScript_) throws IOException {
    // extract the process's binary from the .jar into the temporary directory,
    // if not done yet
    String binDir = Params.glob.getParam("DEVEL", "bindir");

    final String exeName = processIsScript_ ? processName_ : FileUtil.getExeName(processName_);
	_processName = exeName;

    final File extractedProcessFileHandle; 
    if (binDir != null && binDir.trim().length() > 0) {
        // the binDir is set ... but is it sensible?
        final File f = new File(FileUtil.addSlash(binDir) + exeName);
        if (f.exists()) {
            // it's sensible
            extractedProcessFileHandle = f;
        } else {
            // nonsense, ignore it 
            extractedProcessFileHandle = ResourceManager.getExecutableResource(exeName);
        }
    } else {
        // the binDir is not set
        extractedProcessFileHandle = ResourceManager.getExecutableResource(exeName);
    }

    // the whole point of this ArrayList is to eventually remove empty
    // elements, which can cause the invocation to crash on some
    // platforms
    final ArrayList a = new ArrayList(processArgs_.length+1);

    if (dir.equals(extractedProcessFileHandle.getParentFile())) {
        // the executable is in the dir, just use the short name
        // this doesn't actually work ... so actually use the long name
        //cmdArray[0] = extractedProcessFileHandle.getName();
        a.add(extractedProcessFileHandle.getAbsolutePath());
    } else {
        // the executable is not in the dir, use the long name
        a.add(extractedProcessFileHandle.getAbsolutePath());
    }
    for (int i = 0; i < processArgs_.length; ++i) {
        if (!processArgs_[i].equals("")) a.add(processArgs_[i]);
    }
    a.trimToSize();
    
	String[] cmdArray = new String[a.size()];
    cmdArray = (String[]) a.toArray(cmdArray);

    // set environment
    final String ps = System.getProperty("path.separator");
    final String path = "PATH=." + 
        ps + System.getProperty("user.dir") + 
        ps + System.getProperty("user.home") +
        ps + System.getProperty("java.home")
        ;
    final String ldlib = 
        System.getProperty("LD_LIBRARY_PATH") == null
        ? System.getProperty("java.library.path")
        : System.getProperty("LD_LIBRARY_PATH") 
        ;
    final String[] env = new String[] {path, "LD_LIBRARY_PATH=" + ldlib};


    if (true) { // info block 
        Dbg.info("Running " +processName_+"...");
    		String infoMsg = "Command is: ";
        for (int i=0; i<cmdArray.length; i++)
            infoMsg += (cmdArray[i] + " ");
        Dbg.info(infoMsg);
        //Dbg.info("in " + dir);
        for (int i=0; i<env.length; i++) {
            //Dbg.info(env[i]); //PATH:.... and LD_LIBRARY_PATH:...
        }
    }

    _process = Runtime.getRuntime().exec(cmdArray, env, dir);

    // set up buffered streams for stdin/stdout/stderr
    _processStdin = new BufferedOutputStream(_process.getOutputStream());
    _processStdout = new BufferedInputStream(_process.getInputStream());
    //_processStderr = new BufferedInputStream(_process.getErrorStream());

    _processStdinWriter = new PrintWriter(new OutputStreamWriter(_processStdin));
    _processStdoutReader = new LineNumberReader(new InputStreamReader(_processStdout));

    // start a thread that will consume and discard any output by the process to its stderr
    //   => bad implementation -- commenting out: the thread that tries to consume
    //      stderr output eats up significant CPU time from the binary process we've started.
    //      for now, must make sure that the binary process does NOT write to stderr.
//  Thread stderrEater = new Thread(new Runnable() {
//      public void run() {
//          try {
//          while (true) _processStderr.skip(16384);
//          } catch (IOException ioe_) { };
//      }
//      });
//  stderrEater.start();
    }  // ProcessInfo() constructor

    public ProcessInfo(String processName_, String[] processArgs_, File dir) throws IOException {
    this(processName_, processArgs_, dir, false);
    }


    /** Test whether the underlying {@link Process} is still running. */
    public boolean isRunning() {
      try { _process.exitValue(); }
      catch (IllegalThreadStateException e_) { return true; }
      return false;
    }

    /** Get the stdin writer. */
    public PrintWriter getStdinWriter() { return _processStdinWriter; }

    /** Get the stdout reader. */
    public LineNumberReader getStdoutReader() { return _processStdoutReader; }

    /** Write a line of text (followed by endline) to the stdin of the process. */
    public void sendLine(String s_) {
    _processStdinWriter.println(s_); _processStdinWriter.flush();
    }

    /** Destroy the underlying {@link Process} */
    public void destroy() { _process.destroy(); }

    /** Wait for the process to terminate.  Has slightly different semantics from {@link Process#waitFor}:
    does not support interruption. */
    public void waitForTermination() {
    runProcess(new ProcessInfo.ProcessOutputListener() {
        public boolean processLine(String line_) { return true; }
        });
    }

    /**
     * Run the process, calling the given {@link ProcessOutputListener} on each
     * line output by the process to stdout, as soon as the line is output.
     * (Output to stderr is consumed and, for now, ignored.)
     * If the listener's {@link ProcessOutputListener#processLine} method returns
     * false, or if the process terminates, return to the caller.
     * <p>
     * @param listener_ the listener that looks at each line output by the process to stdout,
     *                  and decides whether to continue with {@link #runProcess} or return to caller */
    public void runProcess(ProcessOutputListener listener_) { runProcess(listener_, null /* readStartedCallback */); }

    /**
     * Run the process, calling the given {@link ProcessOutputListener} on each
     * line output by the process to stdout, as soon as the line is output.
     * (Output to stderr is consumed and, for now, ignored.)
     * If the listener's {@link ProcessOutputListener#processLine} method returns
     * false, or if the process terminates, return to the caller.
     * <p>
     * @param listener_ the listener that looks at each line output by the process to stdout,
     *                  and decides whether to continue with {@link #runProcess} or return to caller
     * @param readStartedCallback_ a callback to call once we have started capturing the output
     *        of the process; e.g. it can tell the process (eg by writing to its stdin) to start
     *        producing output.  (if the processs starts writing before {@link #runProcess} starts
     *        capturing its output, the pipe can break and we won't be able to read any future
     *        output from the process).  right now the "guarantee" that readStartedCallback_.run()
     *        will not be called until we're ready to read the process's stdout is based on
     *        some time delays and {@link Thread#yield} calls, which is not quite a guarantee
     *        but does make it highly unlikely that the guarantee will not be fulfilled.
     */
    public void runProcess(ProcessOutputListener listener_, Runnable readStartedCallback_) {
    while(true) {
        String line = null;
        try {
        if (readStartedCallback_ != null) {
            final Runnable runMe = readStartedCallback_;
            Thread t = new Thread(new Runnable() { public void run() {
            // try to ensure that runMe.run() does not start running
            // until _processStdoutReader.readLine() is started
            Thread.yield();
            Thread.yield();
            Thread.yield();
            try { Thread.sleep(500); } catch (InterruptedException e_) { }
            Thread.yield();
            Thread.yield();
            Thread.yield();

            runMe.run();
            }});
            readStartedCallback_ = null;
            t.setPriority(Thread.MIN_PRIORITY);
            t.start();
        }
        line = _processStdoutReader.readLine();
        }
        catch (IOException ioe_) { Dbg.fatal(ioe_); }
        if (line!=null) Dbg.info(_processName + ": " + line);
        if (line == null || !listener_.processLine(line))
        return;
    }  // while(true)
    }  // runProcess()

    /**
     * Interface for looking at the output of a running process and looking
     * for particular text.
     * @see #runProcess
     */
    public static interface ProcessOutputListener {
    /**
     * Look at a line output by the process, and decide whether to continue
     * reading process's output, or to return to the caller of
     * {@link #runProcess}.
     *
     * @param line_ line of output from the process
     * @return <code>true</code> to continue looking at the output,
     *         <code>false</code> to return to caller of {@link #runProcess}.
     */
    public boolean processLine(String line_);
    }

    /**
     * A convenience method to run a process and wait for its termination.
     */
    public static void runTillEnd(String process_, String[] args_, File dir) throws IOException {
    ProcessInfo proc = new ProcessInfo(process_, args_, dir);
    proc.waitForTermination();
    }

}  // class ProcessInfo
