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

package edu.mit.csail.sdg.alloy4whole;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.lang.Thread.UncaughtExceptionHandler;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Version;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.BOLD;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.LINK;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SETLINK;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.CLICK;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SAVE1;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.RESTORE1;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.FLUSH;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.DIVIDER;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.INDENTSHORT;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.INDENTLONG;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.DONE;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.FAIL;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.VIZMSG;

/** This helper method is used by SimpleGUI. */

final class SimpleRunner implements Runnable {

    // Defines the 2 possible exit codes; NOTE: try to keep these between 10 and 15
    // because when the process gets killed by the OS or user, often that will result
    // in an error code >= 16.  On the other hand, the JVM itself often crash with
    // exit code of 0 and 1 and maybe others. By keeping these between 10 and 15,
    // hopefully we can avoid confusing OS-initiated exit codes with our Alloy4-initiated exit codes.
    static final int EXIT_OUT_OF_MEMORY = 11;
    static final int EXIT_UNKNOWN = 12;

    /** the real System.out */
    private static final PrintStream out;

    // Redirect System.out and System.err into nothingness
    static {
        PrintStream nullOut = new PrintStream(new OutputStream() {
            public void write(int b) {}
        });
        System.setOut(nullOut);
        System.setErr(nullOut);
        PrintStream tmp=null;
        try { tmp=new PrintStream(new FileOutputStream(FileDescriptor.out), false, "UTF-8"); } catch(UnsupportedEncodingException ex) { }
        out=tmp;
    }

    /** Emit one of the action code (SAVE1, RESTORE1, FLUSH...) */
    private static void log(byte code) {
        if (out!=null) { out.print((char)code); out.flush(); }
    }

    /** Emit the given message. */
    private static void log(String msg) {
        if (out==null) System.err.print(msg); else out.print(msg);
    }

    /** Emit the given message in both long and short versions; the GUI will choose whether to display the long version or the short version. */
    private static void logIndented(String full, String shortText) {
        if (out==null) System.err.println(full);
        else { out.printf("%c%s%c%s%c", FLUSH, full, INDENTLONG, shortText, INDENTSHORT); out.flush(); }
    }

    /** Emit the given message using bold font. */
    private static void logBold(String msg) {
        if (out==null) System.err.print(msg);
        else { out.print((char)FLUSH); out.print(msg); out.print((char)BOLD); out.flush(); }
    }

    /** Emit the given hyperlink (with msg as the hyperlink label) (with target as the hyperlink destination) */
    private static void logLink(String msg, String target) {
        if (out==null || target==null || target.length()==0) log(msg);
        else { out.printf("%c%s%c%s%c", FLUSH, target, SETLINK, msg, LINK); out.flush(); }
    }

    /** Tell the GUI to popup a dialog box with the given message. */
    private static void logViz(String msg) {
        if (out==null) System.err.println(msg);
        else { out.printf("%c%s%c", FLUSH, msg, VIZMSG); out.flush(); }
    }

    /** Tell the GUI to highlight the text corresponding to the given Pos object. */
    private static void highlight(Pos e) {
        if (out!=null && e!=Pos.UNKNOWN) { out.printf("%c%s%c", FLUSH, "POS: "+e.x+" "+e.y+" "+e.x2+" "+e.y2+" "+e.filename, CLICK); out.flush(); }
    }

    //======================================================================//

    /** The temporary directory to read/write files from. */
    private final String tempdir;

    /** The version number of Alloy4. */
    private final int latestVersion;

    /** The version name of Alloy4. */
    private final String latestName;

    /** S means to solve a command, and N means to enumerate a previous solution. */
    private final char mode;

    /** Construct a new SimpleRunner object. */
    private SimpleRunner(String tempdir, int latestVersion, String latestName, char mode) {
        this.mode=mode;
        this.tempdir=tempdir;
        this.latestVersion=latestVersion;
        this.latestName=latestName;
    }

    public void run() {
        int verbosity=0;
        log(SAVE1);
        try {
            if (mode=='S') {
                final String bundleName = tempdir+File.separatorChar+"cache";
                final SimpleRunnerBundle bundle = SimpleRunnerBundle.read(bundleName);
                if (bundle==null) throw new IOException("Error reading from the file:\n"+bundleName);
                verbosity = bundle.verbosity;
                SimpleReporter.performRegularCommand(out, bundle.cache, bundle.index, bundle.options, bundle.warningNonFatal, tempdir, bundle.verbosity);
            } else {
                String ans=SimpleReporter.performEnumeration(out, tempdir);
                log(RESTORE1);
                if (ans.length()>0) logViz(ans);
                log(RESTORE1);
            }
        }
        catch(OutOfMemoryError ex) {
            System.gc();
            logBold("Fatal Error: Out of memory (Peak at "+(Runtime.getRuntime().totalMemory()/1048576)+"M)\n");
            log(FLUSH);
            Runtime.getRuntime().halt(EXIT_OUT_OF_MEMORY);
        }
        catch(StackOverflowError ex) {
            System.gc();
            logBold("Fatal Error: Out of memory (Stack Overflow due to excessively deep recursion)\n");
            log(FLUSH);
            Runtime.getRuntime().halt(EXIT_OUT_OF_MEMORY);
        }
        catch(Err e) {
            Err err=null;
            log(RESTORE1);
            String msg="fatal";
            if (e instanceof ErrorSyntax) { msg="syntax"; err=e; }
            else if (e instanceof ErrorType) { msg="type"; err=e; }
            if (e.pos==Pos.UNKNOWN)
                logBold("A "+msg+" error has occurred:  ");
            else
                logLink("A "+msg+" error has occurred:  ", "POS: "+e.pos.x+" "+e.pos.y+" "+e.pos.x2+" "+e.pos.y2+" "+e.pos.filename);
            if (verbosity>2) {
                log("(see the ");
                logLink("stacktrace", "MSG: "+e.getTotalTrace());
                log(")\n");
            } else {
                log("\n");
            }
            logIndented(e.toString().trim(), e.msg.trim());
            log("\n");
            if (err==null && latestVersion>Version.buildNumber())
                logBold(
                        "\nNote: You are running Alloy build#"+Version.buildNumber()+
                        ",\nbut the most recent is Alloy build#"+latestVersion+
                        ":\n( version "+latestName+
                        " )\nPlease try to upgrade to the newest version,"+
                "\nas the problem may have been fixed already.\n");
            log(DIVIDER);
            log(FLUSH);
            if (err!=null) highlight(err.pos);
            log(DONE);
            return;
        }
        catch(Throwable e) {
            StringBuilder sb=new StringBuilder();
            for(StackTraceElement st:e.getStackTrace()) { sb.append(st.toString()); sb.append('\n'); }
            log(RESTORE1);
            if (verbosity>2) {
                logBold("An error has occurred: ");
                log("(see the ");
                logLink("stacktrace", "MSG: "+sb.toString());
                log(")\n");
            } else {
                logBold("An error has occurred:\n");
            }
            logIndented(e.getMessage().trim(), e.getMessage().trim());
            log("\n");
            if (latestVersion>Version.buildNumber())
                logBold(
                        "\nNote: You are running Alloy build#"+Version.buildNumber()+
                        ",\nbut the most recent is Alloy build#"+latestVersion+
                        ":\n( version "+latestName+
                        " )\nPlease try to upgrade to the newest version,"+
                "\nas the problem may have been fixed already.\n");
            log(DIVIDER);
            log(FLUSH);
            log(DONE);
            return;
        }
        if (mode=='S') log(DIVIDER);
        log(FLUSH);
        log(DONE);
        System.gc(); // Might as well; the user should be busy looking at the output right now
    }

    //========================================================================================//

    public static void main(String[] args) {
        boolean fail=false;
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {
            public final void uncaughtException(Thread t, Throwable e) {
                // The code here should avoid using new features (eg. Java 1.5)
                // since GNU gij, GNU gcj, and GNU classpath historically have incomplete implementation
                // of the newer Java features
                String a = System.getProperty("java.vm.name");
                String b = System.getProperty("java.vm.info");
                if ((a!=null && a.indexOf("gcj")>=0) || (b!=null && b.indexOf("gcj")>=0)) {
                    logBold("Fatal Error: GNU gij/gcj/classpath is currently unsupported.\nPlease make sure your default Java is the Sun JDK 1.5\nor Sun JRE 1.5 or newer.\n\n");
                } else {
                    logBold("Fatal Error:\n");
                    log(e.toString().trim());
                    log("\n\n");
                }
                Runtime.getRuntime().halt(EXIT_UNKNOWN);
            }
        });
        try { System.loadLibrary("minisat");       } catch(Throwable ex) { } // Pre-load them; on Windows with JDK 1.6, it seems to prevent freezes
        try { System.loadLibrary("minisatprover"); } catch(Throwable ex) { } // Pre-load them; on Windows with JDK 1.6, it seems to prevent freezes
        try { System.loadLibrary("zchaff");        } catch(Throwable ex) { } // Pre-load them; on Windows with JDK 1.6, it seems to prevent freezes
        if (args.length!=3) {
            logBold("Fatal Error: Solver invoked with the wrong number of arguments.\n");
            log("   There were "+args.length+" arguments.\n");
            for(int i=0; i<args.length; i++) log("   arg["+i+"] = \""+args[i]+"\"\n");
            log(DIVIDER);
            log(FAIL);
            fail=true;
        }
        final String thisVersion = fail ? "0" : args[0];
        final int latestVersion = fail ? 0 : Integer.parseInt(args[1]);
        final String latestName = fail ? "" : args[2];
        if (!fail && !thisVersion.equals(Integer.toString(Version.buildNumber()))) {
            logBold("Fatal Error: Solver invoked with incompatible Jar version.\n");
            log("   The main GUI is running build #"+thisVersion+"\n");
            log("   But the solver is running build #"+Version.buildNumber()+"\n");
            log(DIVIDER);
            log(FAIL);
            fail=true;
        }
        Thread t=null;
        byte[] sb=new byte[65536];
        while(true) {
            //
            // Our main thread has a loop that keeps "attempting" to read bytes from System.in,
            // and delegate the SAT-solving-task to a separate "worker thread".
            //
            // This way, if the parent process terminates, then this subprocess is guaranteed to see it
            // nearly immediately (since the inter-process pipe will be broken), and also terminates itself
            // regardless of the status of the SAT solving.
            //
            int sbn=0;
            char first=0;
            while(true) {
                int c=0;
                try { c=System.in.read(); } catch(Throwable ex) { Runtime.getRuntime().halt(EXIT_UNKNOWN); }
                if (c<0) Runtime.getRuntime().halt(EXIT_UNKNOWN);
                if (c==0) break;
                if (sbn>=sb.length) Runtime.getRuntime().halt(EXIT_UNKNOWN);
                if (first==0) first=(char)c; else {sb[sbn]=(byte)c; sbn++;}
            }
            String tempdir=null;
            try {
                tempdir=new String(sb, 0, sbn, "UTF-8");
            } catch (UnsupportedEncodingException ex) {
                Runtime.getRuntime().halt(EXIT_UNKNOWN);
            }
            if (t!=null && t.isAlive()) {
                while(true) {
                    // We only get here if the this child process notifies that parent process that SAT-solving is done,
                    // and that the parent process then immediately assigns another task to the child process.
                    // So, we wait up to 5 seconds for the worker thread to confirm its termination.
                    // If 5 seconds is up, then we assume something terrible has happened.
                    try {t.join(5000); if (!t.isAlive()) break;} catch (InterruptedException e1) {}
                    Runtime.getRuntime().halt(EXIT_UNKNOWN);
                }
            }
            if (fail) Runtime.getRuntime().halt(EXIT_UNKNOWN);
            t=new Thread(new SimpleRunner(tempdir, latestVersion, latestName, first));
            t.start();
        }
    }

    //========================================================================================//
}
