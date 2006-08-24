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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Centralized handler for messages and assertions.
 * Initially designed for debugging messages (hence
 * the name Dbg), this class is now also used for
 * non-debugging messages intended for the user,
 * such as user errors, warnings and progress messages.
 * <p>
 * When you print a debugging message you also give its type
 * (information/warning/error/etc); what is done with messages of
 * each type (printed to console/logged to file/shown in GUI/
 * discarded) can be configured at runtime.
 * <p>
 * Also, messages may be
 * annotated with a "message group name", and only messages belonging
 * to particular group(s) shown during a run -- e.g. only messages
 * related to a certain functionality, or only messages added by a
 * certain programmer.
 * <p>
 * For fatal assertion failures, lets different
 * parts of the program register listeners to be called in case of
 * fatal assertion failure; besides cleanup, the listener can write
 * any debugging info for diagnosis (e.g. state of that program part)
 * into a file to be emailed to the developers.
 * <p>
 * Examples of using {@link Dbg}:
 * <p>
 * <code>
 * <pre>
 * import alloy.util.Dbg;
 * Dbg.info("Starting translation of " + astNode_);
 * Dbg.warn("Something strange");
 * Dbg.unimpl("Cardinalities not yet handled").
 * Dbg.unimpl();  // print stack trace showing what's unimplemented
 * Dbg.chk(myFile!=null, "Could  not write");
 * Dbg.chk(index >= 0);
 * Dbg.info("symm", "Symmetry breaking broken"); // shown only after Dbg.addMsgGroup("symm")
 * Dbg.warn("parser", "Tree should not have shared subtrees"); // shown only after Dbg.addMsgGroup(parser)
 * </pre>
 * </code>
 */
@SuppressWarnings("unchecked")
public class Dbg {
    /** Name of file to which to dump program state in case of
    {@link #fatal fatal error}.
    This file will be emailed to developers for analysis. */
    public static final String STATE_DUMP_FILE_NAME = "allstate.dmp";

    /** Where to email the state dump file */
    public static final String DEVELOPERS_EMAIL = "alloy@mit.edu";

    /** Prevent construction of {@link Dbg} instances */
    private Dbg() { }

    //
    // Debugging message types.  At runtime we can configure what to do
    // with each message type: print to console, log to file, ignore, etc.
    //

    /** Information message -- not indicating an error, but used to
    monitor program progress. */
    public static final int INFO = 0;
    /** Warning message -- something strange/unexpected but not necessarily wrong */
    public static final int WARN = 1;
    /** Fatal error -- no reasonable recovery, program must be terminated.
    Termination is done in an orderly fasion: fatal error listeners,
    and finalizers, are called before exiting, and a bug report is
    posted if the user permits. */
    public static final int FATAL = 2;
    /** Unimplemented: will implement in the future */
    public static final int UNIMPL = 3;
    /** An error in the user's model, not a bug in the code */
    public static final int USER = 4;

    /** Number of distinct message types */
    public static final int NUM_MSG_TYPES = 5;

    /** Names of debug message types */
    public static final String[] sDbgMsgNames = {"INFO", "WARN", "FATAL", "UNIMPL", "USER"};

    //
    // Constants controlling how much of the stack to print with each debug message
    //
    /** Do not include callstack information with debug messages */
    public static final int STACK_NONE = 0;
    /** Print only the name of the calling routine with each debug message */
    public static final int STACK_CALLER = 1;
    /** Print the entire stack with each debug message */
    public static final int STACK_FULL = 2;

    /** Debugging message listener, called when a program issues a debugging
    message.  For each message type, any number of debugging message
    listeners can be {@link Dbg#addDbgMsgListener registered}. */
    public static interface DbgMsgListener {

        /** Process a debugging message.
            @param msgType_ info/warn/error/etc
            @param msg_ error message
            @param thrown_ an exception that was caught --
            e.g. an I/O exception was caught when trying to open a file,
            so the exception is part of the error message; may be <code>null</code>. */
        public void debugMsg(int msgType_, String msg_, Throwable thrown_);

        /** Process a debugging message.
            @param msgType_ info/warn/error/etc
            @param msg_ error message
            @param thrown_ an exception that was caught --
            e.g. an I/O exception was caught when trying to open a file,
            so the exception is part of the error message; may be <code>null</code>. */
        public void debugMsg(int msgType_, Msg msg_, Throwable thrown_);
    }  // interface Dbg.DbgMsgListener

    /** flag set when a user error has occurred */
    public static boolean userError;

    /** flag set when regression tests are running.  if <code>true</code>, no attempts
        should be made to catch otherwise uncaught exceptions */
    public static boolean runningRegTests;

    /** {@link List} of {@link DbgMsgListener}s for each message type. */
    private static final WeakSet[] _sDbgMsgListeners = new WeakSet[NUM_MSG_TYPES];

    /** Files to be dumped in case of fatal error -- files whose contents can help
    diagnose cause of error.  Files are marked for dumping by calling
    {@link #addFileToDump}.  {@link Map} from {@link String} filename to
    {@link String} content of the file. */
    private static Map _sFilesToDump = new HashMap();

    /** Objects to be dumped in case of fatal error.  Added by calling
    {@link #addObjToDump}.  {@link Map} from {@link String} object name
    to {@link String} value. */
    private static Map _sObjsToDump = new HashMap();

    /** Args with which the program was started, for dumping to statedump.
    Set by {@link addArgsToDump}. */
    private static String[] _sMainArgs;

    /** Whether any {@link DbgMsgListener}s have been added.  If none were added before
    first message is displayed, #defaultInit() is called as a backup. */
    private static boolean _sSomeListenerAdded = false;

    /** Add a listener for a given message type.  The same listener can be
    added to listener for several message types, by calling this routine
    more than once. */
    public static void addDbgMsgListener(int msgType_, DbgMsgListener listener_) {
        if (_sDbgMsgListeners[msgType_] == null) {
            _sDbgMsgListeners[msgType_] = new WeakSet();
        }
        _sDbgMsgListeners[msgType_].add(listener_);
        _sSomeListenerAdded = true;
    }

    /** Remove a given listener from the given message type.  (The same listener
    object may remain active for other message types.)  Does nothing
    if the listener was not registered for that type. */
    public static void removeDbgMsgListener(int msgType_, DbgMsgListener listener_) {
        if (_sDbgMsgListeners[msgType_] != null) {
            _sDbgMsgListeners[msgType_].remove(listener_);
        }
    }

    public static void addDbgMsgListener(int msgType1_, int msgType2_,
                                         DbgMsgListener listener_) {
        addDbgMsgListener(msgType1_, listener_);
        addDbgMsgListener(msgType2_, listener_);
    }

    public static void addDbgMsgListener(int msgType1_, int msgType2_,
                                         int msgType3_, DbgMsgListener listener_) {
        addDbgMsgListener(msgType1_, listener_);
        addDbgMsgListener(msgType2_, listener_);
        addDbgMsgListener(msgType3_, listener_);
    }

    public static void addDbgMsgListener(int msgType1_, int msgType2_,
                                         int msgType3_, int msgType4_,
                                         DbgMsgListener listener_) {
        addDbgMsgListener(msgType1_, listener_);
        addDbgMsgListener(msgType2_, listener_);
        addDbgMsgListener(msgType3_, listener_);
        addDbgMsgListener(msgType4_, listener_);
    }

    public static void addDbgMsgListener(DbgMsgListener listener_) {
        for (int i = 0; i < NUM_MSG_TYPES; i++) {
            addDbgMsgListener(i, listener_);
        }
    }

    /** Add contents of a file to the dumpfile in case of fatal error. */
    public static void addFileToDump(String fileName_, String fileContents_) {
        _sFilesToDump.put(fileName_, fileContents_);
    }

    /** Add the args[] array of main() to be dumped in case of fatal error. */
    public static void addArgsToDump(String[] args_) {
        _sMainArgs = new String[args_.length];
        System.arraycopy(args_, 0, _sMainArgs, 0, args_.length);
    }

    /** Add an object whose value is to be dumped in case of fatal error.  A reference
    to the object is saved and its {@link String} value <em>at the time of the dump</em>
    (<b>not</b> at the time of the {@link #addObjToDump} call) will be dumped.
    Multiple calls to {@link #addObjToDump} with the same <code>objName_</code> will
    overwrite the object reference, so only the latest call to {@link #addObjToDump}
    for a given <code>objName_</code> matters. */
    public static void addObjToDump(String objName_, Object obj_) {
        _sObjsToDump.put(objName_, obj_);
    }

    /**
     * Clear the collections files and objects to be dumped in case of error.
     * This is done at the beginning of each new analysis,
     * to ensure that in case of error, only object relevant to the latest
     * analysis (the one that caused the error) will be dumped.
     */
    public static void clearDump() {
        _sObjsToDump.clear();
        _sFilesToDump.clear();
    }

    /** Process a debugging message of a given type.  Convenience routines
    for particular msg types are normally called instead of this one. */
    public static void msg(int msgType_, String msg_, Throwable thrown_) {
        msg(msgType_, new Msg(msg_), thrown_);
    }

    public static void msg(int msgType_, Msg msg_, Throwable thrown_) {
        try {
            if (!_sSomeListenerAdded) defaultInit();
            if (_sDbgMsgListeners[msgType_] == null) return;
            for (Iterator listenerIter = _sDbgMsgListeners[msgType_].iterator(); listenerIter.hasNext();) {
            try {
                DbgMsgListener listener = (DbgMsgListener) listenerIter.next();
                if (listener != null) {
                    listener.debugMsg(msgType_, msg_, thrown_);
                } else {
                    listenerIter.remove();
                }
            } catch (Exception e_) {
                System.out.println("Error during exception processing: " + e_);
		e_.printStackTrace();
            }
            }
        } catch (Exception e_) {
            System.out.println("Error during exception processing: " + e_);
	    e_.printStackTrace();
        }
    }

    /** Output an informational message. */
    public static void info(String msg_, Throwable thrown_) { msg(INFO, msg_, thrown_); }
    public static void info(String msg_) { info(msg_, (Throwable)null); }

    /** Output a warning message */
    public static void warn(String msg_, Throwable thrown_) { msg(WARN, msg_, thrown_ ); }
    public static void warn(String msg_) { warn(msg_, (Throwable)null); }

    /** Output a custom "unimplemented" message */
    public static void unimpl(String msg_) { msg(UNIMPL, msg_, null /* thrown_ */ ); }

    /**
     * Compute a state dump, as a single {@link String} .
     *
     * @see StateDump
     */
    public static String computeStateDump(final String email, final String comment, final String msg_, final Throwable thrown_) {
        // create a memory buffer to which to dump program state
        ByteArrayOutputStream stateDump = new ByteArrayOutputStream();

        PrintWriter w = new PrintWriter(stateDump);
        // write file header
        // ask each FatalErrorListener to dump its state into the file
        w.println();
        w.print("email=");
        w.println(email);
        w.print("comment=");
        w.println(comment);
        w.println();
        w.println("=========================================================");
        w.println("ALLOY ANALYZER STATE DUMP AT " + new Date());
        w.println("=========================================================");
        w.println();
        w.println("Alloy build date: " + Version.BUILD_DATE);
        w.println();

        w.println(_getStackInfo(STACK_FULL));

        w.println("Message: " + msg_);

        if (thrown_ != null) {
            w.println("Exception: ");
            thrown_.printStackTrace(w);
        }

        w.println();

        // dump objects whose contents can help diagnose the error
        for (Iterator entryIter = _sObjsToDump.entrySet().iterator(); entryIter.hasNext();) {
            Map.Entry entry = (Map.Entry)entryIter.next();
            w.println("------------------------------------------------------------");
            w.println("Object: " + entry.getKey());
            w.println("------------------------------------------------------------");
            w.println();
            String objValue;
            try { objValue = entry.getValue().toString(); }
            catch (Exception e_) { objValue = "Exception while computing value: " + e_; }
            w.println(objValue);
            w.println();
        }

        w.println();

        // dump files whose contents can help diagnose the error
        for (Iterator entryIter = _sFilesToDump.entrySet().iterator(); entryIter.hasNext();) {
            Map.Entry entry = (Map.Entry)entryIter.next();
            w.println("------------------------------------------------------------");
            w.println("File: " + entry.getKey());
            w.println("------------------------------------------------------------");
            w.println();
            w.println(entry.getValue());
            w.println();
        }

        // dump system information
        w.flush();
        w.println();
        w.println("Global parameters: \n" + Params.glob);

        try {
            System.getProperties().store(stateDump, "System properties");
        } catch (IOException ioe_) {
            w.println("Error storing system properties: " + ioe_);
        }
        w.println();
        w.println("Free memory: " + Runtime.getRuntime().freeMemory());
        w.println("Total memory: " + Runtime.getRuntime().totalMemory());
        w.println();
        if (_sMainArgs!=null) {
            w.println("Args passed on command-line: ");
            for (int i=0; i<_sMainArgs.length; i++)
            w.println("args[" + i + "]=" + _sMainArgs[i]);
        }

        w.println("=========================================================");
        w.close();

        return stateDump.toString();
    }  // computeStateDump()


    private static String valueOrEmpty(final String[] a, final int i) {
        if (i < a.length) {
            if (a[i] != null) return a[i];
        }
        return "";
    }

    /** Post a bug report, if the user permits. */
    public static void postBugReport(String msg_, Throwable thrown_) {
        try {
	    String[] fields = BugPostAsker.askToPost
		("Sorry, an internal error has occurred.\n" +
		 "You may post a bug report (via HTTP).\n" +
		 "The error report will include your Alloy source and system\n" +
		 "configuration, but no other information. If you'd like to be notified\n" +
		 "about a fix, please enter your email address, and optionally add a comment.");
	    if (fields != null) {
		// fields[0] is email address
		// fields[1] is comment

            final String email = valueOrEmpty(fields, 0);
            final String comment = valueOrEmpty(fields, 1);
		String postResult = BugPoster.postBug(computeStateDump(email, comment, msg_, thrown_));
		System.out.println("Result of bug post:\n" + postResult);
            }

        } catch (RuntimeException e_) {
            Dbg.warn("Bug post failed:\n" + Util.str(e_));
        }
    }

    /**
     * A {@link ThreadGroup} that posts bug reports of all uncaught exceptions,
     * except {@link InterruptedException} which can arise in the
     * normal course of events.
     */
    private static class ExceptionCatcherThreadGroup extends ThreadGroup {

        ExceptionCatcherThreadGroup() { super("Exception-catching thread group"); }

        public void uncaughtException(Thread thread_, Throwable thrown_) {
            if (!(thrown_ instanceof Exception) ||
            thrown_ instanceof InterruptedException ||
            thrown_ instanceof AssertException) {
            super.uncaughtException(thread_, thrown_);
            return;
            }

            Thread.dumpStack();
            System.out.println("Uncaught exception:");
            System.out.println("Thread: " + thread_);
            thrown_.printStackTrace();

            postBugReport("Uncaught exception", thrown_);
        }
    }

    /**
     * Run the {@link Runnable} in such a way that if it, or any {@link Thread}s it starts,
     * throw an uncaught exception, a bug report will be sent if the user permits.
     */

    public static void catchUncaughtExceptions(Runnable r_) {
        ThreadGroup exceptionCatcher = new ExceptionCatcherThreadGroup();
        Thread t = new Thread(exceptionCatcher, r_, "Exception-catching thread");
        t.start();
    }

    /**
     * Output a fatal error message, call fatal error listeners,
     * then throw a runtime exception.
     */
    public static void fatal(String msg_, Throwable thrown_) throws AssertException {
        // still call any listeners to print/log/display the error message
        System.err.println(msg_);
        if (thrown_ != null) System.err.println(thrown_);

        msg(FATAL, msg_, thrown_);
        if (!runningRegTests) {
            postBugReport(msg_, thrown_);
            CleanupManager.exit(1);
        } else {
            throw new AssertException(msg_, thrown_);
        }
    }  // fatal() - fatal error, dumps state and exit.

    public static void fatal(String msg_) {
        fatal(msg_, (Throwable)null);
    }

    public static void fatal(Throwable thrown_) { fatal(thrown_.toString(), thrown_); }

    /**
     * If the argument is false, throw an {@link AssertException}.
     * This is in contrast to {@link #chk}, which causes an immediate
     * fatal error.  Code that calls {@link #check} should ensure
     * that if the assertion fails, no side-effects are left,
     * e.g. that any system resources get released, and any static
     * variables restored to their original value.  A <code>finally</code>
     * clause may be useful for this.
     */
    public static void check(boolean condition_) {
        if (!condition_) Thread.dumpStack();
        if (!condition_) throw new AssertException();
    }

    /**
     * If the argument is false, throw an {@link AssertException} with given message.
     * This is in contrast to {@link #chk}, which causes an immediate
     * fatal error.  Code that calls {@link #check} should ensure
     * that if the assertion fails, no side-effects are left,
     * e.g. that any system resources get released, and any static
     * variables restored to their original value.  A <code>finally</code>
     * clause may be useful for this.
     */
    public static void check(boolean condition_, String msg_) {
        if (!condition_) Thread.dumpStack();
        if (!condition_) throw new AssertException(msg_);
    }

    /**
     * If the argument is null, throw an {@link AssertException}.
     * This is in contrast to {@link #chk}, which causes an immediate
     * fatal error.  Code that calls {@link #check} should ensure
     * that if the assertion fails, no side-effects are left,
     * e.g. that any system resources get released, and any static
     * variables restored to their original value.  A <code>finally</code>
     * clause may be useful for this.
     */
    public static void check(Object arg_) { check(arg_!=null); }
    
    /** warn in source for model with known location */
    public static void warn(Msg msg_) {
        msg(WARN, msg_, (Throwable)null);
    }


    /** user error in source for model with known location */
    public static void user(Msg msg_) {
        userError = true;
        msg(USER, msg_, (Throwable)null);
    }

    /** user error with no known location */
    public static void user(String msg_) {
        userError = true;
        msg(USER, msg_, (Throwable)null);
    }

    /** Check an assertion; if it fails, print a given message and exit. */
    public static void chk(boolean cond_, String msg_) {
        if (!cond_) {
            Thread.dumpStack();
            fatal(msg_, (Throwable)null);
        }
    }

    /** Check an assertion, and exit if it fails. */
    public static void chk(boolean cond_) { chk(cond_, "Fatal error!"); }

    /** Cause an assertion failure. */
    public static void fail(String msg_) { chk(false, msg_); }

    /** Names of message groups for which messages are displayed.  */
    private static List _sActiveMsgGroups = new ArrayList();

    /** Add a message group.  After this, messages tagged with this
    group name will be shown.  Unless a group is explicitly added,
    messages tagged with that group name will be ignored. */
    public static void addMsgGroup(String msgGroup_) { _sActiveMsgGroups.add(msgGroup_); }

    /** Whether messages in a given message group are to be displayed */
    private static boolean _msgGroupOn(String msgGroup_) { return _sActiveMsgGroups.contains(msgGroup_); }

    /** Prepend [groupname] to a message-group debug message. */
    private static String _markGrp(String msgGroup_, String msg_)
    { return "[" + msgGroup_ + "] " + msg_; }

    public static void msg(String msgGroup_, int msgType_, String msg_)
    { if (_msgGroupOn(msgGroup_)) msg(msgType_, _markGrp(msgGroup_, msg_), (Throwable)null); }

    public static void info(String msgGroup_, String msg_)
    { if (_msgGroupOn(msgGroup_)) info(_markGrp(msgGroup_, msg_)); }

    public static void warn(String msgGroup_, String msg_)
    { if (_msgGroupOn(msgGroup_)) warn(_markGrp(msgGroup_, msg_)); }

    public static void fatal(String msgGroup_, String msg_)
    { if (_msgGroupOn(msgGroup_)) fatal(_markGrp(msgGroup_, msg_), (Throwable)null); }

    public static void unimpl(String msgGroup_, String msg_)
    { if (_msgGroupOn(msgGroup_)) unimpl(_markGrp(msgGroup_, msg_)); }

    public static void user(String msgGroup_, String msg_)
    { if (_msgGroupOn(msgGroup_)) user(_markGrp(msgGroup_, msg_)); }

    public static void user(String msgGroup_, Msg msg_) {
        if (_msgGroupOn(msgGroup_)) {
            msg_.message = _markGrp(msgGroup_, msg_.message);
            user(msg_);
        }
    }

    public static void chk(String msgGroup_, boolean cond_, String msg_)
    { if (_msgGroupOn(msgGroup_)) chk(cond_, _markGrp(msgGroup_, msg_)); }

    public static void chk(String msgGroup_, boolean cond_)
    { if (_msgGroupOn(msgGroup_)) chk(cond_); }

    public static Object chk(Object x_) { chk(x_!=null); return x_; }
    public static Object chk(Object x_, String msg_) { chk(x_!=null, msg_); return x_; }

    /** Most common {@link DbgMsgListener} implementation, which dumps the message
    to a given stream.  Can be used to direct messages to screen or to file.
    Prints warnings to the console if */
    public static class DbgMsgStreamPrinter implements DbgMsgListener {

        /** Where to print the debugging messages. */
        private PrintWriter _w;

        /** Whether to close {@link #_w} on exit.  We close it unless it is
            one of the standard streams ({@link System#out} or {@link System#err}). */
        private boolean _closeOnExit;

        /** How much of the stack trace to print with each msg:
            one of {@link #STACK_NONE}, {@link #STACK_CALLER}, {@link #STACK_FULL}. */
        private int _stackPrinting = STACK_NONE;

        /** Create a {@link DbgMsgStreamPrinter} which prints its messages
            to the given {@link OutputStream}. */
        public DbgMsgStreamPrinter(OutputStream s_) {
            _w = new PrintWriter(s_, true /* autoflush */);
            _closeOnExit = !(s_ == System.out || s_ == System.err);
        }

        public DbgMsgStreamPrinter(OutputStream s_, int stackPrinting_) {
            this(s_); _stackPrinting = stackPrinting_;
        }

        /** Print the debugging message, along with its type, to the
            stream, or discard the message if the stream is null. */
        public void debugMsg(int msgType_, String msg_, Throwable thrown_) {
            if (_w != null) {
                _w.println(sDbgMsgNames[msgType_] +
                    Dbg._getStackInfo(_stackPrinting) + " : " + msg_);
                if (thrown_ != null)
                    thrown_.printStackTrace(_w);
            }
        }

        public void debugMsg(int msgType_, Msg msg_, Throwable thrown_) {
            debugMsg(msgType_, msg_.toString(), thrown_);
        }

        protected void finalize() throws Throwable {
            if (_w != null && _closeOnExit) {
                _w.close();
            }
            super.finalize();
        }
    }  // class Dbg.DbgMsgStreamPrinter

    /** Extract from the stack trace the name of the routine which called {@link Dbg} to
    print the message.  That will be the name of the last routine that is not
    a method of {@link Dbg}. */
    private static String _getStackInfo(int stackPrinting_) {
        if (stackPrinting_ == STACK_NONE) return "";
        String stackTrace = _getStackTrace();
        int endDbgInternalTrace = stackTrace.lastIndexOf("alloy.util.Dbg");
        int startCallingMethodName = stackTrace.indexOf('\n', endDbgInternalTrace)+1;
        int endCallingMethodName = (stackPrinting_ == STACK_FULL) ? stackTrace.length() :
            stackTrace.indexOf('\n', startCallingMethodName);
        String result =
            (stackPrinting_ == STACK_CALLER ? ' ' : '\n') +
            stackTrace.substring(startCallingMethodName, endCallingMethodName);
        return (stackPrinting_ == STACK_FULL) ? result : " " + result.trim();
    }

    /** Register default listeners for debugging messages.  By default, they all go
    to the screen. */
    public static void defaultInit() {
        addDbgMsgListener(USER,
                  new DbgMsgStreamPrinter(System.out));
        addDbgMsgListener(WARN, FATAL, UNIMPL,
                  new DbgMsgStreamPrinter(System.out, STACK_FULL));
    }


    /** Return the current stack trace as {@link String} */
    private static String _getStackTrace() {
        // for getting stack trace
        Throwable t = new Throwable();
        // for storing stack trace
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        // printing destination
        PrintStream ps = new PrintStream(os);
        t.printStackTrace(ps);
        return os.toString();
    }

    /**
     * Removes the specified listener for all message types.
     */
    public static void removeDbgMsgListener(DbgMsgListener listener) {
        for (int i = 0; i < _sDbgMsgListeners.length; i++) {
            if (_sDbgMsgListeners[i] != null) {
                removeDbgMsgListener(i, listener);
            }
        }
        
    }

}  // class Dbg
