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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import java.lang.Thread.UncaughtExceptionHandler;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import static edu.mit.csail.sdg.alloy4.OurUtil.*;

/**
 * This class asks the user for permission to email a bug report when an uncaught exception occurs.
 */

public final class MailBug implements UncaughtExceptionHandler {

    /** The version number of the most recent Alloy4 (as queried from alloy.mit.edu); -1 if alloy.mit.edu has not replied yet. */
    private int latestAlloyVersion = (-1);

    /** The name of the most recent Alloy4 (as queried from alloy.mit.edu); "unknown" if alloy.mit.edu has not replied yet. */
    private String latestAlloyVersionName = "unknown";

    /** The URL where the bug report should be sent. */
    private static final String ALLOY_URL = "http://alloy.mit.edu/postbug4.php";

    /** Construct a new MailBug object. */
    public MailBug() { }

    /** This method sets the most recent Alloy version (as queried from alloy.mit.edu) */
    public synchronized void setLatestAlloyVersion (int version, String versionName) {
        latestAlloyVersion = version;
        latestAlloyVersionName = versionName;
    }

    /** This method sends the bugReport by making a HTTP POST request. */
    private static String sendHTTP (String bugReport, Throwable ex) {
        BufferedReader in = null;
        try {
            URLConnection connection = (new URL(ALLOY_URL)).openConnection();
            connection.setDoOutput(true);
            connection.getOutputStream().write(bugReport.getBytes("UTF-8"));
            connection.getOutputStream().flush();
            in = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
            StringBuilder report = new StringBuilder();
            for (String line = in.readLine(); line != null; line = in.readLine()) report.append(line).append('\n');
            return report.toString();
        } catch (Throwable exception) {
            return "Sorry. An error has occurred in posting the bug report.\n\nPlease email this report to alloy@mit.edu directly.\n\n" + dump(ex);
        } finally {
            Util.close(in);
        }
    }

    /** This method concatenates a Throwable's message and stack trace and all its causes into a single String. */
    public static String dump (Throwable ex) {
        StringBuilder sb = new StringBuilder();
        while(ex!=null) {
           sb.append(ex.getClass()).append(": ").append(ex.getMessage()).append('\n');
           StackTraceElement[] trace = ex.getStackTrace();
           if (trace!=null) for(int n=trace.length, i=0; i<n; i++) sb.append(trace[i]).append('\n');
           ex = ex.getCause();
           if (ex!=null) sb.append("caused by...\n");
        }
        return sb.toString();
    }

    /** This method prepares the crash report. */
    private static String prepareCrashReport (Thread thread, Throwable ex, String email, String problem) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        pw.printf("\nAlloy Analyzer %s crash report (Build Date = %s)\n\n", Version.version(), Version.buildDate());
        pw.printf("========================= Email ============================\n%s\n\n", Util.convertLineBreak(email));
        pw.printf("========================= Problem ==========================\n%s\n\n", Util.convertLineBreak(problem));
        pw.printf("========================= Thread Name ======================\n%s\n\n", thread.getName());
        if (ex!=null) {
           pw.printf("========================= Exception ========================\n");
           pw.printf("%s: %s\n\n", ex.getClass().toString(), ex.toString());
           pw.printf("========================= Stack Trace ======================\n%s", dump(ex));
        }
        pw.printf("\n========================= Preferences ======================\n");
        try {
           for(String key: Preferences.userNodeForPackage(Util.class).keys()) {
               String value = Preferences.userNodeForPackage(Util.class).get(key, "");
               pw.printf("%s = %s\n", key, value);
           }
        } catch(BackingStoreException bse) {
           pw.printf("BackingStoreException occurred: %s\n", bse.toString());
        }
        pw.printf("\n========================= System Properties ================");
        pw.printf("\nRuntime.freeMemory() = "+Runtime.getRuntime().freeMemory());
        pw.printf("\nRuntime.totalMemory() = "+Runtime.getRuntime().totalMemory());
        for(Map.Entry<Object,Object> e:System.getProperties().entrySet()) {
           Object k=e.getKey(), v=e.getValue();
           if (!"line.separator".equals(k)) { // We skip "line.separator" since it's useless and makes the email harder to read
              pw.printf("\n%s = %s", (k==null ? "null" : k.toString()), (v==null ? "null" : v.toString()));
           }
        }
        pw.printf("\n\n\n========================= The End ==========================\n\n");
        pw.close();
        sw.flush();
        return sw.toString();
    }

    /** This method sends the crash report. */
    private static void sendCrashReport (Thread thread, Throwable ex, String email, String problem) {
        String report = prepareCrashReport(thread, ex, email, problem);
        try {
            int w = getScreenWidth(), h = getScreenHeight();
            JFrame statusWindow = new JFrame("Sending the bug report... please wait...");
            JButton done = button("Close", Runner.createExit(1));
            JTextArea status = textarea("Sending the bug report... please wait...", 10, 40, false, true, new EmptyBorder(2,2,2,2));
            JScrollPane statusPane = scrollpane(status);
            statusWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            statusWindow.setBackground(Color.LIGHT_GRAY);
            statusWindow.getContentPane().setLayout(new BorderLayout());
            statusWindow.getContentPane().add(statusPane, BorderLayout.CENTER);
            statusWindow.getContentPane().add(done, BorderLayout.SOUTH);
            statusWindow.pack();
            statusWindow.setSize(600,200);
            statusWindow.setLocation(w/2-300, h/2-100);
            statusWindow.setVisible(true);
            status.setText(sendHTTP(report, ex));
            status.setCaretPosition(0);
        } catch(Throwable exception) {
            System.exit(1);
        }
    }

    /** This method returns true if the exception appears to be a Sun GUI bug. */
    public static boolean isGUI(Exception ex) {
        Throwable cause = ex.getCause();
        if (cause instanceof Exception && !isGUI((Exception)cause)) return false;
        StackTraceElement[] trace = ex.getStackTrace();
        if (trace!=null) for(int n=trace.length, i=0; i<n; i++) {
            StackTraceElement elem = trace[i];
            if (!elem.getClassName().startsWith("java."))
              if (!elem.getClassName().startsWith("javax."))
                if (!elem.getClassName().startsWith("sun."))
                   return false;
        }
        return true;
    }

    /** This method is an exception handler for uncaught exceptions. */
    public synchronized void uncaughtException (Thread thread, Throwable ex) {
        if (ex instanceof Exception && isGUI((Exception)ex)) return;
        if (ex!=null) {
           System.out.flush();
           System.err.flush();
           System.err.println("Exception: " + ex.getClass());
           System.err.println("Message: " + ex);
           System.err.println("Stacktrace:");
           System.err.println(dump(ex));
           System.err.flush();
        }
        final String yes = "Send the Bug Report";
        final String no = "Don't Send the Bug Report";
        final JTextField email = textfield("", 20, new LineBorder(Color.DARK_GRAY));
        final JTextArea problem = textarea("", 50, 50, true, false, empty);
        final JScrollPane scroll = scrollpane(problem, new LineBorder(Color.DARK_GRAY), new Dimension(300, 200));
        for(Throwable ex2=ex; ex2!=null; ex2=ex2.getCause()) {
            if (ex2 instanceof StackOverflowError) {
                JOptionPane.showMessageDialog(null, new Object[] {
                   "Sorry. The Alloy Analyzer has run out of stack space.",
                   " ",
                   "Try simplifying your model or reducing the scope.",
                   "And try disabling Options->RecordKodkod.",
                   "And try reducing Options->SkolemDepth to 0.",
                   "And try increasing Options->Stack.",
                   " ",
                   "There is no way for Alloy to continue execution, so pressing OK will shut down Alloy."
                }, "Fatal Error", JOptionPane.ERROR_MESSAGE);
                System.exit(1);
            }
            if (ex2 instanceof OutOfMemoryError) {
                JOptionPane.showMessageDialog(null, new Object[] {
                   "Sorry. The Alloy Analyzer has run out of memory.",
                   " ",
                   "Try simplifying your model or reducing the scope.",
                   "And try disabling Options->RecordKodkod.",
                   "And try reducing Options->SkolemDepth to 0.",
                   "And try increasing Options->Memory.",
                   " ",
                   "There is no way for Alloy to continue execution, so pressing OK will shut down Alloy."
                }, "Fatal Error", JOptionPane.ERROR_MESSAGE);
                System.exit(1);
            }
        }
        if (latestAlloyVersion > Version.buildNumber()) {
            JOptionPane.showMessageDialog(null, new Object[] {
               "Sorry. A fatal error has occurred.",
               " ",
               "You are running Alloy build#"+Version.buildNumber()+",",
               "but the most recent is Alloy build#"+latestAlloyVersion+":",
               "( version "+latestAlloyVersionName+" )",
               " ",
               "Please try to upgrade to the newest version",
               "as the problem may have already been fixed.",
               " ",
               "There is no way for Alloy to continue execution, so pressing OK will shut down Alloy."
            }, "Fatal Error", JOptionPane.ERROR_MESSAGE);
        } else {
            if (JOptionPane.showOptionDialog(null, new Object[] {
               "Sorry. A fatal internal error has occurred.",
               " ",
               "You may submit a bug report (via HTTP).",
               "The error report will include your system",
               "configuration, but no other information.",
               " ",
               "If you'd like to be notified about a fix,",
               "please describe the problem, and enter your email address.",
               " ",
               makeHT("Email:", 5, email, null),
               makeHT("Problem:", 5, scroll, null)
            }, "Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE,
            null, new Object[]{yes,no}, no) == JOptionPane.YES_OPTION) { sendCrashReport(thread, ex, email.getText(), problem.getText()); return; }
        }
        System.exit(1);
    }
}
