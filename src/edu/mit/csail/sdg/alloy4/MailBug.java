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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import static edu.mit.csail.sdg.alloy4.OurUtil.empty;

/**
 * This class asks the user for permission to email a bug report when an uncaught exception occurs.
 */

public final class MailBug implements UncaughtExceptionHandler {

    /** Construct a new MailBug object. */
    public MailBug() { }

    /** The version number of the most recent Alloy4 (as queried from alloy.mit.edu); -1 if alloy.mit.edu has not replied yet. */
    private int latestAlloyVersion = (-1);

    /** The name of the most recent Alloy4 (as queried from alloy.mit.edu); "unknown" if alloy.mit.edu has not replied yet. */
    private String latestAlloyVersionName = "unknown";

    /** Sets the most recent Alloy version (as queried from alloy.mit.edu) */
    public synchronized void setLatestAlloyVersion (int version, String versionName) {
        latestAlloyVersion = version;
        latestAlloyVersionName = versionName;
    }

    /** Helper method that prints a Throwable's stack trace and all its causes as a String. */
    public static String dump(Throwable ex) {
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

    /** This method is an exception handler for uncaught exceptions. */
    public synchronized void uncaughtException (Thread thread, Throwable ex) {
        final String yes = "Send the Bug Report";
        final String no = "Don't Send the Bug Report";
        final JTextField email = OurUtil.textfield("", 20, new LineBorder(Color.DARK_GRAY));
        final JTextArea problem = OurUtil.textarea("", 50, 50, true, false, empty);
        final JScrollPane scroll = OurUtil.scrollpane(problem, new LineBorder(Color.DARK_GRAY));
        scroll.setPreferredSize(new Dimension(300,200));
        if (ex instanceof OutOfMemoryError || ex instanceof StackOverflowError) {
            JOptionPane.showMessageDialog(null, new Object[] {
                    "Sorry. Alloy4 has run out of memory.",
                    " ",
                    "Try simplifying your model or reducing the scope.",
                    "And try disabling Options->RecordKodkod.",
                    "And try reducing Options->SkolemDepth to 0.",
                    "And try increasing Options->Memory.",
                    " ",
                    "There is no way for Alloy4 to continue execution, so pressing OK will shut down Alloy4."
            }, "Fatal Error", JOptionPane.ERROR_MESSAGE);
            System.exit(1);
        }
        else if (!"yes".equals(System.getProperty("debug")) && latestAlloyVersion>Version.buildNumber()) {
            JOptionPane.showMessageDialog(null, new Object[] {
                    "Sorry. A fatal internal error has occurred.",
                    " ",
                    "You are running Alloy build#"+Version.buildNumber()+",",
                    "but the most recent is Alloy build#"+latestAlloyVersion+":",
                    "( version "+latestAlloyVersionName+" )",
                    " ",
                    "Please try to upgrade to the newest version",
                    "as the problem may have already been fixed.",
                    " ",
                    "There is no way for Alloy4 to continue execution, so pressing OK will shut down Alloy4."
            }, "Fatal Error", JOptionPane.ERROR_MESSAGE);
            System.exit(1);
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
                    OurUtil.makeHT("Email:", 5, email, null),
                    OurUtil.makeHT("Problem:", 5, scroll, null)
            }, "Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE,
            null, new Object[]{yes,no}, no)!=JOptionPane.YES_OPTION) { System.exit(1); }
        }
        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter(sw);
        pw.printf("\nAlloy Analyzer %s crash report (Build Date = %s)\n\n", Version.version(), Version.buildDate());
        pw.printf("========================= Email ============================\n%s\n\n", Util.convertLineBreak(email.getText()));
        pw.printf("========================= Problem ==========================\n%s\n\n", Util.convertLineBreak(problem.getText()));
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
        try {
            final JFrame statusWindow = new JFrame();
            final JButton done = new JButton("Close");
            done.addActionListener(new ActionListener() {
               public void actionPerformed(ActionEvent e) { System.exit(1); }
            });
            final JTextArea status = OurUtil.textarea("Sending the bug report... please wait...", 10, 40, false, true, new EmptyBorder(2,2,2,2));
            final JScrollPane statusPane = OurUtil.scrollpane(status);
            statusWindow.setTitle("Sending Bug Report");
            statusWindow.setBackground(Color.LIGHT_GRAY);
            statusWindow.getContentPane().setLayout(new BorderLayout());
            statusWindow.getContentPane().add(statusPane, BorderLayout.CENTER);
            statusWindow.getContentPane().add(done, BorderLayout.SOUTH);
            int w = OurUtil.getScreenWidth(), h = OurUtil.getScreenHeight();
            statusWindow.pack();
            statusWindow.setSize(600,200);
            statusWindow.setLocation(w/2-300,h/2-100);
            statusWindow.setVisible(true);
            statusWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            status.setText(postBug(sw.toString(), ex));
            status.setCaretPosition(0);
        } catch(Throwable exception) {
            System.exit(1);
        }
    }

    /** Post the given string via POST HTTP request. */
    private static String postBug(String bugReport, Throwable ex) {
        final String BUG_POST_URL = "http://alloy.mit.edu/postbug4.php";
        BufferedReader in = null;
        try {
            URLConnection connection = (new URL(BUG_POST_URL)).openConnection();
            connection.setDoOutput(true);
            connection.getOutputStream().write(bugReport.getBytes("UTF-8"));
            connection.getOutputStream().flush();
            in = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
            StringBuilder report = new StringBuilder();
            for (String line = in.readLine(); line != null; line = in.readLine()) report.append(line).append('\n');
            return report.toString();
        } catch (Throwable exception) {
            return "Sorry. An error has occurred in posting the bug report.\n\nPlease email alloy@mit.edu directly.\n\n"+dump(ex);
        } finally {
            if (in!=null) Util.close(in);
        }
    }
}
