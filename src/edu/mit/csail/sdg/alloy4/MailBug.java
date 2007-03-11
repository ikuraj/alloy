package edu.mit.csail.sdg.alloy4;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
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

/**
 * This class asks for permission to email a bug report when an uncaught exception occurs.
 */

public final class MailBug implements UncaughtExceptionHandler {

    /** Construct a new MailBug object. */
    public MailBug() { }

    /** The most recent Alloy version (as queried from alloy.mit.edu); -1 if alloy.mit.edu has not replied yet. */
    private int latestAlloyVersion=(-1);

    /** The name of the most recent Alloy version (as queried from alloy.mit.edu); "" if alloy.mit.edu has not replied yet. */
    private String latestAlloyVersionName="";

    /** Sets the most recent Alloy version (as queried from alloy.mit.edu) */
    public synchronized void setLatestAlloyVersion(int version, String versionName) {
        latestAlloyVersion=version;
        latestAlloyVersionName=versionName;
    }

    /** This method is an exception handler for uncaught exceptions. */
    public synchronized void uncaughtException(Thread thread, Throwable ex) {
        //if (ex!=null) {
        //    System.err.println("-------------------------------------");
        //    ex.printStackTrace(System.err);
        //    System.err.flush();
        //}
        final String yes="Send the Bug Report";
        final String no="Don't Send the Bug Report";
        final JTextField email = new JTextField(20);
        final JTextArea problem = new JTextArea();
        email.setBorder(new LineBorder(Color.DARK_GRAY));
        problem.setBorder(null);
        final JScrollPane scroll=OurUtil.scrollpane(problem);
        scroll.setPreferredSize(new Dimension(300,200));
        scroll.setBorder(new LineBorder(Color.DARK_GRAY));
        if (latestAlloyVersion>Version.buildNumber()) {
            JOptionPane.showMessageDialog(null, new Object[] {
                    "Sorry. A fatal internal error has occurred.",
                    " ",
                    "You are running Alloy build#"+Version.buildNumber()+",",
                    "but the most recent is Alloy build#"+latestAlloyVersion+":",
                    "( version "+latestAlloyVersionName+" )",
                    " ",
                    "Please try to upgrade to the newest version",
                    "as the problem may have already been fixed."
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
                    OurUtil.makeHT("Email:",5,email,null),
                    OurUtil.makeHT("Problem:",5,scroll,null)
            },
            "Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE,
            null, new Object[]{yes,no}, no)!=JOptionPane.YES_OPTION) { System.exit(1); }
        }
        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter(sw);
        pw.printf("\nAlloy Analyzer %s crash report (Build Date = %s)\n\n", Version.version(), Version.buildDate());
        pw.printf("========================= Email ============================\n%s\n\n", email.getText());
        pw.printf("========================= Problem ==========================\n%s\n\n", problem.getText());
        pw.printf("========================= Thread Name ======================\n%s\n\n", thread.getName());
        if (ex!=null) {
           pw.printf("========================= Exception ========================\n"+ex.getClass()+": "+ex+"\n\n");
           pw.printf("========================= Stack Trace ======================\n");
           ex.printStackTrace(pw);
        }
        pw.printf("\n========================= Preferences ======================\n");
        try {
            for(String key: Preferences.userNodeForPackage(Util.class).keys()) {
                String value = Preferences.userNodeForPackage(Util.class).get(key,"");
                pw.printf("%s = %s\n", key, value);
            }
        } catch(BackingStoreException ignore) {
            // Not fatal
        }
        pw.printf("\n========================= System Properties ================\n");
        pw.println("Runtime.freeMemory() = "+Runtime.getRuntime().freeMemory());
        pw.println("Runtime.totalMemory() = "+Runtime.getRuntime().totalMemory());
        for(Map.Entry<Object,Object> e:System.getProperties().entrySet()) {
            Object k=e.getKey(), v=e.getValue();
            if (!"line.separator".equals(k)) {
                // We are skipping "line.separator" since it's useless and it makes the resulting email harder to read
                pw.printf("%s = %s\n", (k==null ? "null" : k.toString()), (v==null ? "null" : v.toString()));
            }
        }
        pw.printf("\n\n========================= The End ==========================\n\n");
        pw.close();
        sw.flush();
        //System.err.println(sw.toString());
        //System.err.flush();
        try {
            final JFrame statusWindow=new JFrame();
            final JButton done=new JButton("Close");
            done.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    System.exit(1);
                }
            });
            final JTextArea status=new JTextArea("Sending the bug report... please wait...");
            status.setEditable(false);
            status.setLineWrap(true);
            status.setWrapStyleWord(true);
            status.setBackground(Color.WHITE);
            status.setForeground(Color.BLACK);
            status.setBorder(new EmptyBorder(2,2,2,2));
            final JScrollPane statusPane=new JScrollPane(status);
            statusPane.setBorder(null);
            statusWindow.setTitle("Sending Bug Report");
            statusWindow.setBackground(Color.LIGHT_GRAY);
            statusWindow.getContentPane().setLayout(new BorderLayout());
            statusWindow.getContentPane().add(statusPane, BorderLayout.CENTER);
            statusWindow.getContentPane().add(done, BorderLayout.SOUTH);
            int w=OurUtil.getScreenWidth(), h=OurUtil.getScreenHeight();
            statusWindow.pack();
            statusWindow.setSize(600,200);
            statusWindow.setLocation(w/2-300,h/2-100);
            statusWindow.setVisible(true);
            status.setText(postBug(sw.toString()));
        } catch(Exception exception) {
            System.exit(1);
        }
    }

    /** Post the given string via POST HTTP request. */
    private static String postBug(String bugReport) {
        final String BUG_POST_URL = "http://alloy.mit.edu/postbug4.php";
        OutputStreamWriter out = null;
        BufferedReader in = null;
        try {
            // open the URL connection
            URLConnection connection = (new URL(BUG_POST_URL)).openConnection();
            connection.setDoOutput(true);
            // write the bug report to the cgi script
            out = new OutputStreamWriter(connection.getOutputStream(), "UTF-8");
            out.write(bugReport);
            out.flush();
            // read the response back from the cgi script
            in = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
            StringBuilder report = new StringBuilder();
            for (String inputLine = in.readLine(); inputLine != null; inputLine = in.readLine()) {
                report.append(inputLine);
                report.append('\n');
            }
            return report.toString();
        } catch (Exception ex) {
            return "Sorry. An error has occurred in posting the bug report.\n\n"
            +"Please email alloy@mit.edu directly and we will work to fix the problem.\n\n"
            +"(Bug posting failed due to Java exception: "+ex.toString()+")";
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch(IOException ignore) {
                    // Nothing further we can do. We already tried closing it...
                }
            }
            if (in != null) {
                try {
                    in.close();
                } catch(IOException ignore) {
                    // Nothing further we can do. We already tried closing it...
                }
            }
        }
    }
}
