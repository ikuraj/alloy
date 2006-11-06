package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Dimension;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;

public final class CrashReport implements Thread.UncaughtExceptionHandler {

    public static final CrashReport defaultReporter = new CrashReport();

    private String mainfile = "";
    private String mainfilecontent = "";
    private Set<String> subfiles = new LinkedHashSet<String>();

    public synchronized void clearSubFiles() { subfiles.clear(); }

    public synchronized void setMainFile(String filename, String content) { mainfile=filename; mainfilecontent=content; }

    public synchronized void addSubFile(String filename) { subfiles.add(filename); }

    public synchronized void uncaughtException(Thread thread, Throwable ex) {
        String yes="Send the Bug Report", no="Don't Send the Bug Report";
        JTextField email = new JTextField(20);
        JTextArea comment = new JTextArea();
        email.setBorder(new LineBorder(Color.DARK_GRAY));
        comment.setBorder(null);
        JScrollPane scroll=OurUtil.scrollpane(comment);
        scroll.setBorder(new LineBorder(Color.DARK_GRAY));
        scroll.setPreferredSize(new Dimension(300,200));
        if (JOptionPane.showOptionDialog(null, new Object[]{
                "Sorry. An internal error has occurred.",
                " ",
                "You may post a bug report (via HTTP).",
                "The error report will include your Alloy source",
                "and system configuration, but no other information.",
                " ",
                "If you'd like to be notified about a fix,",
                "please enter your email adress and optionally add a comment.",
                " ",
                OurUtil.makeHT("Email:",5,email,null),
                OurUtil.makeHT("Comment:",5,scroll,null)
            }, "Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE,
            null, new Object[]{yes,no}, no)!=JOptionPane.YES_OPTION) System.exit(1);
        StringWriter sw=new StringWriter();
        PrintWriter pw=new PrintWriter(sw);
        // Print the email, comment, thread name, exception message, and stack trace
        pw.printf("\nAlloy4 crash report (Build Date = %s)\n", Version.buildDate());
        pw.printf("\n========================= Email ============================\n%s", email.getText());
        pw.printf("\n\n========================= Comment ==========================\n%s", comment.getText());
        pw.printf("\n\n========================= Thread Name ======================\n%s", thread.getName());
        pw.printf("\n\n========================= Exception ========================\n%s",
                ex.getClass().toString()+": "+ex.getMessage());
        pw.printf("\n\n========================= Stack Trace ======================\n");
        ex.printStackTrace(pw);
        pw.printf("\n========================= System Properties ================\n");
        pw.println("Free memory = "+Runtime.getRuntime().freeMemory());
        pw.println("Total memory = "+Runtime.getRuntime().totalMemory());
        for(Map.Entry<Object,Object> e:System.getProperties().entrySet()) {
            Object k=e.getKey(); if (k==null) k="null";
            Object v=e.getValue(); if (v==null) v="null";
            if (k.equals("line.separator")) continue;
            pw.printf("%s = %s\n", k.toString(), v.toString());
        }
        pw.printf("\n\n========================= Main Model =======================\n");
        pw.printf("// %s\n%s\n", mainfile, mainfilecontent);
        for(String e:subfiles) {
            String content=Util.readAll(e).toString();
            pw.printf("\n\n========================= Sub Model ========================\n");
            pw.printf("// %s\n%s\n", e, content);
        }
        pw.printf("\n\n========================= The End ==========================\n");
        pw.close();
        sw.flush();
        System.out.println(sw.toString()); // TODO
        postBug(sw.toString());
        System.exit(1);
    }

    /**
     * Post the given string -- which can be as large as needed -- via POST HTTP request.
     * If we get an error attempting to post the bug (e.g. if an Internet connection
     * is not available, or if the server that accepts bug reports is down), we then
     * write a dump file and ask the user to mail it to the developers.
     */
    private static String postBug(String bugReport) {
        final String NEW_LINE = System.getProperty("line.separator");
        final String BUG_POST_URL = "http://alloy.mit.edu/postbug.php";
        OutputStreamWriter out = null;
        BufferedReader in = null;
        try {
            // open the URL connection
            URL url = new URL(BUG_POST_URL);
            URLConnection connection = url.openConnection();
            connection.setDoOutput(true);
            // write the bug report to the cgi script
            out = new OutputStreamWriter(connection.getOutputStream());
            out.write(bugReport);
            out.flush();
            // read the response back from the cgi script
            in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            StringBuffer report = new StringBuffer();
            for (String inputLine = in.readLine(); inputLine != null; inputLine = in.readLine()) {
                report.append(inputLine).append(NEW_LINE);
            }
            return report.toString();
        } catch (Exception ex) {
            return "Error posting bug report: " + ex.toString();
        } finally {
            if (out != null) { try { out.close(); } catch(Exception ignore) { } }
            if (in != null) { try { in.close(); } catch(Exception ignore) { } }
        }
    }
}
