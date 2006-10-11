package edu.mit.csail.sdg.alloy4.ui;

import java.io.PrintWriter;
import java.io.StringWriter;

public final class CrashReport implements Thread.UncaughtExceptionHandler {

    public void uncaughtException(Thread thread, Throwable ex) {
        // TODO: should ask the user whether we can email this to the Alloy developers or not

        //System.getProperties().store(stateDump, "System properties");
        //w.println("Free memory: " + Runtime.getRuntime().freeMemory());
        //w.println("Total memory: " + Runtime.getRuntime().totalMemory());

//        public static void postBugReport(String msg_, Throwable thrown_) {
//            try {
//                String[] fields = askToPost
//                ("Sorry, an internal error has occurred.\n" +
//                        "You may post a bug report (via HTTP).\n" +
//                        "The error report will include your Alloy source and system\n" +
//                        "configuration, but no other information. If you'd like to be notified\n" +
//                "about a fix, please enter your email address, and optionally add a comment.");
//                if (fields != null) {
//                    // fields[0] is email address
//                    // fields[1] is comment
//
//                    final String email = valueOrEmpty(fields, 0);
//                    final String comment = valueOrEmpty(fields, 1);
//                    String postResult = postBug(computeStateDump(email, comment, msg_, thrown_));
//                    System.out.println("Result of bug post:\n" + postResult);
//                }
//
//            } catch (RuntimeException e_) {
//                Dbg.warn("Bug post failed:\n" + str(e_));
//            }
//        }

        StringWriter sw=new StringWriter();
        PrintWriter pw=new PrintWriter(sw);
        pw.printf("Thread name: %s\n\nException message: %s\n\nStack trace:\n", thread.getName(), ex.getMessage());
        ex.printStackTrace(pw);
        pw.close();
        sw.flush();
        System.out.println(sw.toString());
        System.exit(1);
    }

//    /**
//     * Post the given string -- which can be as large as needed -- via POST HTTP request.
//     * If we get an error attempting to post the bug (e.g. if an Internet connection
//     * is not available, or if the server that accepts bug reports is down), we then
//     * write a dump file and ask the user to mail it to the developers.
//     */
//    public static String postBug(String bugReport_) {
//        OutputStreamWriter out = null;
//        BufferedReader in = null;
//        try {
//            // open the URL connection
//            URL url = new URL(BUG_POST_URL);
//            URLConnection connection = url.openConnection();
//            connection.setDoOutput(true);
//            // write the bug report to the cgi script
//            out = new OutputStreamWriter(connection.getOutputStream());
//            out.write(bugReport_);
//            out.flush();
//            // read the response back from the cgi script
//            in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
//            StringBuffer report = new StringBuffer();
//            for (String inputLine = in.readLine(); inputLine != null; inputLine = in.readLine()) {
//                report.append(inputLine).append(NEW_LINE);
//            }
//            return report.toString();
//        } catch (Exception e_) {
//            return "Error posting bug report: " + NEW_LINE + str(e_);
//        } finally {
//            if (out != null) {
//                try { out.close(); } catch(Exception ignore) { Util.harmless("Dbg",ignore); }
//            }
//            if (in != null) {
//                try { in.close(); } catch(Exception ignore) { Util.harmless("Dbg",ignore); }
//            }
//        }
//    }  // postBug()

//    /** A simple, command-line BugPostAsker implementation. */
//    private static class TextAsker {
//        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
//        String readLine() {
//            try {
//                return reader.readLine();
//            } catch (IOException ex) {
//                return null;
//            }
//        }
//        public String[] ask(String msg) {
//            String answer, email, comment;
//            System.out.println(msg);
//            answer = null;
//            while (answer == null || (!answer.equals("y") && !answer.equals("n"))) {
//                System.out.print("Post bug report [y/n] ? ");
//                answer = readLine();
//                if (answer == null) {
//                    System.out.println("Error reading stdin.  Not posting bug.");
//                    return null;
//                }
//                if (answer.equals("n")) return null;
//            }
//            System.out.print("Email address: ");
//            email = readLine();
//            System.out.println("Comment: (end with a blank line)");
//            String line;
//            ByteArrayOutputStream s = new ByteArrayOutputStream();
//            PrintStream p = new PrintStream(s);
//            line = readLine();
//            while (!line.equals("")) {
//                p.println(line);
//                line = readLine();
//            }
//            comment = s.toString();
//            return new String[]{ email, comment };
//        }
//    }

//    private static final String NEW_LINE = System.getProperty("line.separator");
//    private static final String BUG_POST_URL = "http://alloy.mit.edu/postbug.php";
}
