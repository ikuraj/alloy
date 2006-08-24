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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;

/** Code for automatically posting bug reports over the Internet. */
public class BugPoster {

    private static final String NEW_LINE = System.getProperty("line.separator");

    public static final String BUG_POST_URL = "http://alloy.mit.edu/postbug.php";
    //public static final String BUG_POST_URL = "http://alloy.mit.edu/posttest.php";

    /** Whether to post bug reports.  Useful to disable during development. */
    public static boolean postBugReports = true;

    /**
     * Post the given string -- which can be as large as needed -- via POST HTTP request.
     * If we get an error attempting to post the bug (e.g. if an Internet connection
     * is not available, or if the server that accepts bug reports is down), we then
     * write a dump file and ask the user to mail it to the developers.
     */
    public static String postBug(String bugReport_) {
        if (!postBugReports) return "Bug report posting disabled";

	OutputStreamWriter out = null;
	BufferedReader in = null;

        try {
	    // open the URL connection
            URL url = new URL(BUG_POST_URL);
            URLConnection connection = url.openConnection();
	    connection.setDoOutput(true);
	    
	    // write the bug report to the cgi script
	    out = new OutputStreamWriter(connection.getOutputStream());
            out.write(bugReport_);
	    out.flush();

            // read the response back from the cgi script
            in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            StringBuffer report = new StringBuffer();

            for (String inputLine = in.readLine(); inputLine != null; inputLine = in.readLine()) {
                report.append(inputLine).append(NEW_LINE);
            }

            return report.toString();
        } catch (Exception e_) {
            return "Error posting bug report: " + NEW_LINE + Util.str(e_);
        } finally {
	    if (out != null) {
		try { out.close(); } catch(Exception ignore) {}
	    }
	    if (in != null) {
		try { in.close(); } catch(Exception ignore) {}
	    }
	}
    }  // postBug()

    /** Writes the state dump to a file, and asks the user
    to email the file to the developers.  This is only done
    if we were unable to post the information directly
    via http.  We first try to write the dumpfile
    to the current directory; if that fails, we try
    to write it to the temporary directory; if that fails,
    we write it to stdout. */
    /*private static void _writeDumpFile(String bugReport_) throws IOException {
        Dbg.info("Dumping state to file allstate.dmp");
        PrintWriter out = new PrintWriter(new FileWriter("allstate.dmp", true));
        out.println(bugReport_);
        out.close();
        Dbg.info("Please send the file allstate.dmp to alloy@mit.edu to get the problem fixed.");
    }*/

    public static void main(String[] args) {
	System.out.println(postBug(args[0]));
    }

}  // class BugPoster
