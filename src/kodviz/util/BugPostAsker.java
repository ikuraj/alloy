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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;

/**
 * A standard way to ask the user whether to post a bug report, which will
 * work whether the program uses Swing or not.
 */
public abstract class BugPostAsker {
    /** Prompt the user for email address and comment, asking
     *  whether to post a bug report. Returns array of email-address
     *  and comment if wants to send report, otherwise null.
     */
    public abstract String[] ask(String msg);


    private static BugPostAsker theAsker = new TextAsker();

    public static void setAsker(BugPostAsker asker) {
	theAsker = asker;
    }

    public static String[] askToPost(String msg) {
	return theAsker.ask(msg);
    }

    /** A simple, command-line BugPostAsker implementation. */
    private static class TextAsker extends BugPostAsker {
	BufferedReader reader = 
	    new BufferedReader(new InputStreamReader(System.in));

	String readLine() {
	    try {
		return reader.readLine();
	    } catch (IOException ex) {
		return null;
	    }
	}

        public String[] ask(String msg) {
	    String answer, email, comment;

            System.out.println(msg);

	    answer = null;
	    while (answer == null ||
		   (!answer.equals("y") && !answer.equals("n"))) {
		System.out.print("Post bug report [y/n] ? ");
		answer = readLine();
		if (answer == null) {
		    System.out.println("Error reading stdin.  Not posting bug.");
		    return null;
		}
		if (answer.equals("n")) return null;
	    }

	    System.out.print("Email address: ");
	    email = readLine();

	    System.out.println("Comment: (end with a blank line)");
	    String line;
	    ByteArrayOutputStream s = new ByteArrayOutputStream();
	    PrintStream p = new PrintStream(s);
	    line = readLine();
	    while (!line.equals("")) {
		p.println(line);
		line = readLine();
	    }
	    comment = s.toString();

	    return new String[]{ email, comment };
	}
    }
}


