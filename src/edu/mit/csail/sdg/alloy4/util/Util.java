package edu.mit.csail.sdg.alloy4.util;

import java.io.PrintWriter;

/**
 * An utility class for doing common I/O and XML and GUI operations.
 * 
 * @author Felix Chang
 */
public final class Util {

	/** Constructor is private, since this utility class never needs to be instantiated. */

	private Util() { }
	/**
	 * Write a String into a PrintWriter, and encode special characters if needed.
	 * 
	 * <p/>
	 * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
	 * into the lt; gt; amp; apos; and quot; encoding. And it turns any characters outside of 32..126 range
	 * into the #xHHHH encoding (where HHHH is the 4 digit hexadecimal representation of the character value).
	 * 
	 * @param out - the PrintWriter to write into
	 * @param str - the String to write out
	 */
	public static void encodeXML(PrintWriter out, String str) {
        int n=str.length();
        for(int i=0; i<n; i++) {
            char c=str.charAt(i);
            if (c=='<') { out.write("&lt;"); continue; }
            if (c=='>') { out.write("&gt;"); continue; }
            if (c=='&') { out.write("&amp;"); continue; }
            if (c=='\'') { out.write("&apos;"); continue; }
            if (c=='\"') { out.write("&quot;"); continue; }
            if (c>=32 && c<127) { out.write(c); continue; }
            out.write("&#x");
            String v=Integer.toString((int)c, 16);
            while(v.length()<4) v="0"+v;
            out.write(v);
            out.write(';');
        }
    }

	/**
	 * Write a list of Strings into a PrintWriter, where strs[2n] are written as-is, and strs[2n+1] are XML-encoded.
	 * 
	 * <p/> For example, if you call encodeXML(out, A, B, C, D, E), it desugars into the following:
	 * <br/> out.print(A);
	 * <br/> out.encodeXML(B);
	 * <br/> out.print(C);
	 * <br/> out.encodeXML(D);
	 * <br/> out.print(E);
	 * <br/> In other words, it writes the even entries as-is, and print the odd entries using XML encoding.
	 * 
	 * @param out - the PrintWriter to write into
	 * @param strs - the list of Strings to write out
	 */
	public static void encodeXMLs(PrintWriter out, String... strs) {
		for(int i=0; i<strs.length; i++) {
			if ((i%2)==0) out.print(strs[i]); else encodeXML(out,strs[i]);
		}
	}
}
