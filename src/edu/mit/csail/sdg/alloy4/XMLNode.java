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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;
import java.io.Reader;

public final class XMLNode implements Iterable<XMLNode> {

    /**
     * The attributes given to the element; key and values are never null.
     *
     * <dl><dt><b>Invariants:</b></dt><dd>
     * <ul><li>The field can be empty.
     *     <li>The field is never <code>null</code>.
     *     <li>The keys and the values are strings.
     * </ul></dd></dl>
     */
    private final Map<String,String> attributes = new LinkedHashMap<String,String>();


    /**
     * Child elements of the element.
     *
     * <dl><dt><b>Invariants:</b></dt><dd>
     * <ul><li>The field can be empty.
     *     <li>The field is never <code>null</code>.
     *     <li>The elements are instances of <code>XMLNode</code>
     *         or a subclass of <code>XMLNode</code>.
     * </ul></dd></dl>
     */
    private final List<XMLNode> children = new ArrayList<XMLNode>();


    /**
     * The name of the element; never null.
     *
     * <dl><dt><b>Invariants:</b></dt><dd>
     * <ul><li>The field is <code>null</code> iff the element is not
     *         initialized by either parse or setName.
     *     <li>If the field is not <code>null</code>, it's not empty.
     *     <li>If the field is not <code>null</code>, it contains a valid
     *         XML identifier.
     * </ul></dd></dl>
     */
    private String name = "";

    /**
     * Creates and initializes a new XML element.
     */
    private XMLNode() { }


    public Iterator<XMLNode> iterator() { return children.iterator(); }

    public List<XMLNode> getChildren(String name) {
        List<XMLNode> answer = new ArrayList<XMLNode>(children.size());
        for(int i=0; i<children.size(); i++) {
            XMLNode sub = children.get(i);
            if (name.equalsIgnoreCase(sub.name)) answer.add(sub);
        }
        return answer;
    }

    /** Returns an attribute of the element; if the attribute doesn't exist, return "". */
    public String getAttribute(String name) {
        String ans = this.attributes.get(name);
        return (ans==null) ? "" : ans;
    }


    /** Returns an attribute of the element; if the attribute doesn't exist, return the defaultValue. */
    public String getAttribute(String name, String defaultValue) {
        String ans = this.attributes.get(name);
        return (ans==null) ? defaultValue : ans;
    }

    /** Returns the name of the element. */
    public String getName() { return name; }

    public boolean is(String name) { return this.name.equalsIgnoreCase(name); }

    /** Parse an entire document. */
    public XMLNode(Reader r) throws IOException {
        XMLParser parser = new XMLParser();
        parser.reader = r;
        if (parser.skipNondata()!='<') parser.malform("Expect start of root node.");
        parser.parseElement(this);
        if (parser.skipNondata()!=(-1)) parser.malform("Expect end of file.");
    }

    public static final class XMLParser {
        /**
         * The reader provided by the caller of the parse method.
         *
         * <dl><dt><b>Invariants:</b></dt><dd>
         * <ul><li>The field is not <code>null</code> while the parse method
         *         is running.
         * </ul></dd></dl>
         */
        private Reader reader;

        private void malform(String msg) throws IOException { throw new IOException("Error at line "+y+" column "+x+": "+msg); }

        private void malform() throws IOException { malform(""); }

        private int x=1, y=1, read = (-2); // -2:unused -1:EOFdetected >=0:used

        /** Read the next character, or return -1 if end-of-file is reached. */
        private int read() throws IOException {
            if (read==(-1)) return -1;
            if (read>=0) { int answer=read; read=(-2); return answer; }
            int ans = reader.read();
            if (ans<0) read=(-1); else if (ans=='\n') { x=1; y++; } else { x++; }
            return ans;
        }

        /** Peek without consuming the next character, or return -1 if end-of-file is reached. */
        private int peek() throws IOException {
            if (read<(-1)) read=reader.read();
            return read;
        }

        /** Skip up to and including the character "char1" */
        private void skipUntil(int char1) throws IOException {
            while(true) {
                int ch = read();
                if (ch<0 || ch==char1) return;
            }
        }

        /** Skip up to and including the consecutive characters "char1" and "char2" */
        private void skipUntil(int char1, int char2) throws IOException {
            while(true) {
                int ch = read();
                if (ch<0 || (ch==char1 && peek()==char2)) { read=(-2); return; }
            }
        }

        private void expect(String string) throws IOException {
            for(int i=0; i<string.length(); i++) {
                if (read()!=string.charAt(i)) malform("Expects "+string);
            }
        }

        /** Skip as much nondata as possible, then return the first character after that. */
        private int skipNondata() throws IOException {
           while(true) {
              int ch = read();
              if (ch == '<') {
                 ch = read();
                 if (ch == '?') { skipUntil('?', '>'); continue; }
                 if (ch != '!') { read = ch ; return '<'; }
                 ch = peek();
                 if (ch == '-') { read(); if (read()!='-') malform("Expects comment."); skipUntil('-', '-'); if (read()!='>') malform("Expects end of comment."); continue; }
                 if (skipNondata()!='>') malform("Expects end of <!...>");
                 continue;
              }
              if (ch == '[') {
                 if (skipNondata()!=']') malform("Expects end of [...]");
                 continue;
              }
              if (ch == '\'' || ch == '\"') { skipUntil(ch); continue; }
              if (ch < 0 || ch == ']' || ch=='>') return ch;
           }
        }

        /** Skip whitespace if any, then return the character after that. */
        private int parseSpace() throws IOException {
            while(true) {
                int ch=read();
                if (ch!=' ' && ch!='\t' && ch!='\r' && ch!='\n') return ch;
            }
        }

        /** Parse an element, assuming the initial '<' has already been consumed. */
        private void parseElement(XMLNode target) throws IOException {
            target.name = parseName();
            while(true) {
                read = parseSpace();
                if (read == '=') malform("Unexpected '='");
                if (read == '/') {
                    read();
                    if (read()!='>') malform("Expects '/>'");
                    break;
                }
                if (read == '>') {
                    read();
                    parseContent(target);
                    if (!target.name.equals(parseName())) malform("Start tag and end tag must have matching names.");
                    if (parseSpace()!='>') malform("Expects '</"+target.name+">'");
                    break;
                }
                String key = parseName();
                if (parseSpace()!='=') malform();
                read = parseSpace();
                if (read != '\'' && read != '\"') malform();
                String value = parseValue(read());
                target.attributes.put(key, value);
            }
        }

        /** Parse a name. */
        private String parseName() throws IOException {
            StringBuilder sb = new StringBuilder();
            while(true) {
                int ch = read();
                if (ch<0) malform();
                if (ch==' ' || ch=='\t' || ch=='\r' || ch=='\n' || ch=='=' || ch=='/' || ch=='>') { read=ch; return sb.toString(); }
                sb.append((char)ch);
            }
        }

        /** Parse a value up to delim (which is always either ' or "), assuming the initial ' or " has already been consumed. */
        private String parseValue(int delim) throws IOException {
            StringBuilder sb = new StringBuilder(), sb2 = null;
            while(true) {
                int ch=read();
                if (ch<0) malform();
                if (ch==delim) return sb.toString();
                if (ch=='&') {
                    if (sb2==null) sb2=new StringBuilder(); else sb2.setLength(0);
                    while(true) {
                        if ((ch=read())<0) malform(); else if (ch!=';') sb2.append((char)ch); else break;
                    }
                    if (sb2.length()>2 && sb2.charAt(0)=='#' && sb2.charAt(1)=='x') {
                        try { ch=Integer.parseInt(sb2.substring(2), 16); } catch(NumberFormatException ex) { malform(); }
                    } else if (sb2.length()>1 && sb2.charAt(0)=='#'){
                        try { ch=Integer.parseInt(sb2.substring(1)); } catch(NumberFormatException ex) { malform(); }
                    } else {
                        String name = sb2.toString();
                        if (name.equals("amp")) ch='&';
                        else if (name.equals("quot")) ch='"';
                        else if (name.equals("apos")) ch='\'';
                        else if (name.equals("lt")) ch='<';
                        else if (name.equals("gt")) ch='>';
                        else malform("The entity \"&"+name+";\" is unknown.");
                    }
                }
                sb.append((char)ch);
            }
        }

        /** Parses the content until the rightful closing "</" are consumed. */
        private void parseContent(XMLNode parent) throws IOException {
            again:
            while(true) {
                int ch=read();
                if (ch=='<') {
                    ch=read();
                    if (ch=='/') return;
                    if (ch=='?') { skipUntil('?', '>'); continue; }
                    if (ch=='!') {
                        ch=read();
                        if (ch=='-') { if (read()!='-') malform(); skipUntil('-', '-'); if (read()!='>') malform(); continue; }
                        if (ch!='[') malform();
                        expect("CDATA[");
                        int ah=0, bh=0;
                        while(true) {
                            if ((ch=read())<0) malform();
                            if (ah==']' && bh==']' && ch=='>') continue again;
                            ah=bh; bh=ch;
                        }
                    }
                    read=ch;
                    XMLNode newElem = new XMLNode();
                    parseElement(newElem);
                    parent.children.add(newElem);
                    continue;
                }
            }
        }

    }




}
