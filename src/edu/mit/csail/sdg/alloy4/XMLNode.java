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
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;
import java.io.Reader;

/** Immutable; this class represents an XML element node. */

public final class XMLNode implements Iterable<XMLNode> {

    /** The type of the element; never null. */
    private String type = "";

    /** The set of (key,value) pairs; never null. */
    private final Map<String,String> map = new LinkedHashMap<String,String>();

    /** The list of direct children nodes. */
    private final List<XMLNode> sub = new ArrayList<XMLNode>();

    /** Constructs an empty XMLNode object. */
    private XMLNode() { }

    /** Constructs the root XMLNode by parsing an entire XML document. */
    public XMLNode(Reader r) throws IOException {
        XMLParser parser = new XMLParser(r);
        if (parser.skipNondata(false)!='<') parser.malform("Expects start of root element.");
        parser.parseElement(this);
        if (parser.skipNondata(false)!=(-1)) parser.malform("Expects end of file.");
    }

    /** Simple parser based on XML Specification 1.0 taking into account XML Specification Errata up to 2008/Jan/18. */
    private static final class XMLParser {

        /** The reader for the input XML file. */
        private final Reader reader;

        /** The current x position in the file. */
        private int x = 1;

        /** The current y position in the file. */
        private int y = 1;

        /** The current "readahead" character; -2 if the readahead cache is empty; -1 if EOF is detected; otherwise it is one char. */
        private int read = (-2);

        /** Constructor is private, since we want only XMLNode to be able to construct an instance of this class. */
        private XMLParser(Reader reader) { this.reader = reader; }

        /**
         * Read the next character.
         * @throws IOException if end-of-file is reached.
         * @throws IOException if an I/O error occurred.
         */
        private int read() throws IOException {
            if (read<(-1)) read=reader.read();
            if (read<0) { malform("Unexpected end of file."); } else if (read=='\n') { x=1; y++; } else { x++; }
            int ans = read;
            read = -2;
            return ans;
        }

        /**
         * Peek without consuming the next character, or return -1 if end-of-file is reached.
         * @throws IOException if an I/O error occurred.
         */
        private int peek() throws IOException {
            if (read<(-1)) read=reader.read();
            return read;
        }

        /**
         * Consume up to and including the consecutive characters "char1" and "char2".
         * @throws IOException if we reached end-of-file without seeing the pattern.
         * @throws IOException if an I/O error occurred.
         */
        private void skipUntil(int char1, int char2) throws IOException {
            while(true) {
                int ch = read();
                if (ch==char1 && peek()==char2) { read=(-2); return; }
            }
        }

        /**
         * If the next N characters match the given string (where N == length of string), then consume them, else throw IOException.
         * @throws IOException if the next N characters do not match the given string.
         * @throws IOException if an I/O error occurred.
         */
        private void expect(String string) throws IOException {
            int saveX=x, saveY=y;
            for(int i=0; i<string.length(); i++) {
                if (read()!=string.charAt(i)) { x=saveX; y=saveY; malform("Expects the string \""+string+"\""); }
            }
        }

        /**
         * Skip whitespace if any, then return the first non-whitespace character after that.
         * @throws IOException if after skipping 0 or more white space character we reach end-of-file.
         * @throws IOException if an I/O error occurred.
         */
        private int parseSpace() throws IOException {
            while(true) {
                int ch=read();
                if (ch!=' ' && ch!='\t' && ch!='\r' && ch!='\n') return ch;
            }
        }

        /**
         * Skip as much nondata as possible, then return the first character after that (or -1 if we end up at end-of-file).
         * Specifically, this method consumes as many instances of XMLDecl/doctypedecl/intSubset/extSubsetDecl as possible.
         * If skipText==true, then this method is being called recursively to consume the inner text of XMLDecl/doctypedecl/intSubset/extSubsetDecl.
         * @throws IOException if the XML input is malformed.
         * @throws IOException if an I/O error occurred.
         */
        private int skipNondata(boolean skipText) throws IOException {
           while(true) {
              int ch = peek();
              if (ch<0) return -1;
              read = -2;
              if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') continue;
              if (ch == '<') {
                 ch = read();
                 if (ch == '?') { skipUntil('?', '>'); continue; }
                 if (ch != '!') { read = ch ; return '<'; }
                 if (peek() == '-') {
                     read = -2;
                     if (read()!='-') malform("Expects start of <!--...-->");
                     skipUntil('-', '-');
                     if (read()!='>') malform("Expects end of <!--...-->");
                     continue;
                 }
                 if (skipNondata(true)!='>') malform("Expects end of <!...>");
              }
              else if (!skipText || ch == ']' || ch=='>') { return ch; }
              else if (ch == '[') { if (skipNondata(true)!=']') malform("Expects end of [...]"); }
              else if (ch == '\'' || ch == '\"') { while(read()!=ch) { } }
           }
        }

        /**
         * Parse an element (and all its subelements), assuming the initial "less than" sign has already been consumed.
         * @throws IOException if the XML input is malformed.
         * @throws IOException if an I/O error occurred.
         */
        private void parseElement(XMLNode target) throws IOException {
            target.type = parseName();
            while(true) {
                boolean space = false;
                int ch = read();
                if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') { space=true; ch=parseSpace(); }
                if (ch == '=') malform("Unexpected '='");
                if (ch == '/') {
                   if (read()!='>') malform("Expects '/>'");
                   break;
                }
                if (ch == '>') {
                   parseContent(target);
                   if (!target.type.equals(parseName())) malform("Start tag and end tag must have matching types.");
                   if (parseSpace()!='>') malform("Expects '</"+target.type+">'");
                   break;
                }
                if (!space) malform("Space needed between element type and element attributes.");
                read = ch;
                String key = parseName();
                if (key.length()==0) malform("Attribute name cannot be empty.");
                if (parseSpace()!='=') malform("Expects = after the attribute name.");
                ch = parseSpace();
                if (ch != '\'' && ch != '\"') malform("Expects \' or \" as the start of the attribute value.");
                String value = parseValue(ch);
                target.map.put(key, value);
            }
        }

        /**
         * Parse an element name or attribute name.
         * @throws IOException if the XML input is malformed.
         * @throws IOException if an I/O error occurred.
         */
        private String parseName() throws IOException {
            StringBuilder sb = new StringBuilder();
            while(true) {
                int ch = read();
                if (ch==' ' || ch=='\t' || ch=='\r' || ch=='\n' || ch=='=' || ch=='/' || ch=='<' || ch=='>' || ch=='[' || ch==']') {
                   read=ch;
                   return sb.toString();
                }
                sb.append((char)ch);
            }
        }

        /**
         * Parse a value up to delim (which is always either ' or "), assuming the initial ' or " has already been consumed.
         * @throws IOException if the XML input is malformed.
         * @throws IOException if an I/O error occurred.
         */
        private String parseValue(int delim) throws IOException {
            StringBuilder sb = new StringBuilder(), sb2 = null;
            while(true) {
                int ch=read();
                if (ch==delim) return sb.toString();
                if (ch=='&') {
                    if (sb2==null) sb2=new StringBuilder(); else sb2.setLength(0);
                    while((ch=read()) != ';') sb2.append((char)ch);
                    if (sb2.length()>2 && sb2.charAt(0)=='#' && sb2.charAt(1)=='x') {
                        try { ch=Integer.parseInt(sb2.substring(2), 16); } catch(NumberFormatException ex) { ch=(-1); }
                    } else if (sb2.length()>1 && sb2.charAt(0)=='#'){
                        try { ch=Integer.parseInt(sb2.substring(1)); } catch(NumberFormatException ex) { ch=(-1); }
                    } else {
                        String name = sb2.toString();
                        if (name.equals("amp")) ch='&';
                        else if (name.equals("quot")) ch='"';
                        else if (name.equals("apos")) ch='\'';
                        else if (name.equals("lt")) ch='<';
                        else if (name.equals("gt")) ch='>';
                        else ch=(-1);
                    }
                    if (ch<0) malform("The entity \"&"+sb2.toString()+";\" is unknown.");
                }
                sb.append((char)ch);
            }
        }

        /**
         * Parses the content until the rightful closing "LESS THAN SIGN followed by FORWARD SLASH" are both consumed.
         * @throws IOException if the XML input is malformed.
         * @throws IOException if an I/O error occurred.
         */
        private void parseContent(XMLNode parent) throws IOException {
           again:
           while(true) {
              if (read()!='<') continue;
              int ch=read();
              if (ch=='/') return;
              if (ch=='?') { skipUntil('?', '>'); continue; }
              if (ch=='!') {
                 ch=read();
                 if (ch=='-') {
                    if (read()!='-')  malform("Expects start of <!--...-->");
                    skipUntil('-', '-');
                    if (read()!='>')  malform("Expects end of <!--...-->");
                    continue;
                 }
                 if (ch!='[') malform("Expects <![CDATA[...]]>");
                 expect("CDATA[");
                 for(int ah=0,bh=0; ;) {
                    ch=read();
                    if (ah==']' && bh==']' && ch=='>') { continue again; } else { ah=bh; bh=ch; }
                 }
              }
              read=ch;
              XMLNode newElem = new XMLNode();
              parseElement(newElem);
              parent.sub.add(newElem);
           }
        }

        /** Throws an IOException with the given msg, and associate with it the current line and column location. */
        private void malform(String msg) throws IOException { throw new IOException("Error at line "+y+" column "+x+": "+msg); }
    }

    /** Returns the type of the element. */
    public String getType() { return type; }

    /** Returns true if the type of this element is equal to the given type. */
    public boolean is(String type) { return this.type.equals(type); }

    /** Returns a read-only iterator over the immediate subelements. */
    public Iterator<XMLNode> iterator() { return Collections.unmodifiableList(sub).iterator(); }

    /** Returns a read-only list of the immediate subelements whose type is equal to the given type. */
    public List<XMLNode> getChildren(String type) {
        List<XMLNode> answer = new ArrayList<XMLNode>(sub.size());
        for(int i=0; i<sub.size(); i++) {
           XMLNode x = sub.get(i);
           if (x.type.equals(type)) answer.add(x);
        }
        return Collections.unmodifiableList(answer);
    }

    /** Returns the value associated with the given attribute name; if the attribute doesn't exist, return "". */
    public String getAttribute(String name) {
        String ans = map.get(name);
        return (ans==null) ? "" : ans;
    }

    /** Returns the value associated with the given attribute name; if the attribute doesn't exist, return the defaultValue. */
    public String getAttribute(String name, String defaultValue) {
        String ans = map.get(name);
        return (ans==null) ? defaultValue : ans;
    }
}
