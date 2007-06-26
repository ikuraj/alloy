/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.parser.World;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;

/**
 * Given a World object, generate its metamodel.
 */

public class TranslateAlloyToMetamodel {

    /** Constructor is private, since we want to force all access to this class via the static public methods. */
    private TranslateAlloyToMetamodel() { }

    /** These 5 names are special; if user-defined sigs or fields have these names, then error will occur. */
    private static final String univ="univ", Int="Int", IDX="seq/Int", set="set", ext="extends", in="in";

    /** Records whether it has seen SIGINT or not. */
    private boolean hasInt=false;

    /** Records whether it has seen IDX or not. */
    private boolean hasSeqIdx=false;

    /** Generate a short but unambiguous name for signatures. */
    private String simp(Sig sig) {
        if (sig==UNIV) return univ;
        if (sig==SIGINT) { hasInt=true; return Int; }
        if (sig==SEQIDX) { hasInt=true; hasSeqIdx=true; return IDX; }
        String string=sig.toString();
        if (string.startsWith("/")) string=string.substring(1);
        if (string.startsWith("this/")) string=string.substring(5);
        return string;
    }

    /**
     * Generate the metamodel, and write it into the PrintWriter.
     * @param world - the world that we want to generate the metamodel of
     * @param originalFilename - the original filename where the world came from (this is written to XML file as a comment)
     * @param output - the PrintWriter that will receive the XML output
     */
    private static void make(World world, String originalFilename, PrintWriter output) throws Err {
        TranslateAlloyToMetamodel ta=new TranslateAlloyToMetamodel();
        SafeList<Sig> sigs=world.getAllSigs();
        Util.encodeXMLs(output, "\n<alloy builddate=\"",
                Version.buildDate(),
                "\">\n\n<instance filename=\"",
                originalFilename,
                "\" command=\"show metamodel\">\n\n");
        boolean hasSubset=false;
        // Generate the fields. We put this up front,
        // so we can scan the field types too to see if they have SIGINT or SEQIDX
        List<Sig> ans=new ArrayList<Sig>();
        for(Sig s:sigs) if (!s.builtin) {
            String parent=set;
            if (s instanceof SubsetSig) {
                hasSubset=true;
            } else {
                parent=ta.simp(((PrimSig)s).parent);
            }
            Util.encodeXMLs(output, "<sig name=\"", ta.simp(s), "\" extends=\"", parent, "\"> <atom name=\"", ta.simp(s), "\"/> </sig>\n\n");
            for(Field field:s.getFields()) {
                Util.encodeXMLs(output, "<field name=\"", field.label, "\">\n  <type>");
                ans.clear();
                ans.add(s); // Only the first entry can be a PrimSig or SubsetSig; all other entries will be PrimSig.
                Iterable<List<PrimSig>> fieldtype = field.type.fold();
                for(List<PrimSig> tt:fieldtype) {
                   for(int i=1,n=tt.size(); i<n; i++) {
                       PrimSig t=tt.get(i);
                       if (i>=ans.size()) ans.add(t); else ans.set(i, ((PrimSig)(ans.get(i))).leastParent(t));
                   }
                }
                for(Sig t:ans) Util.encodeXMLs(output, " <sig name=\"", ta.simp(t), "\"/>");
                output.print("</type>\n");
                for(List<PrimSig> tt:fieldtype) {
                    // Here, because we sanitize the first item to "s", we may emit duplicate tuples.
                    // That is okay, since the visualizer's XML reader ignores duplicate tuples.
                    output.print("  <tuple>");
                    boolean first=true;
                    for(PrimSig t:tt) {Util.encodeXMLs(output," <atom name=\"",ta.simp(first?s:t), "\"/>"); first=false;}
                    output.print("</tuple>\n");
                }
                output.print("</field>\n\n");
            }
        }
        // Generate the "in" relation, and the "set" sig.
        // We put this before "extends" so we can look for SIGINT and SEQIDX
        if (hasSubset) {
            output.print("<field name=\""+in+"\">\n  <type> <sig name=\""+set+"\"/> <sig name=\""+univ+"\"/> </type>\n");
            for(Sig s:sigs) if (s instanceof SubsetSig) for(Sig p:((SubsetSig)s).parents) if (p!=UNIV)
              Util.encodeXMLs(output, "  <tuple> <atom name=\"", ta.simp(s), "\"/> "
              +"<atom name=\"", ta.simp(p), "\"/> </tuple>\n");
            output.print("</field>\n\n");
            output.print("<sig name=\""+set+"\" extends=\""+univ+"\"> </sig>\n\n");
        }
        // Generate the "extends" relation, and the "Int" and "seq/Int" sigs.
        output.print("<field name=\""+ext+"\">\n  <type> <sig name=\""+univ+"\"/> <sig name=\""+univ+"\"/> </type>\n");
        for(Sig s:sigs) if (!s.builtin && s instanceof PrimSig) {
            Util.encodeXMLs(output,"  <tuple> <atom name=\"", ta.simp(s),"\"/> <atom name=\"",ta.simp(((PrimSig)s).parent),"\"/> </tuple>\n");
        }
        if (ta.hasInt) output.print("  <tuple> <atom name=\""+Int+"\"/> <atom name=\""+univ+"\"/> </tuple>\n");
        if (ta.hasSeqIdx) output.print("  <tuple> <atom name=\""+IDX+"\"/> <atom name=\""+Int+"\"/> </tuple>\n");
        output.print("</field>\n\n");
        if (ta.hasInt) output.print("<sig name=\""+Int+"\" extends=\""+univ+"\"> <atom name=\""+Int+"\"/> </sig>\n\n");
        if (ta.hasSeqIdx) output.print("<sig name=\""+IDX+"\" extends=\""+Int+"\"> <atom name=\""+IDX+"\"/> </sig>\n\n");
        // Done
        output.print("<sig name=\""+univ+"\"> <atom name=\""+univ+"\"/> </sig>\n\n</instance>\n\n</alloy>\n");
        output.flush();
    }

    /**
     * Generate the metamodel, and write it into the output file (which will be overwritten if it exists).
     * @param world - the world that we want to generate the metamodel of
     * @param originalFilename - the original filename where the world came from (this is written to XML file as a comment)
     * @param outputFileName - the file receiving the output (it will be overwritten if it already exists)
     */
    public static void make(World world, String originalFilename, String outputFileName) throws Err {
        PrintWriter out;
        try {
            out=new PrintWriter(outputFileName,"UTF-8");
        } catch(IOException ex) {
            throw new ErrorFatal("writeXML failed: "+ex.toString());
        }
        make(world, originalFilename, out);
        if (!Util.close(out)) throw new ErrorFatal("writeXML failed to write to \""+outputFileName+"\"");
    }
}
