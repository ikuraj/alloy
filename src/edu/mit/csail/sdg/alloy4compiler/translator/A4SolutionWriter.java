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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import static edu.mit.csail.sdg.alloy4.Util.tail;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.UniqueNameGenerator;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;

/** This helper class contains helper routines for writing an A4Solution object out as an XML file. */

public final class A4SolutionWriter {

    /** If a sig label is "", or a field label is "", we use this as the name instead. */
    private static final String BLANK = "x";

    /** This maps each Sig to a name we've chosen for it. */
    private final IdentityHashMap<Sig,String> sig2name = new IdentityHashMap<Sig,String>();

    /** This maps each Field to a name we've chosen for it. */
    private final IdentityHashMap<Field,String> field2name = new IdentityHashMap<Field,String>();

    /** This is the set of unique names we've generated so far. */
    private final UniqueNameGenerator un = new UniqueNameGenerator();

    /** Whether we've seen a SubsetSig so far. */
    private boolean subset = false;

    /** This is the solution we're writing out. */
    private final A4Solution sol;

    /** This is the output file. */
    private final PrintWriter out;

    /** Convenience helper method that writes out a metafield. */
    private void writeMetaTS(String me, String name, Type type) throws Err {
        int n=type.arity();
        for(List<PrimSig> sigs:type.fold()) {
            Util.encodeXMLs(out, "\n<field name=\"", name, "\">\n");
            out.print("    <type>");
            for(int i=0; i<n; i++) Util.encodeXMLs(out, " <sig name=\"", i==0 ? me : sig2name.get(sigs.get(i)), "\"/>");
            out.print(" </type>\n");
            out.print("    <tuple>");
            for(int i=0; i<n; i++) Util.encodeXMLs(out, " <atom name=\"", i==0 ? me : sig2name.get(sigs.get(i)), "\"/>");
            out.print(" </tuple>\n");
            out.print("</field>\n");
        }
    }

    /** Convenience helper method that writes out a field or a skolem set. */
    private void writeTS(A4TupleSet r, String name, Type type) throws Err {
        int n=r.arity();
        for(List<PrimSig> sigs:type.fold()) {
            if (n>1) {
                Util.encodeXMLs(out, "\n<field name=\"", name, "\">\n");
                out.print("    <type>");
                for(int i=0; i<n; i++) Util.encodeXMLs(out, " <sig name=\"", sig2name.get(sigs.get(i)), "\"/>");
                out.print(" </type>\n");
            } else {
                Util.encodeXMLs(out, "\n<set name=\"", name, "\" type=\"", sig2name.get(sigs.get(0)), "\">\n");
            }
            again2:
            for(A4Tuple t:r) {
                for(int i=0; i<n; i++) {
                    PrimSig s=t.sig(i);
                    if (!s.intersects(sigs.get(i))) continue again2;
                }
                if (n>1) {
                    out.print("    <tuple>");
                    for(int i=0; i<n; i++) Util.encodeXMLs(out, " <atom name=\"", t.atom(i), "\"/>");
                    out.print(" </tuple>\n");
                } else {
                    Util.encodeXMLs(out, "  <atom name=\"", t.atom(0), "\"/>\n");
                }
            }
            out.print(n>1 ? "</field>\n" : "</set>\n");
        }
    }

    /** Convenience helper method that writes out sig "s", and records all its field values. */
    private void writeSig(Sig s) throws Err {
        if (s==UNIV || s==NONE) return;
        A4TupleSet ts = sol==null ? null : ((A4TupleSet)(sol.eval(s)));
        if (s instanceof SubsetSig) {
            if (ts==null) {
                Util.encodeXMLs(out, "\n<sig name=\"", sig2name.get(s), "\" extends=\"set\"> <atom name=\"", sig2name.get(s), "\"/> </sig>\n");
                subset=true;
            } else {
                writeTS(ts, sig2name.get(s), s.type);
            }
        } else {
            Util.encodeXMLs(out, "\n<sig name=\"", sig2name.get(s), "\" extends=\"", sig2name.get(((PrimSig)s).parent), "\"");
            if (s.isOne!=null) out.printf(" isOne=\"true\"");
            if (s.isAbstract!=null) out.printf(" isAbstract=\"true\"");
            if (s.builtin) out.printf(" isBuiltin=\"true\"");
            if (s.isOrdered!=null) out.printf(" isOrdered=\"true\"");
            out.printf(">\n");
            if (ts!=null) {
                for(A4Tuple t:ts) Util.encodeXMLs(out, "  <atom name=\"", t.atom(0), "\"/>\n");
            } else {
                Util.encodeXMLs(out, "  <atom name=\"", sig2name.get(s), "\"/>\n");
            }
            out.printf("</sig>\n");
        }
        for(Field f:s.getFields()) {
            if (sol!=null) {
                ts = (A4TupleSet)(sol.eval(f));
                writeTS(ts, field2name.get(f), f.type);
            } else {
                writeMetaTS(sig2name.get(s), field2name.get(f), f.type);
            }
        }
    }

    /**
     * If sol==null, write the list of Sigs as a Metamodel, else write the solution as an XML file.
     *
     * <p> If two or more sig have the same name, we append ' to the names until no more conflict.
     * <p> If two or more fields have the same name and overlapping first column, we append ' to the names until no more conflict.
     */
    private A4SolutionWriter(A4Solution sol, ConstList<Sig> sigs, String originalFileName, PrintWriter out, Iterable<Func> allMacros) throws Err {
        this.out=out;
        this.sol=sol;
        // We only write out satisfiable instance
        if (sol!=null && !sol.satisfiable())
           throw new ErrorAPI("This solution is unsatisfiable, so there is nothing to write to an XML file.");
        // Add all sig names into the "has seen" set; along the way, rename the sigs so that we don't have duplicate names
        for(Sig s:new Sig[]{UNIV,SIGINT,SEQIDX,NONE}) { // We first add the builtin sigs
            sig2name.put(s, un.make(s.label.length()==0 ? BLANK : s.label));
        }
        for(Sig s:sigs) if (!s.builtin) { // Then we add the non-builtin sigs
            String label = s.label;
            // Many A4Solution objects will have the repetitive "this/" in front of the sig names (since that is
            // the convention of alloy4compiler), so removing "this/" will make the output look nicer.
            // This renaming is safe, since we'll pass it into UniqueNameGenerator to ensure no name clash anyway.
            if (label.startsWith("this/")) label=label.substring(5);
            sig2name.put(s, un.make(label.length()==0 ? BLANK : label));
        }
        // Rename the fields if necessary, and add the field names into the "has seen" set
        for(Sig s:sigs) for(Field f:s.getFields()) {
           String fl = f.label.length()==0 ? BLANK : f.label;
           again:
           while(true) {
             for(Map.Entry<Field,String> e:field2name.entrySet())
                if (fl.equals(e.getValue()) && e.getKey().type.firstColumnOverlaps(f.type))
                   {fl=fl+"'"; continue again;}
             field2name.put(f, fl);
             un.seen(fl);
             break;
           }
        }
        // Write out every sig and field
        Util.encodeXMLs(out, "<instance filename=\"", originalFileName);
        if (sol!=null) {
            Util.encodeXMLs(out, "\" bitwidth=\"", Integer.toString(sol.getBitwidth()),
              "\" command=\"", sol.getOriginalCommand(), "\">\n");
        } else {
            Util.encodeXMLs(out, "\" isMetamodel=\"true\" command=\"show metamodel\">\n");
        }
        writeSig(SIGINT);
        writeSig(SEQIDX);
        for(Sig s:sigs) if (!s.builtin) writeSig(s);
        // Write out all parameter-less Function in the main module
        if (sol!=null) for(final Func pf:allMacros) if (!pf.isPred && pf.params.size()==0) {
            String rname=tail(pf.label);
            while(rname.length()>0 && rname.charAt(0)=='$') rname=rname.substring(1);
            if (rname.length()==0) rname=BLANK;
            rname=un.make("$"+rname);
            A4TupleSet ts;
            try {
                final Object obj=sol.eval(pf.getBody());
                if (!(obj instanceof A4TupleSet)) continue;
                ts=(A4TupleSet)obj;
            } catch(Throwable ex) { continue; } // This is not fatal
            writeTS(ts, rname, pf.returnDecl.type);
        }
        // Write out any Skolem relations that were generated by Kodkod
        if (sol!=null) for(Pair<String,Pair<Type,A4TupleSet>> r:sol.skolems()) {
            String rname=tail(r.a);
            while(rname.length()>0 && rname.charAt(0)=='$') rname=rname.substring(1);
            if (rname.length()==0) rname=BLANK;
            writeTS(r.b.b, un.make("$"+rname), r.b.a);
        }
        // Write UNIV, and then write the "extends" and "in" relations
        if (sol==null) {
            String univ = sig2name.get(UNIV);
            Util.encodeXMLs(out, "<sig name=\"", univ, "\"> <atom name=\"", univ, "\"/> </sig>\n\n");
            if (subset) {
               Util.encodeXMLs(out, "<sig name=\"set\"> </sig>\n\n");
               Util.encodeXMLs(out, "<field name=\"in\">\n  <type> <sig name=\"set\"/> <sig name=\"", univ, "\"/> </type>\n");
               for(Sig s:sigs) if (s instanceof SubsetSig)
                 for(Sig p:((SubsetSig)s).parents) if (p!=UNIV)
                   Util.encodeXMLs(out, "  <tuple> <atom name=\"", sig2name.get(s), "\"/> <atom name=\"", sig2name.get(p), "\"/> </tuple>\n");
               out.print("</field>\n\n");
            }
            Util.encodeXMLs(out, "<field name=\"extends\">\n  <type> <sig name=\"", univ, "\"/> <sig name=\"", univ, "\"/> </type>\n");
            for(Sig s:sigs) if (s!=UNIV && s!=NONE && s instanceof PrimSig)
               Util.encodeXMLs(out,"  <tuple> <atom name=\"", sig2name.get(s),"\"/> <atom name=\"", sig2name.get(((PrimSig)s).parent),"\"/> </tuple>\n");
            out.print("</field>\n\n");
        }
        // Done!
        out.print("\n</instance>\n");
        if (sol!=null && sol.getOriginalFormula().length()>0) {
            Util.encodeXMLs(out, "\n<koutput value=\"", sol.toString(), "\"/>\n\n<kinput value=\"", sol.getOriginalFormula(), "\"/>\n");
        }
    }

    /**
     * If this solution is a satisfiable solution,
     * this method will write it out as &lt;instance&gt;..&lt;/instance&gt; in XML format.
     *
     * <p> If two or more sig have the same name, we append ' to the names until no more conflict.
     * <p> If two or more fields have the same name and overlapping type, we append ' to the names until no more conflict.
     */
    public static void writeInstance(A4Solution sol, PrintWriter out, Iterable<Func> macros) throws Err {
        try {
            new A4SolutionWriter(sol, sol.getAllReachableSigs(), sol.getOriginalFilename(), out, macros);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Error writing the A4Solution XML file.",ex);
        }
        if (out.checkError()) throw new ErrorFatal("Error writing the A4Solution XML file.");
    }

    /**
     * Write the metamodel as &lt;instance&gt;..&lt;/instance&gt; in XML format.
     */
    public static void writeMetamodel(ConstList<Sig> sigs, String originalFilename, PrintWriter out) throws Err {
        try {
            new A4SolutionWriter(null, sigs, originalFilename, out, new ArrayList<Func>(1));
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Error writing the A4Solution XML file.",ex);
        }
        if (out.checkError()) throw new ErrorFatal("Error writing the A4Solution XML file.");
    }
}
