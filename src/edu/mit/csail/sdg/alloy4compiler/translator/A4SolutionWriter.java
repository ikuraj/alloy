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
import java.io.IOException;
import java.io.PrintWriter;
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
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;

/** This helper class contains helper routines for writing an A4Solution object out as an XML file. */

final class A4SolutionWriter {

    /** If a sig label is "", or a field label is "", we use this as the name instead. */
    private static final String BLANK = "x";

    /** This maps each Sig to a name we've chosen for it. */
    private final IdentityHashMap<Sig,String> sig2name = new IdentityHashMap<Sig,String>();

    /** This maps each Field to a name we've chosen for it. */
    private final IdentityHashMap<Field,String> field2name = new IdentityHashMap<Field,String>();

    /** This is the set of unique names we've generated so far. */
    private final UniqueNameGenerator un = new UniqueNameGenerator();

    /** This is the solution we're writing out. */
    private final A4Solution sol;

    /** This is the output file. */
    private final PrintWriter out;

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
        A4TupleSet ts = (A4TupleSet)(sol.eval(s));
        if (s instanceof SubsetSig) {
            writeTS(ts, sig2name.get(s), s.type);
        } else {
            Util.encodeXMLs(out, "\n<sig name=\"", sig2name.get(s), "\"");
            if (!s.isTopLevel()) Util.encodeXMLs(out, " extends=\"", sig2name.get(((PrimSig)s).parent), "\"");
            if (s.isOne!=null) out.printf(" isOne=\"true\"");
            if (s.isAbstract!=null) out.printf(" isAbstract=\"true\"");
            if (s.builtin) out.printf(" isBuiltin=\"true\"");
            if (s.isOrdered!=null) out.printf(" isOrdered=\"true\"");
            out.printf(">\n");
            for(A4Tuple t:ts) Util.encodeXMLs(out, "  <atom name=\"", t.atom(0), "\"/>\n");
            out.printf("</sig>\n");
        }
        for(Field f:s.getFields()) {
            ts=(A4TupleSet)(sol.eval(f));
            writeTS(ts, field2name.get(f), f.type);
        }
    }

    /**
     * If this solution is a satisfiable solution, calling this constructor will write it to an XML file.
     *
     * <p> If two or more sig have the same name, we append ' to the names until no more conflict.
     * <p> If two or more fields have the same name and overlapping first column, we append ' to the names until no more conflict.
     */
    private A4SolutionWriter(A4Solution sol, PrintWriter out, Iterable<Func> allMacros) throws Err {
        this.out=out;
        this.sol=sol;
        // We only write out satisfiable instance
        if (!sol.satisfiable()) throw new ErrorAPI("This solution is unsatisfiable, so there is nothing to write to an XML file.");
        // Add all sig names into the "has seen" set; along the way, rename the sigs so that we don't have duplicate names
        ConstList<Sig> sigs = sol.getAllReachableSigs();
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
        // Write out all user-defined Sig(s) and their Field(s)
        Util.encodeXMLs(out, "\n<alloy builddate=\"", Version.buildDate(),
            "\">\n\n<instance filename=\"", sol.getOriginalFilename(),
            "\" bitwidth=\"", Integer.toString(sol.getBitwidth()),
            "\" command=\"", sol.getOriginalCommand(),"\">\n");
        // Write out every sig and field
        writeSig(SIGINT);
        writeSig(SEQIDX);
        for(Sig s:sigs) if (!s.builtin) writeSig(s);
        // Write out all parameter-less Function in the main module
        for(final Func pf:allMacros) if (!pf.isPred && pf.params.size()==0) {
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
        for(Pair<String,Pair<Type,A4TupleSet>> r:sol.skolems()) {
            String rname=tail(r.a);
            while(rname.length()>0 && rname.charAt(0)=='$') rname=rname.substring(1);
            if (rname.length()==0) rname=BLANK;
            writeTS(r.b.b, un.make("$"+rname), r.b.a);
        }
        // Done!
        out.print("\n</instance>\n");
        String originalFormula=sol.getOriginalFormula();
        if (originalFormula.length()>0) {
            Util.encodeXMLs(out, "\n<koutput value=\"", sol.toString(), "\"/>\n\n<kinput value=\"", originalFormula, "\"/>\n");
        }
        for(Map.Entry<String,String> e: sol.getOriginalSources().entrySet()) {
            Util.encodeXMLs(out, "\n<source filename=\"", e.getKey(), "\" content=\"", e.getValue(), "\"/>\n");
        }
        out.print("\n</alloy>\n");
    }

    /**
     * If this solution is a satisfiable solution, this method will write it to an XML file.
     *
     * <p> If two or more sig have the same name, we append ' to the names until no more conflict.
     * <p> If two or more fields have the same name and overlapping type, we append ' to the names until no more conflict.
     */
    public static void write(A4Solution sol, String filename, Iterable<Func> allMacros) throws Err {
        final PrintWriter out;
        try {
            out=new PrintWriter(filename,"UTF-8");
        } catch(IOException ex) {
            throw new ErrorFatal("Cannot write to the file \""+filename+"\"");
        }
        try {
            new A4SolutionWriter(sol, out, allMacros);
        } catch(Throwable ex) {
            Util.close(out);
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Error writing the A4Solution XML file.",ex);
        }
        if (!Util.close(out)) throw new ErrorFatal("Cannot write to the file \""+filename+"\"");
    }
}
