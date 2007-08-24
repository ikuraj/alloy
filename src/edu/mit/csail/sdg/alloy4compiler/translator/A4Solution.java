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

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import kodkod.ast.BinaryExpression;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;
import kodkod.util.ints.IndexedEntry;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;

/** Immutable; represents an Alloy solution (which is either satisfiable or unsatisfiable). */

public final class A4Solution {

    private int z;

    //================ IMMUTABLE FIELDS ==========================================================================================//

    /** The integer bitwidth of this solution's model; always between 1 and 30. */
    private final int bitwidth;

    /** The original Alloy models file names and file contents used to generate this solution; can be empty if unknown. */
    private final ConstMap<String,String> sources;

    /** The original Alloy main model's file name that generated this solution; can be "" if unknown. */
    private final String filename;

    /** The original command used to generate this solution; can be "" if unknown. */
    private final String command;

    /** The original formula (in expanded Java format) used to generate this solution; can be "" if unknown. */
    public final String formula;

    /** The original formula (as a Kodkod formula object) used to generate this solution; can be null if unknown. */
    private final Formula kFormula;

    /** If not empty, it maps each Sig/Field (and possibly even parameterless Func) to Kodkod expressions. */
    private final ConstMap<Object,Expression> bcc;

    /** If satisfiable, then kEval!=null and can be used to evaluate; if unsatisfiable, then kEval==null. */
    private final Evaluator kEval;

    /** If not null, it's the unmodifiable original Kodkod bounds. */
    private final Bounds kBounds;

    /** The list of all sigs in this solution's model; always complete, has UNIV+SIGINT+SEQIDX+NONE, and has no duplciates. */
    private final ConstList<Sig> sigs;

    /** This maps each Kodkod atom to its corresponding Alloy atom. */
    private final ConstMap<Object,String> map;

    /** This maps each Kodkod atom to its Most-Specific-Sig. */
    private final ConstMap<Object,PrimSig> map2sig;

    //================ MUTABLE FIELDS ============================================================================================//

    /** If nonnull, it caches the result of calling "next()" */
    private A4Solution next=null;

    /** If not null, you can ask it to get another solution. */
    private final Iterator<Solution> kEnumerator;

    /** If not null, it's the unmomdifiable original kodkod unsat core. */
    private final IdentitySet<Formula> kCore;

    /** The map from kodkod unsat core back to Alloy AST (or null if the map is unavailable) */
    private final Map<Formula,List<Object>> core;

    /** Modifiable Skolem->Type map. */
    private final Map<Relation,Type> skolem2type;

    //============================================================================================================================//

    /** Private constructor to ensure TranslateAlloyToKodkod is the only one who can construct this. */
    A4Solution(Iterable<Sig> sigs, ConstMap<Object,Expression> bcc, String filename, Map<String,String> sources,
    String command, Iterator<Solution> kEnumerator, Formula kFormula, Bounds kBounds, int bitwidth, Instance kInstance,
    Map<Relation,Type> skolem2type, Map<Formula,List<Object>> core, IdentitySet<Formula> kCore) throws Err {
        this.skolem2type=skolem2type;
        this.sigs=ConstList.make(sigs);
        this.bcc=ConstMap.make(bcc);
        this.filename=filename;
        this.sources=ConstMap.make(sources);
        this.command=command;
        this.kEnumerator=kEnumerator;
        this.kFormula=kFormula;
        this.kBounds=kBounds.clone();
        this.bitwidth=bitwidth;
        this.core=core;
        this.kCore=kCore;
        if (kInstance!=null) {
            Options options = new Options();
            options.setBitwidth(bitwidth);
            kEval=new Evaluator(kInstance.clone(), options);
        } else {
            kEval=null;
        }
        TempMap<Object,String> m1=new TempMap<Object,String>();
        TempMap<Object,PrimSig> m2=new TempMap<Object,PrimSig>();
        if (kEval!=null) {
            for(Sig s:this.sigs) if (!s.builtin && s.isTopLevel()) rename((PrimSig)s,m1,m2);
            int unused=0;
            for(Tuple tuple: kEval.evaluate(Relation.UNIV)) {
                Object atom = tuple.atom(0);
                String atomstr = atom.toString();
                if (atomstr.indexOf('$')<0) { m2.put(atom, SIGINT); continue; }
                if (!m1.containsKey(atom)) { m1.put(atom, "unused"+unused); unused++; }
            }
        }
        this.map = m1.makeConst();
        this.map2sig = m2.makeConst();
        if (kFormula!=null && this.kBounds!=null) {
            this.formula=TranslateKodkodToJava.convert(kFormula, bitwidth, this.kBounds.universe().iterator(), this.kBounds, map);
        } else {
            this.formula="";
        }
    }

    /** Returns true if this solution was generated by an incremental SAT solver. */
    public synchronized boolean isIncremental() { return kEnumerator!=null; }

    /**
     * If this solution is UNSAT, return itself; else return the next solution (which could be SAT or UNSAT).
     * @throws ErrorAPI if the solver was not an incremental solver
     */
    public synchronized A4Solution next() throws Err {
        if (kEval==null) return this; // If UNSAT, then return myself.
        if (next!=null) return next; // If result is already cached, then return it.
        if (kEnumerator==null)
            throw new ErrorAPI("This solution was not generated by an incremental SAT solver.\n"
            +"Solution enumeration is currently only implemented for MiniSat and SAT4J.");
        Solution sol=kEnumerator.next();
        next=new A4Solution(sigs, bcc, filename, sources, command, kEnumerator,
                kFormula, kBounds, bitwidth,
                sol.instance(), skolem2type, core, null);
        return next;
    }

    /** Map old atom names to new atom names. */
    private String a2s(Object atom) {
        String ans=map.get(atom);
        if (ans==null) return atom.toString(); else return ans;
    }

    /**
     * Recursively rename all atoms to be of the form "SIGNAME[INDEX]" where SIGNAME is the most-specific-sig.
     * <br> <b>PRECONDITION:</b> kEval!=null && bc!=null
     */
    private void rename(PrimSig s, TempMap<Object,String> map, TempMap<Object,PrimSig> map2sig) throws Err {
        for(PrimSig c:s.children()) rename(c, map, map2sig);
        List<String> list=null;
        for(Tuple t:kEval.evaluate(bcc.get(s))) {
            String a=t.atom(0).toString();
            if (map.containsKey(a)) continue; // This means one of the subsig has already claimed this atom.
            if (list==null) list=new ArrayList<String>();
            list.add(a);
        }
        if (list==null) return;
        // Compute the width of the index (eg. width("0")=1,  width("7")=1,  width("23")=2, etc)
        int width=1;
        for(int i=list.size()-1; i>=10; i=i/10) width++;
        // Now, generate the mappings
        // By prepending enough 0 to make the numbers line up, we ensure the atoms will sort lexicographically.
        StringBuilder sb = new StringBuilder();
        String signame = s.toString();
        if (signame.startsWith("this/")) signame=signame.substring(5);
        for(int i=0; i<list.size(); i++) {
            sb.delete(0, sb.length());
            sb.append(signame);
            sb.append('$');
            String x=Integer.toString(i);
            int xlen=x.length();
            while(xlen<width) {sb.append('0'); xlen++;}
            sb.append(x);
            map2sig.put(list.get(i), s);
            map.put(list.get(i), sb.toString());
        }
    }

    public synchronized IdentitySet<Pos> core() {
        IdentitySet<Pos> ans = new IdentitySet<Pos>();
        if (kCore!=null) {
            for(Formula f: kCore) {
                List<Object> x = core.get(f);
                if (x==null) continue;
                for(Object y:x) {
                    if (y instanceof Pos) ans.add( (Pos)y );
                    if (y instanceof Expr) {
                        Expr expr = (Expr)y;
                        ans.add(expr.span());
                        for(Func func:expr.findAllFunctions()) ans.add(func.getBody().span());
                    }
                }
            }
        }
        return ans;
    }

    /**
     * Evaluates the given expression based on the solution, and returns an A4TupleSet, a java Integer, or a java Boolean.
     * @throws Err if the expression has syntax error, type error, or other errors, or is not fully typechecked
     * @throws ErrorAPI if the expression is not from the same world
     * @throws ErrorAPI if this solution is not a satisfiable solution
     */
    public synchronized Object eval(Expr expr) throws Err {
        if (!satisfiable()) throw new ErrorAPI("This solution is unsatisfiable, so no eval() is allowed.");
        Object result = (new TranslateAlloyToKodkod(bcc, bitwidth, null, null)).visitThis(expr);
        if (result instanceof IntExpression) return kEval.evaluate((IntExpression)result);
        if (result instanceof Formula) return kEval.evaluate((Formula)result);
        if (result instanceof Expression) {
            TupleSet tps = (TupleSet) (kEval.evaluate((Expression)result));
            return new A4TupleSet(tps, map, map2sig);
        }
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }











    private static void addAllSubrelation(IdentitySet<Relation> set, Expression ex) {
        while(ex instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)ex;
            if (b.op() != BinaryExpression.Operator.UNION && b.op() != BinaryExpression.Operator.PRODUCT) return;
            addAllSubrelation(set, b.left());
            ex = b.right();
        }
        if (ex instanceof Relation) set.add((Relation)ex);
    }

    // Write out any Skolem relations that were generated by Kodkod
    public synchronized List<Pair<String,Pair<Type,A4TupleSet>>> skolems() {
        Instance inst = kEval.instance();
        List<Pair<String,Pair<Type,A4TupleSet>>> ans = new ArrayList<Pair<String,Pair<Type,A4TupleSet>>>();
        IdentitySet<Relation> rels = new IdentitySet<Relation>();
        for(Sig s:sigs) {
            addAllSubrelation(rels, bcc.get(s));
            for(Field f:s.getFields()) addAllSubrelation(rels, bcc.get(f));
        }
        for(final Relation r:inst.relations()) if (!rels.contains(r)) {
            Type t=skolem2type.get(r);
            if (t==null) continue; // That means we don't know its type
            while (t.arity() < r.arity()) t=UNIV.type.product(t);
            if (t.arity() > r.arity()) continue; // That means something terrible has happened, so let's skip it
            String rn=r.name();
            while(rn.length()>0 && rn.charAt(0)=='$') rn=rn.substring(1);
            Pair<Type,A4TupleSet> ta = new Pair<Type,A4TupleSet>(t, new A4TupleSet(inst.tuples(r), map, map2sig));
            Pair<String,Pair<Type,A4TupleSet>> sta = new Pair<String,Pair<Type,A4TupleSet>>(rn,ta);
            ans.add(sta);
        }
        return ans;
    }


    public synchronized void writeXML(String filename, Iterable<Func> macros) throws Err {
           A4SolutionWriter.write(this, filename, macros);
    }






    /** Dumps the Kodkod solution into String. */
    @Override public String toString() {
        if (kEval==null) return "---OUTCOME---\nUnsatisfiable.\n"; else return dump(kEval.instance());
    }

    /** Dumps a Kodkod solution into String. */
    private String dump(Instance sol) {
        StringBuilder sb=new StringBuilder();
        sb.append("---INSTANCE---\n");
        Iterator<IndexedEntry<TupleSet>> it=sol.intTuples().iterator();
        if (it.hasNext()) {
            sb.append("integers={");
            boolean firstTuple=true;
            while(it.hasNext()) {
                if (firstTuple) firstTuple=false; else sb.append(", ");
                IndexedEntry<TupleSet> e=it.next();
                sb.append(e.index());
                sb.append('=');
                sb.append(a2s(e.value().iterator().next().atom(0)));
            }
            sb.append("}\n");
        }
        for(Map.Entry<Relation,TupleSet> e:sol.relationTuples().entrySet()) {
            if (e.getKey().name().contains("[discard]")) continue;
            sb.append(e.getKey().name());
            sb.append("={");
            boolean firstTuple=true;
            for(Tuple t:e.getValue()) {
                if (firstTuple) firstTuple=false; else sb.append(", ");
                for(int i=0; i<t.arity(); i++) {
                    if (i>0) sb.append("->");
                    sb.append(a2s(t.atom(i)));
                }
            }
            sb.append("}\n");
        }
        sb.append("\n");
        return sb.toString();
    }


    //============================================================================================================================//

    /** Returns true iff the problem is satisfiable. */
    public boolean satisfiable() { return kEval!=null; }

    /** Returns the integer bitwidth of this solution's model; always between 1 and 30. */
    public int getBitwidth() { return bitwidth; }

    /** Returns the list of all sigs in this solution's model; always nonempty. */
    public ConstList<Sig> getAllReachableSigs() { return sigs; }

    /** Returns the original formula (in expanded Java format) used to generate this solution; can be "" if unknown. */
    public String getOriginalFormula() { return formula; }

    /** Returns the original command used to generate this solution; can be "" if unknown. */
    public String getOriginalCommand() { return command; }

    /** Returns the original Alloy main model's file name that generated this solution; can be "" if unknown. */
    public String getOriginalFilename() { return filename; }

    /** Returns the original Alloy models file names and file contents used to generate this solution; can be empty if unknown. */
    public ConstMap<String,String> getOriginalSources() { return sources; }
}
