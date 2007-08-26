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
import edu.mit.csail.sdg.alloy4.ConstSet;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.UniqueNameGenerator;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4.ConstSet.TempSet;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;

/** Immutable; represents an Alloy solution (which is either satisfiable or unsatisfiable). */

public final class A4Solution {

    /** The integer bitwidth of this solution's model; always between 1 and 30. */
    private final int bitwidth;

    /** The original Alloy main model's file name that generated this solution; can be "" if unknown. */
    private final String filename;

    /** The original command used to generate this solution; can be "" if unknown. */
    private final String command;

    /** The original formula (in Java source code format) used to generate this solution; can be "" if unknown. */
    public final String formula;

    /** The original formula (as a Kodkod formula object) used to generate this solution; can be null if unknown. */
    private final Formula kFormula;

    /** The list of all sigs in this solution's model; always complete, has UNIV+SIGINT+SEQIDX+NONE, and has no duplciates. */
    private final ConstList<Sig> sigs;

    /** If not empty, it maps each Sig/Field (and possibly even some parameterless Func) to a Kodkod expression. */
    private final ConstMap<Object,Expression> bcc;

    /** If satisfiable, then kEval!=null and can be used to evaluate; if unsatisfiable, then kEval==null. */
    private final Evaluator kEval;

    /** The unmodifiable original Kodkod bounds (can be null if unknown) */
    private final Bounds kBounds;

    /** This maps each Kodkod atom to its corresponding Alloy atom. */
    private final ConstMap<Object,String> k2atom;

    /** This maps each Kodkod atom to its Most-Specific-Sig. */
    private final ConstMap<Object,PrimSig> k2sig;

    /** The unmomdifiable original kodkod unsat core (can be empty if unknown) */
    private final ConstList<Formula> core;

    /** The map from kodkod Formula to Alloy Expr or Alloy Pos (can be empty if unknown) */
    private final ConstMap<Formula,Object> fmap;

    /** Modifiable Skolem->Type map. */
    private final ConstMap<Relation,Type> skolem2type;

    //============================================================================================================================//

    /** Recursively rename all atoms to be of the form "SIGNAME$INDEX" where SIGNAME is the most-specific-sig. */
    private static void rename
    (Evaluator eval, ConstMap<Object,Expression> bcc, PrimSig s,
    TempMap<Object,String> map, TempMap<Object,PrimSig> map2, UniqueNameGenerator un)
    throws Err {
        for(PrimSig c:s.children()) rename(eval, bcc, c, map, map2, un);
        List<String> list=null;
        for(Tuple t:eval.evaluate(bcc.get(s))) {
            String a=t.atom(0).toString();
            if (map2.containsKey(a)) continue; // This means one of the subsig has already claimed this atom.
            map2.put(t.atom(0), s);
            if (list==null) list=new ArrayList<String>();
            list.add(a);
        }
        if (list==null) return;
        // Compute the width of the index (eg. width("0")=1,  width("7")=1,  width("23")=2, etc)
        int width=1;
        for(int i=list.size()-1; i>=10; i=i/10) width++;
        // Now, generate the new name. By prepending enough 0 to line up the numbers, we ensure the atoms will sort lexicographically
        StringBuilder sb = new StringBuilder();
        String signame = s.toString();
        // Many A4Solution objects will have the repetitive "this/" in front of the sig names (since that is
        // the convention of alloy4compiler), so removing "this/" will make the output look nicer.
        // This renaming is safe, since we'll pass it into UniqueNameGenerator to ensure no name clash anyway.
        if (signame.startsWith("this/")) signame=signame.substring(5);
        signame=un.make(signame);
        for(int i=0; i<list.size(); i++) {
            sb.delete(0, sb.length());
            sb.append(signame);
            sb.append('$');
            String x=Integer.toString(i);
            int xlen=x.length();
            while(xlen<width) {sb.append('0'); xlen++;}
            sb.append(x);
            map.put(list.get(i), sb.toString());
        }
    }

    //============================================================================================================================//

    /**
     * Construct an A4Solution object representing a satisfiable or unsatisfiable solution from Kodkod.
     * @param sigs - the list of Sigs; must be complete, must contain UNIV+SIGINT+SEQIDX+NONE, and has no duplicates
     * @param bcc - maps each Sig and Field (and possibly even some parameterless Func) to a Kodkod expression
     * @param filename - original Alloy main model's file name that generated this solution; can be "" if unknown
     * @param sources - original Alloy models file names and file contents used to generate this solution; can be empty if unknown
     * @param command - original command used to generate this solution; can be "" if unknown
     * @param kEnumerator - if nonnull, we can ask it to get another solution
     * @param kFormula - original formula (as a Kodkod formula object) used to generate this solution; can be null if unknown
     * @param kBounds - original Kodkod bounds used to generate this solution; can be null if unknown
     * @param bitwidth - the integer bitwidth of this solution's model; must be between 1 and 30
     * @param kInstance - the Kodkod instance represented by this A4Solution; nonnull iff this solution is satisfiable
     * @param skolem2type - this maps each known Skolem to its Alloy type
     * @param fmap - map from kodkod Formula to Alloy Expr or Alloy Pos; can be empty or null if the map is unavailable
     * @param core - unsat core in terms of Kodkod formulas; can be null or empty if unknown
     */
    A4Solution(Iterable<Sig> sigs, ConstMap<Object,Expression> bcc, String filename,
    String command, Iterator<Solution> kEnumerator, Formula kFormula, Bounds kBounds, int bitwidth,
    Instance kInstance, Map<Relation,Type> skolem2type, Map<Formula,Object> fmap, Iterable<Formula> core)
    throws Err {
        this.sigs = ConstList.make(sigs);
        this.skolem2type = ConstMap.make(skolem2type);
        this.bcc = ConstMap.make(bcc);
        this.filename = (filename==null ? "" : filename);
        this.command = (command==null ? "" : command);
        this.kEnumerator = kEnumerator;
        this.kBounds = (kBounds!=null ? kBounds.clone() : null);
        this.fmap = ConstMap.make(fmap);
        this.core = ConstList.make(core);
        this.bitwidth = bitwidth;
        if (bitwidth<1 || bitwidth>30) throw new ErrorAPI("The integer bitwidth must be between 1 and 30.");
        TempMap<Object,String> m1 = new TempMap<Object,String>();
        TempMap<Object,PrimSig> m2 = new TempMap<Object,PrimSig>();
        if (kInstance==null) {
            kEval = null;
        } else {
            final UniqueNameGenerator un = new UniqueNameGenerator();
            final Options options = new Options();
            options.setBitwidth(bitwidth);
            kEval = new Evaluator(kInstance.clone(), options);
            // We first process SIGINT
            for(Tuple t:kEval.evaluate(Relation.INTS)) { m2.put(t.atom(0), SIGINT); }
            // We then process SEQIDX so we override the old mapping to SIGINT with the new mapping to SEQIDX
            for(Tuple t:kEval.evaluate(BoundsComputer.SEQ_SEQIDX)) { m2.put(t.atom(0), SEQIDX); }
            // Now, process the non-builtin sigs
            for(Map.Entry<Object,Expression> e:this.bcc.entrySet()) {
                if (e.getKey() instanceof PrimSig) {
                    PrimSig s=(PrimSig)(e.getKey());
                    if (!s.builtin && s.isTopLevel()) rename(kEval, this.bcc, s, m1, m2, un);
                }
            }
            // These are redundant atoms that were not chosen to be in the final instance
            int unused=0;
            for(Tuple tuple: kEval.evaluate(Relation.UNIV)) {
                Object atom = tuple.atom(0);
                if (!m2.containsKey(atom)) { m1.put(atom, "unused"+unused); unused++; }
            }
        }
        k2atom = m1.makeConst();
        k2sig = m2.makeConst();
        if (kFormula!=null && this.kBounds!=null) {
            this.formula=TranslateKodkodToJava.convert(kFormula, bitwidth, this.kBounds.universe().iterator(), this.kBounds, k2atom);
        } else {
            this.formula="";
        }
        this.kFormula = kFormula;
    }

    //============================================================================================================================//

    private static void addAllSubrelation(IdentitySet<Relation> set, Expression ex) { // TODO
        while(ex instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)ex;
            if (b.op() != BinaryExpression.Operator.UNION && b.op() != BinaryExpression.Operator.PRODUCT) return;
            addAllSubrelation(set, b.left());
            ex = b.right();
        }
        if (ex instanceof Relation) set.add((Relation)ex);
    }

    // Write out any Skolem relations that were generated by Kodkod
    public synchronized List<Pair<String,Pair<Type,A4TupleSet>>> skolems() { // TODO
        Instance inst = kEval.instance();
        List<Pair<String,Pair<Type,A4TupleSet>>> ans = new ArrayList<Pair<String,Pair<Type,A4TupleSet>>>();
        IdentitySet<Relation> rels = new IdentitySet<Relation>();
        for(Sig s:getAllReachableSigs()) {
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
            Pair<Type,A4TupleSet> ta = new Pair<Type,A4TupleSet>(t, new A4TupleSet(inst.tuples(r), k2atom, k2sig));
            Pair<String,Pair<Type,A4TupleSet>> sta = new Pair<String,Pair<Type,A4TupleSet>>(rn,ta);
            ans.add(sta);
        }
        return ans;
    }

    //============================================================================================================================//

    /** If not null, you can ask it to get another solution; this field must be synchronized. */
    private final Iterator<Solution> kEnumerator;

    /** If nonnull, it caches the result of calling "next()"; this field must be synchronized. */
    private A4Solution nextCache = null;

    /**
     * If this solution is UNSAT, return itself; else return the next solution (which could be SAT or UNSAT).
     * @throws ErrorAPI if the solver was not an incremental solver
     */
    public synchronized A4Solution next() throws Err {
        if (kEval==null) return this; // Since the solution is UNSAT, just return myself
        if (nextCache!=null) return nextCache; // Since the result was already computed, return it
        if (kEnumerator==null) throw new ErrorAPI("This solution was not generated by an incremental SAT solver.\n"
            +"Solution enumeration is currently only implemented for MiniSat and SAT4J.");
        nextCache = new A4Solution(sigs, bcc, filename, command, kEnumerator, kFormula,
            kBounds, bitwidth, kEnumerator.next().instance(), skolem2type, null, null);
        return nextCache;
    }

    /** Returns true if this solution was generated by an incremental SAT solver. */
    public synchronized boolean isIncremental() { return kEnumerator!=null; }

    //============================================================================================================================//

    /**
     * If this solution is satisfiable, evaluates the given expression and returns an A4TupleSet, a java Integer, or a java Boolean.
     * @throws Err if the expression has syntax error, type error, or other errors, or is not fully typechecked
     * @throws Err if this solution is not a satisfiable solution
     */
    public Object eval(Expr expr) throws Err {
        if (!satisfiable()) throw new ErrorAPI("This solution is unsatisfiable, so no eval() is allowed.");
        if (!expr.errors.isEmpty() && expr.ambiguous) expr = expr.resolve(expr.type, new ArrayList<ErrorWarning>());
        if (!expr.errors.isEmpty()) throw expr.errors.get(0);
        Object result = TranslateAlloyToKodkod.alloy2kodkod(bcc, bitwidth, expr);
        if (result instanceof IntExpression) return kEval.evaluate((IntExpression)result);
        if (result instanceof Formula) return kEval.evaluate((Formula)result);
        if (result instanceof Expression) return new A4TupleSet(kEval.evaluate((Expression)result), k2atom, k2sig);
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }

    //============================================================================================================================//

    /** This caches the result of core() */
    private ConstSet<Pos> coreCache = null;

    /** If this solution is unsatisfiable and its unsat core is available, then return the core; else return an empty set. */
    public synchronized ConstSet<Pos> core() {
        ConstSet<Pos> answer = coreCache;
        if (answer!=null) return answer;
        TempSet<Pos> ans = new TempSet<Pos>();
        if (kEval==null) for(Formula f: core) {
           Object y = fmap.get(f);
           if (y instanceof Pos) ans.add( (Pos)y );
           if (y instanceof Expr) ans.add( ((Expr)y).span() );
           //for (Func func:expr.findAllFunctions()) ans.add(func.getBody().span()); // Include all called functions' bodies also
        }
        coreCache = (answer = ans.makeConst());
        return answer;
    }

    //============================================================================================================================//

    /** This caches the toString() output. */
    private String toStringCache = null;

    /** Dumps the Kodkod solution into String. */
    @Override public synchronized String toString() {
        if (kEval==null) return "---OUTCOME---\nUnsatisfiable.\n";
        String answer = toStringCache;
        if (answer!=null) return answer;
        Instance sol = kEval.instance();
        StringBuilder sb = new StringBuilder();
        sb.append("---INSTANCE---\n" + "integers={");
        boolean firstTuple=true;
        for(IndexedEntry<TupleSet> e:sol.intTuples()) {
            if (firstTuple) firstTuple=false; else sb.append(", ");
            // No need to print e.index() since we've ensured the Int atom's String representation is always equal to ""+e.index()
            Object atom = e.value().iterator().next().atom(0);
            String aname = k2atom.get(atom);
            sb.append(aname==null ? atom.toString() : aname);
        }
        sb.append("}\n");
        for(Map.Entry<Relation,TupleSet> e:sol.relationTuples().entrySet()) {
            if (e.getKey() == BoundsComputer.SIGINT_MAX) continue; // Not interesting
            if (e.getKey() == BoundsComputer.SIGINT_MIN) continue; // Not interesting
            if (e.getKey() == BoundsComputer.SIGINT_NEXT) continue; // Not interesting
            if (e.getKey() == BoundsComputer.SIGINT_ZERO) continue; // Not interesting
            sb.append(e.getKey().name()).append("={");
            firstTuple=true;
            for(Tuple t:e.getValue()) {
                if (firstTuple) firstTuple=false; else sb.append(", ");
                for(int i=0; i<t.arity(); i++) {
                    if (i>0) sb.append("->");
                    String aname = k2atom.get(t.atom(i));
                    sb.append(aname==null ? t.atom(i).toString() : aname);
                }
            }
            sb.append("}\n");
        }
        answer = (sb.append("\n")).toString();
        toStringCache = answer;
        return answer;
    }

    //============================================================================================================================//

    /** Returns true iff the problem is satisfiable. */
    public boolean satisfiable() { return kEval!=null; }

    /** Returns the list of all sigs in this solution's model; always contains UNIV+SIGINT+SEQIDX+NONE and has no duplicates. */
    public ConstList<Sig> getAllReachableSigs() { return sigs; }

    /** Returns the integer bitwidth of this solution's model; always between 1 and 30. */
    public int getBitwidth() { return bitwidth; }

    /** Returns the original formula (in Java source code format) used to generate this solution; can be "" if unknown. */
    public String getOriginalFormula() { return formula; }

    /** Returns the original command used to generate this solution; can be "" if unknown. */
    public String getOriginalCommand() { return command; }

    /** Returns the original Alloy main model's file name that generated this solution; can be "" if unknown. */
    public String getOriginalFilename() { return filename; }
}
