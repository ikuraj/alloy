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

import java.io.IOException;
import java.io.PrintWriter;
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
import kodkod.instance.TupleFactory;
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
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4.ConstSet.TempSet;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
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

    /** This maps each Alloy4 atom to its corresponding Alloy parameter-less function. */
    private final ConstMap<String,Func> a2func;

    /** This maps each Kodkod atom to its corresponding Alloy atom. */
    private final ConstMap<Object,String> k2atom;

    /** This maps each Kodkod atom to its Most-Specific-Sig. */
    private final ConstMap<Object,PrimSig> k2sig;

    /** The unmomdifiable original kodkod low-level unsat core (can be empty if unknown) */
    private final ConstList<Formula> core;

    /** The unmomdifiable original kodkod high-level unsat core (can be empty if unknown) */
    private final ConstList<Formula> hcore;

    /** The map from kodkod Formula to Alloy Expr or Alloy Pos (can be empty if unknown) */
    private final ConstMap<Formula,Object> fmap;

    /** The map from Kodkod Relation to Alloy Type (can be empty or incomplete if unknown) */
    private final ConstMap<Relation,Type> rel2type;

    //============================================================================================================================//

    /** Returns the map that maps each Alloy4 atom to a Alloy4 function whose value is that atom. */
    public ConstMap<String,Func> getAllAtoms() { return a2func; }

    /** Recursively rename all atoms to be of the form "SIGNAME$INDEX" where SIGNAME is the most-specific-sig. */
    private static void rename
    (Evaluator eval, Map<Object,Expression> bcc, PrimSig s,
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
     * @param incoming_bcc - maps each Sig and Field (and possibly even some parameterless Func) to a Kodkod expression
     * @param filename - original Alloy main model's file name that generated this solution; can be "" if unknown
     * @param command - original command used to generate this solution; can be "" if unknown
     * @param kEnumerator - if nonnull, we can ask it to get another solution
     * @param kFormula - original formula (as a Kodkod formula object) used to generate this solution; can be null if unknown
     * @param kBounds - original Kodkod bounds used to generate this solution; can be null if unknown
     * @param bitwidth - the integer bitwidth of this solution's model; must be between 1 and 30
     * @param kInstance - the Kodkod instance represented by this A4Solution; nonnull iff this solution is satisfiable
     * @param rel2type - this maps each known Skolem Relation to its Alloy type (can be empty or null if unknown or incomplete)
     * @param fmap - map from kodkod Formula to Alloy Expr or Alloy Pos; can be empty or null if the map is unavailable
     * @param core - low-level unsat core in terms of Kodkod formulas; can be null or empty if unknown
     * @param hcore - high-level unsat core in terms of Kodkod formulas; can be null or empty if unknown
     */
    A4Solution(Iterable<Sig> sigs, Map<Object,Expression> incoming_bcc, String filename,
    String command, Iterator<Solution> kEnumerator, Formula kFormula, Bounds kBounds, int bitwidth,
    Instance kInstance, Map<Relation,Type> rel2type, Map<Formula,Object> fmap,
    Iterable<Formula> core, Iterable<Formula> hcore)
    throws Err {
        ConstMap.TempMap<Object,Expression> bcc = new ConstMap.TempMap<Object,Expression>(incoming_bcc);
        if (kInstance!=null) kInstance = kInstance.clone();
        this.sigs = ConstList.make(sigs);
        this.rel2type = ConstMap.make(rel2type);
        this.filename = (filename==null ? "" : filename);
        this.command = (command==null ? "" : command);
        this.kEnumerator = kEnumerator;
        this.kBounds = (kBounds!=null ? kBounds.clone() : null);
        this.fmap = ConstMap.make(fmap);
        this.core = ConstList.make(core);
        this.hcore = ConstList.make(hcore);
        this.bitwidth = bitwidth;
        if (bitwidth<1 || bitwidth>30) throw new ErrorAPI("The integer bitwidth must be between 1 and 30.");
        TempMap<Object,String> m1 = new TempMap<Object,String>();
        TempMap<Object,PrimSig> m2 = new TempMap<Object,PrimSig>();
        TempMap<String,Func> m3 = new TempMap<String,Func>();
        if (kInstance==null) {
            kEval = null;
        } else {
            final UniqueNameGenerator un = new UniqueNameGenerator();
            final Options options = new Options();
            options.setBitwidth(bitwidth);
            kEval = new Evaluator(kInstance, options);
            // We first process SIGINT
            for(Tuple t:kEval.evaluate(Relation.INTS)) { m2.put(t.atom(0), SIGINT); }
            // We then process SEQIDX so we override the old mapping to SIGINT with the new mapping to SEQIDX
            for(Tuple t:kEval.evaluate(BoundsComputer.SEQ_SEQIDX)) { m2.put(t.atom(0), SEQIDX); }
            // Now, process the non-builtin sigs
            for(Map.Entry<Object,Expression> e:incoming_bcc.entrySet()) {
                if (e.getKey() instanceof PrimSig) {
                    PrimSig s=(PrimSig)(e.getKey());
                    if (!s.builtin && s.isTopLevel()) rename(kEval, incoming_bcc, s, m1, m2, un);
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
        if (kInstance!=null) { // add each atom as a macro
            List<ExprVar> empty = new ArrayList<ExprVar>();
            TupleFactory tf = kInstance.universe().factory();
            for(Object atom: kInstance.universe()) {
                PrimSig ret = k2sig.get(atom);
                String n = k2atom.get(atom);
                if (ret!=null && n!=null) { // sanity check
                    Func func = new Func(null, n, empty, ret);
                    Relation r = Relation.unary(n);
                    kInstance.add(r, tf.range(tf.tuple(atom), tf.tuple(atom)));
                    bcc.put(func, r);
                    m3.put(n, func);
                }
            }
        }
        this.bcc = bcc.makeConst();
        this.a2func = m3.makeConst();
    }

    //============================================================================================================================//

    /** Assuming "ex" is a product and/or union of Relations, add every mentioned Relation into "set". */
    private static void addAllSubrelation(IdentitySet<Relation> set, Expression ex) {
        while(ex instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)ex;
            if (b.op() != BinaryExpression.Operator.UNION && b.op() != BinaryExpression.Operator.PRODUCT) return;
            addAllSubrelation(set, b.left());
            ex = b.right();
        }
        if (ex instanceof Relation) set.add((Relation)ex);
    }

    /** Return the list of all skolems (each skolem has a name, a Type, and its tupleset. */
    public List<Pair<String,Pair<Type,A4TupleSet>>> skolems() {
        List<Pair<String,Pair<Type,A4TupleSet>>> ans = new ArrayList<Pair<String,Pair<Type,A4TupleSet>>>();
        if (kEval==null) return ans; // That means the solution is unsatisfiable, so there are no skolem values at all
        Instance inst = kEval.instance();
        IdentitySet<Relation> rels = new IdentitySet<Relation>();
        for(Sig s:getAllReachableSigs()) {
            addAllSubrelation(rels, bcc.get(s));
            for(Field f:s.getFields()) addAllSubrelation(rels, bcc.get(f));
        }
        rels.add(BoundsComputer.SEQ_SEQIDX);
        rels.add(BoundsComputer.SIGINT_MAX);
        rels.add(BoundsComputer.SIGINT_MIN);
        rels.add(BoundsComputer.SIGINT_ZERO);
        rels.add(BoundsComputer.SIGINT_NEXT);
        for(final Relation r:inst.relations()) if (!rels.contains(r)) {
            Type t=rel2type.get(r);
            if (t==null) continue; // That means we don't know its type; this should NOT have happened, but let's be safe
            while (t.arity() < r.arity()) t=UNIV.type.product(t);
            if (t.arity() > r.arity()) continue; // That means something terrible has happened, so let's skip it
            String n=r.name();
            while(n.length()>0 && n.charAt(0)=='$') n=n.substring(1);
            Pair<Type,A4TupleSet> ta = new Pair<Type,A4TupleSet>(t, new A4TupleSet(inst.tuples(r), k2atom, k2sig));
            ans.add( new Pair<String,Pair<Type,A4TupleSet>>(n, ta) );
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
            kBounds, bitwidth, kEnumerator.next().instance(), rel2type, fmap, null, null);
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
        if (expr.ambiguous && !expr.errors.isEmpty()) expr = expr.resolve(expr.type, new ArrayList<ErrorWarning>());
        if (!expr.errors.isEmpty()) throw expr.errors.get(0);
        Object result = TranslateAlloyToKodkod.alloy2kodkod(bcc, bitwidth, expr);
        if (result instanceof IntExpression) return kEval.evaluate((IntExpression)result);
        if (result instanceof Formula) return kEval.evaluate((Formula)result);
        if (result instanceof Expression) return new A4TupleSet(kEval.evaluate((Expression)result), k2atom, k2sig);
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }

    //============================================================================================================================//

    /** This caches the result of lowLevelCore(); this field must be synchronized. */
    private Pair<ConstSet<Pos>,ConstSet<Pos>> lCoreCache = null;

    /** If this solution is unsatisfiable and its unsat core is available, then return the core; else return an empty set. */
    public synchronized Pair<ConstSet<Pos>,ConstSet<Pos>> lowLevelCore() {
        Pair<ConstSet<Pos>,ConstSet<Pos>> answer = lCoreCache;
        if (answer!=null) return answer;
        TempSet<Pos> ans1 = new TempSet<Pos>();
        ConstSet<Pos> ans2 = ConstSet.make();
        if (kEval==null) for(Formula f: core) {
           Object y = fmap.get(f);
           if (y instanceof Pos) ans1.add( (Pos)y ); else if (y instanceof Expr) ans1.add( ((Expr)y).span() );
        }
        answer = new Pair<ConstSet<Pos>,ConstSet<Pos>>(ans1.makeConst(), ans2);
        return lCoreCache = answer;
    }

    //============================================================================================================================//

    /** This caches the result of highLevelCore(); this field must be synchronized. */
    private Pair<ConstSet<Pos>,ConstSet<Pos>> hCoreCache = null;

    /** If this solution is unsatisfiable and its unsat core is available, then return the core; else return an empty set. */
    public synchronized Pair<ConstSet<Pos>,ConstSet<Pos>> highLevelCore() {
        Pair<ConstSet<Pos>,ConstSet<Pos>> answer = hCoreCache;
        if (answer!=null) return answer;
        TempSet<Pos> ans1 = new TempSet<Pos>(), ans2 = new TempSet<Pos>();
        if (kEval==null) for(Formula f: hcore) {
           Object y = fmap.get(f);
           if (y instanceof Pos) {
               ans1.add( (Pos)y );
           } else if (y instanceof Expr) {
               Expr expr = (Expr)y;
               ans1.add(expr.span());
               for (Func func: expr.findAllFunctions()) ans2.add(func.getBody().span());
           }
        }
        answer = new Pair<ConstSet<Pos>,ConstSet<Pos>>(ans1.makeConst(), ans2.makeConst());
        return hCoreCache = answer;
    }

    //============================================================================================================================//

    /** This caches the toString() output; this field must be synchronized. */
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

    /** Helper method to write out a full XML file. */
    public void writeXML(String filename) throws Err {
        PrintWriter out=null;
        try {
            out=new PrintWriter(filename,"UTF-8");
            Util.encodeXMLs(out, "\n<alloy builddate=\"", Version.buildDate(), "\">\n\n");
            A4SolutionWriter.writeInstance(this, out, new ArrayList<Func>());
            out.print("\n</alloy>\n");
            if (!Util.close(out)) throw new ErrorFatal("Error writing to the A4Solution XML file "+filename);
        } catch(IOException ex) {
            Util.close(out);
            throw new ErrorFatal("Error writing to the A4Solution XML file "+filename);
        }
    }
}
