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

import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/**
 * Immutable; this class computes the bounds based on the user's instructions in a "run" or "check" command.
 *
 * <p> In particular, it computes the following 3 information:
 * <br> 1) a map from each Sig and Field to a Kodkod expression representing it
 * <br> 2) a Bounds object that defines lower and upper bound for any Kodkod relation mentioned in the map
 * <br> 3) a Formula object that, when combined with the Map and Bounds, fully express the user's intent
 */

final class BoundsComputer {

    //==============================================================================================================//

    /** The constant unary relation representing the smallest Int atom. */
    static final Relation SIGINT_MIN = Relation.unary("Int/min");

    /** The constant unary relation representing the Int atom "0". */
    static final Relation SIGINT_ZERO = Relation.unary("Int/zero");

    /** The constant unary relation representing the largest Int atom. */
    static final Relation SIGINT_MAX = Relation.unary("Int/max");

    /** The constant binary relation representing the "next" relation from each Int atom to its successor. */
    static final Relation SIGINT_NEXT = Relation.binary("Int/next");

    /** The constant unary relation representing the set of all seq/Int atoms. */
    static final Relation SEQ_SEQIDX = Relation.unary("seq/Int");

    /** This caches a readonly empty list of String. */
    private static final ArrayList<String> empty = new ArrayList<String>(0);

    /** Associate the given formula with the given Pos object, then return the given formula. */
    private static Formula add(Formula f, Pos x, Map<Formula,Object> fmap) {
        if (fmap==null || x==null || x==Pos.UNKNOWN) return f;
        if (f instanceof BinaryFormula && ((BinaryFormula)f).op()==BinaryFormula.Operator.AND) {
            add(((BinaryFormula)f).left(), x, fmap);
            add(((BinaryFormula)f).right(), x, fmap);
        }
        if (!fmap.containsKey(f)) fmap.put(f,x);
        return f;
    }

    //=============================================================================//
    // Phase 1: compute the scope, upperbound, lowerbound, then make the universe. //
    //=============================================================================//

    /**
     * Stores the Kodkod univere.
     * This field is initialized by prelim() and will not be modified afterwards.
     */
    private Universe universe;

    /**
     * Stores the integer bitwidth.
     * This field is initialized by prelim() and will not be modified afterwards.
     */
    private int bitwidth;

    /**
     * Stores the maximum sequence length.
     * This field is initialized by prelim() and will not be modified afterwards.
     */
    private int maxseq;

    /**
     * Stores the lowerbound for each signature (as a list of String).
     * This field is initialized by prelim() and will not be modified afterwards.
     */
    private final Map<PrimSig,ArrayList<String>> sig2lower = new LinkedHashMap<PrimSig,ArrayList<String>>();

    /**
     * Stores the upperbound for each signature (as a list of String).
     * This field is initialized by prelim() and will not be modified afterwards.
     */
    private final Map<PrimSig,ArrayList<String>> sig2upper = new LinkedHashMap<PrimSig,ArrayList<String>>();

    /** Returns the current lowerbound (as a list of String) */
    private ArrayList<String> sig2lower(PrimSig sig) {
        ArrayList<String> ans=sig2lower.get(sig);
        if (ans==null) return empty; else return ans;
    }

    /** Returns the current upperbound (as a list of String) */
    private ArrayList<String> sig2upper(PrimSig sig) {
        ArrayList<String> ans=sig2upper.get(sig);
        if (ans==null) return empty; else return ans;
    }

    /** Given a sig, make N atoms for it (Note: SIGINT and SEQIDX atom names are different, and must not use this method) */
    private static void makeAtom(Sig sig, ArrayList<String> list1, ArrayList<String> list2, int n) {
        if (n<=0) return;
        String name=sig.toString();
        if (name.startsWith("this/")) name=name.substring(5);
        // Compute the width of the index (eg. width("0")=1,  width("7")=1,  width("23")=2, etc)
        int width=1;
        for(int i=n-1; i>=10; i=i/10) width++;
        // Now, generate each atom using the format "SIGNAME$INDEX"
        // By prepending the index with 0 so that they're the same width, we ensure they sort lexicographically.
        StringBuilder sb=new StringBuilder();
        for(int i=0; i<n; i++) {
            sb.delete(0, sb.length());
            sb.append(name);
            sb.append('$');
            String x = Integer.toString(i);
            int xlen = x.length();
            while(xlen < width) {sb.append('0'); xlen++;}
            sb.append(x);
            x=sb.toString();
            if (list1!=null) list1.add(x);
            if (list2!=null) list2.add(x);
        }
    }

    /** Computes the lowerbound from bottom-up; it will also set a suitable initial value for each sig's upperbound. */
    private ArrayList<String> computeLowerBound(ScopeComputer sc, PrimSig sig) throws Err {
        if (sig.builtin) return empty;
        int n=sc.sig2scope(sig);
        final ArrayList<String> lower=new ArrayList<String>();
        // First, figure out what atoms *MUST* be in this sig
        for(PrimSig c:sig.children()) lower.addAll(computeLowerBound(sc,c));
        // If MUST>SCOPE, then something went wrong!
        if (n<lower.size()) {
            StringBuilder msg=new StringBuilder();
            msg.append("Scope for sig \"").append(sig).append("\" was miscalculated (").append(n).append(" < #{");
            for(String atom:lower) msg.append(' ').append(atom);
            throw new ErrorFatal(msg.append(" })").toString());
        }
        // Make a copy
        ArrayList<String> upper=new ArrayList<String>(lower);
        if (n>upper.size() && sc.isExact(sig)) {
            // If MUST<SCOPE and s is exact, then add fresh atoms to both LOWERBOUND and UPPERBOUND, then return.
            makeAtom(sig, lower, upper, n-upper.size());
        } else if (n>upper.size() && sig.isTopLevel()) {
            // If MUST<SCOPE and s is inexact but toplevel, then add fresh atoms to the UPPERBOUND, then return.
            makeAtom(sig, null, upper, n-upper.size());
        }
        sig2lower.put(sig, lower);
        sig2upper.put(sig, upper);
        return lower;
    }

    /** Computes the upperbound from top-down. */
    private ArrayList<String> computeUpperBound(ScopeComputer sc, PrimSig sig) throws Err {
        if (sig.builtin) return empty;
        // Sig's upperbound is fully computed. We recursively compute the upperbound for children...
        Set<String> x=new LinkedHashSet<String>(sig2upper(sig));
        // We remove atoms that MUST be in a subsig
        for(PrimSig c:sig.children()) x.removeAll(sig2lower(c));
        // So now X is the set of atoms that MIGHT be in this sig, but MIGHT NOT be in any particular subsig.
        // For each subsig that may need more atom, we say it could potentionally get any of the atom from X.
        for(PrimSig c:sig.children()) if (sc.sig2scope(c) > sig2lower(c).size()) {
            ArrayList<String> tmp = new ArrayList<String>(sig2lower(c));
            tmp.addAll(x);
            sig2upper.put(c, tmp);
            computeUpperBound(sc, c);
        }
        return sig2upper(sig);
    }

    /** Compute the scope, compute the upperbound and lowerbound in terms of String objects, then make the universe. */
    private void prelim(ScopeComputer sc, A4Reporter rep, SafeList<Sig> sigs) throws Err {
        // Generate SIGINT atoms
        bitwidth = sc.getBitwidth();
        maxseq = sc.getMaxSeq();
        Set<String> atoms=new LinkedHashSet<String>();
        int min = 0-(1<<(bitwidth-1)); // Safe since we know 1 <= bitwidth <= 30
        int max = (1<<(bitwidth-1))-1;
        ArrayList<String> ilist=new ArrayList<String>();
        for(int i=min; i<=max; i++) { // Safe since we know 1 <= bitwidth <= 30
            String ii=""+i; // This must be exactly like this, for we depend on the format of atom names for Int
            atoms.add(ii);
            ilist.add(ii);
        }
        sig2lower.put(SIGINT, ilist);
        sig2upper.put(SIGINT, new ArrayList<String>(ilist));
        // Generate other atoms
        for(Sig s:sigs) if (s.isTopLevel()) computeLowerBound( sc , (PrimSig)s );
        for(Sig s:sigs) if (s.isTopLevel()) atoms.addAll( computeUpperBound(sc , (PrimSig)s) );
        this.universe = new Universe(atoms);
    }

    //==================================================//
    // Phase 2: Compute everything else.                //
    //==================================================//

    /**
     * Stores the Kodkod bounds object.
     * This field is initialized by the constructor and will not be modified afterwards.
     */
    private final Bounds bounds;

    /**
     * Stores the exact bound for SIGINT.
     * This field is initialized by the constructor and will not be modified afterwards.
     */
    private final TupleSet intBounds;

    /**
     * Stores the exact bound for SEQIDX.
     * This field is initialized by the constructor and will not be modified afterwards.
     */
    private final TupleSet seqidxBounds;

    /**
     * This maps each Alloy sig/field to an Kodkod expression representing it.
     * This field is initialized by the constructor and will not be modified afterwards.
     */
    private final TempMap<Object,Expression> a2k = new TempMap<Object,Expression>();

    /**
     * Stores the additional set of constraints that are needed to bound everything correctly.
     * This field is initialized by the constructor and will not be modified afterwards.
     */
    private Formula fact = Formula.TRUE;

    /** Query the Bounds object to find the lower/upper bound; throws Err if expr is not Relation, nor a union of Relations. */
    private TupleSet query(boolean findUpper, Expression expr, boolean makeMutable) throws Err {
        if (expr==Relation.NONE) return universe.factory().noneOf(1);
        if (expr==Relation.INTS) return intBounds;
        if (expr==SEQ_SEQIDX) return seqidxBounds;
        if (expr instanceof Relation) {
            TupleSet ans = findUpper ? bounds.upperBound((Relation)expr) : bounds.lowerBound((Relation)expr);
            if (ans!=null) return makeMutable ? ans.clone() : ans;
        }
        else if (expr instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)expr;
            if (b.op() == BinaryExpression.Operator.UNION) {
                TupleSet left = query(findUpper, b.left(), true);
                TupleSet right = query(findUpper, b.right(), false);
                left.addAll(right);
                return left;
            }
        }
        throw new ErrorFatal("Unknown expression encountered during bounds computation: "+expr);
    }

    /** Add the constraint that the sig has exactly "n" elements, or at most "n" elements */
    private Formula size(Sig sig, int n, boolean exact, Map<Formula,Object> fmap) {
        Expression a = a2k.get(sig);
        if (n<=0) return a.no();
        if (n==1) return exact ? a.one() : a.lone();
        Formula f = exact ? Formula.TRUE : null;
        Decls d = null;
        Expression sum = null;
        while(n>0) {
            n--;
            Variable v = Variable.unary("");
            Decl dd = v.oneOf(a);
            if (d==null) d=dd; else d=dd.and(d);
            if (sum==null) sum=v; else { if (f!=null) f=v.intersection(sum).no().and(f); sum=v.union(sum); }
        }
        if (f!=null) return sum.eq(a).and(f).forSome(d); else return a.no().or(sum.eq(a).forSome(d));
    }

    /** Converts the list of String into a unary TupleSet. */
    private TupleSet convert(List<String> list) {
        TupleFactory f=universe.factory();
        TupleSet ans=f.noneOf(1);
        if (list!=null) for(String s:list) ans.add(f.tuple(s));
        return ans;
    }

    /** Allocate relations for nonbuiltin PrimSigs bottom-up. */
    private Expression allocatePrimSig(PrimSig sig, Map<Formula,Object> fmap) throws Err {
        // Recursively allocate all children expressions, and form the union of them
        Expression sum=null;
        for(PrimSig child:sig.children()) {
            Expression childexpr=allocatePrimSig(child, fmap);
            if (sum==null) { sum=childexpr; continue; }
            // subsigs are disjoint
            fact = add(sum.intersection(childexpr).no(), child.isSubsig, fmap).and(fact);
            sum = sum.union(childexpr);
        }
        TupleSet lower=convert(sig2lower.get(sig)), upper=convert(sig2upper.get(sig));
        if (sum==null) {
            // If sig doesn't have children, then sig should make a fresh relation for itself
            Relation r=Relation.unary(sig.label);
            bounds.bound(r, lower, upper);
            sum=r;
        } else if (sig.isAbstract==null) {
            // If sig is abstract with children, then sig == union of them. Else, declare a new relation to act as the remainder.
            Relation r=Relation.unary(sig.label+" remainder");
            for(PrimSig child:sig.children()) {
                // Remove atoms that are KNOWN to be in a subsig;
                // it's okay to mistakenly leave some atoms in, since we will never solve for the "remainder" relation directly;
                // instead, we union the remainder with the children, then solve for the combined solution.
                // (Thus, the more we can remove, the more efficient it gets, but it is not crucial for correctness)
                TupleSet childTS = query(false, a2k.get(child), false);
                lower.removeAll(childTS);
                upper.removeAll(childTS);
            }
            bounds.bound(r, lower, upper);
            sum=sum.union(r);
        }
        a2k.put(sig,sum);
        return sum;
    }

    /** Allocate relations for SubsetSig top-down. */
    private Expression allocateSubsetSig(A4Reporter rep, SubsetSig sig, Map<Formula,Object> fmap) throws Err {
        // We must not visit the same SubsetSig more than once, so if we've been here already, then return the old value right away
        Expression sum = a2k.get(sig);
        if (sum!=null) return sum;
        // Recursively form the union of all parent expressions
        TupleSet ts = universe.factory().noneOf(1);
        for(Sig parent:sig.parents) {
            Expression p = (parent instanceof PrimSig) ? a2k.get(parent) : allocateSubsetSig(rep, (SubsetSig)parent, fmap);
            ts.addAll(query(true, p, false));
            if (sum==null) sum=p; else sum=sum.union(p);
        }
        // Allocate a relation for this subset sig, then bound it
        Relation r=Relation.unary(sig.label);
        rep.bound("Sig "+sig+" in "+ts+"\n");
        bounds.bound(r, ts);
        a2k.put(sig, r);
        // Add a constraint that it is INDEED a subset of the union of its parents
        fact = add(r.in(sum), sig.isSubset, fmap).and(fact);
        return r;
    }

    /** Computes the bounds for sigs/fields, then construct a BoundsComputer object that you can query. */
    private BoundsComputer(ScopeComputer sc, A4Reporter rep, SafeList<Sig> sigs, A4Options options, Map<Formula,Object> fmap)
    throws Err {
        // Perform the prelimenary computation
        prelim(sc, rep, sigs);
        // Create the bounds object
        this.bounds = new Bounds(universe);
        // Bound SIGINT_NEXT, SIGINT_MAX, SIGINT_MIN, SIGINT_ZERO, and SEQ_SEQIDX
        TupleFactory factory = universe.factory();
        this.intBounds = factory.noneOf(1);
        this.seqidxBounds = factory.noneOf(1);
        final TupleSet next = factory.noneOf(2);
        final int min = 0-(1<<(bitwidth-1)); // Safe since we know 1 <= bitwidth <= 30
        final int max = (1<<(bitwidth-1))-1;
        for(int i=min; i<=max; i++) { // Safe since we know 1 <= bitwidth <= 30
            Tuple ii=factory.tuple(""+i);
            bounds.boundExactly(i, factory.range(ii, ii));
            intBounds.add(ii);
            if (i>=0 && i<maxseq) seqidxBounds.add(ii);
            if (i+1<=max) next.add(factory.tuple(""+i, ""+(i+1)));
        }
        bounds.boundExactly(SIGINT_NEXT, next);
        bounds.boundExactly(SIGINT_MAX, factory.range(factory.tuple(""+max), factory.tuple(""+max)));
        bounds.boundExactly(SIGINT_MIN, factory.range(factory.tuple(""+min), factory.tuple(""+min)));
        bounds.boundExactly(SIGINT_ZERO, factory.range(factory.tuple("0"), factory.tuple("0")));
        bounds.boundExactly(SEQ_SEQIDX, seqidxBounds);
        // Bound the PrimSig(s)
        Expression univ = Relation.INTS;
        for(Sig s:sigs) if (!s.builtin && s.isTopLevel()) univ=allocatePrimSig((PrimSig)s, fmap).union(univ);
        a2k.put(UNIV, univ);
        a2k.put(NONE, Relation.NONE);
        a2k.put(SIGINT, Relation.INTS);
        a2k.put(SEQIDX, SEQ_SEQIDX);
        // Bound the SubsetSig(s).
        for(Sig s:sigs) if (s instanceof SubsetSig) allocateSubsetSig(rep, (SubsetSig)s, fmap);
        // Bound the fields
        for(Sig s:sigs) for(Field f:s.getFields()) {
            Type t = (s.isOne!=null) ? UNIV.join(f).type : f.type;
            Relation r = Relation.nary(s.label+"."+f.label, t.arity());
            TupleSet ans = factory.noneOf(r.arity());
            for(List<PrimSig> p:t.fold()) {
                TupleSet upper=null;
                for(PrimSig b:p) {
                    TupleSet tmp=query(true, a2k.get(b), false);
                    if (upper==null) upper=tmp; else upper=upper.product(tmp);
                }
                ans.addAll(upper);
            }
            if (s.isOne!=null) a2k.put(f, a2k.get(s).product(r)); else a2k.put(f,r);
            bounds.bound(r,ans);
        }
        // Add any additional SIZE constraints
        for(Sig s:sigs) if (!s.builtin) {
            Expression exp=a2k.get(s);
            TupleSet upper=query(true,exp,false), lower=query(false,exp,false);
            final int n=sc.sig2scope(s);
            if (s.isOne!=null && (lower.size()!=1 || upper.size()!=1)) {
                rep.bound("Sig "+s+" in "+upper+" with size==1\n");
                fact=add(exp.one(), s.isOne, fmap).and(fact);
                continue;
            }
            if (s.isSome!=null && lower.size()<1) fact=add(exp.some(), s.isSome, fmap).and(fact);
            if (s.isLone!=null && upper.size()>1) fact=add(exp.lone(), s.isLone, fmap).and(fact);
            if (n<0) continue; // This means no scope was specified
            if (sc.isExact(s) && lower.size()==n && upper.size()==n) {
                rep.bound("Sig "+s+" == "+upper+"\n");
            }
            else if (sc.isExact(s)) {
                rep.bound("Sig "+s+" in "+upper+" with size=="+n+"\n");
                fact=size(s,n,true,fmap).and(fact);
            }
            else if (upper.size()<=n){
                rep.bound("Sig "+s+" in "+upper+"\n");
            }
            else {
                rep.bound("Sig "+s+" in "+upper+" with size<="+n+"\n");
                fact=size(s,n,false,fmap).and(fact);
            }
        }
        // Turn everything read-only
        a2k.makeConst();
    }

    /**
     * Computes the bounds, constraints, and the Sig/Field-to-Expression map:
     * <br> 1) The map maps each sig and field to a Kodkod Expression
     * <br> 2) The Bounds defines lower and upper bound for any Kodkod relation mentioned in the map
     * <br> 3) The Formula, when combined with the Map and Bounds, fully expresses the user's intent
     * <p>
     *
     * @param sc - the ScopeComputer that bounds each PrimSig to a scope
     * @param rep - the reporter that may receive diagnostic messages
     * @param sigs - the list of all sigs
     * @param options - the Alloy options object
     * @param fmap - this map will receive additional mappings between each constraint to a Pos object
     */
    static Pair<Pair<Bounds,Formula>,ConstMap<Object,Expression>> compute
    (ScopeComputer sc, A4Reporter rep, SafeList<Sig> sigs, A4Options options, Map<Formula,Object> fmap)
    throws Err {
        BoundsComputer bc = new BoundsComputer(sc, rep, sigs, options, fmap);
        Pair<Bounds,Formula> a = new Pair<Bounds,Formula>(bc.bounds, bc.fact);
        return new Pair<Pair<Bounds,Formula>,ConstMap<Object,Expression>>(a, bc.a2k.makeConst());
    }
}
