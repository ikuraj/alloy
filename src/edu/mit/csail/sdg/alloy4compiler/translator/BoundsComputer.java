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

import java.util.ArrayList;
import java.util.List;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.Translator;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.parser.Command;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/**
 * Immutable; this class computes the bounds based on the user's instructions in a "run" or "check" command.
 */

final class BoundsComputer {

    //==============================================================================================================//

    /** The Kodkod-to-Alloy map. */
    private final Map<Formula,List<Object>> core;
    private Formula core(Formula f, Pos x) {
        if (core==null || x==null) return f;
        if (f instanceof BinaryFormula && ((BinaryFormula)f).op()==BinaryFormula.Operator.AND) {
            core(((BinaryFormula)f).left(), x);
            core(((BinaryFormula)f).right(), x);
            return f;
        }
        List<Object> list=core.get(f);
        if (list==null) { list=new ArrayList<Object>(3); core.put(f,list); }
        list.add(x);
        return f;
    }

    /** The factory we use to generate the Tuple objects (null if the factory hasn't been constructed yet). */
    private TupleFactory factory=null;

    /** Returns the factory we use to generate the Tuple objects (null if the factory hasn't been constructed yet). */
    public TupleFactory factory() { return factory; }

    /** Stores the exact bound for SIGINT (null if it isn't computed yet). */
    private TupleSet intBounds=null;

    /** Stores the exact bound for SEQ_SEQIDX (null if it isn't computed yet). */
    private TupleSet seqidxBounds=null;

    /** The resulting Bounds object (null if the Bounds object hasn't been constructed yet). */
    private Bounds bounds=null;

    /** Returns an unmodifiable view of the resulting Bounds object. */
    Bounds getBounds() { return bounds.unmodifiableView(); }

    static final Relation SIGINT_MIN = Relation.unary("[discard] util/integer min");
    static final Relation SIGINT_ZERO = Relation.unary("[discard] util/integer zero");
    static final Relation SIGINT_MAX = Relation.unary("[discard] util/integer max");
    static final Relation SIGINT_NEXT = Relation.binary("[discard] util/integer next");
    static final Relation SEQ_SEQIDX = Relation.unary("[discard] seq's index");

    //==============================================================================================================//

    /** Stores the lowerbound for each signature (as a set of String) */
    private final Map<PrimSig,SafeList<String>> sig2lower = new LinkedHashMap<PrimSig,SafeList<String>>();

    /** Caches the lowerbound for each signature (as a TupleSet); when sig2lower is modified, the cached value is discarded. */
    private final Map<Sig,TupleSet> sig2lb=new LinkedHashMap<Sig,TupleSet>();

    /** Add 1 value. */
    private void sig2lower_add(PrimSig sig, String newValue) {
        sig2lb.clear(); // We clear all, since some SubsetSig may also indirectly depend on this sig.
        SafeList<String> ans=sig2lower.get(sig);
        if (ans==null) { ans=new SafeList<String>(); ans.add(newValue); sig2lower.put(sig,ans); return; }
        if (!ans.contains(newValue)) ans.add(newValue);
    }

    /** Returns the current lowerbound (as a set of String) */
    private SafeList<String> sig2lower(PrimSig sig) {
        SafeList<String> ans=sig2lower.get(sig);
        if (ans==null) return empty; else return ans.dup();
    }

    /** Returns the current lowerbound (as a TupleSet); can only be called after the factory object is constructed. */
    private TupleSet sig2lb(Sig sig) throws Err {
        if (factory==null) throw new ErrorFatal("BoundsComputer.sig2lb() called with null factory.");
        if (sig==NONE) return factory.noneOf(1);
        if (sig==SIGINT) return intBounds;
        if (sig==SEQIDX) return seqidxBounds;
        if (sig==UNIV) return factory.noneOf(1);
        TupleSet ans=sig2lb.get(sig);
        if (ans==null) {
            ans=factory.noneOf(1);
            if (sig instanceof PrimSig) {
                for(String a:sig2lower((PrimSig)sig)) ans.add(factory.tuple(a));
            } else {
                return ans; // Don't even bother caching it
            }
            sig2lb.put(sig,ans);
        }
        return ans;
    }

    //==============================================================================================================//

    /** Stores the upperbound for each signature (as a set of String) */
    private final Map<PrimSig,SafeList<String>> sig2upper = new LinkedHashMap<PrimSig,SafeList<String>>();

    /** Caches the upperbound for each signature (as a TupleSet); when sig2upper is modified, the cached value is discarded. */
    private final Map<Sig,TupleSet> sig2ub=new LinkedHashMap<Sig,TupleSet>();

    /** Add 1 value. */
    private void sig2upper_add(PrimSig sig, String newValue) {
        sig2ub.clear(); // We clear all, since some SubsetSig may also indirectly depend on this sig.
        SafeList<String> ans=sig2upper.get(sig);
        if (ans==null) { ans=new SafeList<String>(); ans.add(newValue); sig2upper.put(sig,ans); return; }
        if (!ans.contains(newValue)) ans.add(newValue);
    }

    /** Add 0 or more values. */
    private void sig2upper_addAll(PrimSig sig, Collection<String> newValues) {
        sig2ub.clear(); // We clear all, since some SubsetSig may also indirectly depend on this sig.
        SafeList<String> ans=sig2upper.get(sig);
        if (ans==null && newValues.size()>0) { ans=new SafeList<String>(newValues.size()); sig2upper.put(sig,ans); }
        for(String x:newValues) if (!ans.contains(x)) ans.add(x);
    }

    /** Returns the current upperbound (as a set of String) */
    private SafeList<String> sig2upper(PrimSig sig) {
        SafeList<String> ans=sig2upper.get(sig);
        if (ans==null) return empty; else return ans.dup();
    }

    /** Returns the current upperbound (as a TupleSet); can only be called after the factory object is constructed. */
    private TupleSet sig2ub(Sig sig) throws Err {
        if (factory==null) throw new ErrorFatal("BoundsComputer.sig2ub() called with null factory.");
        if (sig==NONE) return factory.noneOf(1);
        if (sig==SIGINT) return intBounds;
        if (sig==SEQIDX) return seqidxBounds;
        if (sig==UNIV) return factory.allOf(1);
        TupleSet ans=sig2ub.get(sig);
        if (ans==null) {
            ans=factory.noneOf(1);
            if (sig instanceof PrimSig) {
                for(String a:sig2upper((PrimSig)sig)) ans.add(factory.tuple(a));
            } else {
                for(Sig s:((SubsetSig)sig).parents) ans.addAll(sig2ub(s));
            }
            sig2ub.put(sig,ans);
        }
        return ans;
    }

    //==============================================================================================================//

    /** This stores the set of nonuseful relations. */
    private final IdentitySet<Expression> discard = new IdentitySet<Expression>();

    /** Returns a copy of the set of nonuseful relations. */
    IdentitySet<Expression> getDiscards() { return new IdentitySet<Expression>(discard); }

    //==============================================================================================================//

    /** This maps each signature to the Kodkod expression we generated for it. */
    private final Map<Sig,Expression> sig2expr = new LinkedHashMap<Sig,Expression>();

    /** Given a signature, return its associated Kodkod expression (null if no expression was assigned for it) */
    Expression expr(Sig x) {
        if (x==SEQIDX) return SEQ_SEQIDX;
        if (x==SIGINT) return Expression.INTS;
        if (x==NONE) return Expression.NONE;
        return sig2expr.get(x);
    }

    // private final Map<Pair<Sig,Integer>,Relation> sig2expri = new LinkedHashMap<Pair<Sig,Integer>,Relation>();
    //
    // /**
    //  * Given a signature, return a Kodkod expression referencing its i-th atom.
    //  * (Note: you cannot call this method with UNIV, SEQIDX, SIGINT, or NONE)
    //  * (Note: if a Kodkod expression doesn't already exist for it, we will create a new one and cache it)
    //  */
    /*
    Expression expr(Sig s, int i) throws Err {
        final int save=i;
        Pair<Sig,Integer> p = new Pair<Sig,Integer>(s,i);
        Relation r = sig2expri.get(p);
        if (r==null) {
            TupleSet u=sig2ub(s), l=sig2lb(s);
            if (i>=0) for(Tuple tp:u) {
                if (i>0) {i--; continue;}
                if (!l.contains(tp))
                    throw new ErrorSyntax("Sig "+s+" is not guaranteed to have the "+save+"th atom.");
                TupleSet tps = factory.noneOf(1);
                tps.add(tp);
                r=Relation.unary("[discard]");
                bounds.boundExactly(r, tps);
                sig2expri.put(p, r);
                return r;
            }
            throw new ErrorSyntax("Sig "+s+" cannot have the "+save+"th atom.");
        }
        return r;
    }
    */

    //==============================================================================================================//

    /** This maps each field to the Kodkod expression we generated for it. */
    private final Map<Field, Expression> field2expr = new LinkedHashMap<Field,Expression>();

    /** Given a field, return its associated Kodkod expression (null if no expression was assigned for it) */
    Expression expr(Field x) {
        Expression ans = field2expr.get(x);
        if (x.sig.isOne!=null && ans!=null) ans = expr(x.sig).product(ans);
        return ans;
    }

    /** Given a field, return its associated Kodkod expression (null if no expression was assigned for it) */
    Expression exprWithoutFirst(Field x) {
        Expression ans = field2expr.get(x);
        return ans;
    }

    //==============================================================================================================//

    /** The integer bitwidth. */
    private final int bitwidth;

    /** Returns the integer bitwidth. */
    int getBitwidth() { return bitwidth; }

    //==============================================================================================================//

    /** The maximum sequence length. */
    private final int maxseq;

    /** Returns the integer bitwidth. */
    int getMaxSeq() { return maxseq; }

    //==============================================================================================================//

    /** The set of additional constraints we discover as we compute the bounds. */
    private Formula kfact=Formula.TRUE;

    /** Returns the set of additional constraints that we discover as we computed the bounds. */
    Formula getFacts() { return kfact; }

    //==============================================================================================================//

    /** This caches a readonly empty list of String. */
    private static final SafeList<String> empty=(new SafeList<String>(0)).dup();

    //==============================================================================================================//

    /** Given a sig, make N atoms for it (Note: SIGINT and SEQIDX atom names are different, and must not use this method) */
    private void makeAtom(Sig sig, SafeList<String> list1, SafeList<String> list2, int n) {
        if (n<=0) return;
        String name=sig.toString();
        if (name.startsWith("/")) name=name.substring(1);
        if (name.startsWith("this/")) name=name.substring(5);
        // Compute the width of the index (eg. width("0")=1,  width("7")=1,  width("23")=2, etc)
        int width=1;
        for(int i=n-1; i>=10; i=i/10) width++;
        // Now, generate the atoms
        StringBuilder sb=new StringBuilder();
        for(int i=0;i<n;i++) {
            sb.delete(0, sb.length());
            sb.append(name);
            sb.append('[');
            String x=Integer.toString(i);
            int xlen=x.length();
            while(xlen<width) {sb.append('0'); xlen++;}
            sb.append(x);
            sb.append(']');
            x=sb.toString();
            if (list1!=null) list1.add(x);
            if (list2!=null) list2.add(x);
        }
        // The format of the atoms is "SIGNAME[INDEX]".
        // By prepending the index with 0 so that they're the same width, we ensure they sort lexicographically.
        // By using '[' which is illegal in a signature name, we ensure such atoms are unique.
    }

    //==============================================================================================================//

    /** Computes the lowerbound from bottom-up; it will also set a suitable initial value for each sig's upperbound. */
    private Collection<String> computeLowerBound(ScopeComputer sc, PrimSig sig) throws Err {
        if (sig.builtin) return empty;
        int n=sc.sig2scope(sig);
        final SafeList<String> atoms=new SafeList<String>();
        // First, figure out what atoms *MUST* be in this sig
        for(PrimSig c:sig.children()) atoms.addAll(computeLowerBound(sc,c));
        // If MUST>SCOPE, then something went wrong!
        if (n<atoms.size()) {
            String msg="";
            for(String atom:atoms) msg=msg+" "+atom;
            throw new ErrorAPI(sig.pos, "Scope for sig \""+sig+"\" was miscalculated ("+n+" < {"+msg+" })");
        }
        // Make a copy
        SafeList<String> returnValue=new SafeList<String>(atoms);
        if (n>atoms.size() && sc.isExact(sig)) {
            // If MUST<SCOPE and s is exact, then add fresh atoms to both LOWERBOUND and UPPERBOUND, then return.
            makeAtom(sig, returnValue, atoms, n-atoms.size());
        } else if (n>atoms.size() && sig.isTopLevel()) {
            // If MUST<SCOPE and s is inexact but toplevel, then add fresh atoms to the UPPERBOUND, then return.
            makeAtom(sig, null, atoms, n-atoms.size());
        }
        sig2lower.put(sig,returnValue);
        sig2upper.put(sig,atoms);
        return returnValue.dup();
    }

    //==============================================================================================================//

    /** Computes the upperbound from top-down. */
    private SafeList<String> computeUpperBound(ScopeComputer sc, PrimSig sig) throws Err {
        if (sig.builtin) return empty;
        // Sig's upperbound is fully computed. We recursively compute the upperbound for children...
        Set<String> x=new LinkedHashSet<String>(sig2upper(sig));
        // Let X = the set of atoms that MIGHT be in this sig, but MIGHT NOT be in any particular subsig.
        for(PrimSig c:sig.children()) x.removeAll(sig2lower(c));
        // For each subsig that may need more atom, we say it could potentionally get any of the atom from X
        for(PrimSig c:sig.children()) if (sc.sig2scope(c) > sig2lower(c).size()) {
            sig2upper.put(c, new SafeList<String>(sig2lower(c)));
            sig2upper_addAll(c,x);
            computeUpperBound(sc, c);
        }
        return sig2upper(sig);
    }

    //==============================================================================================================//

    /** Query the Bounds object to find the upperbound; returns null if expr is not Relation, nor a union of Relations. */
    private TupleSet queryUpper(Bounds bounds, Expression expr, boolean makeMutable) {
        if (expr instanceof Relation) {
            TupleSet ans = bounds.upperBound((Relation)expr);
            if (ans!=null && makeMutable) return ans.clone(); else return ans;
        }
        if (expr instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)expr;
            if (b.op() == BinaryExpression.Operator.UNION) {
                TupleSet left = queryUpper(bounds, b.left(), true);
                if (left==null) return null;
                TupleSet right = queryUpper(bounds, b.right(), false);
                if (right==null) return null;
                left.addAll(right);
                return left;
            }
        }
        return null;
    }

    /** Query the Bounds object to find the lowerbound; returns null if expr is not Relation, nor a union of Relations. */
    private TupleSet queryLower(Bounds bounds, Expression expr, boolean makeMutable) {
        if (expr instanceof Relation) {
            TupleSet ans = bounds.lowerBound((Relation)expr);
            if (ans!=null && makeMutable) return ans.clone(); else return ans;
        }
        if (expr instanceof BinaryExpression) {
            BinaryExpression b = (BinaryExpression)expr;
            if (b.op() == BinaryExpression.Operator.UNION) {
                TupleSet left = queryLower(bounds, b.left(), true);
                if (left==null) return null;
                TupleSet right = queryLower(bounds, b.right(), false);
                if (right==null) return null;
                left.addAll(right);
                return left;
            }
        }
        return null;
    }

    /**
     * Allocate relations bottom-up.
     *
     * <p> We also experimented with giving a fresh relation to each sig,
     * then add constraints that "union of children sig is in parent sig"
     * (and in the case of abstract parent, to say that it is an equality).
     * But emperically, we found that it is slightly better to only give
     * fresh relations to leaf sigs, and to only create "remainder relations"
     * for nonabstract parents when needed.
     */
    private Expression allocateRelation(PrimSig sig) throws Err {
        Expression sum=null;
        // Recursively allocate all children expressions, and form the union of them
        for(PrimSig child:sig.children()) {
            Expression childexpr=allocateRelation(child);
            if (sum==null) {
                sum=childexpr;
            } else {
                // subsigs are disjoint
                kfact=core(sum.intersection(childexpr).no(),
                        Pos.UNKNOWN.addComment("subsigs of "+sig+" must be disjoint"))
                        .and(kfact);
                sum=sum.union(childexpr);
            }
        }
        // If sig doesn't have children, then sig should make a fresh relation for itself
        if (sum==null) {
            Relation r=Relation.unary(sig.toString());
            discard.add(r);
            bounds.bound(r, sig2lb(sig), sig2ub(sig));
            sig2expr.put(sig,r);
            return r;
        }
        // If sig is abstract with children, then sig == union of them
        if (sig.isAbstract!=null) { sig2expr.put(sig,sum); return sum; }
        // All else: declare a new relation to act as the remainder.
        Relation r=Relation.unary(sig.toString());
        discard.add(r);
        TupleSet lower=sig2lb(sig).clone();
        TupleSet upper=sig2ub(sig).clone();
        for(PrimSig child:sig.children()) {
            // Remove atoms that are KNOWN to be in a subsig;
            // it's okay to mistakenly leave some atoms in, since we will never solve for the "remainder" relation directly;
            // instead, we union the remainder with the children, then solve for the combined solution.
            // (Thus, the more we can remove, the more efficient it gets, but it is not crucial for correctness)
            lower.removeAll(sig2lb(child));
            upper.removeAll(sig2lb(child));
        }
        bounds.bound(r, lower, upper);
        sum=sum.union(r);
        sig2expr.put(sig,sum);
        return sum;
    }

    //==============================================================================================================//

    /** Add the constraint that "a" has exactly "n" elements. */
    private void sizeEQUAL(Sig sig, int n) {
        Expression a = expr(sig);
        Pos comment = Pos.UNKNOWN.addComment("sig["+sig+"] is scoped to have exactly "+n+" atoms");
        if (n==0) { kfact=core(a.no(), comment).and(kfact); return; }
        if (n==1) { kfact=core(a.one(), comment).and(kfact); return; }
        Formula moreFact=Formula.TRUE;
        Decls d=null;
        Expression sum=null;
        while(n>0) {
            n--;
            Variable v=Variable.unary("[discard]");
            if (d==null) d=v.oneOf(a); else d=v.oneOf(a).and(d);
            if (sum==null) sum=v; else { moreFact=v.intersection(sum).no().and(moreFact); sum=v.union(sum); }
        }
        kfact=core(sum.eq(a).and(moreFact).forSome(d), comment).and(kfact);
    }

    //==============================================================================================================//

    /** Add the constraint that "a" has at most "n" elements. */
    private void sizeLTE(Sig sig, int n) {
        Expression a = expr(sig);
        Pos comment = Pos.UNKNOWN.addComment("sig["+sig+"] is scoped to have at most "+n+" atoms");
        if (n==0) { kfact=core(a.no(), comment).and(kfact); return; }
        if (n==1) { kfact=core(a.lone(), comment).and(kfact); return; }
        Decls d=null;
        Expression sum=null;
        while(n>0) {
            n--;
            Variable v=Variable.unary("[discard]");
            if (d==null) d=v.oneOf(a); else d=v.oneOf(a).and(d);
            if (sum==null) sum=v; else sum=v.union(sum);
        }
        kfact=core(a.no().or(sum.eq(a).forSome(d)), comment).and(kfact);
    }

    //==============================================================================================================//

    /** This method computes the bounds for sigs/fields, then return a BoundsComputer object that you can query. */
    BoundsComputer(Module world, A4Options options, final Command cmd, final Map<Formula,List<Object>> core) throws Err {
        this.core=core;
        final A4Reporter rep=A4Reporter.getReporter();
        final SafeList<Sig> sigs=world.getAllSigsInTheWorld();
        final Set<String> atoms=new LinkedHashSet<String>();
        // Determine the scope and bitwidth
        final ScopeComputer sc=new ScopeComputer(world, cmd);
        this.bitwidth=sc.getBitwidth();
        this.maxseq=sc.getMaxSeq();
        // Generate SIGINT atoms
        int min=0-(1<<(bitwidth-1));
        int max=(1<<(bitwidth-1))-1;
        for(int i=min; i<=max; i++) { // We know 1 <= bitwidth <= 30
            String ii=""+i; // This must be exactly like this, for we depend on the format of atom names for Int
            atoms.add(ii);
            sig2lower_add(SIGINT, ii);
            sig2upper_add(SIGINT, ii);
        }
        // Generate other atoms
        for(Sig s:sigs) if (s.isTopLevel()) { computeLowerBound(sc,(PrimSig)s); }
        for(Sig s:sigs) if (s.isTopLevel()) { SafeList<String> x=computeUpperBound(sc,(PrimSig)s); atoms.addAll(x); }
        // Create the universe and the bounds
        final Universe universe = new Universe(atoms);
        this.factory = universe.factory();
        this.bounds = new Bounds(universe);
        // Bound SIGINT, SIGINT_MAX, SIGINT_NEXT, and SEQIDX
        this.intBounds=factory.noneOf(1);
        this.seqidxBounds=factory.noneOf(1);
        for(int j=0,i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++,j++) {
            Tuple ii=factory.tuple(sig2upper(SIGINT).get(j));
            bounds.boundExactly(i, factory.range(ii,ii));
            intBounds.add(ii);
            if (i>=0 && i<sc.getMaxSeq()) seqidxBounds.add(ii);
            if (j==0) {
                TupleSet next=factory.noneOf(2);
                for(int k=min; k<max; k++) next.add(factory.tuple(""+k).product(factory.tuple(""+(k+1))));
                bounds.boundExactly(SIGINT_NEXT, next);
                bounds.boundExactly(SIGINT_MAX, factory.range(factory.tuple(""+max), factory.tuple(""+max)));
                bounds.boundExactly(SIGINT_ZERO, factory.range(factory.tuple("0"), factory.tuple("0")));
                bounds.boundExactly(SIGINT_MIN, factory.range(factory.tuple(""+min), factory.tuple(""+min)));
            }
        }
        bounds.boundExactly(SEQ_SEQIDX, seqidxBounds);
        // Bound the PrimSig(s).
        Expression univ=Relation.INTS;
        for(Sig s:sigs) if (!s.builtin && s.isTopLevel()) {
            univ=allocateRelation((PrimSig)s).union(univ);
        }
        sig2expr.put(UNIV, univ);
        // Bound the SubsetSig(s).
        for(Sig s:sigs) if (s instanceof SubsetSig) {
            Relation r=Relation.unary(s.toString());
            discard.add(r);
            sig2expr.put(s,r);
            TupleSet ts=sig2ub(s);
            rep.bound("Sig "+s+" in "+ts.toString()+"\n");
            if (s.isSome!=null) kfact=core(r.some(), s.isSome).and(kfact);
            if (s.isOne!=null) kfact=core(r.one(), s.isOne).and(kfact);
            if (s.isLone!=null && ts.size()>1) kfact=core(r.lone(), s.isLone).and(kfact);
            bounds.bound(r, ts);
            // If X in Y1+..+Yn, then X in Y1+..+Yn
            Expression sum=null;
            for (Sig parent:((SubsetSig)s).parents) sum = (sum==null) ? expr(parent) : sum.union(expr(parent));
            if (sum!=null) kfact=core(r.in(sum), s.isSubset).and(kfact);
        }
        // Add extra cardinality constraints for PrimSig(s)
        for(Sig s:sigs) if (!s.builtin && s instanceof PrimSig) {
            Expression exp=expr(s);
            TupleSet upper=queryUpper(bounds,exp,false), lower=queryLower(bounds,exp,false);
            final int n=sc.sig2scope(s);
            if (s.isSome!=null && (lower==null || lower.size()<1)) kfact=core(exp.some(), s.isSome).and(kfact);
            if (s.isOne!=null && (lower==null || lower.size()!=1 || upper==null || upper.size()!=1)) kfact=core(exp.one(), s.isOne).and(kfact);
            if (s.isLone!=null && (upper==null || upper.size()>1)) kfact=core(exp.lone(), s.isLone).and(kfact);
            if (sc.isExact(s) && lower!=null && lower.size()==n && upper!=null && upper.size()==n) {
                rep.bound("Sig "+s+" == "+upper+"\n");
            }
            else if (sc.isExact(s)) {
                rep.bound("Sig "+s+" in "+sig2ub(s)+" with size=="+n+"\n");
                sizeEQUAL(s,n);
            }
            else if (upper!=null && upper.size()<=n){
                rep.bound("Sig "+s+" in "+upper+"\n");
            }
            else {
                rep.bound("Sig "+s+" in "+sig2ub(s)+" with size<="+n+"\n");
                sizeLTE(s,n);
            }
        }
        // Bound the fields. Must do this AFTER sigs due to util/ordering.als special encoding.
        for(Sig s:sigs) for(Field f:s.getFields()) {
            if (s.isOne==null) {
                Relation r=Relation.nary(s.toString()+"."+f.label, f.type.arity());
                discard.add(r);
                field2expr.put(f,r);
                bounds.bound(r, comp(f));
            } else {
                Relation r=Relation.nary(s.toString()+"."+f.label, f.type.arity()-1);
                discard.add(r);
                field2expr.put(f,r);
                bounds.bound(r, comp(UNIV.join(f)));
            }
        }
    }

    /** Compute the upperbound for a field. */
    private TupleSet comp(Expr f) throws Err {
        Type t=f.type;
        int a=t.arity(); if (a<1) throw new ErrorFatal("Attempting to create a 0-arity TupleSet.");
        TupleSet ans=factory.noneOf(a);
        for(List<PrimSig> r:t.fold()) {
            TupleSet upper=null;
            for(PrimSig b:r) {
                TupleSet tmp=sig2ub(b);
                if (upper==null) upper=tmp; else upper=upper.product(tmp);
            }
            ans.addAll(upper);
        }
        return ans;
    }

    private boolean simplify_in(Expression a, Expression b, Options opt) {
        if (a instanceof Relation) {
            Relation r=(Relation)a;
            TupleSet u=bounds.upperBound(r);
            TupleSet l=bounds.lowerBound(r);
            TupleSet t=factory.setOf(b.arity(), Translator.approximate(b,bounds,opt).denseIndices());
            t.retainAll(u);
            if (!t.containsAll(l)) return false; // This means the upperbound is shrunk BELOW the lowerbound.
            bounds.bound(r,l,t);
        }
        return true;
    }

    /** Simplifier; returns false if the formula becomes trivially unsatisfiable. */
    boolean simplify(Formula form, Options opt) {
        boolean flag1=true, flag2=true;
        if (form instanceof BinaryFormula) {
            BinaryFormula f=(BinaryFormula)form;
            if (f.op() == BinaryFormula.Operator.AND) {
                flag1=simplify(f.left(), opt);
                flag2=simplify(f.right(), opt);
            }
        } else if (form instanceof ComparisonFormula) {
            ComparisonFormula f=(ComparisonFormula)form;
            flag1=simplify_in(f.left(), f.right(), opt);
            if (f.op() == ComparisonFormula.Operator.EQUALS) flag2=simplify_in(f.right(), f.left(), opt);
        }
        return flag1 && flag2;
    }

}
