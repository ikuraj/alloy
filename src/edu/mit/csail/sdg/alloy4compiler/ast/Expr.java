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

package edu.mit.csail.sdg.alloy4compiler.ast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents a formula or expression.
 *
 * <p>  <b>Invariant:</b>  pos!=null
 * <br> <b>Invariant:</b>  type!=null
 * <br> <b>Invariant:</b>  type==EMPTY iff errors.size()>0
 * <br> <b>Invariant:</b>  mult==0 || mult==1 || mult==2
 * <br> <b>Invariant:</b>  weight>0
 */

public abstract class Expr {

    /** Accepts the return visitor. */
    abstract Object accept(VisitReturn visitor) throws Err;

    /**
     * If this expression is ambiguous, resolve it and return an unambiguous copy of this Expr, else return the Expr as-is.
     * (And if t.size()>0, it represents the set of tuples whose presence/absence is relevent to the parent expression)
     * (Note: it's possible for t to be EMPTY, or even ambiguous!)
     *
     * <p> On success: the return value (and all its subnodes) will be well-typed and unambiguous
     * <p> On failure: the return value's "errors" list will be nonempty
     */
    public abstract Expr resolve(Type t, Collection<ErrorWarning> warnings);

    /** The filename, line, and column position in the original Alloy model file (cannot be null). */
    public final Pos pos;

    /** The type for this node; EMPTY if it is not well-typed. */
    public final Type type;

    /** The list of errors on this node; nonempty iff this.type==EMPTY */
    public final JoinableList<Err> errors;

    /**
     * This field records whether the node is a multiplicity constraint.
     *
     * <br> If it's 2, that means it is an arrow multiplicity constraint (X ?->? X),
     *      or has the form (A -> B) where A and/or B is an arrow multiplicity constraint.
     *
     * <br> If it's 1, that means it is a multiplicity constraint of the form (? X)
     *
     * <br> If it's 0, that means it does not have either form.
     */
    public final int mult;

    /**
     * Each leaf Expr has a weight value (which can be 0 or higher);
     * by default, each nonleaf Expr's weight is the sum of its children's weights.
     */
    public final long weight;

    /** True if this expression is ExprChoice, or this expression contains an ExprChoice subexpression. */
    final boolean ambiguous;

    /** This is an unmodifiable empty list of Err objects. */
    static final JoinableList<Err> emptyListOfErrors = new JoinableList<Err>();

    /**
     * Constructs a new expression node
     *
     * @param pos - the original position in the file (null if unknown)
     *
     * @param ambiguous - true if this node is ExprChoice or it contains an ExprChoice subnode
     *
     * @param type - the type (null if this expression has not been typechecked)
     *
     * @param mult - the multiplicity (which must be 0, 1, or 2)
     * <br>If it's 2, that means it is a multiplicity constraint (X ?->? X),
     *     or has the form (A -> B) where A and/or B is a multiplicity constraint.
     * <br>If it's 1, that means it is a multiplicity constraint of the form (? X)
     * <br>If it's 0, that means it does not have either form.
     *
     * @param weight - the weight of this node and all subnodes
     *
     * @param errors - the list of errors associated with this Expr node
     */
    Expr (Pos pos, boolean ambiguous, Type type, int mult, long weight, JoinableList<Err> errors) {
        this.pos=(pos==null ? Pos.UNKNOWN : pos);
        this.ambiguous=ambiguous;
        if (errors==null) errors=emptyListOfErrors;
        if (type==EMPTY && errors.size()==0) errors=errors.append(new ErrorType(pos, "This expression failed to be typechecked"));
        this.mult=(mult<0 || mult>2) ? 0 : mult;
        this.type=(errors.size()>0 || type==null) ? EMPTY : type;
        this.weight=(weight>0) ? weight : 0;
        this.errors=errors;
    }

    /**
     * This must only be called by Sig's constructor.
     * <p> if search!=null, we will look at "search", "search.parent()", "search.parent().parent()"... to try to find
     * the oldest parent whose "hint_isLeaf" flag is true (and if so, we'll use that node's type as the type)
     */
    Expr (Pos pos, Type type, PrimSig search) {
        while (search!=null) {
            if (search.hint_isLeaf) {
                type=search.type;
                break; // We can break early, because if an older node is also leaf, then this node would have inherited it too
            }
            search=search.parent;
        }
        this.pos=(pos==null ? Pos.UNKNOWN : pos);
        this.ambiguous=false;
        this.type=(type==null || type==EMPTY) ? Type.make((PrimSig)this) : type;
        this.mult=0;
        this.weight=0;
        this.errors=emptyListOfErrors;
    }

    /** Returns a Pos object representing the entire span of this Expr and all its subexpressions. */
    public abstract Pos span();

    /**
     * Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation.
     * (If indent<0, it will be printed in one line without line break)
     */
    public abstract void toString(StringBuilder out, int indent);

    /** Print a brief text description of it and all subnodes. */
    @Override public String toString() { StringBuilder sb=new StringBuilder(); toString(sb,-1); return sb.toString(); }

    /** A return visitor that determines whether the node (or a subnode) contains a predicate/function call. */
    private static final VisitQuery hasCall = new VisitQuery() {
        @Override public final Object visit(ExprCall x) { return this; }
    };

    /** Returns true if the node (or a subnode) is a predicate/function call. */
    final boolean hasCall() throws Err { return accept(hasCall)!=null; }

    /** Transitively returns a set that contains all predicates/functions that this expression calls directly or indirectly. */
    public final Iterable<Func> findAllFunctions() {
        final IdentitySet<Func> seen = new IdentitySet<Func>();
        final List<Func> todo = new ArrayList<Func>();
        final VisitQuery q = new VisitQuery() {
            @Override public final Object visit(ExprCall x) { if (seen.add(x.fun)) todo.add(x.fun); return null; }
        };
        try {
            q.visitThis(this);
            while(!todo.isEmpty()) { q.visitThis(todo.remove(todo.size()-1).getBody()); }
        } catch(Err ex) { } // Exception should not occur
        return seen;
    }

    //================================================================================//
    // Below are convenience methods for building up expressions from subexpressions. //
    //================================================================================//

    /**
     * Returns the formula (this and x)
     * <p> this and x must both be formulas
     * <p> Note: as a special guarantee, if x==null, then the method will return this Expr object as-is.
     */
    public final Expr and(Expr x) { return (x==null) ? this : ExprBinary.Op.AND.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this or x)
     * <p> this and x must both be formulas
     */
    public final Expr or(Expr x) { return ExprBinary.Op.OR.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this iff x)
     * <p> this and x must both be formulas
     */
    public final Expr iff(Expr x) { return ExprBinary.Op.IFF.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this implies x)
     * <p> this and x must both be formulas
     */
    public final Expr implies(Expr x) { return ExprBinary.Op.OR.make(span().merge(x.span()), not(), x); }

    /**
     * Returns the expression (this.x)
     * <p> 1. this must be a set or relation
     * <p> 2. x must be a set or relation
     * <p> 3. at most one of them can be a unary set
     */
    public final Expr join(Expr x) { return ExprBinary.Op.JOIN.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this <: x)
     * <p> this must be a unary set
     * <p> x must be a set or relation
     */
    public final Expr domain(Expr x) { return ExprBinary.Op.DOMAIN.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this :> x)
     * <p> this must be a set or relation
     * <p> x must be a unary set
     */
    public final Expr range(Expr x) { return ExprBinary.Op.RANGE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this intersects x)
     * <p> this and x must be expressions with the same arity
     */
    public final Expr intersect(Expr x) { return ExprBinary.Op.INTERSECT.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this++x)
     * <p> this and x must be expressions with the same arity
     */
    public final Expr override(Expr x) { return ExprBinary.Op.PLUSPLUS.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this+x)
     * <p> this and x must be expressions with the same arity, or both be integer expressions
     */
    public final Expr plus(Expr x) { return ExprBinary.Op.PLUS.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this-x)
     * <p> this and x must be expressions with the same arity, or both be integer expressions
     */
    public final Expr minus(Expr x) { return ExprBinary.Op.MINUS.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this==x)
     * <p> this and x must be expressions with the same arity, or both be integer expressions
     */
    public final Expr equal(Expr x) { return ExprBinary.Op.EQUALS.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this &lt; x)
     * <p> this and x must both be integer expressions
     */
    public final Expr lt(Expr x) { return ExprBinary.Op.LT.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this &lt;= x)
     * <p> this and x must both be integer expressions
     */
    public final Expr lte(Expr x) { return ExprBinary.Op.LTE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this &gt; x)
     * <p> this and x must both be integer expressions
     */
    public final Expr gt(Expr x) { return ExprBinary.Op.GT.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this &gt;= x)
     * <p> this and x must both be integer expressions
     */
    public final Expr gte(Expr x) { return ExprBinary.Op.GTE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the formula (this in x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation or multiplicity constraint
     * <p> this and x must have the same arity
     */
    public final Expr in(Expr x) { return ExprBinary.Op.IN.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression (this -> x) which can also be regarded as a multiplicity constraint (this set->set x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr product(Expr x) { return ExprBinary.Op.ARROW.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this set->some x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr any_arrow_some(Expr x) { return ExprBinary.Op.ANY_ARROW_SOME.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this set->one x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr any_arrow_one(Expr x) { return ExprBinary.Op.ANY_ARROW_ONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this set->lone x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr any_arrow_lone(Expr x) { return ExprBinary.Op.ANY_ARROW_LONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this some->set x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr some_arrow_any(Expr x) { return ExprBinary.Op.SOME_ARROW_ANY.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this some->some x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr some_arrow_some(Expr x) { return ExprBinary.Op.SOME_ARROW_SOME.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this some->one x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr some_arrow_one(Expr x) { return ExprBinary.Op.SOME_ARROW_ONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this some->lone x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr some_arrow_lone(Expr x) { return ExprBinary.Op.SOME_ARROW_LONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this one->set x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr one_arrow_any(Expr x) { return ExprBinary.Op.ONE_ARROW_ANY.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this one->some x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr one_arrow_some(Expr x) { return ExprBinary.Op.ONE_ARROW_SOME.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this one->one x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr one_arrow_one(Expr x) { return ExprBinary.Op.ONE_ARROW_ONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this one->lone x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr one_arrow_lone(Expr x) { return ExprBinary.Op.ONE_ARROW_LONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this lone->set x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr lone_arrow_any(Expr x) { return ExprBinary.Op.LONE_ARROW_ANY.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this lone->some x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr lone_arrow_some(Expr x) { return ExprBinary.Op.LONE_ARROW_SOME.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this lone->one x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr lone_arrow_one(Expr x) { return ExprBinary.Op.LONE_ARROW_ONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this lone->lone x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr lone_arrow_lone(Expr x) { return ExprBinary.Op.LONE_ARROW_LONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the multiplicity constraint (this isSeq->lone x)
     * <p> this must be a set or relation
     * <p> x must be a set or relation
     */
    public final Expr isSeq_arrow_lone(Expr x) { return ExprBinary.Op.ISSEQ_ARROW_LONE.make(span().merge(x.span()), this, x); }

    /**
     * Returns the expression/integer/formula (this =&gt; x else y)
     * <p> this must be a formula
     * <p> x and y must both be expressions of the same arity, or both be integer expressions, or both be formulas
     */
    public final Expr ite(Expr x, Expr y) { return ExprITE.make(this, x, y); }

    /**
     * Returns the formula (all...| this)
     * <p> this must be a formula
     */
    public final Expr forAll(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.ALL.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the formula (no...| this)
     * <p> this must be a formula
     */
    public final Expr forNo(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.NO.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the formula (lone...| this)
     * <p> this must be a formula
     */
    public final Expr forLone(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.LONE.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the formula (one ...| this)
     * <p> this must be a formula
     */
    public final Expr forOne(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.ONE.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the formula (some...| this)
     * <p> this must be a formula
     */
    public final Expr forSome(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.SOME.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the comprehension expression {...|this}
     * <p> this must be a formula
     * <p> each declaration must be a "one-of" quantification over a unary set
     */
    public final Expr comprehensionOver(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.COMPREHENSION.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Returns the integer (sum...| this)
     * <p> this must be an integer expression
     * <p> each declaration must be a "one-of" quantification over a unary set
     */
    public final Expr sumOver(ExprVar firstVar, ExprVar... moreVars) {
        Pos p = firstVar.span();
        for(ExprVar v:moreVars) p=p.merge(v.span());
        return ExprQuant.Op.SUM.make(p, null, Util.prepend(Util.asList(moreVars), firstVar), this);
    }

    /**
     * Return a quantified variable (label: some this)
     * <p> this must be already fully typechecked, and must be a unary set
     * <p> the label is only used for pretty-printing, and does not need to be unique
     */
    public final ExprVar someOf(String label) {
        Expr x = Resolver.cset(this);
        x = ExprUnary.Op.SOMEOF.make(span(), x);
        return ExprVar.make(span(), label, x);
    }

    /**
     * Return a quantified variable (label: lone this)
     * <p> this must be already fully typechecked, and must be a unary set
     * <p> the label is only used for pretty-printing, and does not need to be unique
     */
    public final ExprVar loneOf(String label) {
        Expr x = Resolver.cset(this);
        x = ExprUnary.Op.LONEOF.make(span(), x);
        return ExprVar.make(span(), label, x);
    }

    /**
     * Return a quantified variable (label: one this)
     * <p> this must be already fully typechecked, and must be a unary set
     * <p> the label is only used for pretty-printing, and does not need to be unique
     */
    public final ExprVar oneOf(String label) {
        Expr x = Resolver.cset(this);
        x = ExprUnary.Op.ONEOF.make(span(), x);
        return ExprVar.make(span(), label, x);
    }

    /**
     * Return a quantified variable (label: set this)
     * <p> this must be already fully typechecked, and must be a set or relation
     * <p> the label is only used for pretty-printing, and does not need to be unique
     */
    public final ExprVar setOf(String label) {
        Expr x = Resolver.cset(this);
        x = ExprUnary.Op.SETOF.make(span(), x);
        return ExprVar.make(span(), label, x);
    }

    /**
     * Returns the formula (not this)
     * <p> this must be a formula
     */
    public final Expr not() { return ExprUnary.Op.NOT.make(span(), this); }

    /**
     * Returns the formula (no this)
     * <p> this must be a set or a relation
     */
    public final Expr no() { return ExprUnary.Op.NO.make(span(), this); }

    /**
     * Returns the formula (some this)
     * <p> this must be a set or a relation
     */
    public final Expr some() { return ExprUnary.Op.SOME.make(span(), this); }

    /**
     * Returns the formula (lone this)
     * <p> this must be a set or a relation
     */
    public final Expr lone() { return ExprUnary.Op.LONE.make(span(), this); }

    /**
     * Returns the formula (one this)
     * <p> this must be a set or a relation
     */
    public final Expr one() { return ExprUnary.Op.ONE.make(span(), this); }

    /**
     * Returns the expression (~this)
     * <p> this must be a binary relation
     */
    public final Expr transpose() { return ExprUnary.Op.TRANSPOSE.make(span(), this); }

    /**
     * Returns the expression (*this)
     * <p> this must be a binary relation
     */
    public final Expr reflexiveClosure() { return ExprUnary.Op.RCLOSURE.make(span(), this); }

    /**
     * Returns the expression (^this)
     * <p> this must be a binary relation
     */
    public final Expr closure() { return ExprUnary.Op.CLOSURE.make(span(), this); }

    /**
     * Returns the integer expression (#this) truncated to the current integer bitwidth.
     * <p> this must be a set or a relation
     */
    public final Expr cardinality() { return ExprUnary.Op.CARDINALITY.make(span(), this); }

    /**
     * Returns the integer expression "int[this]"
     * <p> this must be a unary set
     */
    public final Expr cast2int() { return ExprUnary.Op.CAST2INT.make(span(), this); }

    /**
     * Returns the singleton set "Int[this]"
     * <p> this must be an integer expression
     */
    public final Expr cast2sigint() { return ExprUnary.Op.CAST2SIGINT.make(span(), this); }
}
