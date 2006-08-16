package edu.mit.csail.sdg.alloy4;


/**
 * Immutable; represents a formula or expression;
 * subclasses must also be immutable.
 * @author Felix Chang
 */

public abstract class Expr {

    /**
     * This field records the filename, line and column position
     * in the original Alloy model file (cannot be null).
     */
    public final Pos pos;

    /**
     * This field records the type of this node
     * (null if this node has not been typechecked).
     */
    public final Type type;

    /**
     * This field records whether the node is a multiplicity constraint.
     *
     * <br>If it's 2, that means it is a multiplicity constraint (X ?-&gt;? X),
     * or has the form (A -&gt; B) where A and/or B is a multiplicity constraint.
     * <br>If it's 1, that means it is a multiplicity constraint of the form (? X)
     * <br>If it's 0, that means it does not have either form.
     */
    public final int mult;

    /**
     * Accepts the return visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitReturn
     */
    public abstract Object accept(VisitReturn visitor);

    /**
     * Accepts the desugar visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitDesugar
     */
    public abstract Expr accept(VisitDesugar visitor);

    /**
     * Accepts the desugar2 visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitDesugar2
     */
    public abstract Expr accept(VisitDesugar2 visitor,Type obj);

    /**
     * Constructs a new expression node
     *
     * @param pos - the original position in the file
     *
     * @param type - the type (null if this expression has not been typechecked)
     *
     * @param mult - the multiplicity
     * <br>If it's 2, that means it is a multiplicity constraint (X ?-&gt;? X),
     * or has the form (A -&gt; B) where A and/or B is a multiplicity constraint.
     * <br>If it's 1, that means it is a multiplicity constraint of the form (? X)
     * <br>If it's 0, that means it does not have either form.
     *
     * @throws ErrorInternal if position==null
     */
    public Expr(Pos pos, Type type, int mult) {
        this.pos=nonnull(pos);
        this.type=type;
        this.mult=mult;
    }

    /** Convenience method that constructs a syntax error exception */
    public final ErrorSyntax syntaxError(String s) {
        return new ErrorSyntax(pos,s);
    }

    /** Convenience method that constructs a type error exception */
    public final ErrorType typeError(String s) {
        return new ErrorType(pos,this,s);
    }

    /** Convenience method that constructs an internal error exception  */
    public final ErrorInternal internalError(String s) {
        return new ErrorInternal(pos,this,s);
    }

    /**
     * Convenience method that checks if <b>a</b> is null or not;
     * (it returns <b>a</b> if nonnull, and throws an exception if null).
     *
     * @return <b>a</b> if it is not null
     * @throws ErrorInternal if <b>a</b> is null
     */
    public final<T> T nonnull(T a) {
        if (a==null) throw internalError("NullPointerException"); else return a;
    }

    /**
     * Returns true if and only if <b>this</b> has the form (:set X)
     * and X's arity is 1.
     * @throws ErrorInternal if <b>this</b> is not a fully-typechecked expression
     */
    public final boolean isSetOf1ary() {
        if (this.type==null)
            throw internalError(
            "isSetOf1ary() cannot be called until typechecking has finished");
        if (!(this instanceof ExprUnary)) return false;
        if (((ExprUnary)this).op!=ExprUnary.Op.SETMULT) return false;
        return this.type.arity()==1;
    }

    /**
     * Returns true if and only if <b>this</b> has the form (:one X)
     * and X's arity is 1.
     * @throws ErrorInternal if <b>this</b> is not a fully-typechecked expression
     */
    public final boolean isOneOf1ary() {
        if (this.type==null)
            throw internalError(
            "isOneOf1ary() cannot be called until typechecking has finished");
        if (!(this instanceof ExprUnary)) return false;
        if (((ExprUnary)this).op!=ExprUnary.Op.ONEMULT) return false;
        return this.type.arity()==1;
    }

    /**
     * Convenience method that
     * returns the subexpression of an ExprUnary expression.
     * @return ((ExprUnary)this).sub if <b>this</b> is an ExprUnary expression
     * @throws ErrorInternal if <b>this</b> is not an ExprUnary expression
     */
    public final Expr getUnarySub() {
        if (!(this instanceof ExprUnary))
            throw internalError("getUnarySub() called on a non-unary object!");
        return ((ExprUnary)this).sub;
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b> in <b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr in(Expr b) {
        return ExprBinary.Op.IN.make(this.pos, this, b, Type.FORMULA);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (! <b>this</b>)
     * <br>Note: <b>this</b> node must be fully typechecked.
     */
    public final Expr not() {
        return ExprUnary.Op.NOT.make(this.pos, this, Type.FORMULA);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b> &amp;&amp; <b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr and(Expr b) {
        return ExprBinary.Op.AND.make(this.pos, this, b, Type.FORMULA);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b> || <b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr or(Expr b) {
        return ExprBinary.Op.OR.make(this.pos, this, b, Type.FORMULA);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b> =&gt; <b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr implies(Expr b) {
        return ExprBinary.Op.IMPLIES.make(this.pos, this, b, Type.FORMULA);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b>.<b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr join(Expr b) {
        Type ans=this.type.join(b.type);
        if (ans.arity()<1) throw internalError("Cannot perform Expr.join(x)");
        return new ExprJoin(this.pos, this, b, ans);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing (<b>this</b> -&gt; <b>b</b>)
     * <br>Note: <b>this</b> and <b>b</b> must both be fully typechecked.
     */
    public final Expr product(Expr b) {
        Type ans=this.type.product_of_anyEmptyness(b.type);
        if (ans.arity()<1) throw internalError("Cannot perform Expr.product(x)");
        return ExprBinary.Op.ARROW.make(this.pos, this, b, ans);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing
     * (<b>this</b> =&gt; <b>a</b> else <b>b</b>)
     * <br>Note: <b>this</b>, <b>a</b>, and <b>b</b>
     * must all be fully typechecked.
     * @throws ErrorInternal if <b>a</b> and <b>b</b> are incompatible.
     */
    public final Expr ite(Expr a,Expr b) {
        Type ans;
        if (a.type.isInt && b.type.isInt)
            ans=Type.INT;
        else if (a.type.isBool && b.type.isBool)
            ans=Type.FORMULA;
        else {
            ans=a.type.union(b.type);
            if (ans.arity()<1)
                throw internalError("Cannot perform ITE on incompatible expressions!");
        }
        return new ExprITE(this.pos, this, a, b, ans);
    }

    /**
     * Convenience method that
     * returns a typechecked node representing the univ relation.
     * @param p - the position in the file where the univ relation is used
     */
    public static final Expr univ(Pos p) {
        return new ExprName(p, "/$builtin/$univ", ParaSig.UNIV, ParaSig.UNIV.type);
    }
}
