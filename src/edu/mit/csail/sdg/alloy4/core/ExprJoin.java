package edu.mit.csail.sdg.alloy4.core;



/**
 * Immutable; represents a relational join expression.
 *
 * <br/>
 * <br/> Invariant: left!=null && left.mult==0
 * <br/> Invariant: right!=null && right.mult==0
 *
 * @author Felix Chang
 */

public final class ExprJoin extends Expr {

    /** Accepts the return visitor. */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor bottom-up. */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor top-down. */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.accept(this,type);
    }

    /** The left-hand-side expression. */
    public final Expr left;

    /** The right-hand-side expression. */
    public final Expr right;

    /**
     * Constructs a typechecked ExprJoin expression.
     *
     * @param pos - the original position in the file
     * @param left - the left-hand-side expression
     * @param right - the right-hand-side expression
     * @param type - the type
     *
     * @throws ErrorInternal if pos==null, left==null, or right==null
     * @throws ErrorSyntax if left.mult!=0 or right.mult!=0
     */
    public ExprJoin (Pos pos, Expr left, Expr right, Type type) {
        super(pos, type, 0);
        if (left.mult != 0) throw left.syntaxError("Multiplicity expression not allowed here");
        if (right.mult != 0) throw right.syntaxError("Multiplicity expression not allowed here");
        this.left=nonnull(left);
        this.right=nonnull(right);
    }

    /**
     * Constructs an untypechecked ExprJoin expression.
     *
     * @param pos - the original position in the file
     * @param left - the left-hand-side expression
     * @param right - the right-hand-side expression
     *
     * @throws ErrorInternal if pos==null, left==null, or right==null
     * @throws ErrorSyntax if left.mult!=0 or right.mult!=0
     */
    public ExprJoin (Pos pos, Expr left, Expr right) {
        this(pos, left, right, null);
    }
}
