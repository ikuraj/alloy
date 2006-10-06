package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.helper.ErrorInternal;
import edu.mit.csail.sdg.alloy4.helper.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.helper.Pos;

/**
 * Immutable; represents a relational join expression
 * (though the typechecker may later replace this with an ExprCall node).
 *
 * <p/>
 * Before typechecking, it's not easy to tell if expressions
 * like "a[b]" or "b.a" are joins or calls.
 *
 * <p/>
 * So instead, everyone (except the typechecker) must create only ExprJoin nodes.
 * The typechecker will convert it to an appropriate ExprCall node if it's a call.
 *
 * <p/> <b>Invariant:</b>  left!=null && left.mult==0
 * <p/> <b>Invariant:</b>  right!=null && right.mult==0
 *
 * @author Felix Chang
 */

public final class ExprJoin extends Expr {

    /** Accepts the return visitor. */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.visit(this);
    }

    /** Accepts the typecheck visitor bottom-up. */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.visit(this);
    }

    /** Accepts the typecheck visitor top-down. */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.visit(this,type);
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
