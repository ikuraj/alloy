package edu.mit.csail.sdg.alloy4.core;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p/> <b>Invariant:</b>  left!=null  &&  left is not ""  &&  left does not contain '/' nor '@'
 * <p/> <b>Invariant:</b>  right!=null &&  right.mult==0
 * <p/> <b>Invariant:</b>  sub!=null   &&  sub.mult==0
 *
 * @author Felix Chang
 */

public final class ExprLet extends Expr {

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

    /** The name of the LET variable. */
    public final String left;

    /** The value for the LET variable. */
    public final Expr right;

    /** The body of the LET expression. */
    public final Expr sub;

    /**
     * Constructs a typechecked LET expression.
     *
     * @param pos - the original position in the file
     * @param left - the name of the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression
     * @param type - the type
     *
     * @throws ErrorInternal if pos==null, left==null, right==null, or sub==null
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     * @throws ErrorSyntax if left contains '/' or '@'
     * @throws ErrorSyntax if left is equal to ""
     */
    public ExprLet(Pos pos, String left, Expr right, Expr sub, Type type) {
        super(pos, type, 0);
        this.left=nonnull(left);
        this.right=nonnull(right);
        this.sub=nonnull(sub);
        if (right.mult != 0) throw right.syntaxError("Multiplicity expression not allowed here");
        if (sub.mult != 0) throw sub.syntaxError("Multiplicity expression not allowed here");
        if (left.length()==0) throw syntaxError("The LET variable must not be empty!");
        if (left.indexOf('/')>=0) throw syntaxError("The LET variable cannot contain \'/\'");
        if (left.indexOf('@')>=0) throw syntaxError("The LET variable cannot contain \'@\'");
    }

    /**
     * Constructs an untypechecked LET expression.
     *
     * @param pos - the original position in the file
     * @param left - the name of the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression
     *
     * @throws ErrorInternal if pos==null, left==null, right==null, or sub==null
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     * @throws ErrorSyntax if left contains '/' or '@'
     * @throws ErrorSyntax if left is equal to ""
     */
    public ExprLet(Pos pos, String left, Expr right, Expr sub) {
        this(pos, left, right, sub, null);
    }
}
