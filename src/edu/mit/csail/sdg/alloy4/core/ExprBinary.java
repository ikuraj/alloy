package edu.mit.csail.sdg.alloy4.core;

/**
 * Immutable; represents an expression of the form (x OP y).
 *
 * <p/> <b>Invariant:</b> op!=null && left!=null && right!=null
 * <p/> <b>Invariant:</b> this.mult!=1
 * <p/> <b>Invariant:</b> this.mult==2 => this.op is one of the 16 arrow operators
 * <p/> <b>Invariant:</b> left.mult!=1
 * <p/> <b>Invariant:</b> left.mult==2 => this.op is one of the 16 arrow operators
 * <p/> <b>Invariant:</b> right.mult==1 => this.op==IN
 * <p/> <b>Invariant:</b> right.mult==2 => (this.op==IN || this.op is one of the 16 arrow operators)
 *
 * @author Felix Chang
 */

public final class ExprBinary extends Expr {

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

    /** The binary operator. */
    public final Op op;

    /** The left-hand-side expression. */
    public final Expr left;

    /** The right-hand-side expression. */
    public final Expr right;

    /**
     * Determines whether the expression (left op right)
     * is an arrow multiplicity constraint.
     * <br>
     * That means it is of the form (A ?->? B), or it is (A->B)
     * where A and/or B is an arrow multiplicity constraint.
     */
    private static boolean isArrowMult(Op op, Expr left, Expr right) {
        if (!op.isArrow) return false;
        return left.mult==2 || right.mult==2 || op!=Op.ARROW;
    }

    /**
     * Constructs an ExprBinary expression.
     *
     * @param pos - the original position in the file
     * @param op - the operator
     * @param left - the left-hand-side expression
     * @param right - the right-hand-side expression
     * @param type - the type (null if this expression has not been typechecked)
     *
     * @throws ErrorInternal if pos==null || left==null || right==null
     * @throws ErrorInternal if op is one of the 16 arrow operators && (left.mult==1 || right.mult==1)
     * @throws ErrorInternal if op isn't one of the 16 arrow operators && left.mult!=0
     * @throws ErrorInternal if op isn't one of the 16 arrow operators && op!=Op.IN && right.mult!=0
     */
    private ExprBinary(Pos pos, Op op, Expr left, Expr right, Type type) {
        super(pos, type, isArrowMult(op,left,right)?2:0);
        // See ExprUnary.java for why we have to call makeMult() here.
        if (op==Op.IN && (right instanceof ExprUnary)) right=((ExprUnary)right).makeMult();
        this.op=op;
        this.left=nonnull(left);
        this.right=nonnull(right);
        if (op.isArrow) {
            if (left.mult==1)
                throw left.syntaxError("Set-multiplicity expression not allowed here");
            if (right.mult==1)
                throw right.syntaxError("Set-multiplicity expression not allowed here");
        } else {
            if (left.mult!=0)
                throw left.syntaxError("Multiplicity expression is not allowed here");
            if (op!=Op.IN && right.mult!=0)
                throw right.syntaxError("Multiplicity expression is not allowed here");
        }
    }

    /**
     * Generate a new type error exception with "msg" as the message,
     * and include the left-hand-type "lefttype"
     * and right-hand-type "righttype" in the message.
     */
    public final ErrorType typeError(String msg, Type leftType, Type rightType) {
        return typeError(msg+" Left type = "+leftType+" Right type = "+rightType);
    }

    /** This class contains all possible binary operators. */
    public enum Op {
        /** -&gt;           */  ARROW("->",true),
        /** -&gt;some       */  ANY_ARROW_SOME("->some",true),
        /** -&gt;one        */  ANY_ARROW_ONE("->one",true),
        /** -&gt;lone       */  ANY_ARROW_LONE("->lone",true),
        /** some-&gt;       */  SOME_ARROW_ANY("some->",true),
        /** some-&gt;some   */  SOME_ARROW_SOME("some->some",true),
        /** some-&gt;one    */  SOME_ARROW_ONE("some->one",true),
        /** some-&gt;lone   */  SOME_ARROW_LONE("some->lone",true),
        /** one-&gt;        */  ONE_ARROW_ANY("one->",true),
        /** one-&gt;some    */  ONE_ARROW_SOME("one->some",true),
        /** one-&gt;one     */  ONE_ARROW_ONE("one->one",true),
        /** one-&gt;lone    */  ONE_ARROW_LONE("one->lone",true),
        /** lone-&gt;       */  LONE_ARROW_ANY("lone->",true),
        /** lone-&gt;some   */  LONE_ARROW_SOME("lone->some",true),
        /** lone-&gt;one    */  LONE_ARROW_ONE("lone->one",true),
        /** lone-&gt;lone   */  LONE_ARROW_LONE("lone->lone",true),
        /** &lt;:           */  DOMAIN("<:",false),
        /** :&gt;           */  RANGE(":>",false),
        /** &amp;           */  INTERSECT("&",false),
        /** ++              */  PLUSPLUS("++",false),
        /** +               */  PLUS("+",false),
        /** -               */  MINUS("-",false),
        /** =               */  EQUALS("=",false),
        /** &lt;            */  LT("<",false),
        /** =&lt;           */  LTE("=<",false),
        /** &gt;            */  GT(">",false),
        /** &gt;=           */  GTE(">=",false),
        /** in              */  IN("in",false),
        /** &amp;&amp;      */  AND("&&",false),
        /** ||              */  OR("||",false),
        /** &lt;=&gt;       */  IFF("<=>",false),
        /** =&gt;           */  IMPLIES("=>",false);

        /**
         * The constructor.
         * @param label - the label (for printing debugging messages)
         * @param isArrow - true if this operator is one of the 16 arrow operators
         */
        Op(String label, boolean isArrow) {
            this.label=label;
            this.isArrow=isArrow;
        }

        /** The human readable label for this operator. */
        private final String label;

        /**
         * True if and only if this operator is the Cartesian
         * product "->", or is a multiplicity arrow of the form "?->?".
         */
        public final boolean isArrow;

        /**
         * Constructs an untypechecked ExprBinary expression
         * with "this" as the operator.
         *
         * @param pos - the original position in the file
         * @param left - the left-hand-side expression
         * @param right - the right-hand-side expression
         *
         * @throws ErrorInternal if pos==null || left==null || right==null
         * @throws ErrorInternal if op is one of the 16 arrow operators && (left.mult==1 || right.mult==1)
         * @throws ErrorInternal if op isn't one of the 16 arrow operators && left.mult!=0
         * @throws ErrorInternal if op isn't one of the 16 arrow operators && op!=Op.IN && right.mult!=0
         */
        public final ExprBinary make(Pos pos, Expr left, Expr right) {
            return new ExprBinary(pos, this, left, right, null);
        }

        /**
         * Constructs a typechecked ExprBinary expression
         * with "this" as the operator, and "type"
         * as the type.
         *
         * @param pos - the original position in the file
         * @param left - the left-hand-side expression
         * @param right - the right-hand-side expression
         * @param type - the type for the expression
         *
         * @throws ErrorInternal if pos==null || left==null || right==null
         * @throws ErrorInternal if op is one of the 16 arrow operators && (left.mult==1 || right.mult==1)
         * @throws ErrorInternal if op isn't one of the 16 arrow operators && left.mult!=0
         * @throws ErrorInternal if op isn't one of the 16 arrow operators && op!=Op.IN && right.mult!=0
         */
        public final ExprBinary make(Pos pos, Expr left, Expr right, Type type) {
            return new ExprBinary(pos, this, left, right, type);
        }

        /** Returns the human readable label for this operator. */
        @Override public final String toString() { return label; }
    }
}
