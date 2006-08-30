package edu.mit.csail.sdg.alloy4.core;



/**
 * Immutable; represents an expression of the form (x OP y).
 *
 * <br/>
 * <br/> Invariant: op!=null && left!=null && right!=null
 * <br/> Invariant: this.mult!=1
 * <br/> Invariant: this.mult==2 => op is one of the 16 arrow operators
 * <br/> Invariant: left.mult!=1
 * <br/> Invariant: left.mult==2 => op is one of the 16 arrow operators
 * <br/> Invariant: right.mult==1 => op==Op.IN
 * <br/> Invariant: right.mult==2 => (op==Op.IN || op is one of the 16 arrow operators)
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
     * That means it is of the form (A ?-&gt;? B), or it is (A-&gt;B)
     * where A and/or B is an arrow multiplicity constraint.
     */
    private static boolean isMult(Op op, Expr left, Expr right) {
        if (!op.isArrow()) return false;
        return left.mult>0 || right.mult>0 || op!=Op.ARROW;
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
        super(pos, type, isMult(op,left,right)?2:0);
        // See ExprUnary.java for why we have to call makeMult() here.
        if (op==Op.IN && (right instanceof ExprUnary)) right=((ExprUnary)right).makeMult();
        this.op=op;
        this.left=nonnull(left);
        this.right=nonnull(right);
        if (op.isArrow()) {
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
    public final ErrorType typeError(String msg,Type leftType,Type rightType) {
        return typeError(msg+" LeftType="+leftType+" RightType="+rightType);
    }

    /** This class contains all possible binary operators. */
    public enum Op {
        /** -&gt;           */  ARROW("->"),
        /** -&gt;some       */  ANY_ARROW_SOME("->some"),
        /** -&gt;one        */  ANY_ARROW_ONE("->one"),
        /** -&gt;lone       */  ANY_ARROW_LONE("->lone"),
        /** some-&gt;       */  SOME_ARROW_ANY("some->"),
        /** some-&gt;some   */  SOME_ARROW_SOME("some->some"),
        /** some-&gt;one    */  SOME_ARROW_ONE("some->one"),
        /** some-&gt;lone   */  SOME_ARROW_LONE("some->lone"),
        /** one-&gt;        */  ONE_ARROW_ANY("one->"),
        /** one-&gt;some    */  ONE_ARROW_SOME("one->some"),
        /** one-&gt;one     */  ONE_ARROW_ONE("one->one"),
        /** one-&gt;lone    */  ONE_ARROW_LONE("one->lone"),
        /** lone-&gt;       */  LONE_ARROW_ANY("lone->"),
        /** lone-&gt;some   */  LONE_ARROW_SOME("lone->some"),
        /** lone-&gt;one    */  LONE_ARROW_ONE("lone->one"),
        /** lone-&gt;lone   */  LONE_ARROW_LONE("lone->lone"),
        /** &lt;:           */  DOMAIN("<:"),
        /** :&gt;           */  RANGE(":>"),
        /** &amp;           */  INTERSECT("&"),
        /** ++              */  PLUSPLUS("++"),
        /** +               */  PLUS("+"),
        /** -               */  MINUS("-"),
        /** =               */  EQUALS("="),
        /** &lt;            */  LT("<"),
        /** =&lt;           */  LTE("=<"),
        /** &gt;            */  GT(">"),
        /** &gt;=           */  GTE(">="),
        /** in              */  IN("in"),
        /** &amp;&amp;      */  AND("&&"),
        /** ||              */  OR("||"),
        /** &lt;=&gt;       */  IFF("<=>"),
        /** =&gt;           */  IMPLIES("=>");

        /** The constructor. */
        Op(String l) {label=l;}

        /**
         * Returns true if and only if this operator is the Cartesian
         * product "-&gt;", or is a multiplicity arrow of the form "?-&gt;?".
         */
        public boolean isArrow() {
            return this.compareTo(ARROW)>=0 && this.compareTo(LONE_ARROW_LONE)<=0;
        }

        /** The human readable label for this operator. */
        private final String label;

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
