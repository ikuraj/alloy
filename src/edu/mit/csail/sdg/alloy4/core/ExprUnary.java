package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; represents a unary expression of the form (OP subexpression).
 *
 * <p/> <b>Invariant:</b>  op!=null && sub!=null && sub.mult==0
 *
 * @author Felix Chang
 */

public final class ExprUnary extends Expr {

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

    /** The unary operator. */
    public final Op op;

    /** The subexpression. */
    public final Expr sub;

    /**
     * Constructs a new unary expression.
     *
     * @param pos - the original position in the file
     * @param op - the operator
     * @param sub - the subexpression
     * @param type - the type (null if this expression has not been typechecked)
     *
     * @throws ErrorInternal if pos==null or sub==null
     * @throws ErrorSyntax if sub.mult!=0
     */
    private ExprUnary(Pos pos, Op op, Expr sub, Type type) {
        super(pos, type, (op==Op.SOMEMULT || op==Op.LONEMULT || op==Op.ONEMULT || op==Op.SETMULT)?1:0);
        this.op=op;
        this.sub=nonnull(sub);
        if (sub.mult != 0) throw sub.syntaxError("Multiplicity expression not allowed here");
    }

    /** This class contains all possible unary operators. */
    public enum Op {
        /** :some x (where x is a set or relation) */  SOMEMULT(":some"),
        /** :lone x (where x is a set or relation) */  LONEMULT(":lone"),
        /** :one  x (where x is a set or relation) */  ONEMULT(":one"),
        /** :set  x (where x is a set or relation) */  SETMULT(":set"),
        /** not   f (where f is a formula)         */  NOT("not"),
        /** no    x (where x is a set or relation) */  NO("no"),
        /** some  x (where x is a set or relation) */  SOME("some"),
        /** lone  x (where x is a set or relation) */  LONE("lone"),
        /** one   x (where x is a set or relation) */  ONE("one"),
        /** transpose                              */  TRANSPOSE("~"),
        /** reflexive closure                      */  RCLOSURE("*"),
        /** closure                                */  CLOSURE("^"),
        /** cardinality                            */  CARDINALITY("#"),
        /** intAtom-to-integer                     */  SUM("sum"),
        /** integer-to-intAtom                     */  INTTOATOM("$");

        /** The constructor */
        Op(String label) {this.label=label;}

        /** The human readable label for this operator */
        private final String label;

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }

        /**
         * Constructs an untypechecked ExprUnary expression with "this" as the operator.
         *
         * @param pos - the original position in the file
         * @param sub - the subexpression
         *
         * @throws ErrorInternal if pos==null or sub==null
         * @throws ErrorSyntax if op==Op.ALL (since this is no longer supported)
         * @throws ErrorSyntax if sub.mult!=0
         */
        public final Expr make(Pos pos, Expr sub) { return new ExprUnary(pos,this,sub,null); }

        /**
         * Constructs a typechecked ExprUnary expression
         * with "this" as the operator, and "type" as the type.
         *
         * @param pos - the original position in the file
         * @param sub - the subexpression
         * @param type - the type
         *
         * @throws ErrorInternal if pos==null or sub==null
         * @throws ErrorSyntax if oo==Op.ALL (since this is no longer supported)
         * @throws ErrorSyntax if sub.mult!=0
         */
        public final Expr make(Pos pos, Expr sub, Type type) { return new ExprUnary(pos,this,sub,type); }
    }
}
