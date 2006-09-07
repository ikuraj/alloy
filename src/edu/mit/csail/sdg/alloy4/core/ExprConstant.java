package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; represents a constant in the AST.
 *
 * <p/> <b>Invariant:</b>  op!=null
 *
 * @author Felix Chang
 */

public final class ExprConstant extends Expr {

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

    /** The type of constant. */
    public final Op op;

    /** If this node is a number constant, then this field stores the number. */
    private final int num;

    /**
     * Return the actual number if this node is a number constant (and throws an error if it is not).
     *
     * @return the number (if this node is a number constant)
     * @throws ErrorInternal (if this node is not a number constant)
     */
    public int num() {
        if (op!=Op.NUMBER) throw internalError("This node is not a number constant");
        return num;
    }

    /**
     * Constructs an ExprConstant node.
     *
     * @param pos - the original position in the file
     * @param op - the choice of which constant it is
     * @param type - the type for this expression
     * @param num - the number (if this is a number constant)
     *
     * @throws ErrorInternal if pos==null
     */
    private ExprConstant(Pos pos, Op op, Type type, int num) {
        super(pos, type, 0);
        this.op=op;
        this.num=num;
    }

    /** This class contains all possible constant types. */
    public enum Op {
        /** the builtin "iden" relation */  IDEN("iden"),
        /** the builtin "univ" sig      */  UNIV("univ"),
        /** the builtin "none" sig      */  NONE("none"),
        /** the builtin "Int"  sig      */  SIGINT("Int"),
        /** an integer constant         */  NUMBER("NUMBER");

        /** The constructor. */
        Op(String l) {label=l;}

        /** The human readable label for this operator. */
        private final String label;

        /**
         * Constructs a set/relation constant with "this" as the operator.
         *
         * @param pos - the original position in the file
         * @throws ErrorInternal if pos==null
         * @throws ErrorInternal if this is not one of {UNIV,NONE,SIGINT,IDEN}
         */
        public final ExprConstant make(Pos pos) {
            if (this==UNIV)
                return new ExprConstant(pos, this, ParaSig.UNIV.type, 0);
            if (this==NONE)
                return new ExprConstant(pos, this, ParaSig.NONE.type, 0);
            if (this==SIGINT)
                return new ExprConstant(pos, this, ParaSig.SIGINT.type, 0);
            if (this==IDEN)
                return new ExprConstant(pos, this, ParaSig.UNIV.type.product_of_anyEmptyness(ParaSig.UNIV.type), 0);
            throw new ErrorInternal(pos, this, "Illegal operator "+this+" in ExprConstant.Op.make(pos)");
        }

        /**
         * Constructs a number constant.
         *
         * @param pos - the original position in the file
         * @param num - the string representation of the number
         * @throws ErrorInternal if this!=NUMBER
         * @throws ErrorInternal if pos==null or num==null
         * @throws ErrorSyntax if num cannot be parsed into a Java int
         */
        public final ExprConstant make(Pos pos, String num) {
            if (this!=NUMBER)
                throw new ErrorInternal(pos, this, "Illegal operator "+this+" in ExprConstant.Op.make(pos,num)");
            if (num==null)
                throw new ErrorInternal(pos, this, "NullPointerException");
            try {
                int n=Integer.parseInt(num);
                return new ExprConstant(pos, this, Type.INT, n);
            } catch(NumberFormatException e) {
                throw new ErrorSyntax(pos, "The number "+num
                        +" is too small or too large to be stored in a Java integer!");
            }
        }

        /** Returns the human readable label for this operator. */
        @Override public final String toString() { return label; }
    }
}
