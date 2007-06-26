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

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;

/**
 * Immutable; represents a constant in the AST.
 */

public final class ExprConstant extends Expr {

    /** The type of constant. */
    public final Op op;

    /** If this node is a number constant, then this field stores the number, else this field stores 0. */
    private final int num;

    /** Return the actual number if this node is a number constant (and returns 0 if it is not). */
    public int num() { return num; }

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() { return pos; }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            if (op==Op.NUMBER) out.append(num); else out.append(op);
        } else {
            for(int i=0; i<indent; i++) out.append(' ');
            switch(op) {
              case TRUE: out.append("true\n"); return;
              case FALSE: out.append("false\n"); return;
              case IDEN: out.append("iden\n"); return;
            }
            out.append(num).append('\n');
        }
    }

    /**
     * Constructs an ExprConstant node.
     *
     * @param pos - the original position in the file
     * @param op - the choice of which constant it is
     * @param num - the number (if this is a number constant); it should be 0 if this is not a number constant
     */
    private ExprConstant(Pos pos, Op op, int num) {
        super(pos, (op==Op.IDEN ? Type.make2(UNIV) : (op==Op.NUMBER ? Type.INT : Type.FORMULA)));
        this.op=op;
        this.num=num;
    }

    /** The "TRUE" boolean value. */
    public static Expr TRUE = new ExprConstant(null, Op.TRUE, 0);

    /** The "FALSE" boolean value. */
    public static Expr FALSE = new ExprConstant(null, Op.TRUE, 0);

    /** The "iden" relation. */
    public static Expr IDEN = new ExprConstant(null, Op.IDEN, 0);

    /** The "0" integer. */
    public static Expr ZERO = new ExprConstant(null, Op.NUMBER, 0);

    /** The "1" integer. */
    public static Expr ONE = new ExprConstant(null, Op.NUMBER, 1);

    /** Constructs the integer "n" */
    public static Expr makeNUMBER(int n) { return new ExprConstant(null, ExprConstant.Op.NUMBER, n); }

    /** This class contains all possible constant types. */
    public enum Op {
        /** true                               */  TRUE("true"),
        /** false                              */  FALSE("false"),
        /** the builtin "iden" relation        */  IDEN("iden"),
        /** an integer constant                */  NUMBER("NUMBER");

        /** The constructor. */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator. */
        private final String label;

        /**
         * Makes an ExprConstant node
         * @param pos - the original position in the source file (can be null if unknown)
         * @param num - the number (this number is ignored if op!=NUMBER)
         */
        public final Expr make(Pos pos, int num) {
            return new ExprConstant(pos, this, (this==NUMBER ? num : 0));
        }

        /** Returns the human readable label for this operator. */
        @Override public final String toString() { return label; }
    }

    /** Typechecks an ExprConstant object (first pass). */
    @Override Expr check(final TypeCheckContext cx) { return this; }

    /** Typechecks an ExprConstant object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p) { return this; }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
