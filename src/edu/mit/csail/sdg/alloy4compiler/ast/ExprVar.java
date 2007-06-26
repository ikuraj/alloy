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

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.addOne;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.unambiguous;

/**
 * Immutable; represents a "quantified variable" or "substitution variable" in the AST.
 */

public class ExprVar extends Expr {

    /** Stores a String label associated with this variable; it's used for pretty-printing and does not have to be unique. */
    public final String label;

    /** The expression that this variable is quantified over; null if this variable is not a quantified variable */
    public final Expr expr;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = (expr==null ? pos : pos.merge(expr.span())));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(label);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("Var ").append(label).append(" with type=").append(type).append('\n');
        }
    }

    /**
     * Constructs a quantification ExprVar variable
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param expr - the bounding expression for this variable (it must be a set or relation expression with unambiguous type)
     */
    public ExprVar(Pos pos, String label, Expr expr) throws Err {
        super(pos, expr.type, 0, expr.weight);
        this.label = (label==null ? "" : label);
        this.expr = addOne(unambiguous(cset(expr)));
    }

    /**
     * Constructs a substitution ExprVar variable
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param type - the type for this variable (it must be an unambiguous type)
     */
    public ExprVar(Pos pos, String label, Type type) throws Err {
        super(pos, type, 0, 0);
        this.label = (label==null ? "" : label);
        this.expr = null;
        unambiguous(this);
    }

    /** Typechecks an ExprName object (first pass). */
    @Override Expr check(final TypeCheckContext cx) { return this; }

    /** Typechecks an ExprVar object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type type) { return this; }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
