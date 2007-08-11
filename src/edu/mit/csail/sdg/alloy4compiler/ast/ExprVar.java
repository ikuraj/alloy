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

import java.util.Collection;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents a LET or QUANTIFICATION variable in the AST.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (type.unambiguous() && type==expr.type)
 */

public final class ExprVar extends Expr {

    /** The label associated with this variable; it's used for pretty-printing and does not have to be unique. */
    public final String label;

    /** The expression that this variable is quantified over or substituted by; always nonnull. */
    public final Expr expr;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(expr.span()));
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

    /** Constructs an ExprVar object */
    private ExprVar(Pos pos, String label, Expr expr, Err extraError) {
        super(pos, expr.type, 0, expr.weight, expr.errors.appendIfNotNull(extraError));
        this.label = (label==null ? "" : label);
        this.expr = expr;
    }

    /**
     * Constructs an ExprVar variable whose expr is well-typed (if not, the resulting node's error list will be nonempty)
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param expr - the quantification/substitution expression for this variable; <b> it must already be fully resolved </b>
     */
    public static ExprVar make(Pos pos, String label, Expr expr) {
        ErrorType e=null;
        if (expr.errors.size()==0) {
            if (expr.type==EMPTY)
                e=new ErrorType(expr.span(), "This expression failed to be typechecked.");
            else if (!expr.type.unambiguous())
                e=new ErrorType(expr.span(), "This expression is ambiguous. Its possible types are:\n"+expr.type);
        }
        return new ExprVar(pos, label, expr, e);
    }

    /** Resolves this expression. */
    @Override public ExprVar resolve(Type p, Collection<ErrorWarning> warns) throws Err {
        Expr newExpr = expr.resolve(p, warns);
        if (expr==newExpr) return this;
        return new ExprVar(pos, label, newExpr, new ErrorType(newExpr.span(), "This expression was not fully resolved."));
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
