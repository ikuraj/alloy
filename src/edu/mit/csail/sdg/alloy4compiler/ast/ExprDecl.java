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
import edu.mit.csail.sdg.alloy4.ErrorWarning;

/**
 */

public final class ExprDecl extends Expr {

    /** The variable. */
    public final ExprVar var;

    /** The expression. */
    public final Expr expr;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = ( p = pos.merge(var.span()).merge(expr.span()) );
        return p;
    }

    /** Constructs an ExprDecl expression. */
    private ExprDecl(Pos pos, ExprVar var, Expr expr) {
        super(pos, expr.type, 0, 0, expr.errors);
        this.var = var;
        this.expr = expr;
    }

    public static ExprDecl make(Pos pos, ExprVar var, Expr expr) {
        return new ExprDecl(pos, var, expr);
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
    }

    /** Typechecks an ExprDecl object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        Expr expr = this.expr.check(cx);
        return (expr==this.expr) ? this : make(pos, var, expr);
    }

    /** Typechecks an ExprDecl object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p, Collection<ErrorWarning> warns) throws Err {
        Expr expr = this.expr.check(cx, p, warns);
        return (expr==this.expr) ? this : make(pos, var, expr);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
