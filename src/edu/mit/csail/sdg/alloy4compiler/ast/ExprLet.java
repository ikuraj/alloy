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
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (var.type.unambiguos() && sub.mult==0)
 */

public final class ExprLet extends Expr {

    /** The LET variable. */
    public final ExprVar var;

    /** The body of the LET expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = var.span().merge(sub.span()));
        return p;
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("(let ").append(var.label).append("=... | ");
            sub.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("let with type=").append(type).append('\n');
            var.toString(out, indent+2);
            sub.toString(out, indent+2);
        }
    }

    //=============================================================================================================//

    /** Constructs a LET expression. */
    private ExprLet(ExprVar var, Expr sub, JoinableList<Err> errs) {
        super(Pos.UNKNOWN, sub.ambiguous, sub.type, 0, var.weight+sub.weight, errs);
        this.var=var;
        this.sub=sub;
    }

    //=============================================================================================================//

    /**
     * Constructs a LET expression.
     *
     * @param var - the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "var" as a free variable)
     */
    public static Expr make(ExprVar var, Expr sub) {
        JoinableList<Err> errs = var.errors.join(sub.errors);
        if (sub.mult != 0)
            errs = errs.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
        if (var.expr.mult!=0)
            errs = errs.append(new ErrorSyntax(var.expr.span(), "Multiplicity expression not allowed here."));
        return new ExprLet(var, sub, errs);
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warnings) {
        if (errors.size()>0) return this;
        // If errors.size()==0, then the variable is always already fully resolved, so we only need to resolve sub
        Expr newSub = sub.resolve(p, warnings);
        return (sub==newSub) ? this : make(var, newSub);
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (!errors.isEmpty()) throw errors.get(0);
        return visitor.visit(this);
    }
}
