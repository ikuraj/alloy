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
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p> <b>Invariant:</b>  type!=null => (var.mult==0 && var.expr.mult==0 && sub.mult==0)
 */

public final class ExprLet extends Expr {

    /** The list of warnings. */
    private final ConstList<ErrorWarning> warnings;

    /** The LET variable. */
    public final ExprVar var;

    /** The body of the LET expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(var.span()).merge(sub.span()));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("(let ").append(var.label).append("=... in ");
            sub.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("let with type=").append(type).append('\n');
            var.toString(out, indent+2);
            sub.toString(out, indent+2);
        }
    }

    /** Constructs a LET expression. */
    private ExprLet(Pos pos, Type t, ExprVar var, Expr sub, JoinableList<Err> errs, Collection<ErrorWarning> warnings) {
        super(pos, t, 0, var.weight+sub.weight, errs);
        this.var=var;
        this.sub=sub;
        this.warnings=ConstList.make(warnings);
    }

    /**
     * Constructs a LET expression.
     *
     * @param pos - the original position in the file corresponding to the tokens "var=" (can be null if unknown)
     * @param var - the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "var" as a free variable)
     */
    public static Expr make(Pos pos, ExprVar var, Expr sub) { return make(pos,var,sub,null); }

    /**
     * Constructs a LET expression.
     *
     * @param pos - the original position in the file corresponding to the tokens "var=" (can be null if unknown)
     * @param var - the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "var" as a free variable)
     */
    public static Expr make(Pos pos, ExprVar var, Expr sub, Collection<ErrorWarning> warnings) {
        JoinableList<Err> errs = var.errors.join(sub.errors);
        Type t = (var.type!=null && var.type!=EMPTY) ? sub.type : null;
        if (sub.mult != 0)
            errs = errs.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
        if (var.expr!=null && var.expr.mult!=0)
            errs = errs.append(new ErrorSyntax(var.expr.span(), "Multiplicity expression not allowed here."));
        return new ExprLet(pos, t, var, sub, errs, warnings);
    }

    /** Typechecks an ExprLet object (second pass). */
    @Override public Expr check(final Type p, Collection<ErrorWarning> warns) throws Err {
        if (var.type==null || var.type==EMPTY) throw new ErrorFatal("Internal typechecker invariant violated.");
        Expr sub = this.sub.check(p, warns);
        if (warnings.size()>0) warns.addAll(warnings);
        if (sub==this.sub) return this; else return make(pos, var, sub, null);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
