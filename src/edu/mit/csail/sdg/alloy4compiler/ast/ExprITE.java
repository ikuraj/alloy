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
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.ccform;

/**
 * Immutable; represents an if-then-else expression.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (cond.mult==0 && left.mult==0 && right.mult==0)
 */

public final class ExprITE extends Expr {

    /** The condition formula. */
    public final Expr cond;

    /** The then-clause. */
    public final Expr left;

    /** The else-clause. */
    public final Expr right;

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = cond.span().merge(right.span()).merge(left.span()));
        return p;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append('(');
            cond.toString(out,-1);
            out.append(" => ");
            left.toString(out,-1);
            out.append(" else ");
            right.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("if-then-else with type=").append(type).append('\n');
            cond.toString(out, indent+2);
            left.toString(out, indent+2);
            right.toString(out, indent+2);
        }
    }

    /** Constructs a ExprITE expression. */
    private ExprITE(Expr cond, Expr left, Expr right, Type type, JoinableList<Err> errs) {
        super(null, type, 0, cond.weight+left.weight+right.weight, errs);
        this.cond=cond;
        this.left=left;
        this.right=right;
    }

    /**
     * Constructs a ExprITE expression.
     *
     * @param cond - the condition formula
     * @param left - the then-clause
     * @param right - the else-clause
     */
    public static Expr make(Expr cond, Expr left, Expr right) {
        JoinableList<Err> errs = cond.errors.join(left.errors).join(right.errors);
        if (cond.mult != 0) errs = errs.append(new ErrorSyntax(cond.span(), "Multiplicity expression not allowed here."));
        if (left.mult != 0) errs = errs.append(new ErrorSyntax(left.span(), "Multiplicity expression not allowed here."));
        if (right.mult != 0) errs = errs.append(new ErrorSyntax(right.span(), "Multiplicity expression not allowed here."));
        Type c=EMPTY;
        while(left.errors.isEmpty() && right.errors.isEmpty()) {
            Type a=left.type, b=right.type;
            c = a.unionWithCommonArity(b);
            if (a.is_int && b.is_int) c=Type.makeInt(c);
            if (a.is_bool && b.is_bool) c=Type.makeBool(c);
            if (c==EMPTY) {
                if (Type.SIGINT2INT) {
                    if (a.is_int && b.intersects(SIGINT.type)) { right=right.cast2int(); continue; }
                    if (b.is_int && a.intersects(SIGINT.type)) { left=left.cast2int(); continue; }
                }
                if (Type.INT2SIGINT) {
                    if (a.is_int && b.hasArity(1)) { left=left.cast2sigint(); continue; }
                    if (b.is_int && a.hasArity(1)) { right=right.cast2sigint(); continue; }
                }
                errs = errs.append(new ErrorType(cond.span().merge(right.span()).merge(left.span()),
                    "The then-clause and the else-clause must match.\nIts then-clause has type: "
                    + a + "\nand the else-clause has type: " + b));
            }
            break;
        }
        return new ExprITE(cond, left, right, c, errs.appendIfNotNull(ccform(cond)));
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) {
        Type a=left.type, b=right.type;
        if (p.size()>0) {
            a=a.intersect(p);
            b=b.intersect(p);
            if (p.is_int) { a=Type.makeInt(a); b=Type.makeInt(b); }
            if (p.is_bool) { a=Type.makeBool(a); b=Type.makeBool(b); }
            if (left.type.hasTuple() && !a.hasTuple()) warns.add(new ErrorWarning(left.span(),"This subexpression is redundant."));
            if (right.type.hasTuple() && !b.hasTuple()) warns.add(new ErrorWarning(right.span(),"This subexpression is redundant."));
        } else {
            a=p;
            b=p;
        }
        Expr cond = this.cond.resolve(Type.FORMULA, warns);
        Expr left = this.left.resolve(a, warns);
        Expr right = this.right.resolve(b, warns);
        return (cond==this.cond && left==this.left && right==this.right) ? this : make(cond,left,right);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
