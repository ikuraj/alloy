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
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cform;

/**
 * Immutable; represents an if-then-else expression.
 *
 * <p> <b>Invariant:</b>  cond.mult==0
 * <p> <b>Invariant:</b>  left.mult==0
 * <p> <b>Invariant:</b>  right.mult==0
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

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = cond.span().merge(left.span()).merge(right.span()));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
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
    private ExprITE(Expr cond, Expr left, Expr right, Type type) throws Err {
        super(null, type, 0, cond.weight+left.weight+right.weight);
        this.cond=cond;
        this.left=left;
        this.right=right;
        if (cond.mult != 0) throw new ErrorSyntax(cond.span(), "Multiplicity expression not allowed here.");
        if (left.mult != 0) throw new ErrorSyntax(left.span(), "Multiplicity expression not allowed here.");
        if (right.mult != 0) throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
    }

    /**
     * Constructs a ExprITE expression.
     *
     * @param cond - the condition formula
     * @param left - the then-clause
     * @param right - the else-clause
     *
     * @throws ErrorSyntax if cond.mult!=0, left.mult!=0, or right.mult!=0
     * @throws ErrorType if cond.type!=null && left.type!=null && right.type!=null && the types cannot possibly be legal together
     */
    public static Expr make(Expr cond, Expr left, Expr right) throws Err {
        if (cond.type==null || left.type==null || right.type==null) return new ExprITE(cond,left,right,null);
        cform(cond);
        Type a=left.type, b=right.type, c=a.unionWithCommonArity(b);
        if (a.is_int && b.is_int) c=Type.makeInt(c);
        if (a.is_bool && b.is_bool) c=Type.makeBool(c);
        if (c.size()>0 || c.is_bool || c.is_int) return new ExprITE(cond,left,right,c);
        if (TypeCheckContext.auto_sigint2int) {
            if (a.is_int && b.intersects(SIGINT.type)) return make(cond, left, right.cast2int());
            if (b.is_int && a.intersects(SIGINT.type)) return make(cond, left.cast2int(), right);
        }
        if (TypeCheckContext.auto_int2sigint) {
            if (a.is_int && b.hasArity(1)) return make(cond, left.cast2sigint(), right);
            if (b.is_int && a.hasArity(1)) return make(cond, left, right.cast2sigint());
        }
        Pos pos = cond.span().merge(left.span()).merge(right.span());
        throw new ErrorType(pos, "The then-clause and the else-clause must match.\nIts then-clause has type(s) "+a+"\nand the else-clause has type(s) "+b);
    }

    /** Typechecks an ExprITE object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        Expr cond=this.cond.check(cx); cform(cond);
        Expr left=this.left.check(cx); if (left.type==null) throw new ErrorType(left.span(), "This expression fails to be typechecked.");
        Expr right=this.right.check(cx); if (right.type==null) throw new ErrorType(right.span(), "This expression fails to be typechecked.");
        if (cond==this.cond && left==this.left && right==this.right) return this; else return make(cond,left,right);
    }

    /** Typechecks an ExprITE object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p) throws Err {
        Type a=left.type, b=right.type;
        if (p.size()>0) {
          if (a.hasTuple()) {
            a=a.intersect(p);
            if (a.hasNoTuple()) A4Reporter.getReporter().warning(new ErrorWarning(left.span(), "This expression is redundant in its parent if-then-else expression."));
          } else {
            a=Type.EMPTY;
          }
          if (b.hasTuple()) {
            b=b.intersect(p);
            if (b.hasNoTuple()) A4Reporter.getReporter().warning(new ErrorWarning(right.span(), "This expression is redundant in its parent if-then-else expression."));
          } else {
            b=Type.EMPTY;
          }
        } else {
          a=(b=p);
        }
        Expr cond=this.cond.check(cx, Type.FORMULA);
        Expr left=this.left.check(cx, a);
        Expr right=this.right.check(cx, b);
        if (cond==this.cond && left==this.left && right==this.right) return this; else return make(cond,left,right);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
