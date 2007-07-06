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

import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4compiler.parser.Context;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p> <b>Invariant:</b>  right.mult==0 && sub.mult==0
 */

public final class ExprLet extends Expr {

    /** The LET variable. */
    public final ExprVar left;

    /** The value for the LET variable. */
    public final Expr right;

    /** The body of the LET expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(left.span()).merge(right.span()).merge(sub.span()));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("(let ");
            left.toString(out,-1);
            out.append("=... in ");
            sub.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("let with type=").append(type).append('\n');
            left.toString(out, indent+2);
            right.toString(out, indent+2);
            sub.toString(out, indent+2);
        }
    }

    /**
     * Constructs a LET expression.
     *
     * @param pos - the original position in the file (corresponding to the lexical tokens "left=")
     * @param left - the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "left" as a free variable)
     *
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     * @throws ErrorAPI if left.expr!=null (since the variable must be a "substitution var" rather than a "quantified var"
     * @throws ErrorType if left.type.equals(right.type) is false
     * @throws ErrorType if right.type==null or (right.type.size()==0 && !right.type.is_int && !right.type.is_bool)
     * @throws ErrorType if sub.type==null or (sub.type.size()==0 && !sub.type.is_int && !sub.type.is_bool)
     */
    private ExprLet(Pos pos, ExprVar left, Expr right, Expr sub) throws Err {
        super(pos, (left.type==null || right.type==null) ? null : sub.type, 0, left.weight+right.weight+sub.weight);
        this.left=left;
        this.right=right;
        this.sub=sub;
        if (left.type!=null && left.expr!=null)
            throw new ErrorAPI(sub.span(), "This variable must be a substitution variable rather than a quantification variable.");
        if (sub.mult != 0)
            throw new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here.");
        if (right.mult != 0)
            throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
        if (this.type==null)
            return;
        if (sub.type==null || (sub.type.size()==0 && !sub.type.is_int && !sub.type.is_bool))
            throw new ErrorType(sub.span(), "This expression failed to be typechecked.");
        if (right.type==null || (right.type.size()==0 && !right.type.is_int && !right.type.is_bool))
            throw new ErrorType(right.span(), "This expression failed to be typechecked.");
        if (!right.type.equals(left.type))
            throw new ErrorType(pos,
            "The substitution variable's type must match the substitution expression.\nThe variable's type is "
            +left.type+"\nBut the substitution expression's type is "+right.type);
    }

    /**
     * Constructs a LET expression.
     *
     * @param pos - the original position in the file (can be null if unknown)
     * @param left - the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "left" as a free variable)
     *
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     * @throws ErrorType if left.type.equals(right.type) is false
     * @throws ErrorType if right.type==null or (right.type.size()==0 && !right.type.is_int && !right.type.is_bool)
     * @throws ErrorType if sub.type==null or (sub.type.size()==0 && !sub.type.is_int && !sub.type.is_bool)
     */
    public static Expr make(Pos pos, ExprVar left, Expr right, Expr sub) throws Err {
        return new ExprLet(pos, left, right, sub);
    }

    /** Typechecks an ExprLet object (first pass). */
    @Override Expr check(final TypeCheckContext cxx) throws Err {
        if (left.type==null) {
            Context cx=((Context)cxx);
            Expr right=cx.resolve(this.right);
            ExprVar var=new ExprVar(pos, left.label, right.type);
            cx.put(left.label, var);
            Expr sub=cx.check(this.sub);
            cx.remove(left.label);
            return new ExprLet(pos, var, right, sub);
        }
        Expr right=cxx.check(this.right);
        Expr sub=cxx.check(this.sub);
        if (right==this.right && sub==this.sub) return this; else return new ExprLet(pos, left, right, sub);
    }

    /** Typechecks an ExprLet object (second pass). */
    @Override Expr check(final TypeCheckContext cx, final Type p) throws Err {
        if (left.type==null) throw new ErrorFatal("Internal typechecker invariant violated.");
        Expr right=cx.check(this.right, this.right.type);
        Expr sub=cx.check(this.sub, p);
        if (right==this.right && sub==this.sub) return this; else return new ExprLet(pos, left, right, sub);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
