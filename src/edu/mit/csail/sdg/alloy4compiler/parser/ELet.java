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

package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCustom;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p> <b>Invariant:</b>  left!=""
 * <p> <b>Invariant:</b>  left does not contain '/' nor '@'
 * <p> <b>Invariant:</b>  right.mult==0 && sub.mult==0
 */

final class ELet extends ExprCustom {

    /** The name of the LET variable. */
    final String left;

    /** The value for the LET variable. */
    final Expr right;

    /** The body of the LET expression. */
    final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(sub.span()));
        return p;
    }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("(let ").append(left).append("=... in ");
            sub.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) out.append(' ');
            out.append("let with type=").append(type).append('\n');
            for(int i=0; i<indent+2; i++) out.append(' ');
            out.append(left).append('\n');
            right.toString(out, indent+2);
            sub.toString(out, indent+2);
        }
    }

    /**
     * Constructs a LET expression.
     *
     * @param pos - the original position in the file (corresponding to lexical tokens "left=")
     * @param type - the combined expression's type
     * @param left - the name of the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression
     *
     * @throws ErrorSyntax if left=="", or left contains '/' or '@'
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     */
    private ELet(Pos pos, Type type, String left, Expr right, Expr sub) throws Err {
        super(pos, type, 0, right.weight+sub.weight);
        this.left=left;
        this.right=right;
        this.sub=sub;
        if (right.mult != 0) throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
        if (sub.mult != 0) throw new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here.");
        if (left.length()==0) throw new ErrorSyntax(this.pos, "Let variable name cannot be empty.");
        if (left.indexOf('/')>=0) throw new ErrorSyntax(this.pos, "Let variable name cannot contain \'/\'");
        if (left.indexOf('@')>=0) throw new ErrorSyntax(this.pos, "Let variable name cannot contain \'@\'");
    }

    /**
     * Constructs a LET expression.
     * <p> NOTE: the resulting expression will always be un-typechecked; you need to explicitly typecheck it
     *
     * @param pos - the original position in the file (corresponding to lexical tokens "left=")
     * @param left - the name of the LET variable
     * @param right - the value for the LET variable
     * @param sub - the body of the LET expression
     *
     * @throws ErrorSyntax if left=="", or left contains '/' or '@'
     * @throws ErrorSyntax if right.mult!=0 or sub.mult!=0
     */
    static Expr make(Pos pos, String left, Expr right, Expr sub) throws Err {
        return new ELet(pos, null, left, right, sub);
    }

    /** Typechecks an ELet object (first pass). */
    @Override public Expr check(final TypeCheckContext cxx) throws Err {
        Context cx=((Context)cxx);
        Expr right=cx.resolve(this.right);
        ExprVar var=new ExprVar(pos, left, right.type);
        cx.put(left, var);
        Expr sub=cx.check(this.sub);
        cx.remove(left);
        return ExprLet.make(pos, var, right, sub);
    }

    /** Typechecks an ELet object (second pass). */
    @Override public Expr check(final TypeCheckContext cx, final Type p) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }
}
