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
import java.util.List;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.ccset;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents the builtin disjoint[] predicate.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (all x:args | x.mult==0)
 */

public final class ExprBuiltin extends Expr {

    /** The unmodifiable list of arguments. */
    public final ConstList<Expr> args;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr a:args) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("disjoint[");
            for(int i=0; i<args.size(); i++) { if (i>0) out.append(", "); out.append(args.get(i)); }
            out.append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("disjoint[] with type=").append(type).append('\n');
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    /** Constructs an ExprBuiltin node. */
    private ExprBuiltin(Pos pos, Type type, ConstList<Expr> args, long weight, JoinableList<Err> errs) {
        super(pos, type, 0, weight, errs);
        this.args = args;
    }

    /** Generates the expression disj[arg1, args2, arg3...] */
    public static Expr makeDISJOINT(Pos pos, List<Expr> args) {
        JoinableList<Err> errs=JoinableList.emptylist();
        TempList<Expr> newargs=new TempList<Expr>(args.size());
        Type type=Type.FORMULA, commonArity=null;
        long weight=0;
        if (args.size()<2)
            errs=errs.append(new ErrorSyntax(pos, "The builtin disjoint[] predicate must be called with at least two arguments."));
        for(Expr a:args) {
            errs = errs.join(a.errors);
            weight = weight + a.weight;
            if (a.mult!=0) errs = errs.append(new ErrorSyntax(a.span(), "Multiplicity expression not allowed here."));
            Expr b=a;
            if (b.type==EMPTY) {
                type=EMPTY;
            } else {
                b=cset(b);
                Err berr=ccset(b);
                if (berr!=null) { errs=errs.append(berr); type=EMPTY; }
                else if (commonArity==null) commonArity=b.type;
                else commonArity=commonArity.intersect(b.type);
            }
            newargs.add(b);
        }
        if (commonArity!=null && commonArity==EMPTY) errs=errs.append(new ErrorType(pos,
           "The builtin predicate disjoint[] cannot be used among expressions of different arities."));
        return new ExprBuiltin(pos, type, newargs.makeConst(), weight, errs);
    }

    /** Typechecks an ExprBuiltin object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed = false;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i), y=cset(x.check(cx));
            if (x!=y) changed=true;
            args.add(y);
        }
        if (changed) return makeDISJOINT(pos, args.makeConst()); else return this;
    }

    /** Typechecks an ExprBuiltin object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p, Collection<ErrorWarning> warns) throws Err {
        p=EMPTY;
        for(int i=0; i<this.args.size(); i++) {
            if (i==0) p=this.args.get(i).type; else p=p.unionWithCommonArity(this.args.get(i).type);
        }
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed = false;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i), y=cset(x.check(cx, p, warns));
            if (x!=y) changed=true;
            args.add(y);
        }
        if (changed) return makeDISJOINT(pos, args.makeConst()); else return this;
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
