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

import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;

/**
 * Immutable; represents a builtin predicate or function.
 *
 * <p> <b>Invariant:</b>  all x:args | x.mult==0
 */

public final class ExprBuiltin extends Expr {

    /** This class contains all possible constant types. */
    public enum Op {
        /** the builtin "disjoint" predicate */  DISJOINT("disjoint[]");

        /** The constructor. */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator. */
        private final String label;

        /** Returns the human readable label for this operator. */
        @Override public final String toString() { return label; }
    }

    /** The type of constant. */
    public final Op op;

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
            out.append("builtin ").append(op).append(" with type=").append(type).append('\n');
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    /** Constructs an ExprBuiltin node. */
    private ExprBuiltin(Pos pos, Type type, Op op, ConstList<Expr> args, long weight) throws Err {
        super(pos, type, 0, weight);
        this.op = op;
        this.args = args;
        if (args.size()<1) throw new ErrorSyntax(span(), "The builtin disjoint[] predicate must be called with at least one argument.");
        if (type==null) return;
        Type commonArity=null;
        for(Expr a:args) {
            if (commonArity==null) commonArity=a.type; else commonArity=commonArity.intersect(a.type);
        }
        if (commonArity.size()==0) throw new ErrorType(span(), "The builtin predicate disjoint[] cannot be used among expressions of different arities.");
    }

    /**
     * Generates the expression disj[arg1, args2, arg3...].
     * <p> If args.size()<1, we will throw a SyntaxError immediately.
     * <p> If args.size()<2, we will throw a SyntaxError during the typechecking phase.
     */
    public static Expr makeDISJOINT(Pos pos, List<Expr> args) throws Err {
        TempList<Expr> newargs=new TempList<Expr>(args.size());
        Type type=Type.FORMULA;
        long weight=0;
        for(Expr a:args) {
            if (a.mult!=0) throw new ErrorSyntax(a.span(), "Multiplicity expression not allowed here.");
            Expr b=a;
            if (b.type==null) type=null; else b=cset(b);
            newargs.add(b);
            weight += b.weight;
        }
        return new ExprBuiltin(pos, type, ExprBuiltin.Op.DISJOINT, newargs.makeConst(), weight);
    }

    /** Typechecks an ExprBuiltin object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        if (args.size()<2) throw new ErrorSyntax(span(), "The builtin predicate disjoint[] must be called with 2 or more arguments.");
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed=false;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i), y=cset(cx.check(x));
            if (x!=y) changed=true;
            args.add(y);
        }
        if (changed) return makeDISJOINT(pos, args.makeConst()); else return this;
    }

    /** Typechecks an ExprBuiltin object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p) throws Err {
        if (args.size()<2) throw new ErrorSyntax(pos, "The builtin disjoint[] predicate must be called with 2 or more arguments.");
        for(int i=0; i<args.size(); i++) {
            if (i==0) p=args.get(i).type; else p=p.intersect(args.get(i).type);
        }
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed=false;
        int arity=0;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i), y=cset(cx.check(x,p));
            if (x!=y) changed=true;
            args.add(y);
            int newArity=y.type.arity();
            if (newArity<0) throw new ErrorType(y.span(), "This expression is ambiguous. Its possible types are:\n" + y.type);
            if (i==0)
              arity=newArity;
            else if (arity!=newArity)
              throw new ErrorType(span(), "The builtin predicate disjoint[] cannot be used among expressions of different arities.");
        }
        if (changed) return makeDISJOINT(pos, args.makeConst()); else return this;
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
