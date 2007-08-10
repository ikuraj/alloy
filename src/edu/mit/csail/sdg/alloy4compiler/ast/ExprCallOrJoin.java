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
import java.util.Set;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.parser.Context;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;

/**
 * Immutable; represents either a relational join or a function/predicate call.
 *
 * <p>
 * Note: Before typechecking, it's not easy to tell if expressions like "a[b]" or "b.a" are joins or calls,
 * so the parser will generate "ExprCallOrJoin" nodes instead. During typechecking, these nodes will be replaced
 * with the correct ExprCall or ExprBinary.Op.JOIN nodes.
 *
 * <p>
 * Furthermore, if a is a primitive integer, and b is the Int signature,
 * then a.join(b) is converted into a.cast2sigint()
 *
 * <p> <b>Invariant:</b>  left.mult==0 && right.mult==0
 */

public final class ExprCallOrJoin extends Expr {

    /** The left-hand-side expression. */
    public final Expr left;

    /** The right-hand-side expression. */
    public final Expr right;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(left.span()).merge(right.span()));
        return p;
    }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            left.toString(out,-1);
            out.append('.');
            right.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append(". with type=").append(type).append('\n');
            left.toString(out, indent+2);
            right.toString(out, indent+2);
        }
    }

    /**
     * Returns true if the function's parameters have reasonable intersection with the list of arguments.
     * <br> If args.length() > f.params.size(), the extra arguments are ignored by this check
     * <br> <b>Precondition</b>: args.length() >= f.params.size()
     */
    private static boolean applicable(Func f, List<Expr> args) {
        int i=0;
        for(ExprVar d:f.params) {
            Type dt=d.type;
            Type arg=args.get(i).type;
            i++;
            // The reason we don't say (arg.arity()!=d.value.type.arity())
            // is because the arguments may not be fully resolved yet.
            if (!arg.hasCommonArity(dt)) return false;
            if (arg.hasTuple() && dt.hasTuple() && !arg.intersects(dt)) return false;
        }
        return true;
    }

    /**
     * Construct the result of calling "ch" with the given list of arguments
     * <br> <b>Precondition</b>: (ch instanceof Expr) or (ch instanceof Func)
     * <br> <b>Precondition</b>: cset(args[i]) for all i
     * @return EBadCall, or EBadJoin, or a possibly well-typed Expr
     */
    static Expr makeCallOrJoin(Pos pos, Object ch, ConstList<Expr> args, Expr THISorNULL) {
        Expr ans;
        int i=0, n=args.size();
        if (ch instanceof Expr) {
            ans = (Expr)ch;
        } else {
            Func f = (Func)ch;
            i = f.params.size();
            if (i==n+1 && THISorNULL!=null && THISorNULL.type!=null && THISorNULL.type.hasArity(1)) {
                // If we're inside a sig, and there is a unary variable bound to "this",
                // we should consider it as a possible FIRST ARGUMENT of a fun/pred call
                ConstList<Expr> args2=Util.prepend(args, THISorNULL);
                if (applicable(f,args2)) return ExprCall.make(pos, f, args2, 1);
            }
            if (i>n) return new ExprBadCall(pos, f, args);
            if (!applicable(f,args)) return new ExprBadCall(pos, f, args.subList(0,i));
            ans = ExprCall.make(pos, f, args.subList(0,i), 0);
        }
        for(; i<n; i++) {
            Expr x = args.get(i);
            // TODO: you lost the original JOIN's pos
            if (x.type.join(ans.type).size()==0) return new ExprBadJoin(x.span().merge(ans.span()), x, ans);
            ans = ExprBinary.Op.JOIN.make(x.span().merge(ans.span()), x, ans);
        }
        return ans;
    }

    /** Constructs a new EJoin expression. */
    private ExprCallOrJoin (Pos pos, Type type, Expr left, Expr right) throws Err {
        super(pos, type, 0, left.weight+right.weight);
        if (left.mult != 0) throw new ErrorSyntax(left.span(), "Multiplicity expression not allowed here.");
        if (right.mult != 0) throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
        this.left=left;
        this.right=right;
    }

    /**
     * Construct a new EJoin expression.
     *
     * @param pos - the original position in the file
     * @param left - the left-hand-side expression
     * @param right - the right-hand-side expression
     *
     * @throws ErrorSyntax if left.mult!=0 or right.mult!=0
     */
    public static Expr make(Pos pos, Expr left, Expr right) throws Err {
        return new ExprCallOrJoin(pos, null, left, right);
    }

    /** Typechecks an EJoin object (first pass). */
    @Override public Expr check(final TypeCheckContext cxx) throws Err {
        // First, check whether it could be a legal function/predicate call
        Context cx=(Context)cxx;
        TempList<Expr> objects=new TempList<Expr>();
        int n=1;
        Expr ptr=right;
        while (ptr instanceof ExprCallOrJoin) { n++; ptr=((ExprCallOrJoin)ptr).right; }
        if (ptr instanceof ExprVar && !cx.has(((ExprVar)ptr).label)) {
            Set<Object> choices = cx.resolve(ptr.pos, ((ExprVar)ptr).label);
            TempList<Expr> tempargs=new TempList<Expr>();
            for(Expr temp=this; temp instanceof ExprCallOrJoin; temp=((ExprCallOrJoin)temp).right) {
                Expr temp2=((ExprCallOrJoin)temp).left;
                temp2=cset(temp2.check(cx));
                tempargs.add(0,temp2);
            }
            ConstList<Expr> args=tempargs.makeConst();
            // If we're inside a sig, and there is a unary variable bound to "this", we should
            // consider it as a possible FIRST ARGUMENT of a fun/pred call
            Expr THIS = (cx.rootsig!=null) ? cx.get("this",null) : null;
            boolean hasValidBound=false;
            for(Object ch:choices) {
                Expr x = makeCallOrJoin(ptr.pos, ch, args, THIS);
                if (x.type!=null) if (x.type.is_int || x.type.is_bool || x.type.size()>0) hasValidBound=true;
                objects.add(x);
            }
            if (hasValidBound) return ExprChoice.make(ptr.pos, objects.makeConst());
        }
        // Next, check to see if it is the special builtin function "Int[]"
        Expr left = this.left.check(cx);
        Expr right = this.right.check(cx);
        if (left.type.is_int && right instanceof ExprUnary && ((ExprUnary)right).op==ExprUnary.Op.NOOP && ((ExprUnary)right).sub==Sig.SIGINT)
            return left.cast2sigint();
        if (left.type.is_int && right instanceof Sig && ((Sig)right)==Sig.SIGINT)
            return left.cast2sigint();
        // All else, we treat it as a relational join
        Expr test = ExprBinary.Op.JOIN.make(pos, this.left, this.right).check(cx);
        if (objects.size()==0) return test;
        if (test.type!=null && test.type.size()>0) objects.add(test);
        return ExprChoice.make(ptr.pos, objects.makeConst());
    }

    /** Typechecks an EJoin object (second pass). */
    @Override public Expr check(TypeCheckContext cx, Type p, Collection<ErrorWarning> warns) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed this node.
     */
    @Override final Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }
}
