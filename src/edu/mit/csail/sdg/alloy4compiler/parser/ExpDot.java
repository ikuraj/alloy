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

import java.util.List;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadJoin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprChoice;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Resolver;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;

public final class ExpDot extends Exp {

    public final Exp left;
    public final Exp right;

    public ExpDot(Pos pos, Exp left, Exp right) {
        super(pos);
        this.left=left;
        this.right=right;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(left.span()).merge(right.span()); span=p; }
        return p;
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

    public Expr check(Context cx, List<ErrorWarning> warnings) throws Err {
        int warningSize = warnings.size();
        // First, check whether it could be a legal function/predicate call
        TempList<Expr> objects=new TempList<Expr>();
        int n=1;
        Exp ptr=right;
        while (ptr instanceof ExpDot) { n++; ptr=((ExpDot)ptr).right; }
        if (ptr instanceof ExpName && !cx.has(((ExpName)ptr).name)) {
            Set<Object> choices = cx.resolve(ptr.pos, ((ExpName)ptr).name);
            TempList<Expr> tempargs=new TempList<Expr>();
            for(Exp temp=this; temp instanceof ExpDot; temp=((ExpDot)temp).right) {
                Expr temp2 = ((ExpDot)temp).left.check(cx, warnings);
                temp2 = Resolver.cset(temp2);
                tempargs.add(0, temp2);
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
        while(warnings.size() > warningSize) warnings.remove(warnings.size()-1);
        Expr left = this.left.check(cx, warnings);
        Expr right = this.right.check(cx, warnings);
        if (left.type!=null && left.type.is_int && right instanceof ExprUnary && ((ExprUnary)right).op==ExprUnary.Op.NOOP && ((ExprUnary)right).sub==Sig.SIGINT)
            return left.cast2sigint();
        if (left.type!=null && left.type.is_int && right instanceof Sig && ((Sig)right)==Sig.SIGINT)
            return left.cast2sigint();
        // All else, we treat it as a relational join
        Expr test = ExprBinary.Op.JOIN.make(pos, left, right);
        if (objects.size()==0) return test;
        if (test.type!=null && test.type.size()>0) objects.add(test);
        return ExprChoice.make(ptr.pos, objects.makeConst());
    }
}
