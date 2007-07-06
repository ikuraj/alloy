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
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * Immutable; represents an illegal pred/fun call.
 *
 * <p> <b>Invariant:</b>  this.type==null && this.fun!=null && this.args!=null
 */

final class ExprBadCall extends Expr {

    /** The function. */
    final Func fun;

    /** The unmodifiable list of arguments. */
    final ConstList<Expr> args;

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
            out.append(fun.label).append('[');
            for(int i=0; i<args.size(); i++) { if (i>0) out.append(", "); out.append(args.get(i)); }
            out.append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("bad call to \"").append(fun).append("\"\n");
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    /**
     * Returns true if the function's parameters have reasonable intersection with the list of arguments.
     * <br> If args.length() > f.params.size(), the extra arguments are ignored by this check
     * <br> <b>Precondition</b>: args.length() >= f.params.size()
     */
    private static boolean applicable(Func f, List<Expr> args) throws Err {
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
    static Expr make(Pos pos, Object ch, ConstList<Expr> args, Expr THISorNULL) throws Err {
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
        if (ans.type==null || (!ans.type.is_int && !ans.type.is_bool && ans.type.size()==0))
            throw new ErrorFatal("Internal typechecker invariant violated.");
        for(; i<n; i++) {
            Expr x = args.get(i);
            // TODO: you lost the original JOIN's pos
            try {
              if (x.type.join(ans.type).size()==0) return new ExprBadJoin(x.span().merge(ans.span()), x, ans);
              // The line above is not STRICTLY necessary, since the "catch Err" below will occur.
              // But this allows us to avoid paying the runtime penalty of throwing an exception and then catching it
              ans = ExprBinary.Op.JOIN.make(x.span().merge(ans.span()), x, ans);
            } catch(Err ex) {
              return new ExprBadJoin(x.span().merge(ans.span()), x, ans);
            }
        }
        return ans;
    }

    /** Constructs an ExprBadCall object. */
    private ExprBadCall(Pos pos, Func fun, ConstList<Expr> args) throws Err {
        super(pos, null, 0, 0); // weight can be set to anything (such as 0), since a EBadCall will never be in the ultimate Expr
        this.fun=fun;
        this.args=args;
    }

    /** Typechecks an ExprBadCall object (first pass). */
    @Override public Expr check(final TypeCheckContext cx) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /** Typechecks an ExprBadCall object (second pass). */
    @Override public Expr check(final TypeCheckContext cx, Type t) throws Err {
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
