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
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an illegal pred/fun call.
 *
 * <p> <b>Invariant:</b>  this.type==EMPTY && this.errors.size()==1
 */

public final class ExprBadCall extends Expr {

    /** The function. */
    private final Func fun;

    /** The unmodifiable list of arguments. */
    private final ConstList<Expr> args;

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

    /** Construct the appropriate error message for this node. */
    private static ErrorType complain(Pos pos, Func fun, ConstList<Expr> args) {
        StringBuilder sb=new StringBuilder("This cannot be a correct call to ").append(fun);
        sb.append(fun.params.size()==0 ? ".\nIt has no parameters,\n" : ".\nThe parameters are\n");
        for(ExprVar v:fun.params) {
            sb.append("  ").append(v.label).append(": ").append(v.type).append('\n');
        }
        sb.append(args.size()==0 || fun.params.size()==0 ? "so the arguments cannot be empty.\n" : "so the arguments cannot be\n");
        for(Expr v:args.subList(0, fun.params.size())) {
            sb.append("  ");
            v.toString(sb,-1);
            sb.append(" (type = ").append(v.type).append(")\n");
        }
        return new ErrorType(pos, sb.toString());
    }

    /** Constructs an ExprBadCall object. */
    public ExprBadCall(Pos pos, Func fun, ConstList<Expr> args) {
        super(pos, EMPTY, 0, 0, new JoinableList<Err>(complain(pos, fun, args)));
        this.fun=fun;
        this.args=args;
    }

    /** Resolves this expression. */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed this node.
     */
    @Override Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }
}
