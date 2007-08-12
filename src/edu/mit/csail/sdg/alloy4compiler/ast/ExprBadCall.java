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
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an illegal pred/fun call.
 *
 * <p> <b>Invariant:</b>  this.type==EMPTY && this.errors.size()>0
 */

public final class ExprBadCall extends Expr {

    /** The function. */
    private final Func fun;

    /** The unmodifiable list of arguments. */
    private final ConstList<Expr> args;

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr a:args) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(fun.label).append('[');
            for(int i=0; i<args.size(); i++) { if (i>0) out.append(", "); args.get(i).toString(out,-1); }
            out.append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("ExprBadCall: ").append(fun.isPred?"pred ":"fun ").append(fun.label).append('\n');
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    /** Constructs an ExprBadCall object. */
    private ExprBadCall(Pos pos, boolean ambiguous, Func fun, ConstList<Expr> args, JoinableList<Err> errors) {
        super(pos, ambiguous, EMPTY, 0, 0, errors);
        this.fun=fun;
        this.args=args;
    }

    /** Constructs an ExprBadCall object. */
    public static Expr make(final Pos pos, final Func fun, final ConstList<Expr> args) {
        boolean ambiguous = false;
        JoinableList<Err> errors = emptyListOfErrors;
        for(Expr x:args) {
            ambiguous = ambiguous || x.ambiguous;
            errors = errors.join(x.errors);
        }
        if (errors.isEmpty()) {
            StringBuilder sb=new StringBuilder("This cannot be a correct call to ");
            sb.append(fun.isPred?"pred ":"fun ");
            sb.append(fun.label);
            sb.append(fun.params.size()==0 ? ".\nIt has no parameters,\n" : ".\nThe parameters are\n");
            for(ExprVar v:fun.params) {
                sb.append("  ").append(v.label).append(": ").append(v.type).append('\n');
            }
            sb.append(args.size()==0||fun.params.size()==0 ? "so the arguments cannot be empty.\n" : "so the arguments cannot be\n");
            for(Expr v:args.subList(0, fun.params.size())) {
                sb.append("  ");
                v.toString(sb, -1);
                sb.append(" (type = ").append(v.type).append(")\n");
            }
            errors = errors.append(new ErrorType(pos, sb.toString()));
        }
        return new ExprBadCall(pos, ambiguous, fun, args, errors);
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err { throw errors.get(0); }
}
