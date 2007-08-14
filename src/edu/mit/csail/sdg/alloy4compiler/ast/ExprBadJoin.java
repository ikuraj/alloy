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
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an illegal relation join.
 *
 * <p> <b>Invariant:</b>  this.type==EMPTY && this.errors.size()>0
 */

public final class ExprBadJoin extends Expr {

    /** The left-hand-side expression. */
    private final Expr left;

    /** The right-hand-side expression. */
    private final Expr right;

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(closingBracket).merge(right.span()).merge(left.span()));
        return p;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            left.toString(out,-1);
            out.append('.');
            right.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("ExprBadJoin:\n");
            left.toString(out, indent+2);
            right.toString(out, indent+2);
        }
    }

    /** Constructs an ExprBadJoin node. */
    private ExprBadJoin(Pos pos, Pos closingBracket, Expr left, Expr right, JoinableList<Err> errors) {
        super(pos, closingBracket, (left.ambiguous || right.ambiguous), EMPTY, 0, 0, errors);
        this.left=left;
        this.right=right;
    }

    /** Constructs an ExprBadJoin node. */
    public static Expr make(Pos pos, Pos closingBracket, Expr left, Expr right) {
        JoinableList<Err> errors = left.errors.join(right.errors);
        if (errors.isEmpty()) {
            StringBuilder sb=new StringBuilder("This cannot be a legal relational join where\nleft hand side is ");
            left.toString(sb,-1);
            sb.append(" (type = ").append(left.type).append(")\nright hand side is ");
            right.toString(sb,-1);
            sb.append(" (type = ").append(right.type).append(")\n");
            errors = errors.append(new ErrorType(pos, sb.toString()));
        }
        return new ExprBadJoin(pos, closingBracket, left, right, errors);
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err { throw errors.get(0); }
}
