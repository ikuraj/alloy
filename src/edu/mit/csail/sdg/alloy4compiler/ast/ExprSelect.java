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
import edu.mit.csail.sdg.alloy4.ErrorWarning;

/**
 * Immutable; this selects the i-th atom from a sig (or returns an empty set if the i-th atom does not exist)
 */

public final class ExprSelect extends Expr {

    /** The expression. */
    public final Sig expr;

    /** The index. */
    public final int index;

    /** {@inheritDoc} */
    @Override public Pos span() { return pos; }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            expr.toString(out,-1);
            out.append('[').append(index).append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            expr.toString(out,-1);
            out.append('[').append(index).append("]\n");
        }
    }

    /** Constructs an ExprSelect node. */
    ExprSelect(Pos pos, Sig expr, int index, long weight) {
        super(pos, null, false, expr.type, 0, weight, null);
        this.expr=expr;
        this.index=index;
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type type, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
