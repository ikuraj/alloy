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
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary.Op;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; represents a unary expression of the form "(OP subexpression)" */

final class ExpUnary extends Exp {

    /** The unary operator. */
    public final Op op;

    /** The subexpression. */
    public final Exp sub;

    /** Constructs an ExpUnary object. */
    public ExpUnary(Pos pos, Op op, Exp sub) {
        super(pos);
        this.op=op;
        this.sub=sub;
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(sub.span());
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr newSub = sub.check(cx, warnings);
        return op.make(pos, newSub);
    }
}
