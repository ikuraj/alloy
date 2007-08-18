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
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.Op;

/** Immutable; represents a constant in the AST. */

final class ExpConstant extends Exp {

    /** The type of constant. */
    public final Op op;

    /** If this node is a number constant, then this field stores the number or index, else this field stores 0. */
    public final int num;

    /** Constructs a constant. */
    public ExpConstant(Pos pos, Op op, int num) {
        super(pos);
        this.op=op;
        this.num=(op==Op.NUMBER ? num : 0);
    }

    /** {@inheritDoc} */
    public Pos span() { return pos; }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) { return op.make(pos, num); }
}
