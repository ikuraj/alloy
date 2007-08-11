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
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an illegal pred/fun call.
 *
 * <p> <b>Invariant:</b>  this.type==EMPTY && this.errors.size()==1
 */

public final class ExprBad extends Expr {

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() { return pos; }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) { }

    /** Constructs an ExprBad object. */
    public ExprBad(Pos pos, Err error) {
        super(pos, EMPTY, 0, 0, new JoinableList<Err>(error));
    }

    /** Typechecks an ExprBadCall object (second pass). */
    @Override public Expr check(Type t, Collection<ErrorWarning> warns) { return this; }

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed this node.
     */
    @Override Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }
}
