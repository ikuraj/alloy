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
import edu.mit.csail.sdg.alloy4.JoinableList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an illegal node.
 *
 * <p> <b>Invariant:</b>  this.type==EMPTY && this.errors.size()==1
 */

public final class ExprBad extends Expr {

    /** The original source text that caused the error. */
    private final String originalText;

    /** {@inheritDoc} */
    @Override public Pos span() { return pos; }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(originalText);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("ExprBad: ").append(originalText).append('\n');
        }
    }

    /** Constructs an ExprBad object. */
    public ExprBad(Pos pos, String originalText, Err error) {
        super(pos, false, EMPTY, 0, 0, new JoinableList<Err>(error));
        this.originalText = originalText;
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err { throw errors.get(0); }
}
