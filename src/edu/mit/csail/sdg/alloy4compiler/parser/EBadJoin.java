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

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCustom;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext;

/**
 * Immutable; represents an illegal relation join.
 *
 * <p> <b>Invariant:</b>  this.type==null
 * <p> <b>Invariant:</b>  left.type.is_int || left.type.is_bool || left.type.size()>0
 * <p> <b>Invariant:</b>  right.type.is_int || right.type.is_bool || right.type.size()>0
 */

final class EBadJoin extends ExprCustom {

    /** The left-hand-side expression. */
    final Expr left;

    /** The right-hand-side expression. */
    final Expr right;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(left.span()).merge(right.span()));
        return p;
    }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            left.toString(out,-1);
            out.append('.');
            right.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("EBadJoin with type=").append(type).append('\n');
            left.toString(out, indent+2);
            right.toString(out, indent+2);
        }
    }

    /**
     * Constructs an EBadJoin node.
     * <p> <b>Precondition:</b>  left.type.is_int || left.type.is_bool || left.type.size()>0
     * <p> <b>Precondition:</b>  right.type.is_int || right.type.is_bool || right.type.size()>0
     */
    EBadJoin(Pos pos, Expr left, Expr right) throws Err {
        super(pos, null, 0, 0); // weight can be set to anything (such as 0), since a EBadJoin will never be in the ultimate Expr
        this.left=left;
        this.right=right;
    }

    /** Typechecks an EBadJoin object (first pass). */
    public Expr check(final TypeCheckContext cx) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /** Typechecks an EBadJoin object (second pass). */
    public Expr check(final TypeCheckContext cx, Type t) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }
}
