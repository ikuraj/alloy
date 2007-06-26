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

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;

/**
 * Immutable; represents a field in the AST.
 */

public final class ExprField extends Expr {

    /** The field that this AST node refers to. */
    public final Field field;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() { return pos; }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        field.toString(out,indent);
    }

    /**
     * Constructs an ExprName expression representing a field.
     *
     * @param pos - the original position in the file
     * @param field - the field
     */
    public ExprField(Pos pos, Field field, long weight) throws Err {
        super(pos, field.type, 0, weight);
        this.field=field;
    }

    /** Typechecks an ExprField object (first pass). */
    @Override Expr check(final TypeCheckContext cx) { return this; }

    /** Typechecks an ExprField object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type t) { return this; }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
