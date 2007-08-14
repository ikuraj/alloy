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
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;

/** Immutable; this is the super class of all untypechecked AST nodes. */

abstract class Exp {

    /** The filename, line, and column position in the original Alloy model file (cannot be null). */
    public final Pos pos;

    /** Constructs an Exp node. */
    public Exp(Pos pos) {
        this.pos=(pos==null ? Pos.UNKNOWN : pos);
    }

    /** Returns a Pos object representing the entire span of this Exp and all its subexpressions. */
    public abstract Pos span();

    /**
     * Consults the current lexical context, and converts this Exp node into an equivalent Expr node
     * (along the way, if we detect any type warnings, add them to the listOfWarnings)
     */
    public abstract Expr check(Context cx, List<ErrorWarning> listOfWarnings);

    /** Convenience method that constructs the expression "not this" */
    public final Exp not() { return new ExpUnary(null, ExprUnary.Op.NOT, this); }
}
