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

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.Pos;

/**
 * Immutable; represents a user-defined AST node.
 *
 * <p> This class is abstract, so you have to extend it to create custom defined nodes.
 *
 * <p> Your typechecking methods check(Expr) and check(Expr,Type) must replace all
 * custom nodes with suitable regular nodes.
 */

public abstract class ExprCustom extends Expr {

    /** Constructor for an ExprCustom object. */
    public ExprCustom(Pos pos, Type type, int mult, long weight) throws Err { super(pos,type,mult,weight); }

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed all ExprCustom nodes.
     */
    @Override final Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }

    /** Typechecks an ExprCustom object (first pass). */
    @Override public abstract Expr check(final TypeCheckContext cx) throws Err;

    /** Typechecks an ExprCustom object (second pass). */
    @Override public abstract Expr check(final TypeCheckContext cx, Type t) throws Err;
}
