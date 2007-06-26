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
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;

/**
 * This abstract class defines what a Return Visitor's interface should be.
 */

public abstract class VisitReturn {

    /** Constructs a VisitReturn object. */
    public VisitReturn() { }

    /** This is the start method that begins a traversal over the given expression. */
    public final Object visit(Expr x) throws Err { return x.accept(this); }

    /** Visits an ExprAnd node. */
    public abstract Object visit(ExprAnd x) throws Err;

    /** Visits an ExprBinary node. */
    public abstract Object visit(ExprBinary x) throws Err;

    /** Visits an ExprBuiltin node. */
    public abstract Object visit(ExprBuiltin x) throws Err;

    /** Visits an ExprCall node. */
    public abstract Object visit(ExprCall x) throws Err;

    /** Visits an ExprITE node. */
    public abstract Object visit(ExprITE x) throws Err;

    /** Visits an ExprLet node. */
    public abstract Object visit(ExprLet x) throws Err;

    /** Visits an ExprConstant node. */
    public abstract Object visit(ExprConstant x) throws Err;

    /** Visits an ExprQuant node. */
    public abstract Object visit(ExprQuant x) throws Err;

    /** Visits an ExprUnary node. */
    public abstract Object visit(ExprUnary x) throws Err;

    /** Visits an ExprVar node. */
    public abstract Object visit(ExprVar x) throws Err;

    /** Visits an ExprSig node. */
    public abstract Object visit(ExprSig x) throws Err;

    /** Visits an ExprField node. */
    public abstract Object visit(ExprField x) throws Err;

    /** Visits a Sig node. */
    public abstract Object visit(Sig x) throws Err;

    /** Visits a Field node. */
    public abstract Object visit(Field x) throws Err;
}
