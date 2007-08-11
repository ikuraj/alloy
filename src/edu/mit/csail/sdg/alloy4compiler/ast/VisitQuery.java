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
 * This abstract class implements a Query visitor that walks over an Expr and its subnodes.
 * <br> As soon as one of the node returns a nonnull value, the nonnull value will be propagated to be the output of this visitor.
 *
 * <p> This default implementation will return null on all the leaf Expr nodes and thus the final answer will be null.
 * <br> To implement a particular query, you need to extend this class.
 */

public abstract class VisitQuery extends VisitReturn {

    /** Constructs a VisitQuery object. */
    public VisitQuery() { }

    /** Visits an ExprQuant node (all a:X1, b:X2... | F) by calling accept() on a:X1, b:X2... and then on F. */
    @Override public Object visit(ExprQuant x) throws Err {
        for(Expr y:x.vars) { Object ans=y.accept(this); if (ans!=null) return ans; }
        return x.sub.accept(this);
    }

    /** Visits an ExprBuiltin node F[X1,X2,X3..] by calling accept() on X1, X2, X3... */
    @Override public Object visit(ExprBuiltin x) throws Err {
        for(Expr y:x.args) { Object ans=y.accept(this); if (ans!=null) return ans; }
        return null;
    }

    /** Visits an ExprCall node F[X1,X2,X3..] by calling accept() on X1, X2, X3... */
    @Override public Object visit(ExprCall x) throws Err {
        for(Expr y:x.args) { Object ans=y.accept(this); if (ans!=null) return ans; }
        return null;
    }

    /** Visits an ExprBinary node (A OP B) by calling accept() on A then B. */
    @Override public Object visit(ExprBinary x) throws Err {
        Object ans=x.left.accept(this);
        if (ans==null) ans=x.right.accept(this);
        return ans;
    }

    /** Visits an ExprLet node (let A=B | F) by calling accept() on A, B, then F. */
    @Override public Object visit(ExprLet x) throws Err {
        Object ans=x.var.accept(this);
        if (ans==null) ans=x.sub.accept(this);
        return ans;
    }

    /** Visits an ExprITE node (C => X else Y) by calling accept() on C, X, then Y. */
    @Override public Object visit(ExprITE x) throws Err {
        Object ans=x.cond.accept(this);
        if (ans==null) ans=x.left.accept(this);
        if (ans==null) ans=x.right.accept(this);
        return ans;
    }

    /** Visits an ExprUnary node (OP X) by calling accept() on X. */
    @Override public Object visit(ExprUnary x) throws Err {
        return x.sub.accept(this);
    }

    /** Visits an ExprVar node by calling accept on its bounding expression (and return null if the ExprVar does not have a bounding expression) */
    @Override public Object visit(ExprVar x) throws Err {
        if (x.expr==null) return null;
        return x.expr.accept(this);
    }

    /** Visits an ExprConstant node (this default implementation simply returns null) */
    @Override public Object visit(ExprConstant x) throws Err {
        return null;
    }

    /** Visits a Sig node (this default implementation simply returns null) */
    @Override public Object visit(Sig x) throws Err {
        return null;
    }

    /** Visits a Field node (this default implementation simply returns null) */
    @Override public Object visit(Field x) throws Err {
        return null;
    }
}
