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

package edu.mit.csail.sdg.alloy4compiler.translator;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;

/**
 * This class converts AST into AST while attempting to maximize the number
 * of toplevel conjunctions. This helps improve the precision of the unsat core result.
 */

public final class ConvToConjunction extends VisitReturn {

    /** {@inheritDoc} */
    @Override public Object visit(ExprBinary x) throws Err {
        if (x.op == ExprBinary.Op.AND) {
            Expr a = (Expr) visitThis(x.left);
            Expr b = (Expr) visitThis(x.right);
            return a.and(b);
        }
        return x;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprQuant x) throws Err {
        if (x.op == ExprQuant.Op.ALL) {
            Expr s = x.sub;
            while(s instanceof ExprUnary && ((ExprUnary)s).op==ExprUnary.Op.NOOP) s=((ExprUnary)s).sub;
            if (s instanceof ExprBinary && ((ExprBinary)s).op==ExprBinary.Op.AND) {
                Expr a = (Expr) visitThis(x.op.make(x.pos, x.closingBracket, x.vars, ((ExprBinary)s).left));
                Expr b = (Expr) visitThis(x.op.make(x.pos, x.closingBracket, x.vars, ((ExprBinary)s).right));
                return a.and(b);
            }
        }
        return x;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprUnary x) throws Err {
        if (x.op == ExprUnary.Op.NOOP) {
            return (Expr) visitThis(x.sub);
        }
        if (x.op == ExprUnary.Op.NOT && x.sub instanceof ExprBinary) {
            ExprBinary bin = (ExprBinary)(x.sub);
            if (bin.op == ExprBinary.Op.OR) {
                Expr a = (Expr) visitThis(bin.left.not());
                Expr b = (Expr) visitThis(bin.right.not());
                return a.and(b);
            }
        }
        return x;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprBuiltin x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(ExprCall x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(ExprConstant x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(ExprITE x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(ExprLet x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(ExprVar x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(Sig x) { return x; }

    /** {@inheritDoc} */
    @Override public Object visit(Field x) { return x; }
}
