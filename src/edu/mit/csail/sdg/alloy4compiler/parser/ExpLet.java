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
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;

/** Immutable; represents an expression of the form (let a=b | x). */

final class ExpLet extends Exp {

    /** The LET variable. */
    public final ExpName left;

    /** The value bound to the LET variable. */
    public final Exp right;

    /** The body of the LET expression. */
    public final Exp sub;

    /**
     * Constructs a LET expression.
     * @param pos - the position of the original '=' token in the text file (or null if unknown)
     */
    public ExpLet(Pos pos, ExpName left, Exp right, Exp sub) {
        super(pos);
        this.left=left;
        this.right=right;
        this.sub=sub;
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(sub.span()).merge(left.span()).merge(right.span()); span=p; }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr right = this.right.check(cx, warnings);
        right = right.resolve(right.type, warnings);
        ExprVar left = ExprVar.make(this.left.pos, this.left.name, right);
        cx.put(this.left.name, left);
        Expr sub = this.sub.check(cx, warnings);
        cx.remove(this.left.name);
        return ExprLet.make(pos, left, sub);
    }
}
