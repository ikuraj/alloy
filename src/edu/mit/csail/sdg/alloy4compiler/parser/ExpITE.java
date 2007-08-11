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

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;

public final class ExpITE extends Exp {

    public final Exp formula;
    public final Exp left;
    public final Exp right;

    public ExpITE(Pos pos, Exp formula, Exp left, Exp right) {
        super(pos);
        this.formula=formula;
        this.left=left;
        this.right=right;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(formula.span()).merge(left.span()).merge(right.span()); span=p; }
        return p;
    }

    public Expr check(Context cx, List<ErrorWarning> warnings) throws Err {
        Expr f = formula.check(cx, warnings);
        Expr a = left.check(cx, warnings);
        Expr b = right.check(cx, warnings);
        return ExprITE.make(f, a, b);
    }
}
