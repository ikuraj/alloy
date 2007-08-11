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
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;

public final class ExpLet extends Exp {

    public final ExpName left;
    public final Exp right;
    public final Exp sub;

    public ExpLet(Pos pos, ExpName left, Exp right, Exp sub) {
        super(pos);
        this.left=left;
        this.right=right;
        this.sub=sub;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(left.span()).merge(right.span()).merge(sub.span()); span=p; }
        return p;
    }

    public Expr check(Context cx, List<ErrorWarning> warnings) throws Err {
        Expr right = Context.resolveExp(this.right.check(cx, warnings), warnings);
        ExprVar left = ExprVar.make(this.left.pos, this.left.name, right);
        cx.put(this.left.name, left);
        Expr sub = this.sub.check(cx, warnings);
        cx.remove(this.left.name);
        return ExprLet.make(left, sub);
    }
}
