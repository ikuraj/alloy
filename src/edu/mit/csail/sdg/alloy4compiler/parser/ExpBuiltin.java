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

import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;

/** Immutable; represents the builtin disjoint[] predicate. */

final class ExpBuiltin extends Exp {

    /** The unmodifiable list of arguments. */
    public ConstList<Exp> args;

    /** Constructs an ExpBuiltin node. */
    public ExpBuiltin(Pos pos, Collection<Exp> args) { super(pos); this.args = ConstList.make(args); }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos; for(Exp a:args) p=p.merge(a.span()); span=p; }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) throws Err {
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i).check(cx, warnings);
            args.add(x);
        }
        return ExprBuiltin.makeDISJOINT(pos, args.makeConst());
    }
}
