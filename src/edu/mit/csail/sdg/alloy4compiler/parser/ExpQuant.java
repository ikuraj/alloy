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

import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Resolver;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant.Op;

public final class ExpQuant extends Exp {

    public final Pos closingBracket;
    public final Op op;
    public final ConstList<ExpDecl> decls;
    public final Exp sub;

    public ExpQuant(Pos pos, Pos closingBracket, Op op, List<ExpDecl> decls, Exp sub) {
        super(pos);
        this.closingBracket=closingBracket;
        this.op=op;
        this.decls=ConstList.make(decls);
        this.sub=sub;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(closingBracket).merge(sub.span()); for(ExpDecl d:decls) p=p.merge(d.span()); span=p; }
        return p;
    }

    public Expr check(Context cx, List<ErrorWarning> warnings) throws Err {
        Expr guard=null;
        final TempList<ExprVar> tempvars=new TempList<ExprVar>();
        for(ExpDecl d: decls) {
            Expr v = Resolver.addOne(Context.resolveExpSet(d.expr.check(cx, warnings), warnings));
            List<Expr> disjoints = (d.disjoint!=null && d.names.size()>1) ? (new ArrayList<Expr>(d.names.size())) : null;
            for(ExpName n: d.names) {
                ExprVar var = ExprVar.make(n.pos, n.name, v);
                cx.put(n.name, var);
                tempvars.add(var);
                if (disjoints!=null) disjoints.add(var);
            }
            if (disjoints!=null) guard=ExprBuiltin.makeDISJOINT(d.disjoint, disjoints).and(guard);
        }
        Expr sub = (op==Op.SUM)
            ? Context.resolveExpInt(this.sub.check(cx, warnings), warnings)
            : Context.resolveExpFormula(this.sub.check(cx, warnings), warnings);
        for(ExpDecl d: decls) for(ExpName n: d.names) cx.remove(n.name);
        if (guard!=null) {
            switch(op) {
              case SUM: sub=guard.ite(sub, ExprConstant.ZERO); break;
              case ALL: sub=guard.implies(sub); break;
              default: sub=guard.and(sub);
            }
        }
        return op.make(pos, closingBracket, tempvars.makeConst(), sub);
    }
}
