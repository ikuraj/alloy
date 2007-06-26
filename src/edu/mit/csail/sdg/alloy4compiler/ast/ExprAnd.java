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

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cform;

/**
 * Immutable; represents a conjunction of 2 or more formula(s).
 */

public final class ExprAnd extends Expr {

    /** The immutable list of conjuncts; always contains 2 or more entries. */
    public final ConstList<Expr> list;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr x:list) p=p.merge(x.span());
            span=p;
        }
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            for(int i=0; i<list.size(); i++) { if (i>0) out.append(" && "); out.append(list.get(i)); }
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("conjunction of "+list.size()+" formulas with type=").append(type).append('\n');
            for(Expr x:list) { x.toString(out, indent+2); }
        }
    }

    /** Constructs an ExprAnd node. */
    private ExprAnd(Pos pos, Type type, ConstList<Expr> list, long weight) throws Err {
        super(pos, type, 0, weight);
        this.list = list;
    }

    /**
     * Returns the formula (x and y)
     *
     * @param pos - the original position in the source file (can be null if unknown)
     * @param x - if x is typed, it must be a formula
     * @param y - if y is typed, it must be a formula
     */
    public static Expr make(Pos pos, Expr x, Expr y) throws Err {
        int n=0;
        if (x instanceof ExprAnd) n=n+((ExprAnd)x).list.size(); else n++;
        if (y instanceof ExprAnd) n=n+((ExprAnd)y).list.size(); else n++;
        TempList<Expr> list=new TempList<Expr>(n);
        if (x instanceof ExprAnd) list.addAll(((ExprAnd)x).list); else list.add(x);
        if (y instanceof ExprAnd) list.addAll(((ExprAnd)y).list); else list.add(y);
        ConstList<Expr> clist=list.makeConst();
        Type type=Type.FORMULA;
        long weight=0;
        for(Expr e:clist) {
            weight += e.weight;
            if (e.mult!=0) throw new ErrorSyntax(e.span(), "Multiplicity expression not allowed here.");
            if (e.type==null) type=null; else cform(e);
        }
        return new ExprAnd(pos, type, clist, weight);
    }

    /** Typechecks an ExprAnd object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        TempList<Expr> newlist = new TempList<Expr>(list.size());
        boolean changed=false;
        long weight=0;
        for(Expr x:list) {
            Expr y=cx.check(x);
            cform(y);
            if (x!=y) changed=true;
            newlist.add(y);
            weight += y.weight;
        }
        if (!changed) return this; else return new ExprAnd(pos, Type.FORMULA, newlist.makeConst(), weight);
    }

    /** Typechecks an ExprAnd object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type t) throws Err {
        TempList<Expr> newlist = new TempList<Expr>(list.size());
        boolean changed=false;
        long weight=0;
        for(Expr x:list) {
            Expr y=cx.check(x, Type.FORMULA);
            cform(y);
            if (x!=y) changed=true;
            newlist.add(y);
            weight += y.weight;
        }
        if (!changed) return this; else return new ExprAnd(pos, Type.FORMULA, newlist.makeConst(), weight);
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
