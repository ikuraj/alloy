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

import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant.Op;
import edu.mit.csail.sdg.alloy4compiler.parser.Context;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br>
 * <br>  (all    &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br>  (no     &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br>  (lone   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (one    &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (some   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (sum    &nbsp;       a,b:t, c,d:v &nbsp; | expression)
 * <br>  {a,b:t, &nbsp; c,d:v &nbsp; | &nbsp; formula}
 * <br>  {a,b:t, &nbsp; c,d:v}
 * <br>
 *
 * <br> <b>Invariant:</b> sub.mult==0
 * <br> <b>Invariant:</b> decls.size()>0
 */

public final class ExprQuantt extends Expr {

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    final Op op;

    /** If nonnull, it is the closing curly bracket's position. */
    final Pos closingBracket;

    /** The unmodifiable list of untypechecked variable declarations; never empty. */
    final ConstList<EDecl> decls;

    /** The body of the quantified expression. */
    final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(EDecl d:decls) p=p.merge(d.span());
            p=p.merge(sub.span());
            if (closingBracket!=null) p=p.merge(closingBracket);
            span=p;
        }
        return p;
    }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            if (op==Op.COMPREHENSION) out.append('{'); else out.append('(').append(op).append(' ');
            boolean first=true;
            for(EDecl d:decls) for(String n:d.names) { if (!first) out.append(','); first=false; out.append(n); }
            if (op!=Op.COMPREHENSION || !(sub instanceof ExprConstant) || ((ExprConstant)sub).op!=ExprConstant.Op.TRUE)
               {out.append(" | "); sub.toString(out,-1);}
            if (op==Op.COMPREHENSION) out.append('}'); else out.append(')');
        } else {
            for(int i=0; i<indent; i++) out.append(' ');
            out.append("Quantification(").append(op).append(") with type=").append(type).append('\n');
            for(EDecl v:decls) {
                for(int i=0; i<indent+2; i++) out.append(' ');
                if (v.disjoint) out.append("disjoint ");
                for(int n=0; n<v.names.size(); n++) { if (n>0) out.append(", "); out.append(v.names.get(n)); }
                out.append(" in\n");
                v.value.toString(out,indent+4);
            }
            for(int i=0; i<indent+2; i++) out.append(' ');
            out.append("in\n");
            sub.toString(out, indent+4);
        }
    }

    /** Constructs a new quantified expression. */
    private ExprQuantt(Pos pos, Pos close, Op op, Type type, List<EDecl> decls, Expr sub, long weight) throws Err {
        super(pos, type, 0, weight);
        this.closingBracket=close;
        this.op=op;
        this.decls=ConstList.make(decls);
        this.sub=sub;
        if (this.decls.size()==0) throw new ErrorAPI("The list of variable declarations cannot be empty");
        String dup=EDecl.findDuplicateName(this.decls);
        if (dup!=null) throw new ErrorSyntax(span(), "The name \""+dup
            +"\" appears more than once in this declaration; while it is semantically legal, it is very likely a human error.");
        if (sub.mult!=0) throw new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here");
    }

    /** Constructs a new quantified expression. */
    public static final Expr make(Pos pos, Pos closingBracket, Op op, List<EDecl> decls, Expr sub) throws Err {
        long weight=sub.weight;
        if (decls!=null) for(EDecl d:decls) weight += d.value.weight;
        return new ExprQuantt(pos, closingBracket, op, null, decls, sub, weight);
    }

    /** Typechecks an EQuant object (first pass). */
    @Override public Expr check(final TypeCheckContext cxx) throws Err {
        Context cx=(Context)cxx;
        TempList<ExprVar> vars=new TempList<ExprVar>();
        Expr guard=null;
        for(EDecl d:decls) {
            final Expr v=TypeCheckContext.addOne(cx.resolve_set(d.value));
            final Type t=v.type;
            if (t.hasNoTuple()) A4Reporter.getReporter().warning(new ErrorWarning(v.span(), "This expression is always empty."));
            List<Expr> disjoints = (d.disjoint && d.names.size()>1) ? (new ArrayList<Expr>(d.names.size())) : null;
            for(String n:d.names) {
                ExprVar var = new ExprVar(d.pos, n, v);
                cx.put(n,var);
                vars.add(var);
                if (disjoints!=null) disjoints.add(var);
            }
            if (disjoints!=null) guard=ExprBuiltin.makeDISJOINT(d.disjointPosition, disjoints).and(guard);
        }
        Expr sub = (op==Op.SUM) ? cx.resolve_int(this.sub) : cx.resolve_formula(this.sub);
        for(EDecl d:decls) for(String n:d.names) cx.remove(n);
        if (guard!=null) {
          switch(op) {
            case SUM: sub=guard.ite(sub, ExprConstant.ZERO); break;
            case ALL: sub=guard.implies(sub); break;
            default: sub=guard.and(sub);
          }
        }
        return op.make(pos, closingBracket, vars.makeConst(), sub);
    }

    //=============================================================================================================//

    /** Typechecks an EQuant object (second pass). */
    @Override public Expr check(final TypeCheckContext cx, Type p) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    //=============================================================================================================//

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed this node.
     */
    @Override final Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }
}
