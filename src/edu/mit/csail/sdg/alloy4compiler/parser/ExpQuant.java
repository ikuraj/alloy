/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant.Op;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br>
 * <br>  (all  a,b:t | formula)
 * <br>  (no   a,b:t | formula)
 * <br>  (lone a,b:t | formula)
 * <br>  (one  a,b:t | formula)
 * <br>  (some a,b:t | formula)
 * <br>  (sum  a,b:t | integer expression)
 * <br>  {a,b:t | formula}
 * <br>  {a,b:t }
 */

final class ExpQuant extends Exp {

    /** If nonnull, it is the closing curly bracket's position. */
    public final Pos closingBracket;

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** The unmodifiable list of variables. */
    public final ConstList<Decl> decls;

    /** The body of the quantified expression. */
    public final Exp sub;

    /** Constructs a new quantified expression. */
    public ExpQuant(Pos pos, Pos closingBracket, Op op, List<Decl> decls, Exp sub) throws Err {
        super(pos);
        this.closingBracket=closingBracket;
        this.op=op;
        this.decls=ConstList.make(decls);
        this.sub=sub;
        for(Decl d:decls) {
            if (d.isPrivate!=null) {
                ExpName n=d.names.get(0);
                throw new ErrorSyntax(d.isPrivate.merge(n.pos), "Local variable \""+n.name+"\" is always private already.");
            }
            if (d.disjoint2!=null) {
                ExpName n=d.names.get(d.names.size()-1);
                throw new ErrorSyntax(d.disjoint2.merge(n.pos), "Local variable \""+n.name+"\" cannot be bound to a 'disjoint' expression.");
            }
        }
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(closingBracket).merge(sub.span());
            for(Decl d:decls) p=p.merge(d.span());
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr guard=null;
        final TempList<ExprVar> tempvars=new TempList<ExprVar>();
        for(Decl d: decls) {
            Expr v = d.expr.check(cx, warnings).resolve_as_set(warnings);
            // If the variable declaration is unary, and does not have any multiplicity symbol, we assume it's "one of"
            if (v.mult==0 && v.type.arity()==1) v=ExprUnary.Op.ONEOF.make(null, v);
            // Now, check if the quantification involves meta atoms
            /*
            PrimSig mSig=cx.rootmodule.metaSig(), mField=cx.rootmodule.metaField();
            if ((mSig!=null && v.type.intersects(mSig.type)) || (mField!=null && v.type.intersects(mField.type))) {
               if (op!=ExprQuant.Op.ALL && op!=ExprQuant.Op.SOME) {
                  return new ExprBad(pos, d.names.get(0).name, new ErrorType(pos, "You can only quantify \"all\" and \"some\" over meta atoms."));
               }
            }
            */
            // Otherwise...
            int num = d.names.size();
            List<Expr> disjoints = (num>1 && d.disjoint!=null) ? (new ArrayList<Expr>(num)) : null;
            for(ExpName n: d.names) {
                ExprVar var = ExprVar.make(n.pos, n.name, v);
                cx.put(n.name, var);
                tempvars.add(var);
                if (disjoints!=null) disjoints.add(var);
            }
            if (disjoints!=null) guard=ExprBuiltin.makeDISJOINT(d.disjoint, null, disjoints).and(guard);
        }
        Expr sub = this.sub.check(cx, warnings);
        if (op==Op.SUM) sub=sub.resolve_as_int(warnings); else sub=sub.resolve_as_formula(warnings);
        for(Decl d: decls) for(ExpName n: d.names) cx.remove(n.name);
        if (guard!=null) {
            switch(op) {
              case SUM: sub=guard.ite(sub, ExprConstant.ZERO); break;
              case ALL: sub=guard.implies(sub); break;
              default: sub=guard.and(sub);
            }
        }
        return op.make(pos, closingBracket, tempvars.makeConst(), sub);
    }

    /** {@inheritDoc} */
    @Override public String toString() {
        boolean first=true;
        String ans;
        if (op!=Op.COMPREHENSION) ans="("+op+' '; else ans="{";
        for(Decl d:decls) {
            for(ExpName n:d.names) {
                if (first) { first=false; } else { ans=ans+", "; }
                ans=ans+n.name;
            }
        }
        ans=ans+" | "+sub;
        return (op!=Op.COMPREHENSION) ? (ans+')') : (ans+'}');
    }
}
