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

import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.cset;

/**
 * Immutable; represents a call.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (all x:args | x.mult==0)
 */

public final class ExprCall extends Expr {

    /** The actual function being called; never null. */
    public final Func fun;

    /** The list of arguments to the call. */
    public final ConstList<Expr> args;

    /** Caches the span() result. */
    private Pos span=null;

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr a:args) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(fun.label).append('[');
            for(int i=0; i<args.size(); i++) { if (i>0) out.append(", "); args.get(i).toString(out,-1); }
            out.append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("call ").append(fun).append(" with type=").append(type).append('\n');
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    //============================================================================================================//

    /** This visitor assumes the input expression is already fully typechecked, and derive a tight bound on the return type. */
    private static class DeduceType extends VisitReturn {
        private final Env<ExprVar,Type> env=new Env<ExprVar,Type>();
        private DeduceType() { }
        @Override public Object visit(ExprITE x) throws Err {
            Type t = (Type) (x.left.accept(this));
            if (t.size()==0) return t; // This means x.left is either a formula, or an integer expression
            Type t2 = (Type) (x.right.accept(this));
            return t.unionWithCommonArity(t2);
        }
        @Override public Object visit(ExprBinary x) throws Err {
            switch(x.op) {
              case GT: case GTE: case LT: case LTE: case IFF: case EQUALS: case IN: case OR: case AND: return Type.FORMULA;
            }
            Type a=(Type)(x.left.accept(this));
            Type b=(Type)(x.right.accept(this));
            switch(x.op) {
              case JOIN: return a.join(b);
              case DOMAIN: return b.domainRestrict(a);
              case RANGE: return a.rangeRestrict(b);
              case INTERSECT: return a.intersect(b);
              case PLUSPLUS: return a.unionWithCommonArity(b);
              case PLUS: return (a.is_int && b.is_int) ? Type.makeInt(a.unionWithCommonArity(b)) : a.unionWithCommonArity(b);
              case MINUS: return (a.is_int && b.is_int) ? Type.makeInt(a.pickCommonArity(b)) : a.pickCommonArity(b);
              default: return a.product(b);
            }
        }
        @Override public Object visit(ExprUnary x) throws Err {
            Type t=(Type)(x.sub.accept(this));
            switch(x.op) {
              case NOOP: case LONEOF: case ONEOF: case SETOF: case SOMEOF: return t;
              case CARDINALITY: case CAST2INT: return Type.INT;
              case CAST2SIGINT: return Sig.SIGINT.type;
              case TRANSPOSE: return t.transpose();
              case CLOSURE: return t.closure();
              case RCLOSURE: return Type.make2(Sig.UNIV);
              default: return Type.FORMULA;
            }
        }
        @Override public Object visit(ExprConstant x) {
            switch(x.op) {
              case IDEN: return Type.make2(Sig.UNIV);
              case NUMBER: return Type.INT;
              default: return Type.FORMULA;
            }
        }
        @Override public Object visit(ExprQuant x) throws Err {
            if (x.op == ExprQuant.Op.SUM) return Type.INT;
            if (x.op != ExprQuant.Op.COMPREHENSION) return Type.FORMULA;
            Type ans=null;
            for(ExprVar v: x.vars) {
                Type t=(Type)(v.expr.accept(this));
                env.put(v, t);
                if (ans==null) ans=t; else ans=ans.product(t);
            }
            for(ExprVar v: x.vars) env.remove(v);
            return (ans==null) ? EMPTY : ans;
        }
        @Override public Object visit(ExprLet x) throws Err {
            env.put(x.var, (Type)(x.var.expr.accept(this)));
            Object ans=x.sub.accept(this);
            env.remove(x.var);
            return ans;
        }
        @Override public Object visit(ExprCall x)    { return x.fun.returnDecl.type; }
        @Override public Object visit(ExprVar x)     { Type t=env.get(x); return (t!=null && t!=EMPTY) ? t : x.type; }
        @Override public Object visit(Sig x)         { return x.type; }
        @Override public Object visit(Field x)       { return x.type; }
        @Override public Object visit(ExprBuiltin x) { return Type.FORMULA; }
    }

    //============================================================================================================//

    /** Constructs an ExprCall node with the given function "pred/fun" and the list of arguments "args". */
    private ExprCall(Pos pos, boolean ambiguous, Type type, Func fun, ConstList<Expr> args, long weight, JoinableList<Err> errs) {
        super(pos, ambiguous, type, 0, weight, errs);
        this.fun = fun;
        this.args = args;
    }

    //============================================================================================================//

    /** Constructs an ExprCall node with the given predicate/function "fun" and the list of arguments "args". */
    public static Expr make(Pos pos, Func fun, List<Expr> args, long extraWeight) {
        boolean ambiguous = false;
        JoinableList<Err> errs = emptyListOfErrors;
        if (extraWeight<0) extraWeight=0;
        if (args==null) args=ConstList.make();
        TempList<Expr> newargs=new TempList<Expr>(args.size());
        if (args.size() != fun.params.size()) {
            errs = errs.append(
              new ErrorSyntax(pos, ""+fun+" has "+fun.params.size()+" parameters but is called with "+args.size()+" arguments."));
        }
        for(int i=0; i<args.size(); i++) {
            final int a = (i<fun.params.size()) ? fun.params.get(i).type.arity() : 0;
            final Expr x = cset(args.get(i));
            ambiguous = ambiguous || x.ambiguous;
            errs = errs.join(x.errors);
            extraWeight = extraWeight + x.weight;
            if (x.mult!=0) errs = errs.append(new ErrorSyntax(x.span(), "Multiplicity expression not allowed here."));
            if (a>0 && x.errors.isEmpty() && !x.type.hasArity(a))
              errs=errs.append(new ErrorType(x.span(), "This should have arity "+a+" but instead its possible type(s) are "+x.type));
            newargs.add(x);
        }
        Type t=Type.FORMULA;
        if (!fun.isPred && errs.size()==0) {
            final Type tt = fun.returnDecl.type;
            try {
                // This provides a limited form of polymorphic function,
                // by using actual arguments at each call site to derive a tighter bound on the return value.
                DeduceType d = new DeduceType();
                for(int i=0; i<args.size(); i++) {
                    ExprVar param=fun.params.get(i);
                    d.env.put(param, newargs.get(i).type.extract(param.type.arity()));
                }
                t = (Type) (fun.returnDecl.accept(d));
                if (t==null || t.is_int || t.is_bool || t.arity()!=tt.arity()) t=tt; // Just in case an error occurred...
            } catch(Throwable ex) {
                t=tt; // Just in case an error occurred...
            }
        }
        return new ExprCall(pos, ambiguous, t, fun, newargs.makeConst(), extraWeight, errs);
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) {
        if (errors.size()>0) return this;
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed=false;
        long w=0;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i);
            Expr y=cset(x.resolve(fun.params.get(i).type, warns)); // Use the function's param type to narrow down the choices
            if (x!=y) changed=true;
            args.add(y);
            w = w + y.weight;
        }
        return changed ? make(pos, fun, args.makeConst(), weight-w) : this;
    }

    //============================================================================================================//

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (!errors.isEmpty()) throw errors.get(0);
        return visitor.visit(this);
    }
}
