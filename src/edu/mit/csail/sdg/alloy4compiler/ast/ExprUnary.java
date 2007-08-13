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
import edu.mit.csail.sdg.alloy4.DirectedGraph;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type.ProductType;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.cform;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.cset;

/**
 * Immutable; represents a unary expression of the form "(OP subexpression)"
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => sub.mult==0
 */

public final class ExprUnary extends Expr {

    /** The unary operator. */
    public final Op op;

    /** The subexpression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) { if (op==Op.NOOP && pos!=Pos.UNKNOWN) span=(p=pos); else span=(p=pos.merge(sub.span())); }
        return p;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            switch(op) {
              case SOMEOF: out.append("some "); break;
              case LONEOF: out.append("lone "); break;
              case ONEOF: out.append("one "); break;
              case SETOF: out.append("set "); break;
              case CAST2INT: out.append("int["); sub.toString(out,-1); out.append(']'); return;
              case CAST2SIGINT: out.append("Int["); sub.toString(out,-1); out.append(']'); return;
              case NOOP: break;
              default: out.append(op).append(' ');
            }
            sub.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append(op).append(" with type=").append(type).append('\n');
            sub.toString(out, indent+2);
        }
    }

    /** Constructs an unary expression. */
    private ExprUnary(Pos pos, Op op, Expr sub, Type type, long weight, JoinableList<Err> errors) {
        super(pos, sub.ambiguous, type, (op==Op.SOMEOF||op==Op.LONEOF||op==Op.ONEOF||op==Op.SETOF)?1:0, weight, errors);
        this.op = op;
        this.sub = sub;
    }

    /** This class contains all possible unary operators. */
    public enum Op {
        /** :some x (where x is a unary set)                             */  SOMEOF("some of"),
        /** :lone x (where x is a unary set)                             */  LONEOF("lone of"),
        /** :one  x (where x is a unary set)                             */  ONEOF("one of"),
        /** :set  x (where x is a set or relation)                       */  SETOF("set of"),
        /** not   f (where f is a formula)                               */  NOT("!"),
        /** no    x (where x is a set or relation)                       */  NO("no"),
        /** some  x (where x is a set or relation)                       */  SOME("some"),
        /** lone  x (where x is a set or relation)                       */  LONE("lone"),
        /** one   x (where x is a set or relation)                       */  ONE("one"),
        /** transpose                                                    */  TRANSPOSE("~"),
        /** reflexive closure                                            */  RCLOSURE("*"),
        /** closure                                                      */  CLOSURE("^"),
        /** cardinality of x (truncated to the current integer bitwidth) */  CARDINALITY("#"),
        /** IntAtom-to-integer                                           */  CAST2INT("Int->int"),
        /** integer-to-IntAtom                                           */  CAST2SIGINT("int->Int"),
        /** No-Op                                                        */  NOOP("NOOP");

        /** The constructor */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator */
        private final String label;

        /**
         * Construct an ExprUnary node.
         * @param pos - the original position of the "unary operator" in the file (can be null if unknown)
         * @param sub - the subexpression
         *
         * <p>  Alloy4 disallows multiplicity like this:   "variable : one (X lone-> Y)",
         * <br> that is, a some/lone/one in front of an arrow multiplicity declaration.
         * <br> Alloy4 does allow "variable : set (X lone-> Y)", where we ignore the word "set".
         * <br> (This desugaring is done by the ExprUnary.Op.make() method, so ExprUnary's constructor never sees it)
         */
        public final Expr make(Pos pos, Expr sub) { return make(pos, sub, sub.weight, null); }

        /**
         * Construct an ExprUnary node.
         * @param pos - the original position of the "unary operator" in the file (can be null if unknown)
         * @param sub - the subexpression
         * @param weight - the weight
         * @param extraError - if nonnull, it will be appended as an extra error
         *
         * <p>  Alloy4 disallows multiplicity like this:   "variable : one (X lone-> Y)",
         * <br> that is, a some/lone/one in front of an arrow multiplicity declaration.
         * <br> Alloy4 does allow "variable : set (X lone-> Y)", where we ignore the word "set".
         * <br> (This desugaring is done by the ExprUnary.Op.make() method, so ExprUnary's constructor never sees it)
         */
        public final Expr make(Pos pos, Expr sub, long weight, Err extraError) {
            Err e=null;
            JoinableList<Err> errors = sub.errors.appendIfNotNull(extraError);
            if (sub.mult!=0) {
               if (this==SETOF) return sub;
               errors=errors.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
            }
            switch(this) {
               case NOOP: break;
               case NOT: sub=cform(sub); break;
               case CAST2SIGINT: sub=cint(sub); break;
               case CAST2INT: if (sub.type==Type.INT) return sub; else {sub=cset(sub); break;} // Shortcut if it can only be integer
               default: sub=cset(sub);
            }
            Type type=sub.type;
            if (sub.errors.size()==0) switch(this) {
              case SOMEOF: case LONEOF: case ONEOF: case SETOF:
                if (this==SETOF) type=Type.removesBoolAndInt(sub.type); else type=sub.type.extract(1);
                if (type==EMPTY) e=new ErrorType(sub.span(), "After the some/lone/one multiplicity symbol, " +
                   "this expression must be a unary set.\nInstead, its possible type(s) are:\n" + sub.type);
                break;
              case NOT: case NO: case SOME: case LONE: case ONE:
                type=Type.FORMULA;
                break;
              case TRANSPOSE:
                type=sub.type.transpose();
                if (type==EMPTY) e=new ErrorType(sub.span(), "~ can be used only with a binary relation.\n" +
                   "Instead, its possible type(s) are:\n"+sub.type);
                break;
              case RCLOSURE: case CLOSURE:
                type=sub.type.closure();
                if (type==EMPTY) e=new ErrorType(sub.span(), label+" can be used only with a binary relation.\n" +
                   "Instead, its possible type(s) are:\n"+sub.type);
                if (this==RCLOSURE) type=Type.make2(UNIV);
                break;
              case CARDINALITY:
                type=Type.INT;
                break;
              case CAST2INT:
                if (!sub.type.hasArity(1)) e=new ErrorType(sub.span(), "int[] can be used only with a unary set.\n" +
                   "Instead, its possible type(s) are:\n"+sub.type);
                type=Type.INT;
                break;
              case CAST2SIGINT:
                type=SIGINT.type;
                break;
            }
            return new ExprUnary(pos, this, sub, type, ((weight<sub.weight) ? sub.weight : weight), errors.appendIfNotNull(e));
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) {
        if (errors.size()>0) return this;
        ErrorWarning w1=null, w2=null;
        Type s=p;
        switch(op) {
          case TRANSPOSE: case RCLOSURE: case CLOSURE:
            if (op!=Op.TRANSPOSE && type.join(type).hasNoTuple())
               w1=new ErrorWarning(pos, this+" is redundant since its domain and range are disjoint: "+sub.type.extract(2));
            s = (op!=Op.TRANSPOSE) ? resolveClosure(p, sub.type) : sub.type.transpose().intersect(p).transpose() ;
            if (p.hasTuple() && s==EMPTY)
               w2=new ErrorWarning(sub.span(),
               "The value of this expression does not contribute to the value of the parent.\nParent's relevant type = "
               +p+"\nThis expression's type = "+sub.type.extract(2));
            break;
          case CARDINALITY: case NO: case ONE: case SOME: case LONE:
            s=Type.removesBoolAndInt(sub.type);
            break;
          case CAST2SIGINT:
            s=Type.INT;
            break;
          case NOT:
            s=Type.FORMULA;
            break;
          case CAST2INT:
            s=sub.type.intersect(SIGINT.type);
            if (s.hasNoTuple())
               w1=new ErrorWarning(sub.span(),
               "This expression should contain integer atoms.\nInstead, its possible type(s) are:\n"+sub.type.extract(1));
            break;
        }
        Expr sub = this.sub.resolve(s, warns);
        if (w1!=null) warns.add(w1);
        if (w2!=null) warns.add(w2);
        return (sub==this.sub) ? this : op.make(pos, sub, weight-(this.sub.weight)+sub.weight, null);
    }

    //============================================================================================================//

    /**
     * Helper method that computes the relevant type for a closure expression.
     *
     * <p> Return Value == { c1->c2 | c1->c2 in childType, AND exists p1->p2 in parentType
     *                       where p1..c1..c2..p2 is a path in the closure graph }
     *
     * <p>
     * We need to do this because of situations like this follow:
     * Suppose e's type is "A->B + B->C".
     * Therefore, ^e = A->B + B->C + A->C which makes sense.
     * But as we compute the relevance type back down, we may have lost some entries,
     * and possibly end up with only A->B + A->C so we need to rediscover the relevant edges.
     */
    private static Type resolveClosure (Type parent, Type child) {
        IdentitySet<PrimSig> nodes = new IdentitySet<PrimSig>();
        DirectedGraph<PrimSig> graph = new DirectedGraph<PrimSig>();
        // For each (v1->v2) in childType, add (v1->v2) into the graph.
        for (ProductType c:child) if (c.arity()==2) {
            PrimSig a=c.get(0), b=c.get(1);
            nodes.add(a);
            nodes.add(b);
            graph.addEdge(a,b);
        }
        // For each distinct v1 and v2 in the graph where v1&v2!=empty, add the edges v1->v2 and v2->v1.
        for (PrimSig a:nodes) for (PrimSig b:nodes) if (a!=b && a.intersects(b)) graph.addEdge(a,b);
        // For each a->b in ParentType:
        // 1) add a
        // 2) add b
        // 3) if a has subtypes/supertypes in the graph, connect between a and them.
        // 4) if b has subtypes/supertypes in the graph, connect between b and them.
        for (ProductType p:parent) if (p.arity()==2) {
            PrimSig a=p.get(0), b=p.get(1);
            // Add edges between a and all its subtypes and supertypes
            if (!nodes.contains(a)) {
                for (PrimSig x:nodes) if (a.intersects(x)) { graph.addEdge(a,x); graph.addEdge(x,a); }
                nodes.add(a);
            }
            // Add edges between b and all its subtypes and supertypes
            if (!nodes.contains(b)) {
                for (PrimSig x:nodes) if (b.intersects(x)) { graph.addEdge(b,x); graph.addEdge(x,b); }
                nodes.add(b);
            }
        }
        // For each c1->c2 in childType, add c1->c2 into the finalType if there exists p1->p2 in parentType
        // such that p1->..->c1->c2->..->p2 is a path in the graph.
        Type answer=Type.EMPTY;
        for (ProductType c:child) if (c.arity()==2) {
            PrimSig c1=c.get(0), c2=c.get(1);
            for (ProductType p:parent) if (p.arity()==2) {
                PrimSig p1=p.get(0), p2=p.get(1);
                if (graph.hasPath(p1,c1) && graph.hasPath(c2,p2)) { answer=answer.merge(c); break; }
            }
        }
        return answer;
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (!errors.isEmpty()) throw errors.get(0);
        return visitor.visit(this);
    }
}
