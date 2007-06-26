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

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.DirectedGraph;
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
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cform;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;

/**
 * Immutable; represents a unary expression of the form "(OP subexpression)"
 *
 * <p> <b>Invariant:</b>  sub.mult==0
 */

public final class ExprUnary extends Expr {

    /** The unary operator. */
    public final Op op;

    /** The subexpression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(sub.span()));
        return p;
    }

    /** Constructs an unary expression. */
    private ExprUnary(Pos pos, Op op, Expr sub, Type type) throws Err {
        super( pos , type , (op==Op.SOMEOF||op==Op.LONEOF||op==Op.ONEOF||op==Op.SETOF)?1:0 , sub.weight);
        this.op=op;
        this.sub=sub;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            switch(op) {
              case SOMEOF: out.append("some"); break;
              case LONEOF: out.append("lone"); break;
              case ONEOF: out.append("one"); break;
              case SETOF: out.append("set"); break;
              case CAST2INT: out.append("int["); sub.toString(out,-1); out.append(']'); return;
              case CAST2SIGINT: out.append("Int["); sub.toString(out,-1); out.append(']'); return;
            }
            out.append(' ');
            sub.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append(op).append(" with type=").append(type).append('\n');
            sub.toString(out, indent+2);
        }
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
        /** integer-to-IntAtom                                           */  CAST2SIGINT("int->Int");

        /** The constructor */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator */
        private final String label;

        /**
         * Construct an ExprUnary node.
         * @param pos - the original position of the "unary operator" in the file (can be null if unknown)
         * @param sub - the subexpression
         * @throws ErrorSyntax if (this!=SETOF) and (sub.mult!=0)
         *
         * <p>  Alloy4 disallows multiplicity like this:   "variable : one (X lone-> Y)",
         * <br> that is, a some/lone/one in front of an arrow multiplicity declaration.
         * <br> Alloy4 does allow "variable : set (X lone-> Y)", where we ignore the word "set".
         * <br> (This desugaring is done by the ExprUnary.Op.make() method, so ExprUnary constructor never sees it)
         */
        public final Expr make(Pos pos, Expr sub) throws Err {
            if (sub.mult!=0) {
               if (this==SETOF) return sub;
               throw new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here.");
            }
            Type type=null;
            if (sub.type!=null) switch(this) {
              case NOT:
                cform(sub);
                type=Type.FORMULA;
                break;
              case RCLOSURE: case CLOSURE:
                sub=cset(sub);
                type=sub.type.closure();
                if (type.size()==0) throw new ErrorType(sub.span(), label+" can be used only with a binary relation.\nInstead, its possible type(s) are:\n"+sub.type);
                if (this==RCLOSURE) type=Type.make2(UNIV);
                break;
              case SOMEOF: case LONEOF: case ONEOF: case SETOF:
                sub=cset(sub);
                if (this==SETOF) type=Type.removesBoolAndInt(sub.type); else type=sub.type.extract(1);
                if (type.size()==0) throw new ErrorType(sub.span(), "After the some/lone/one multiplicity symbol, this expression must be a unary set.\nInstead, its possible type(s) are:\n"+sub.type);
                break;
              case SOME: case LONE: case ONE: case NO:
                sub=cset(sub);
                type=Type.FORMULA;
                break;
              case CARDINALITY:
                sub=cset(sub);
                type=Type.INT;
                break;
              case CAST2INT:
                if (sub.type.is_int && !sub.type.is_bool && sub.type.size()==0) return sub; // Shortcut if it can only be an integer anyway
                sub=cset(sub);
                if (sub.type.hasArity(1)) { type=Type.INT; break; }
                throw new ErrorType(sub.span(), "int[] can be used only with a unary set.\nInstead, its possible type(s) are:\n"+sub.type);
              case CAST2SIGINT:
                sub=cint(sub);
                type=SIGINT.type;
                break;
              case TRANSPOSE:
                sub=cset(sub);
                type=sub.type.transpose();
                if (type.size()>0) break;
                throw new ErrorType(sub.span(), "~ can be used only with a binary relation.\nInstead, its possible type(s) are:\n"+sub.type);
            }
            return new ExprUnary(pos, this, sub, type);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //============================================================================================================//

    /** Typechecks an ExprUnary object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        Expr sub = cx.check(this.sub);
        return (sub==this.sub) ? this : op.make(pos,sub);
    }

    //============================================================================================================//

    /** Typechecks an ExprUnary object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p) throws Err {
        Type s;
        switch(op) {
          case TRANSPOSE: case RCLOSURE: case CLOSURE:
            if (op!=Op.TRANSPOSE && type.join(type).hasNoTuple())
               A4Reporter.getReporter().warning(new ErrorWarning(pos, op+" is redundant since the domain and range are disjoint: "+sub.type.extract(2)));
            s = (op==Op.TRANSPOSE) ? sub.type.transpose().intersect(p).transpose() : resolveClosure(p, sub.type);
            if (p.hasTuple() && s.hasNoTuple())
               A4Reporter.getReporter().warning(new ErrorWarning(sub.span(), "The value of this expression does not contribute to the value of the parent."));
            break;
          case SOMEOF: case LONEOF: case ONEOF: case SETOF:
            s = p;
            break;
          case CAST2SIGINT:
            s = Type.INT;
            break;
          case NOT:
            s = Type.FORMULA;
            break;
          default: // CARDINALITY CAST2INT NO ONE SOME LONE:
            if (op==Op.CAST2INT && !SIGINT.type.intersects(sub.type))
               A4Reporter.getReporter().warning(new ErrorWarning(sub.span(), "This expression should contain integer atoms.\nInstead, its possible type(s) are:\n"+sub.type));
            s = Type.removesBoolAndInt(sub.type);
        }
        if (!s.is_int && !s.is_bool && s.size()==0) s=sub.type; // We try to continue the best we can.
        Expr sub = this.sub.check(cx,s);
        return (sub==this.sub) ? this : op.make(pos,sub);
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
    private static Type resolveClosure (Type parent, Type child) throws Err {
        IdentitySet<PrimSig> nodes=new IdentitySet<PrimSig>();
        DirectedGraph<PrimSig> graph=new DirectedGraph<PrimSig>();
        // For each (v1->v2) in childType, add (v1->v2) into the graph.
        for (ProductType c:child) if (c.arity()==2) {
            PrimSig a=c.get(0), b=c.get(1);
            nodes.add(a); nodes.add(b); graph.addEdge(a,b);
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

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
