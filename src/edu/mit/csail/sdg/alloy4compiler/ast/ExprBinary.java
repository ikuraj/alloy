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
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4compiler.ast.Type.ProductType;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cform;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;

/**
 * Immutable; represents an expression of the form (x OP y).
 *
 * <p> <b>Invariant:</b>  this.mult!=1
 * <p> <b>Invariant:</b>  this.mult==2 => this.op is one of the 17 arrow operators
 * <p> <b>Invariant:</b>  left.mult!=1
 * <p> <b>Invariant:</b>  left.mult==2 => this.op is one of the 17 arrow operators
 * <p> <b>Invariant:</b>  right.mult==1 => this.op==IN
 * <p> <b>Invariant:</b>  right.mult==2 => (this.op==IN || this.op is one of the 17 arrow operators)
 */

public final class ExprBinary extends Expr {

    /** The binary operator. */
    public final Op op;

    /** The left-hand-side expression. */
    public final Expr left;

    /** The right-hand-side expression. */
    public final Expr right;

    /** Caches the span() result. */
    private Pos span=null;

    /** Constructs a new ExprBinary node. */
    private ExprBinary(Pos pos, Op op, Expr left, Expr right, Type type) throws Err {
        super( pos , type , (op.isArrow && (left.mult==2 || right.mult==2 || op!=Op.ARROW))?2:0 , left.weight+right.weight);
        this.op=op;
        this.left=left;
        this.right=right;
        if (op.isArrow) {
           if (left.mult==1) throw new ErrorSyntax(left.span(), "Multiplicity expression not allowed here.");
           if (right.mult==1) throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
        } else {
           if (left.mult!=0) throw new ErrorSyntax(left.span(), "Multiplicity expression not allowed here.");
           if (op!=Op.IN && right.mult!=0) throw new ErrorSyntax(right.span(), "Multiplicity expression not allowed here.");
        }
    }

    /**
     * Convenience method that generates a type error exception with "msg" as the message,
     * and includes the left and right bounding types in the message.
     */
    private static ErrorType boundingTypeError(Pos pos, String msg, Type leftType, Type rightType) {
        return new ErrorType(pos, msg+"\nLeft bounding type = "+leftType+"\nRight bounding type = "+rightType);
    }

    /**
     * Convenience method that issues a warning message to the current reporter, with "msg" as the message,
     * and includes the left and right bounding types in the message.
     */
    private static void boundingTypeWarning(Pos pos, String msg, Type leftType, Type rightType) {
        A4Reporter.getReporter().warning(new ErrorWarning(pos, msg+"\nLeft bounding type = "+leftType+"\nRight bounding type = "+rightType));
    }

    /**
     * Convenience method that issues a warning message to the current reporter, with "msg" as the message,
     * and includes the left and right relevance types in the message.
     */
    private static void relevanceWarning(Pos pos, String msg, Type leftType, Type rightType) {
        A4Reporter.getReporter().warning(new ErrorWarning(pos, msg+"\nLeft relevant type = "+leftType+"\nRight relevant type = "+rightType));
    }

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = pos.merge(left.span()).merge(right.span()));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            left.toString(out,-1);
            out.append(' ').append(op).append(' ');
            right.toString(out,-1);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append(op).append(" with type=").append(type).append('\n');
            left.toString(out, indent+2);
            right.toString(out, indent+2);
        }
    }

    //============================================================================================================//

    /** This class contains all possible binary operators. */
    public static enum Op {
        /** -&gt;           */  ARROW("->",true),
        /** -&gt;some       */  ANY_ARROW_SOME("->some",true),
        /** -&gt;one        */  ANY_ARROW_ONE("->one",true),
        /** -&gt;lone       */  ANY_ARROW_LONE("->lone",true),
        /** some-&gt;       */  SOME_ARROW_ANY("some->",true),
        /** some-&gt;some   */  SOME_ARROW_SOME("some->some",true),
        /** some-&gt;one    */  SOME_ARROW_ONE("some->one",true),
        /** some-&gt;lone   */  SOME_ARROW_LONE("some->lone",true),
        /** one-&gt;        */  ONE_ARROW_ANY("one->",true),
        /** one-&gt;some    */  ONE_ARROW_SOME("one->some",true),
        /** one-&gt;one     */  ONE_ARROW_ONE("one->one",true),
        /** one-&gt;lone    */  ONE_ARROW_LONE("one->lone",true),
        /** lone-&gt;       */  LONE_ARROW_ANY("lone->",true),
        /** lone-&gt;some   */  LONE_ARROW_SOME("lone->some",true),
        /** lone-&gt;one    */  LONE_ARROW_ONE("lone->one",true),
        /** lone-&gt;lone   */  LONE_ARROW_LONE("lone->lone",true),
        /** isSeq-&gt;lone  */  ISSEQ_ARROW_LONE("isSeq->lone",true),
        /** .               */  JOIN(".",false),
        /** &lt;:           */  DOMAIN("<:",false),
        /** :&gt;           */  RANGE(":>",false),
        /** &amp;           */  INTERSECT("&",false),
        /** ++              */  PLUSPLUS("++",false),
        /** +               */  PLUS("+",false),
        /** -               */  MINUS("-",false),
        /** =               */  EQUALS("=",false),
        /** &lt;            */  LT("<",false),
        /** =&lt;           */  LTE("=<",false),
        /** &gt;            */  GT(">",false),
        /** &gt;=           */  GTE(">=",false),
        /** in              */  IN("in",false),
        /** &amp;&amp;      */  AND("&&",false),
        /** ||              */  OR("||",false),
        /** &lt;=&gt;       */  IFF("<=>",false);

        /**
         * The constructor.
         * @param label - the label (for printing debugging messages)
         * @param isArrow - true if this operator is one of the 17 arrow operators
         */
        private Op(String label, boolean isArrow) {
            this.label=label;
            this.isArrow=isArrow;
        }

        /** The human readable label for this operator. */
        private final String label;

        /**
         * True if and only if this operator is the Cartesian product "->", a "seq" multiplicity,
         * or is a multiplicity arrow of the form "?->?".
         */
        public final boolean isArrow;

        /**
         * Constructs a new ExprBinary node.
         * @param pos - the original position in the source file (can be null if unknown)
         * @param left - the left hand side expression
         * @param right - the right hand side expression
         */
        public final Expr make(Pos pos, Expr left, Expr right) throws Err {
            Type a=left.type, b=right.type, type=null;
            if (a!=null && b!=null) switch(this) {
              case LT: case LTE: case GT: case GTE:
                  left = cint(left);
                  right = cint(right);
                  type = Type.FORMULA;
                  break;
              case AND: case OR: case IFF:
                  cform(left);
                  cform(right);
                  type = Type.FORMULA;
                  break;
              case PLUSPLUS:
                  left = cset(left);
                  right = cset(right);
                  type = left.type.unionWithCommonArity(right.type);
                  if (type.size()==0) throw boundingTypeError(pos, "++ can be used only between two expressions of the same arity.", left.type, right.type);
                  break;
              case PLUS: case MINUS: case EQUALS:
                  if (this==EQUALS) {
                      if (a.hasCommonArity(b) || (a.is_int && b.is_int)) { type=Type.FORMULA; break; }
                  } else {
                      type = (this==PLUS ? a.unionWithCommonArity(b) : a.pickCommonArity(b));
                      if (a.is_int && b.is_int) { type=Type.makeInt(type); break; }
                      if (type.size()>0) break;
                  }
                  if (TypeCheckContext.auto_sigint2int) {
                      if (a.is_int && b.intersects(SIGINT.type)) return make(pos, left, right.cast2int());
                      if (b.is_int && a.intersects(SIGINT.type)) return make(pos, left.cast2int(), right);
                  }
                  if (TypeCheckContext.auto_int2sigint) {
                      if (a.is_int && b.hasArity(1)) return make(pos, left.cast2sigint(), right);
                      if (b.is_int && a.hasArity(1)) return make(pos, left, right.cast2sigint());
                  }
                  throw boundingTypeError(pos, this+" can be used only between 2 expressions of the same arity, or between 2 integer expressions.",a,b);
              case IN:
                  left=cset(left);
                  right=cset(right);
                  if (left.type.hasCommonArity(right.type)) { type=Type.FORMULA; break; }
                  throw boundingTypeError(pos, this+" can be used only between 2 expressions of the same arity.", left.type, right.type);
              case JOIN:
                  left=cset(left); right=cset(right); type=left.type.join(right.type);
                  if (type.size()==0) throw boundingTypeError(pos, "You cannot perform relational join between two unary sets.", left.type, right.type);
                  break;
              case DOMAIN:
                  left=cset(left); right=cset(right); type=right.type.domainRestrict(left.type);
                  if (type.size()==0) throw new ErrorType(left.span(), "This must be a unary set, but instead it has the following possible type(s):\n"+left.type);
                  break;
              case RANGE:
                  left=cset(left); right=cset(right); type=left.type.rangeRestrict(right.type);
                  if (type.size()==0) throw new ErrorType(right.span(), "This must be a unary set, but instead it has the following possible type(s):\n"+right.type);
                  break;
              case INTERSECT:
                  left=cset(left); right=cset(right); type=left.type.intersect(right.type);
                  if (type.size()==0) throw boundingTypeError(pos, "& can be used only between 2 expressions of the same arity.", left.type, right.type);
                  break;
              default:
                  left=cset(left); right=cset(right); type=left.type.product(right.type);
            }
            return new ExprBinary(pos, this, left, right, type);
        }

        /** Returns the human readable label for this operator. */
        @Override public final String toString() { return label; }
    }

    //============================================================================================================//

    /** Typechecks an ExprBinary object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        Expr a=left.check(cx);
        Expr b=right.check(cx);
        if (a.type==null) throw new ErrorType(a.span(), "This expression failed to be typechecked.");
        if (b.type==null) throw new ErrorType(b.span(), "This expression failed to be typechecked.");
        return (a==left && b==right) ? this : op.make(pos, a, b);
    }

    //============================================================================================================//

    /** Typechecks an ExprBinary object (second pass). */
    @Override Expr check(TypeCheckContext cx, Type p) throws Err {
        Type a=left.type, b=right.type;
        bigbreak:
        switch(op) {
          case LT: case LTE: case GT: case GTE: {
            a=(b=Type.INT);
            break;
          }
          case AND: case OR: case IFF: {
            a=(b=Type.FORMULA);
            break;
          }
          case EQUALS: {
            a=a.intersect(b);
            if (left.type.is_int && right.type.is_int) a=Type.makeInt(a);
            if (left.type.hasTuple() && right.type.hasTuple() && a.hasNoTuple())
                boundingTypeWarning(pos, "== is redundant, because the left and right expressions are always disjoint.", left.type, right.type);
            b=a;
            break;
          }
          case IN: {
            a=a.pickCommonArity(b);
            b=b.intersect(a);
            if (left.type.hasNoTuple())
                boundingTypeWarning(pos, "Subset operator is redundant, because the left expression is always empty.", left.type, right.type);
            else if (right.type.hasNoTuple())
                boundingTypeWarning(pos, "Subset operator is redundant, because the right expression is always empty.", left.type, right.type);
            else if (b.hasNoTuple())
                boundingTypeWarning(pos, "Subset operator is redundant, because the left and right expressions are always disjoint.", left.type, right.type);
            break;
          }
          case INTERSECT: {
            if (type.hasNoTuple()) boundingTypeWarning(pos, "& is irrelevant because the 2 expressions are always disjoint.",a,b);
            a=p.intersect(a);
            b=p.intersect(b);
            break;
          }
          case PLUSPLUS: case PLUS: {
            if (op==Op.PLUS && p.is_int) { a=Type.INT; b=Type.INT; break; }
            a=p.intersect(a);
            b=p.intersect(b);
            if (a.hasNoTuple()) relevanceWarning(pos, this+" is irrelevant since the left expression is redundant.",a,b);
            if (b.hasNoTuple()) relevanceWarning(pos, this+" is irrelevant since the right expression is redundant.",a,b);
            if (op==Op.PLUSPLUS && !b.canOverride(a)) relevanceWarning(pos, "++ is irrelevant since the right expression can never override the left expression.",a,b);
            break;
          }
          case MINUS: {
            if (p.is_int) { a=Type.INT; b=Type.INT; break; }
            if (type.hasNoTuple()) boundingTypeWarning(pos, "- is irrelevant since the left and right expressions are always disjoint.",a,b);
            a=p;
            b=p.intersect(b);
            if (b.hasNoTuple()) relevanceWarning(pos, "- is irrelevant since the right expression is redundant.",a,b);
            break;
          }
          case JOIN: {
            if (type.hasNoTuple()) boundingTypeWarning(pos, "The join operation here always yields an empty set.", a, b);
            a=(b=Type.EMPTY);
            for (ProductType aa: left.type) for (ProductType bb: right.type) if (p.hasArity(aa.arity()+bb.arity()-2)) {
              PrimSig j = aa.get(aa.arity()-1).intersect(bb.get(0));
              if (j!=Sig.NONE) {
                 for (ProductType cc:p.intersect(aa.join(bb))) if (!cc.isEmpty()) {
                   List<PrimSig> bts = new ArrayList<PrimSig>(cc.arity() + 1);
                   for(int i=0; i<cc.arity(); i++) bts.add(cc.get(i));
                   bts.add(aa.arity()-1, j);
                   a = a.merge(Type.make(bts, 0, aa.arity()));
                   b = b.merge(Type.make(bts, aa.arity()-1, bts.size()));
                 }
              }
            }
            if (a.size()==0 || b.size()==0) { // We try to continue the best we can; we should have issued a relevance warning already.
              a=(b=Type.EMPTY);
              for (ProductType aa: left.type) for (ProductType bb: right.type)
                if (p.hasArity(aa.arity()+bb.arity()-2) && aa.get(aa.arity()-1).intersects(bb.get(0))) {a=a.merge(aa); b=b.merge(bb);}
            }
            if (a.size()==0 || b.size()==0) { // We try to continue the best we can; we should have issued a relevance warning already.
              a=(b=Type.EMPTY);
              for (ProductType aa: left.type) for (ProductType bb: right.type)
                if (p.hasArity(aa.arity()+bb.arity()-2)) {a=a.merge(aa); b=b.merge(bb); }
            }
            break;
          }
          case DOMAIN: {
            // leftType' = {r1 | r1 in leftType and there exists r2 in rightType such that r1<:r2 in parentType}
            // rightType' = {r2 | r2 in rightType and there exists r1 in leftType such that r1<:r2 in parentType}
            if (type.hasNoTuple()) boundingTypeWarning(pos, "<: is irrelevant because the result is always empty.", a, b);
            Type leftType=Type.EMPTY, rightType=Type.EMPTY;
            for (ProductType aa:a) if (aa.arity()==1) for (ProductType bb:b) if (p.hasArity(bb.arity()))
                for (ProductType cc:p.intersect(bb.columnRestrict(aa.get(0), 0))) if (!cc.isEmpty()) {
                    leftType  = leftType.merge(cc, 0, 1);
                    rightType = rightType.merge(cc);
                }
            if (leftType.size()==0 || rightType.size()==0) {
                // We try to proceed the best we can; we would have issued a relevance warning already.
                leftType = a.extract(1);
                rightType = b.pickCommonArity(p);
            }
            a=leftType; b=rightType; break;
          }
          case RANGE: {
            // leftType' = {r1 | r1 in leftType and there exists r2 in rightType such that r1:>r2 in parentType}
            // rightType' = {r2 | r2 in rightType and there exists r1 in leftType such that r1:>r2 in parentType}
            if (type.hasNoTuple()) boundingTypeWarning(pos, ":> is irrelevant because the result is always empty.", a, b);
            Type leftType=Type.EMPTY, rightType=Type.EMPTY;
            for(ProductType bb:b) if (bb.arity()==1) for(ProductType aa:a) if (p.hasArity(aa.arity()))
                for (ProductType cc:p.intersect(aa.columnRestrict(bb.get(0), aa.arity()-1))) if (!cc.isEmpty()) {
                  leftType  = leftType.merge(cc);
                  rightType = rightType.merge(cc, aa.arity()-1, aa.arity());
                }
            if (leftType.size()==0 || rightType.size()==0) {
               // We try to proceed the best we can; we would have issued a relevance warning already.
               leftType = a.pickCommonArity(p);
               rightType = b.extract(1);
            }
            a=leftType; b=rightType; break;
          }
          default: {
            // leftType'  == {r1 | r1 in leftType and there exists r2 in rightType such that r1->r2 in parentType}
            // rightType' == {r2 | r2 in rightType and there exists r1 in leftType such that r1->r2 in parentType}
            if (a.hasTuple()) {
                if (b.hasNoTuple()) boundingTypeWarning(pos, "The left expression of -> is irrelevant because the right expression is always empty.", a, b);
            } else {
                if (b.hasTuple()) boundingTypeWarning(pos, "The right expression of -> is irrelevant because the left expression is always empty.", a, b);
            }
            Type leftType=Type.EMPTY, rightType=Type.EMPTY;
            for (ProductType aa:a) if (!aa.isEmpty())
              for (ProductType bb:b) if (!bb.isEmpty() && p.hasArity(aa.arity()+bb.arity()))
                for (ProductType cc:p.intersect(aa.product(bb))) if (!cc.isEmpty()) {
                   leftType  = leftType.merge(cc, 0, aa.arity());
                   rightType = rightType.merge(cc, aa.arity(), cc.arity());
                }
            // We try to proceed the best we can; we would have issued a relevance warning already.
            if (leftType.size()==0 || rightType.size()==0) break;
            a=leftType; b=rightType;
          }
        }
        Expr left=this.left.check(cx,a);
        Expr right=this.right.check(cx,b);
        if (left.type==null) throw new ErrorType(left.span(), "This expression failed to be typechecked.");
        if (right.type==null) throw new ErrorType(right.span(), "This expression failed to be typechecked.");
        return (left==this.left && right==this.right) ? this : op.make(pos, left, right);
    }

    //============================================================================================================//

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
