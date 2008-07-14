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

package edu.mit.csail.sdg.alloy4compiler.sim;

import java.util.HashMap;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;

/** Mutable; represents an instance. */

public final class SimContext extends VisitReturn<Object> {

    /** The exact values of each sig. */
    final Map<Sig,SimTupleset> sigs = new HashMap<Sig,SimTupleset>();

    /** The exact values of each field. */
    final Map<Field,SimTupleset> fields = new HashMap<Field,SimTupleset>();

    /** Caches the result of evaluating "iden"; must be cleared whenever sig contents change. */
    private SimTupleset cacheIDEN = null;

    /** The chosen bitwidth */
    private final int bitwidth;

    /** The minimum allowed integer based on the chosen bitwidth. */
    private final int min;

    /** The maximum allowed integer based on the chosen bitwidth. */
    private final int max;

    /** Construct a new simulation context with the given bitwidth. */
    public SimContext(int bitwidth) throws Err {
        if (bitwidth<1 || bitwidth>32) throw new ErrorType("Bitwidth must be between 1 and 32.");
        this.bitwidth = bitwidth;
        if (bitwidth==32) {max=Integer.MAX_VALUE; min=Integer.MIN_VALUE;} else {max=(1<<(bitwidth-1))-1; min=(0-max)-1;}
    }

    /** Truncate the given integer based on the current chosen bitwidth */
    private int trunc(int i) { return (i<<(32-bitwidth))>>(32-bitwidth); }

    /** Remove the "ExprUnary NOP" in front of an expression. */
    private static Expr deNOP(Expr x) {
        while(x instanceof ExprUnary && ((ExprUnary)x).op==ExprUnary.Op.NOOP) x=((ExprUnary)x).sub;
        return x;
    }

    /**
     * Convenience method that evalutes x and casts the result to be a Kodkod Formula.
     * @return the formula - if x evaluates to a Formula
     * @throws ErrorFatal - if x does not evaluate to a Formula
     */
    private boolean cform(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof Boolean) return Boolean.TRUE.equals(y);
        throw new ErrorFatal(x.span(), "This should have been a formula.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod IntExpression.
     * @return the integer expression - if x evaluates to an IntExpression
     * @throws ErrorFatal - if x does not evaluate to an IntExpression
     */
    private int cint(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof Integer) return trunc((Integer)y);
        throw new ErrorFatal(x.span(), "This should have been an integer expression.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod Expression.
     * @return the expression - if x evaluates to an Expression
     * @throws ErrorFatal - if x does not evaluate to an Expression
     */
    private SimTupleset cset(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof SimTupleset) return (SimTupleset)y;
        throw new ErrorFatal(x.span(), "This should have been a set or a relation.\nInstead it is "+y);
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        switch(x.op) {
          case ARROW: case ANY_ARROW_LONE: case ANY_ARROW_ONE: case ANY_ARROW_SOME:
          case LONE_ARROW_ANY: case LONE_ARROW_LONE: case LONE_ARROW_ONE: case LONE_ARROW_SOME:
          case ONE_ARROW_ANY: case ONE_ARROW_LONE: case ONE_ARROW_ONE: case ONE_ARROW_SOME:
          case SOME_ARROW_ANY: case SOME_ARROW_LONE: case SOME_ARROW_ONE: case SOME_ARROW_SOME:
              return cset(x.left).product(cset(x.right));
          case ISSEQ_ARROW_LONE:
              // FIXTHIS
          case IN:
              // FIXTHIS
          case JOIN:
              return cset(x.left).join(cset(x.right));
          case AND:
              return cform(x.left) && cform(x.right); // Java always has the short-circuit behavior
          case OR:
              return cform(x.left) || cform(x.right); // Java always has the short-circuit behavior
          case IFF:
              return cform(x.left) == cform(x.right);
          case SHA:
              return cint(x.left) >> cint(x.right); // FIXTHIS needs to double check
          case SHR:
              return cint(x.left) >>> cint(x.right); // FIXTHIS needs to double check
          case SHL:
              return trunc(cint(x.left) << cint(x.right)); // FIXTHIS needs to double check
          case INTERSECT:
              return cset(x.left).intersect(cset(x.right));
          case GT:
              return cint(x.left) > cint(x.right);
          case GTE:
              return cint(x.left) >= cint(x.right);
          case LT:
              return cint(x.left) < cint(x.right);
          case LTE:
              return cint(x.left) <= cint(x.right);
          case DOMAIN:
              a=deNOP(x.left); b=deNOP(x.right);
              if (a instanceof Sig && b instanceof Field && ((Field)b).sig==a) return cset(b); // simple optimization
              return cset(a).domain(cset(b));
          case RANGE:
              return cset(x.left).range(cset(x.right));
          case EQUALS:
              if (x.left.type.is_int) return cint(x.left)==cint(x.right); else return cset(x.left).eq(cset(x.right));
          case MINUS:
              // Special exception to allow "0-8" to not throw an exception, where 7 is the maximum allowed integer (when bitwidth==4)
              // (likewise, when bitwidth==5, then +15 is the maximum allowed integer, and we want to allow 0-16 without throwing an exception)
              if (a instanceof ExprConstant && ((ExprConstant)a).op==ExprConstant.Op.NUMBER && ((ExprConstant)a).num()==0)
                 if (b instanceof ExprConstant && ((ExprConstant)b).op==ExprConstant.Op.NUMBER && ((ExprConstant)b).num()==max+1)
                    return min;
              if (x.left.type.is_int) return trunc(cint(x.left)-cint(x.right)); else return cset(x.left).difference(cset(x.right));
          case PLUS:
              if (x.left.type.is_int) return trunc(cint(x.left)+cint(x.right)); else return cset(x.left).union(cset(x.right));
          case PLUSPLUS:
              return cset(x.left).override(cset(x.right));
          case MUL:
              return trunc(cint(x.left) * cint(x.right));
          case DIV:
              { int p=cint(x.left), q=cint(x.right), r=(p==0 ? 0 : (q==0 ? (p<0 ? 1 : -1) : (p/q))); return trunc(r); }
          case REM:
              { int p=cint(x.left), q=cint(x.right), r=(p==0 ? 0 : (q==0 ? (p<0 ? 1 : -1) : (p/q))); return trunc(p-r*q); }
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprBinary.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprBuiltin x) throws Err {
        SimTupleset[] ans = new SimTupleset[x.args.size()];
        for(int i=1; i<ans.length; i++) {
           for(int j=0; j<i; j++) {
              if (ans[i]==null) if ((ans[i]=cset(x.args.get(i))).size()==0) continue;
              if (ans[j]==null) if ((ans[j]=cset(x.args.get(j))).size()==0) continue;
              if (ans[j].intersects(ans[i])) return false;
           }
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprCall x) throws Err {
        // FIXTHIS Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprConstant x) throws Err {
        switch(x.op) {
          case FALSE:
             return Boolean.FALSE;
          case TRUE:
             return Boolean.TRUE;
          case MIN:
             return min;
          case MAX:
             return max;
          case NEXT:
             // FIXTHIS "next"
          case NUMBER:
             int n = x.num();
             if (n<min) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
             if (n>max) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
             return n;
          case IDEN:
             if (cacheIDEN==null) return cacheIDEN=cset(Sig.UNIV).iden(); else return cacheIDEN;
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprConstant.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprITE x) throws Err {
        if (cform(x.cond)) return visitThis(x.left); else return visitThis(x.right);
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprLet x) throws Err {
        // FIXTHIS Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprQuant x) throws Err {
        // FIXTHIS Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprUnary x) throws Err {
        switch(x.op) {
          case LONEOF:
          case ONEOF:
          case SETOF:
          case SOMEOF:      return cset(x.sub);
          case NOOP:        return visitThis(x);
          case CARDINALITY: return trunc(cset(x.sub).size());
          case NO:          return cset(x.sub).size()==0;
          case LONE:        return cset(x.sub).size()<=1;
          case ONE:         return cset(x.sub).size()==1;
          case SOME:        return cset(x.sub).size()>=1;
          case NOT:         return cform(x.sub) ? Boolean.FALSE : Boolean.TRUE;
          case CAST2SIGINT: return SimTupleset.wrap(cint(x.sub));
          case CAST2INT: {
              int ans = 0;
              for(Object[] t: cset(x.sub).tuples) {
                  if (t.length==1 && t[0] instanceof Integer) ans = ans + ((Integer)(t[0]));
              }
              return ans;
          }
          case CLOSURE:    return cset(x.sub).closure();
          case RCLOSURE:   return cset(x.sub).closure().union(cset(ExprConstant.IDEN));
          case TRANSPOSE:  return cset(x.sub).transpose();
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprUnary.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprVar x) throws Err {
        // FIXTHIS Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override public Object visit(Sig x) throws Err {
        Object ans = sigs.get(x);
        if (ans==null) throw new ErrorFatal("Unknown sig "+x+" encountered during evaluation."); else return ans;
    }

    /** {@inheritDoc} */
    @Override public Object visit(Field x) throws Err {
        Object ans = fields.get(x);
        if (ans==null) throw new ErrorFatal("Unknown field "+x+" encountered during evaluation."); else return ans;
    }
}
