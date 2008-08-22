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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprList;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;

/** Mutable; represents an instance. */

public final class SimContext extends VisitReturn<Object> {

    /** This maps the current local variables (LET, QUANT, Function Param) to the actual SimTupleset/Integer/Boolean */
    private Env<ExprVar,Object> env = new Env<ExprVar,Object>();

    /** The exact values of each sig, field, and skolem (Note: it must not cache the value of a "defined" field) */
    private final Map<Expr,SimTupleset> sfs = new HashMap<Expr,SimTupleset>();

    /** Caches parameter-less functions to a Boolean, Integer, or SimTupleset. */
    private final Map<Func,Object> cacheForConstants = new IdentityHashMap<Func,Object>();

    /** This is used to detect "function recursion" (which we currently do not allow). */
    private final List<Func> current_function = new ArrayList<Func>();

    /** If nonnull, it caches the result of evaluating "iden"; must be cleared whenever sig contents change. */
    private SimTupleset cacheIDEN = null;

    /** If nonnull, it caches the result of evaluting "fun/next"; must be cleared whenever bitwidth changes. */
    private SimTupleset cacheNEXT = null;

    /** The chosen bitwidth */
    private final int bitwidth;

    /** The chosen maxseq length. */
    private final int maxseq;

    /** The shiftmask based on the chosen bitwidth. */
    private final int shiftmask;

    /** The minimum allowed integer based on the chosen bitwidth. */
    private final int min;

    /** The maximum allowed integer based on the chosen bitwidth. */
    private final int max;

    /** Construct a new simulation context with the given bitwidth and the given maximum sequence length. */
    public SimContext(int bitwidth, int maxseq) throws Err {
        if (bitwidth<1 || bitwidth>32) throw new ErrorType("Bitwidth must be between 1 and 32.");
        this.bitwidth = bitwidth;
        this.maxseq = maxseq;
        if (bitwidth==32) { max=Integer.MAX_VALUE; min=Integer.MIN_VALUE; } else { max=(1<<(bitwidth-1))-1; min=(0-max)-1; }
        if (maxseq < 0)   throw new ErrorSyntax("The maximum sequence length cannot be negative.");
        if (maxseq > max) throw new ErrorSyntax("With integer bitwidth of "+bitwidth+", you cannot have sequence length longer than "+max);
        shiftmask = (1 << (32 - Integer.numberOfLeadingZeros(bitwidth-1))) - 1;
        SimTupleset allInts = SimTupleset.range(min, max);
        SimTupleset allIdx = SimTupleset.range(0, maxseq-1);
        sfs.put(Sig.UNIV, allInts);
        sfs.put(Sig.SIGINT, allInts);
        sfs.put(Sig.SEQIDX, allIdx);
        sfs.put(Sig.STRING, SimTupleset.EMPTY);
        sfs.put(Sig.NONE, SimTupleset.EMPTY);
    }

    /** Construct a deep copy. */
    public SimContext(SimContext old) throws Err {
        bitwidth = old.bitwidth;
        maxseq = old.maxseq;
        min = old.min;
        max = old.max;
        shiftmask = old.shiftmask;
        cacheIDEN = old.cacheIDEN;
        cacheNEXT = old.cacheNEXT;
        env = old.env.dup();
        for(Map.Entry<Expr,SimTupleset> e: old.sfs.entrySet()) sfs.put(e.getKey(), e.getValue());
    }

    /**
     * Add an atom to a sig, then automatically add it to all parent sigs as well.
     * <p> The resulting instance may or may not satisfy all facts, and should be checked for consistency.
     * @throws ErrorAPI if attempting to add an atom to an abstract sig with children, or a builtin sig, or a subset sig.
     */
    public void addAtom(Sig sig, String atom) throws Err {
        if (sig.builtin) throw new ErrorAPI("Cannot add an atom to a builtin sig.");
        if (sig instanceof SubsetSig) throw new ErrorAPI("Cannot add an atom to a subset sig.");
        PrimSig s = (PrimSig)sig;
        if (s.isAbstract!=null && !s.children().isEmpty()) throw new ErrorAPI("Cannot add an atom to an abstract parent sig.");
        SimTupleset add = SimTupleset.wrap(atom);
        while(s != null) {
           SimTupleset old = sfs.get(s);
           if (old==null) old = SimTupleset.EMPTY;
           if (add.in(old)) return;
           old = old.union(add);
           sfs.put(s, old);
           s = s.parent;
        }
    }

    /**
     * Delete an atom from all sigs.
     * <p> The resulting instance may or may not satisfy all facts, and should be checked for consistency.
     * @throws ErrorAPI if attempting to delete from "Int".
     */
    public void deleteAtom(String atom) throws Err {
        SimTupleset wrap = SimTupleset.wrap(atom);
        for(Map.Entry<Expr,SimTupleset> x: sfs.entrySet()) {
            if (x.getKey() instanceof Sig) {
                Sig s = (Sig)(x.getKey());
                if (!s.builtin) x.setValue(x.getValue().difference(wrap));
            }
        }
    }

    /**
     * Modifies the given sig to be associated with the given unary value.
     * <p> The resulting instance may or may not satisfy all facts, and should be checked for consistency.
     */
    public void assign(Sig sig, SimTupleset value) throws Err {
        if (value.arity()>1) throw new ErrorType("Evaluator encountered an error: sig "+sig.label+" arity must not be " + value.arity());
        sfs.put(sig, value);
        cacheForConstants.clear();
        cacheIDEN = null;
    }

    /**
     * Modifies the given field to be associated with the given value.
     * <p> The resulting instance may or may not satisfy all facts, and should be checked for consistency.
     */
    public void assign(Field field, SimTupleset value) throws Err {
        if (value.size()>0 && value.arity()!=field.type.arity()) throw new ErrorType("Evaluator encountered an error: field "+field.label+" arity must not be " + value.arity());
        if (field.boundingFormula==null) throw new ErrorAPI("Evaluator cannot modify the value of a defined field.");
        sfs.put(field, value);
        cacheForConstants.clear();
        cacheIDEN = null;
    }

    /**
     * Modifies the given global var to be associated with the given value.
     * <p> The resulting instance may or may not satisfy all facts, and should be checked for consistency.
     */
    public void assign(ExprVar var, SimTupleset value) throws Err {
        if (value.size()>0 && value.arity()!=var.type.arity()) throw new ErrorType("Evaluator encountered an error: skolem "+var.label+" arity must not be " + value.arity());
        sfs.put(var, value);
        cacheForConstants.clear();
        cacheIDEN = null;
    }

    /** Truncate the given integer based on the current chosen bitwidth */
    private int trunc(int i) { return (i<<(32-bitwidth)) >> (32-bitwidth); }

    /**
     * Convenience method that evalutes x and casts the result to be a boolean.
     * @return the boolean - if x evaluates to a boolean
     * @throws ErrorFatal - if x does not evaluate to a boolean
     */
    public boolean cform(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y = visitThis(x);
        if (y instanceof Boolean) return Boolean.TRUE.equals(y);
        throw new ErrorFatal(x.span(), "This should have been a formula.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a int.
     * @return the int - if x evaluates to an int
     * @throws ErrorFatal - if x does not evaluate to an int
     */
    public int cint(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y = visitThis(x);
        if (y instanceof Integer) return trunc((Integer)y);
        throw new ErrorFatal(x.span(), "This should have been an integer expression.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a tupleset
     * @return the tupleset - if x evaluates to a tupleset
     * @throws ErrorFatal - if x does not evaluate to a tupleset
     */
    public SimTupleset cset(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y = visitThis(x);
        if (y instanceof SimTupleset) return (SimTupleset)y;
        throw new ErrorFatal(x.span(), "This should have been a set or a relation.\nInstead it is "+y);
    }

    /** Helper method that evaluates the formula "a in b" */
    private boolean isIn(SimTupleset a, Expr right) throws Err {
        if (right instanceof ExprUnary) {
            // Handles possible "unary" multiplicity
            ExprUnary y=(ExprUnary)(right);
            if (y.op==ExprUnary.Op.ONEOF)  { return a.size()==1 && a.in(cset(y.sub)); }
            if (y.op==ExprUnary.Op.SETOF)  { return                a.in(cset(y.sub)); }
            if (y.op==ExprUnary.Op.LONEOF) { return a.size()<=1 && a.in(cset(y.sub)); }
            if (y.op==ExprUnary.Op.SOMEOF) { return a.size()>=1 && a.in(cset(y.sub)); }
        }
        if (right instanceof ExprBinary && right.mult!=0 && ((ExprBinary)right).op.isArrow) {
            // Handles possible "binary" or higher-arity multiplicity
            return isInBinary(a, (ExprBinary)right);
        }
        return a.in(cset(right));
    }

    /** Helper method that evaluates the formula "r in (a ?->? b)" */
    private boolean isInBinary(SimTupleset R, ExprBinary ab) throws Err {
       // Special check for ISSEQ_ARROW_LONE
       if (ab.op == ExprBinary.Op.ISSEQ_ARROW_LONE) {
          List<String> list = R.getAllAtoms(0);
          int next=0;
          String nextStr=SimTupleset.canon("0");
          while(list.size() > 0) {
             for(int n=list.size(), i=0; ; i++) if (i>=n) return false; else if (nextStr==list.get(i)) { list.set(i, list.get(n-1)); list.remove(n-1); n--; break; }
             next++;
             nextStr=SimTupleset.canon(String.valueOf(next));
             if (next<0 && list.size()>0) return false; // shouldn't happen, but if it wraps around and yet list.size()>0 then we indeed have illegal tuples, so we return false
          }
       }
       // "R in A ->op B" means for each tuple a in A, there are "op" tuples in r that begins with a.
       SimTupleset left = cset(ab.left);
       for(int i=0; i<left.size(); i++) {
         SimTupleset ans = R.beginWith(left, i);
         switch(ab.op) {
            case ISSEQ_ARROW_LONE:
            case ANY_ARROW_LONE: case SOME_ARROW_LONE: case ONE_ARROW_LONE: case LONE_ARROW_LONE: if (!(ans.size()<=1)) return false; else break;
            case ANY_ARROW_ONE:  case SOME_ARROW_ONE:  case ONE_ARROW_ONE:  case LONE_ARROW_ONE:  if (!(ans.size()==1)) return false; else break;
            case ANY_ARROW_SOME: case SOME_ARROW_SOME: case ONE_ARROW_SOME: case LONE_ARROW_SOME: if (!(ans.size()>=1)) return false; else break;
         }
         if (!isIn(ans, ab.right)) return false;
       }
       // "R in A op-> B" means for each tuple b in B, there are "op" tuples in r that end with b.
       SimTupleset right = cset(ab.right);
       for(int i=0; i<right.size(); i++) {
         SimTupleset ans = R.endWith(right, i);
         switch(ab.op) {
            case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE: if (!(ans.size()<=1)) return false; else break;
            case ONE_ARROW_ANY:  case ONE_ARROW_SOME:  case ONE_ARROW_ONE:  case ONE_ARROW_LONE:  if (!(ans.size()==1)) return false; else break;
            case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE: if (!(ans.size()>=1)) return false; else break;
         }
         if (!isIn(ans, ab.left)) return false;
       }
       return true;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        switch(x.op) {
          case ARROW: case ANY_ARROW_LONE: case ANY_ARROW_ONE: case ANY_ARROW_SOME:
          case LONE_ARROW_ANY: case LONE_ARROW_LONE: case LONE_ARROW_ONE: case LONE_ARROW_SOME:
          case ONE_ARROW_ANY: case ONE_ARROW_LONE: case ONE_ARROW_ONE: case ONE_ARROW_SOME:
          case SOME_ARROW_ANY: case SOME_ARROW_LONE: case SOME_ARROW_ONE: case SOME_ARROW_SOME:
          case ISSEQ_ARROW_LONE:
              return cset(x.left).product(cset(x.right));
          case IN:
              return isIn(cset(x.left), x.right);
          case JOIN:
              return cset(x.left).join(cset(x.right));
          case AND:
              return cform(x.left) && cform(x.right); // Java always has the short-circuit behavior
          case OR:
              return cform(x.left) || cform(x.right); // Java always has the short-circuit behavior
          case IFF:
              return cform(x.left) == cform(x.right);
          case SHA:
              return trunc(cint(x.left) >> (shiftmask & cint(x.right)));
          case SHR:
              return trunc(cint(x.left) >>> (shiftmask & cint(x.right)));
          case SHL:
              return trunc(cint(x.left) << (shiftmask & cint(x.right)));
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
              return cset(x.left).domain(cset(x.right));
          case RANGE:
              return cset(x.left).range(cset(x.right));
          case EQUALS:
              if (x.left.type.is_int) return cint(x.left)==cint(x.right); else return cset(x.left).equal(cset(x.right));
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
    @Override public Object visit(ExprList x) throws Err {
        if (x.op==ExprList.Op.AND) {
           for(Expr e:x.args) if (!cform(e)) return false;
           return true;
        }
        if (x.op==ExprList.Op.OR) {
           for(Expr e:x.args) if (cform(e)) return true;
           return false;
        }
        if (x.op==ExprList.Op.TOTALORDER) {
            SimTupleset elem = cset(x.args.get(0)), first = cset(x.args.get(1)), next = cset(x.args.get(2));
            return next.totalOrder(elem, first);
        }
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
        final Func f = x.fun;
        final Object candidate = f.params.size()==0 ? cacheForConstants.get(f) : null;
        if (candidate!=null) return candidate;
        final Expr body = f.getBody();
        final int n = f.params.size();
        for(Func ff:current_function) if (ff==f) throw new ErrorSyntax(x.span(), ""+f+" cannot call itself recursively!");
        Env<ExprVar,Object> newenv = new Env<ExprVar,Object>();
        for(int i=0; i<n; i++) newenv.put(f.params.get(i), cset(x.args.get(i)));
        Env<ExprVar,Object> oldenv = env;
        env = newenv;
        current_function.add(f);
        Object ans = visitThis(body);
        env = oldenv;
        current_function.remove(current_function.size()-1);
        if (f.params.size()==0) cacheForConstants.put(f, ans);
        return ans;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprConstant x) throws Err {
        switch(x.op) {
          case NUMBER:
             int n = x.num();
             if (n<min) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
             if (n>max) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
             return n;
          case FALSE: return Boolean.FALSE;
          case TRUE: return Boolean.TRUE;
          case MIN: return min;
          case MAX: return max;
          case EMPTYNESS: return SimTupleset.EMPTY;
          case STRING: return SimTupleset.wrap(x.string);
          case NEXT: if (cacheNEXT==null) return cacheNEXT=SimTupleset.next(min,max); else return cacheNEXT;
          case IDEN: if (cacheIDEN==null) return cacheIDEN=cset(Sig.UNIV).iden(); else return cacheIDEN;
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprConstant.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprITE x) throws Err {
        if (cform(x.cond)) return visitThis(x.left); else return visitThis(x.right);
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprLet x) throws Err {
        env.put(x.var, visitThis(x.var.expr));
        Object ans = visitThis(x.sub);
        env.remove(x.var);
        return ans;
    }

    private int enumerate(final List<String[]> store, int sum, final ExprQuant x, final Expr body, final int i) throws Err { // if op is ALL NO SOME ONE LONE then it always returns 0 1 2
       final int n = x.vars.size();
       final ExprVar v = x.vars.get(i);
       final SimTupleset e = cset(v.expr);
       final Iterator<SimTupleset> it;
       if      (v.expr.mult==1 && ((ExprUnary)(v.expr)).op==ExprUnary.Op.LONEOF) it = e.loneOf();
       else if (v.expr.mult==1 && ((ExprUnary)(v.expr)).op==ExprUnary.Op.ONEOF)  it = e.oneOf();
       else if (v.expr.mult==1 && ((ExprUnary)(v.expr)).op==ExprUnary.Op.SOMEOF) it = e.someOf();
       else                                                                      it = e.setOf();
       while(it.hasNext()) {
          final SimTupleset binding = it.next();
          if (v.expr.mult==2 && !isIn(binding, v.expr)) continue;
          env.put(v, binding);
          if (i<n-1) sum = enumerate(store, sum, x, body, i+1);
             else if (x.op==ExprQuant.Op.SUM) sum += cint(body);
             else if (x.op!=ExprQuant.Op.COMPREHENSION) { sum += cform(body)?1:0; if (sum>=2) return 2; } // no need to enumerate further
             else if (cform(body)) { String[] add = new String[n]; for(int j=0; j<n; j++) add[j]=((SimTupleset)(env.get(x.vars.get(j)))).getAtom(); store.add(add); }
          env.remove(v);
       }
       return sum;
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprQuant x) throws Err {
        if (x.op == ExprQuant.Op.COMPREHENSION) {
           List<String[]> ans = new ArrayList<String[]>();
           enumerate(ans, 0, x, x.sub, 0);
           return SimTupleset.make(ans);
        }
        if (x.op == ExprQuant.Op.ALL)  return enumerate(null, 0, x, x.sub.not(), 0) == 0;
        if (x.op == ExprQuant.Op.NO)   return enumerate(null, 0, x, x.sub,       0) == 0;
        if (x.op == ExprQuant.Op.SOME) return enumerate(null, 0, x, x.sub,       0) >= 1;
        if (x.op == ExprQuant.Op.LONE) return enumerate(null, 0, x, x.sub,       0) <= 1;
        if (x.op == ExprQuant.Op.ONE)  return enumerate(null, 0, x, x.sub,       0) == 1;
        if (x.op == ExprQuant.Op.SUM)  return trunc(enumerate(null, 0, x, x.sub, 0));
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprQuant.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprUnary x) throws Err {
        switch(x.op) {
          case LONEOF:
          case ONEOF:
          case SETOF:
          case SOMEOF:      return cset(x.sub);
          case NOOP:        return visitThis(x.sub);
          case CARDINALITY: return trunc(cset(x.sub).size());
          case NO:          return cset(x.sub).size()==0;
          case LONE:        return cset(x.sub).size()<=1;
          case ONE:         return cset(x.sub).size()==1;
          case SOME:        return cset(x.sub).size()>=1;
          case NOT:         return cform(x.sub) ? Boolean.FALSE : Boolean.TRUE;
          case CAST2SIGINT: return SimTupleset.wrap(String.valueOf(cint(x.sub)));
          case CAST2INT:    return trunc(cset(x.sub).sum());
          case CLOSURE:     return cset(x.sub).closure();
          case RCLOSURE:    return cset(x.sub).closure().union(cset(ExprConstant.IDEN));
          case TRANSPOSE:   return cset(x.sub).transpose();
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprUnary.accept()");
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprVar x) throws Err {
        Object ans = env.get(x);
        if (ans==null) ans = sfs.get(x);
        if (ans==null) {
           SimTupleset ts = sfs.get(Sig.UNIV);
           if (ts!=null) for(String a: ts.getAllAtoms(0)) if (x.label.equals(a)) return SimTupleset.wrap(a);
        }
        if (ans==null) throw new ErrorFatal(x.pos, "Variable \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /** {@inheritDoc} */
    @Override public SimTupleset visit(Sig x) throws Err {
        if (x==Sig.NONE) return SimTupleset.EMPTY;
        Object ans = sfs.get(x);
        if (ans instanceof SimTupleset) return (SimTupleset)ans; else throw new ErrorFatal("Unknown sig "+x+" encountered during evaluation.");
    }

    /** {@inheritDoc} */
    @Override public SimTupleset visit(Field x) throws Err {
        if (x.boundingFormula==null) return cset(x.definition);
        Object ans = sfs.get(x);
        if (ans instanceof SimTupleset) return (SimTupleset)ans; else throw new ErrorFatal("Unknown field "+x+" encountered during evaluation.");
    }
}
