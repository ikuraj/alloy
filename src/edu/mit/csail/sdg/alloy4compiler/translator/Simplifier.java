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

package edu.mit.csail.sdg.alloy4compiler.translator;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.MailBug;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.instance.TupleSet;

/**
 * Immutable; this class shrinks the unknowns as much as possible in order to reduce the number of variables in final CNF.
 *
 * <p> Currently it recognizes the following patterns:
 *
 * <p> (1) When it sees "A in B", it will try to derive a safe upperbound for B, and then remove
 *         any excess unknowns from A's upperbound.
 *
 * <p> (2) When it sees "A = B", it will try to simplify A assuming "A in B", and then simplify B assuming "B in A".
 */

public class Simplifier {

    /** Reporter for receiving debug messages. */
    private A4Reporter rep = null;

    /** The A4Solution object we are attempting to simplify. */
    private A4Solution sol = null;

    /** Construct a Simplifier object. */
    public Simplifier() { }

    /** Simplify sol.bounds() based on the set of formulas; subclasses should override this method to implement different simplification algorithms. */
    public boolean simplify(A4Reporter rep, A4Solution sol, Iterable<Formula> formulas) throws Err {
        this.rep = rep;
        this.sol = sol;
        boolean ans = true;
        for(Formula f: formulas) ans = simplify(f) && ans; // Note: even if we get false, we want to keep going so that we simplify bounds further
        return ans;
    }

    /** Simplify the bounds based on the fact that "a is subset of b"; return false if we discover the formula is unsat. */
    private final boolean simplify_in(Expression a, Expression b) {
       if (a instanceof Relation) {
          try {
             Relation r = (Relation)a;
             TupleSet ub = sol.query(true, r, false), lb = sol.query(false, r, false), t = sol.approximate(b);
             t.retainAll(ub);
             if (!t.containsAll(lb)) { rep.debug("Comment: Simplify "+a+" "+ub.size()+"->false\n"); return false; } // This means the upperbound is shrunk BELOW the lowerbound.
             if (t.size() < ub.size()) { rep.debug("Comment: Simplify "+a+" "+ub.size()+"->"+t.size()+"\n"); sol.shrink(r,lb,t); }
          } catch(Throwable ex) {
             rep.debug("Comment: Simplify "+a+" exception: "+ex+"\n"+MailBug.dump(ex).trim()+"\n"); // Not fatal; let's report it to the debug() reporter
          }
       }
       return true;
    }

    /** Simplify the bounds based on the fact that "form is true"; return false if we discover the formula is unsat. */
    private final boolean simplify (Formula form) {
       boolean flag1=true, flag2=true;
       if (form instanceof BinaryFormula) {
          BinaryFormula f=(BinaryFormula)form;
          if (f.op() == BinaryFormula.Operator.AND) {
             flag1=simplify(f.left());
             flag2=simplify(f.right());
          }
       } else if (form instanceof ComparisonFormula) {
          ComparisonFormula f=(ComparisonFormula)form;
          flag1=simplify_in(f.left(), f.right());
          if (f.op() == ComparisonFormula.Operator.EQUALS) flag2=simplify_in(f.right(), f.left());
       }
       return flag1 && flag2;
    }
}
