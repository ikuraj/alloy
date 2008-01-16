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

import java.util.List;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.Translator;
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;

/**
 * This class simplfies the Kodkod bounds by trying to infer as much partial instance information as possible.
 *
 * <p>
 * Currently it recognizes two forms:
 *
 * <p> (1) When it sees "A in B", it will try to derive a safe upperbound for B, and then remove
 *         any excess unknowns from A's upperbound.
 *
 * <p> (2) When it sees "A = B", it will try to simplify A assuming "A in B", and then simplify B assuming "B in A".
 */

final class Simplifier {

    /** Constructor is private, since this class never needs to be instantiated. */
    private Simplifier() { }

    /** Simplify the bounds based on the fact that "a is subset of b"; return false if we discover the formula is unsat. */
    private static boolean simplify_in(Bounds bounds, Expression a, Expression b, Options opt) {
        if (a instanceof Relation) {
            Relation r=(Relation)a;
            TupleSet u=bounds.upperBound(r);
            TupleSet l=bounds.lowerBound(r);
            TupleSet t=u.universe().factory().setOf(b.arity(), Translator.approximate(b,bounds,opt).denseIndices());
            t.retainAll(u);
            if (!t.containsAll(l)) return false; // This means the upperbound is shrunk BELOW the lowerbound.
            bounds.bound(r,l,t);
        }
        return true;
    }

    /** Simplify the bounds based on the fact that "form is true"; return false if we discover the formula is unsat. */
    private static boolean simplify(Bounds bounds, Formula form, Options opt) {
        boolean flag1=true, flag2=true;
        if (form instanceof BinaryFormula) {
            BinaryFormula f=(BinaryFormula)form;
            if (f.op() == BinaryFormula.Operator.AND) {
                flag1=simplify(bounds, f.left(), opt);
                flag2=simplify(bounds, f.right(), opt);
            }
        } else if (form instanceof ComparisonFormula) {
            ComparisonFormula f=(ComparisonFormula)form;
            flag1=simplify_in(bounds, f.left(), f.right(), opt);
            if (f.op() == ComparisonFormula.Operator.EQUALS) flag2=simplify_in(bounds, f.right(), f.left(), opt);
        }
        return flag1 && flag2;
    }

    /** Simplify the bounds based on the fact that "every formula in list is true"; return false if we discover the formula is unsat. */
    static boolean simplify(Bounds bounds, List<Formula> list, Options opt) {
        boolean ans = true;
        for(Formula f: list) ans = simplify(bounds, f, opt) && ans; // Note: even if we get false, we want to keep going so that we simplify bounds further
        return ans;
    }
}
