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
        for(Formula f: list) ans = ans && simplify(bounds, f, opt);
        return ans;
    }
}
