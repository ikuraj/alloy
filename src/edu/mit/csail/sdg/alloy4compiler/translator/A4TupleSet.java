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

import java.util.Iterator;
import java.util.NoSuchElementException;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/** Immutable; represents a collection of Alloy tuples; comparison is by identity rather than by value. */

public final class A4TupleSet implements Iterable<A4Tuple> {

    /** The Kodkod tupleset. */
    private final TupleSet tuples;

    /** The A4Solution that this came from. */
    private final A4Solution sol;

    /**
     * Construct a TupleSet from the kodkod TupleSet, while renaming each atom using the atom2name map in sol.
     * <br> NOTE: caller must ensure the Kodkod tupleset is not modified, since we expect the resulting A4Tupleset to be constant.
     */
    A4TupleSet(TupleSet tuples, A4Solution sol) throws ErrorAPI {
        if (!sol.satisfiable()) throw new ErrorAPI("This solution is unsatisfiable.");
        this.tuples = tuples;
        this.sol = sol;
    }

    /** Returns a read-only iterator that iterates over each tuple in this TupleSet. */
    public Iterator<A4Tuple> iterator() {
        return new Iterator<A4Tuple>() {
            private final Iterator<Tuple> it = tuples.iterator();
            public final boolean hasNext() { return it.hasNext(); }
            public final A4Tuple next() {
                if (!it.hasNext()) throw new NoSuchElementException();
                return new A4Tuple(it.next(), sol);
            }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }

    /** Returns the arity. */
    public int arity() { return tuples.arity(); }

    /** Returns the number of tuples in this tuple set. */
    public int size() { return tuples.size(); }

    /** Prints a human-readable description of this TupleSet. */
    @Override public String toString() {
        StringBuilder sb=new StringBuilder("{");
        for(A4Tuple t:this) {
            if (sb.length()>1) sb.append(", ");
            sb.append(t);
        }
        return sb.append('}').toString();
    }
}
