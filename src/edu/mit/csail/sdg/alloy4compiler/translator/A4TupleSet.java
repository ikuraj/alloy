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
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/** Immutable; represents a collection of Alloy tuples; comparison is by identity rather than by value. */

public final class A4TupleSet implements Iterable<A4Tuple> {

    /** The Kodkod tupleset. */
    private final TupleSet tuples;

    /** The KodkodAtom_to_AlloyAtom rename map. */
    private final ConstMap<Object,String> atomMap;

    /** The kodkodAtom_to_MostSpecificSig map. */
    private final ConstMap<Object,PrimSig> sigMap;

    /**
     * Construct a TupleSet from the kodkod TupleSet, while renaming each atom using the given atomMap.
     * <br> NOTE: caller must ensure the Kodkod tupleset is not modified, since we expect the resulting A4Tupleset to be constant.
     */
    A4TupleSet(TupleSet tuples, ConstMap<Object,String> atomMap, ConstMap<Object,PrimSig> sigMap) {
        this.tuples = tuples;
        this.atomMap = atomMap;
        this.sigMap = sigMap;
    }

    /**
     * Construct a new TupleSet as the merge of this TupleSet and that TupleSet.
     * @throws ErrorAPI if this.atomMap != that.atomMap
     * @throws ErrorAPI if this.sigMap != that.atomMap
     * @throws ErrorAPI if this.tuples.universe() != that.tuples.universe()
     */
    A4TupleSet merge(A4TupleSet that) throws ErrorAPI {
        if (atomMap!=that.atomMap) throw new ErrorAPI("Cannot combine A4TupleSet from different universe.");
        if (sigMap!=that.sigMap) throw new ErrorAPI("Cannot combine A4TupleSet from different universe.");
        if (tuples.universe()!=that.tuples.universe()) throw new ErrorAPI("Cannot combine A4TupleSet from different universe.");
        TupleSet x = tuples.clone();
        x.addAll(that.tuples);
        return new A4TupleSet(x, atomMap, sigMap);
    }

    /** Returns a read-only iterator that iterates over each tuple in this TupleSet. */
    public Iterator<A4Tuple> iterator() {
        return new Iterator<A4Tuple>() {
            private final Iterator<Tuple> it = tuples.iterator();
            public final boolean hasNext() { return it.hasNext(); }
            public final A4Tuple next() {
                if (!it.hasNext()) throw new NoSuchElementException();
                return new A4Tuple(it.next(), atomMap, sigMap);
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
            sb.append(t.toString());
        }
        return sb.append('}').toString();
    }
}
