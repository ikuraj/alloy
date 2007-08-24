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
