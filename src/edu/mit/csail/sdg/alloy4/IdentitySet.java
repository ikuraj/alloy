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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Mutable; this implements a set based on reference identity; the null value is allowed.
 *
 * <p>
 * Since this violates java.util.Set's contract of using object equality,
 * this class intentionally does not implement the java.util.Set interface.
 *
 * <p>
 * For simplicity, and since the remove() feature isn't needed,
 * this implementation does not provide the remove() method.
 *
 * <p>
 * By making this simplification, we are able to provide an iterator
 * that returns the elements in the order they were inserted,
 * and allow concurrent insertion and iteration (that is, we can iterate
 * over the set while adding elements to the set at the same time).
 * The iterator is guaranteed to iterate over exactly the elements
 * that existed at the time that the iterator was created.
 *
 * <p><b>Invariant:</b>      hashmap.containsKey(x) if and only if (x==list.get(i) for some i)
 *
 * <p><b>Thread Safety:</b>  Safe
 *
 * @param <T> - the type of element
 */

public final class IdentitySet<T> implements Iterable<T> {

    /** This map's key set is used to store the set of elements; the values are ignored. */
    private final IdentityHashMap<T,Object> hashmap = new IdentityHashMap<T,Object>();

    /**
     * This list also stores the same set of elements as this.hashmap;
     * this allows the iterator to return the elements in the order they were inserted.
     */
    private final List<T> list = new ArrayList<T>();

    /** Constructs an empty set. */
    public IdentitySet () { }

    /** Constructs a set containing the elements from the given collection. */
    public IdentitySet (Iterable<? extends T> collection) {
        for(T elem: collection) {
            add(elem);
        }
    }

    /** Returns the number of elements currently in this set. */
    public synchronized int size() {
        return hashmap.size();
    }

    /** Returns whether the given element is in the set. */
    public synchronized boolean contains(T item) {
        return hashmap.containsKey(item);
    }

    /** Adds the given element into the set if it's not in the set already. */
    public synchronized void add(T item) {
        if (!hashmap.containsKey(item)) {
            hashmap.put(item, null);
            list.add(item);
        }
    }

    /**
     * Returns an iterator that iterates over elements in this set
     * (in the order that they were inserted).
     *
     * <p> Note: This iterator's remove() method always throws UnsupportedOperationException.
     *
     * <p> Note: This iterator always returns exactly the set of elements that existed
     * at the time that the iterator was created (even if the set is modified after that point).
     */
    public synchronized Iterator<T> iterator() {
        final int listsize = list.size();
        return new Iterator<T>() {
            private final int max = listsize;
            private int now = 0;
            public final boolean hasNext() {
                return now < max;
            }
            public final T next() {
                if (now >= max) {
                    throw new NoSuchElementException();
                }
                synchronized(IdentitySet.this) {
                    T answer=list.get(now);
                    now++;
                    return answer;
                }
            }
            public final void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }
}
