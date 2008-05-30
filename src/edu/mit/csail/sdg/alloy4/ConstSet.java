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

package edu.mit.csail.sdg.alloy4;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Iterator;
import java.io.Serializable;

/**
 * This implements an unmodifiable set.
 *
 * @param <K> - the type of element
 */

public final class ConstSet<K> implements Serializable, Set<K> {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableSet set. */
    private final Set<K> set;

    /** This caches a readonly empty Set. */
    private static final ConstSet<Object> emptyset = new ConstSet<Object>(new HashSet<Object>(1));

    /** Construct an unmodifiable map with the given set as the backing store. */
    private ConstSet(Set<? extends K> set) {
        this.set = Collections.unmodifiableSet(set);
    }

    /** Return an unmodifiable empty set. */
    @SuppressWarnings("unchecked")
    public static<K> ConstSet<K> make() {
        return (ConstSet<K>) emptyset;
    }

    /**
     * Return an unmodifiable set with the same elements as the given set.
     * (If set==null, we'll return an unmodifiable empty set)
     */
    public static<K> ConstSet<K> make(Set<K> set) {
        if (set instanceof ConstSet) return (ConstSet<K>)set;
        if (set==null || set.isEmpty()) return make(); else return new ConstSet<K>(new LinkedHashSet<K>(set));
    }

    /** {@inheritDoc} */
    @Override public boolean equals(Object that) { return this==that || set.equals(that); }

    /** {@inheritDoc} */
    @Override public int hashCode() { return set.hashCode(); }

    /** {@inheritDoc} */
    @Override public String toString() { return set.toString(); }

    /** {@inheritDoc} */
    public boolean contains(Object element) { return set.contains(element); }

    /** {@inheritDoc} */
    public Iterator<K> iterator() { return set.iterator(); }

    /** {@inheritDoc} */
    public Object[] toArray() { return set.toArray(); }

    /** {@inheritDoc} */
    public <T> T[] toArray(T[] array) { return set.toArray(array); }

    /** {@inheritDoc} */
    public boolean containsAll(Collection<?> collection) { return set.containsAll(collection); }

    /** {@inheritDoc} */
    public int size() { return set.size(); }

    /** {@inheritDoc} */
    public boolean isEmpty() { return set.isEmpty(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean add(K element) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean addAll(Collection<? extends K> collection) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean remove(Object element) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean removeAll(Collection<?> collection) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean retainAll(Collection<?> collection) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public void clear() { throw new UnsupportedOperationException(); }
}
