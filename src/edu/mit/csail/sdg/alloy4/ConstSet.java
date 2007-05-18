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

package edu.mit.csail.sdg.alloy4;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Iterator;

/**
 * Immutable; this implements an unmodifiable set.
 *
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <K> - the type of element
 */

public final class ConstSet<K> implements Serializable, Set<K> {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableSet set. */
    private final Set<K> set;

    /** This caches a readonly empty Set. */
    private static final Set<Object> emptyset = Collections.unmodifiableSet(new HashSet<Object>(1));

    /** Construct an unmodifiable empty set. */
    @SuppressWarnings("unchecked")
    public ConstSet() {
        this.set=(Set<K>)emptyset;
    }

    /** Construct an unmodifiable set containing the elements from the given set. */
    @SuppressWarnings("unchecked")
    public ConstSet(Set<? extends K> set) {
        if (set.size()==0)
            this.set=(Set<K>)emptyset;
        else if (set instanceof ConstSet)
            this.set=((ConstSet<K>)set).set;
        else
            this.set=Collections.unmodifiableSet(new LinkedHashSet<K>(set));
    }

    /** Returns true if that is a Set with the same elements as this set. */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Set)) return false;
        return set.equals(that);
    }

    /** Computes a hash code that is consistent with equals(). */
    @Override public int hashCode() { return set.hashCode(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean contains(Object o) { return set.contains(o); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public Iterator<K> iterator() { return set.iterator(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public Object[] toArray() { return set.toArray(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public <T> T[] toArray(T[] a) { return set.toArray(a); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean containsAll(Collection<?> c) { return set.containsAll(c); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public int size() { return set.size(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean isEmpty() { return set.isEmpty(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean add(K o) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean addAll(Collection<? extends K> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean remove(Object o) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean removeAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public boolean retainAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.Set
     * @inheritDoc
     */
    public void clear() { throw new UnsupportedOperationException(); }
}
