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
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <K> - the type of element
 */

public final class ConstSet<K> implements Serializable, Set<K> {

    /**
     * This implements a modifiable set that can be used to construct a ConstSet.
     *
     * <p><b>Thread Safety:</b>  Not safe.
     *
     * @param <K> - the type of element
     */
    public static final class TempSet<K> {
        /** The underlying set. */
        private final LinkedHashSet<K> set;
        /** Nonnull iff this set is no longer modifiable. */
        private ConstSet<K> cset=null;
        /** Construct a new empty modifiable TempSet. */
        public TempSet()                     { this.set = new LinkedHashSet<K>(); }
        /** Construct a new modifiable TempSet with the initial entries equal to the given set. */
        public TempSet(Set<? extends K> set) { this.set = new LinkedHashSet<K>(set); }
        /** Returns a String representation. */
        @Override public String toString()   { return set.toString(); }
        /** Return the number of entries in this set. */
        public int size()                    { return set.size(); }
        /** Return true if the given key is in the set. */
        public boolean contains(Object k)    { return set.contains(k); }
        /** Remove the element (if it exists). */
        public void remove(K k)                            { if (cset!=null) throw new UnsupportedOperationException(); set.remove(k); }
        /** Remove every element that appears in the given collection. */
        public void removeAll(Collection<? extends K> set) { if (cset!=null) throw new UnsupportedOperationException(); this.set.removeAll(set); }
        /** Add the given element to the set. */
        public void add(K k)                               { if (cset!=null) throw new UnsupportedOperationException(); set.add(k); }
        /** Turn this TempSet unmodifiable, then construct a ConstSet backed by this TempSet. */
        @SuppressWarnings("unchecked")
        public ConstSet<K> makeConst() { if (cset==null) cset=set.isEmpty()?(ConstSet<K>)emptyset:new ConstSet<K>(set); return cset; }
    }

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableSet set. */
    private final Set<K> set;

    /** This caches a readonly empty Set. */
    private static final ConstSet<Object> emptyset = new ConstSet<Object>(new HashSet<Object>(1));

    /** Construct an unmodifiable set containing the elements from the given set. */
    private ConstSet(Set<? extends K> set) {
        this.set=Collections.unmodifiableSet(set);
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
    @SuppressWarnings("unchecked")
    public static<K> ConstSet<K> make(Set<? extends K> set) {
        if (set instanceof ConstSet) return (ConstSet<K>)set;
        if (set==null || set.isEmpty()) return (ConstSet<K>)emptyset;
        return new ConstSet<K>(new LinkedHashSet<K>(set));
    }

    /** {@inheritDoc} */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Set)) return false;
        return set.equals(that);
    }

    /** {@inheritDoc} */
    @Override public int hashCode() { return set.hashCode(); }

    /** {@inheritDoc} */
    @Override public String toString() { return set.toString(); }

    /** {@inheritDoc} */
    public boolean contains(Object o) { return set.contains(o); }

    /** {@inheritDoc} */
    public Iterator<K> iterator() { return set.iterator(); }

    /** {@inheritDoc} */
    public Object[] toArray() { return set.toArray(); }

    /** {@inheritDoc} */
    public <T> T[] toArray(T[] a) { return set.toArray(a); }

    /** {@inheritDoc} */
    public boolean containsAll(Collection<?> c) { return set.containsAll(c); }

    /** {@inheritDoc} */
    public int size() { return set.size(); }

    /** {@inheritDoc} */
    public boolean isEmpty() { return set.isEmpty(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean add(K o) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean addAll(Collection<? extends K> c) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean remove(Object o) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean removeAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public boolean retainAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /** This set is readonly, so this method always throws UnsupportedOperationException. */
    public void clear() { throw new UnsupportedOperationException(); }
}
