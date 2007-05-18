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
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;

/**
 * Immutable; this implements an unmodifiable list.
 *
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <T> - the type of element
 */

public final class ConstList<T> implements Serializable, List<T> {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableList list. */
    private final List<T> list;

    /** This caches a readonly empty list. */
    private static final List<Object> emptylist = Collections.unmodifiableList(new ArrayList<Object>(0));

    /** Construct an unmodifiable empty list. */
    @SuppressWarnings("unchecked")
    public ConstList() {
        this.list=(List<T>)emptylist;
    }

    /** Construct an unmodifiable list with the same elements as the given collection. */
    @SuppressWarnings("unchecked")
    public ConstList(Iterable<? extends T> collection) {
        if (!collection.iterator().hasNext()) {
            this.list=(List<T>)emptylist;
        } else if (collection instanceof ConstList) {
            this.list=((ConstList<T>)collection).list;
        } else {
            ArrayList<T> list=new ArrayList<T>();
            for(T elem:collection) list.add(elem);
            this.list=Collections.unmodifiableList(list);
        }
    }

    /** Construct an unmodifiable list with the same elements as the given collection. */
    @SuppressWarnings("unchecked")
    public ConstList(Collection<? extends T> collection) {
        if (collection.size()==0) {
            this.list=(List<T>)emptylist;
        } else if (collection instanceof ConstList) {
            this.list=((ConstList<T>)collection).list;
        } else {
            ArrayList<T> list=new ArrayList<T>(collection.size());
            for(T elem:collection) list.add(elem);
            this.list=Collections.unmodifiableList(list);
        }
    }

    /** Returns true if that is a List with the same elements in the same order as this list. */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof List)) return false;
        return list.equals(that);
    }

    /** Computes a hash code that is consistent with equals() */
    @Override public int hashCode() { return list.hashCode(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean contains(Object item) { return list.contains(item); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean containsAll(Collection<?> collection) { return list.containsAll(collection); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public T get(int i) { return list.get(i); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public int size() { return list.size(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean isEmpty() { return list.size()==0; }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public Iterator<T> iterator() { return list.iterator(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public Object[] toArray() { return list.toArray(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public <E> E[] toArray(E[] a) { return list.toArray(a); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean add(T o) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean remove(Object o) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean addAll(Collection<? extends T> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean addAll(int index, Collection<? extends T> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean removeAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public boolean retainAll(Collection<?> c) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public void clear() { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public T set(int index, T element) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public void add(int index, T element) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public T remove(int index) { throw new UnsupportedOperationException(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public int indexOf(Object o) { return list.indexOf(o); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public int lastIndexOf(Object o) { return list.lastIndexOf(o); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public ListIterator<T> listIterator() { return list.listIterator(); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public ListIterator<T> listIterator(int index) { return list.listIterator(index); }

    /**
     * Specified by java.util.List
     * @inheritDoc
     */
    public List<T> subList(int fromIndex, int toIndex) { return list.subList(fromIndex,toIndex); }
}
