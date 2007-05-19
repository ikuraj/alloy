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

    /**
     * Mutable; this implements a modifiable list that can be used to construct a ConstList.
     *
     * <p><b>Thread Safety:</b>  Not safe.
     *
     * @param <T> - the type of element
     */
    public static final class TempList<T> {
        /** The underlying list. */
        private final ArrayList<T> list;
        /** Nonnull iff this list is no longer modifiable. */
        private ConstList<T> clist=null;
        /** Construct a new empty modifiable TempList. */
        public TempList()                     { this.list = new ArrayList<T>(); }
        /** Construct a new empty modifiable TempList with an initial capacity of n. */
        public TempList(int n)                { this.list = new ArrayList<T>(n); }
        /** Construct a new modifiable TempList with the initial content being n references to the given elem. */
        public TempList(int n, T elem)        { this.list = new ArrayList<T>(n); while(n>0) {list.add(elem); n--;} }
        /** Construct a new modifiable TempList with the initial content equal to the given collection. */
        public TempList(Collection<T> collection) { this.list = new ArrayList<T>(collection); }
        /** Returns the size of the list. */
        public int size()                     { return list.size(); }
        /** Returns the i-th element. */
        public T get(int index)               { return list.get(index); }
        /** Removes then returns the i-th element. */
        public T remove(int index)            { if (clist!=null) throw new UnsupportedOperationException(); return list.remove(index); }
        /** Removes the first occurrence of the element (if it exists). */
        public boolean remove(T elem)         { if (clist!=null) throw new UnsupportedOperationException(); return list.remove(elem); }
        /** Add the given element at the given index. */
        public void add(int index, T elem)    { if (clist!=null) throw new UnsupportedOperationException(); list.add(index,elem); }
        /** Append the given element to the list. */
        public void add(T elem)               { if (clist!=null) throw new UnsupportedOperationException(); list.add(elem); }
        /** Append the elements in the given collection to the list. */
        public void addAll(Collection<T> all) { if (clist!=null) throw new UnsupportedOperationException(); list.addAll(all); }
        /** Changes the i-th element to be elem. */
        public void set(int index, T elem)    { if (clist!=null) throw new UnsupportedOperationException(); list.set(index,elem); }
        /** Turns this TempList unmodifiable, then construct a ConstList backed by this TempList. */
        public ConstList<T> makeConst()       { if (clist==null) if (list.size()==0) clist=ConstList.make(); else clist=new ConstList<T>(list); return clist; }
    }

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The underlying Collections.unmodifiableList list. */
    private final List<T> list;

    /** This caches an unmodifiable empty list. */
    private static final ConstList<Object> emptylist = new ConstList<Object>(new ArrayList<Object>(0));

    /** Construct an unmodifiable list with the given arraylist as its backing store. */
    private ConstList(List<? extends T> list) {
        this.list=Collections.unmodifiableList(list);
    }

    /** Return an unmodifiable empty list. */
    @SuppressWarnings("unchecked")
    public static<T> ConstList<T> make() {
        return (ConstList<T>) emptylist;
    }

    /** Return an unmodifiable list consisting of "n" references to "elem". */
    @SuppressWarnings("unchecked")
    public static<T> ConstList<T> make(int n, T elem) {
        if (n<=0) return (ConstList<T>) emptylist;
        ArrayList<T> ans=new ArrayList<T>(n);
        while(n>0) { ans.add(elem); n--; }
        return new ConstList<T>(ans);
    }

    /**
     * Return an unmodifiable list with the same elements as the given collection.
     * (If collection==null, we'll return an unmodifiable empty list)
     */
    @SuppressWarnings("unchecked")
    public static<T> ConstList<T> make(Iterable<? extends T> collection) {
        if (collection instanceof ConstList) return (ConstList<T>)collection;
        if (collection==null || !collection.iterator().hasNext()) return (ConstList<T>)emptylist;
        ArrayList<T> ans=new ArrayList<T>();
        for(T elem:collection) ans.add(elem);
        return new ConstList<T>(ans);
    }

    /**
     * Return an unmodifiable list with the same elements as the given collection.
     * (If collection==null, we'll return an unmodifiable empty list)
     */
    @SuppressWarnings("unchecked")
    public static<T> ConstList<T> make(Collection<? extends T> collection) {
        if (collection instanceof ConstList) return (ConstList<T>)collection;
        if (collection==null || collection.isEmpty()) return (ConstList<T>)emptylist;
        return new ConstList<T>(new ArrayList<T>(collection));
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
