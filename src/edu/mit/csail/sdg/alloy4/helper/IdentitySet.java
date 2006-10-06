package edu.mit.csail.sdg.alloy4.helper;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Mutable; this implements a set based on reference identity; the null value is allowed.
 *
 * <p/>
 * Since this violates java.util.Set's contract of using object equality,
 * this class intentionally does not implement the java.util.Set interface.
 *
 * <p/>
 * For simplicity, and since the remove() feature isn't needed,
 * this implementation does not provide the remove() method.
 *
 * <p/>
 * By making this simplification, we are able to ensure its iterator
 * always returns the elements in the order they were inserted.
 *
 * <p/><b>Invariant:</b> (hashmap.containsKey(x)) iff (x==list.get(i) for some i)
 *
 * @author Felix Chang
 *
 * @param <T> - the type of element
 */

public final class IdentitySet<T> implements Iterable<T> {

    /** This map's key set is used to store the set of elements; the values are ignored. */
    private final IdentityHashMap<T,Object> hashmap=new IdentityHashMap<T,Object>();

    /**
     * This array also stores the set of elements;
     * this allows the iterator to return the elements in a deterministic order.
     */
    private final List<T> list=new ArrayList<T>();

    /** Constructs an empty set. */
    public IdentitySet() { }

    /** Returns whether the element x is in the set. */
    public boolean contains(T x) { return hashmap.containsKey(x); }

    /** Adds the element x into the set; if x is already in the set, then this method does nothing. */
    public void add(T x) { if (!hashmap.containsKey(x)) { hashmap.put(x,null); list.add(x); } }

    /**
     * Returns an iterator that iterates over elements in this set
     * (in the order that they were inserted).
     *
     * <p/> Note: This iterator's remove() method always throws UnsupportedOperationException.
     *
     * <p/> Note: This iterator always returns exactly the set of elements that existed
     * at the time that the iterator was created (even if the set is modified after that point).
     */
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            private final int max=list.size();
            private int now=0;
            public final boolean hasNext() { return now<max; }
            public final T next() { if (now>=max) throw new NoSuchElementException(); now++; return list.get(now-1); }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }
}
