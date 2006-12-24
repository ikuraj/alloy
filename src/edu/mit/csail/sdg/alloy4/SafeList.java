package edu.mit.csail.sdg.alloy4;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Mutable; this implements a list that does not support the remove() and set() methods.
 *
 * <p>
 * By making this sacrifice, we are able to provide an iterator
 * that returns the elements in the order they were inserted,
 * and allow concurrent insertion and iteration (that is, we can iterate
 * over the list while adding elements to the list at the same time).
 * The iterator is guaranteed to iterate over exactly the elements
 * that existed at the time that the iterator was created.
 * 
 * <p>
 * This avoids many unnecessary list copies, while preserving data integrity.
 *
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <T> - the type of element
 */

public final class SafeList<T> implements Iterable<T> {

    /** The actual list of elements. */
    private final List<T> list;

    /** Constructs an empty list. */
    public SafeList() { list=new ArrayList<T>(); }

    /** Constructs an empty list with the initial capacity. */
    public SafeList(int initialCapacity) { list=new ArrayList<T>(initialCapacity); }

    /** Constructs a list containing the elements from the given collection. */
    public SafeList(Collection<T> initialValue) { list=new ArrayList<T>(initialValue); }
    
    /** Add an element into the list. */
    public synchronized void add(T item) { list.add(item); }

    /** Get an element from the list. */
    public synchronized T get(int i) { return list.get(i); }

    /** Returns the size of the list. */
    public synchronized int size() { return list.size(); }

    /**
     * Returns an iterator that iterates over elements in this set
     * (in the order that they were inserted).
     *
     * <p> Note: This iterator's remove() method always throws UnsupportedOperationException.
     *
     * <p> Note: This iterator always returns exactly the set of elements that existed
     * at the time that the iterator was created (even if the list is modified after that point).
     */
    public synchronized Iterator<T> iterator() {
        return new Iterator<T>() {
            private final int max=list.size();
            private int now=0;
            public final boolean hasNext() { return now<max; }
            public final T next() {
                if (now>=max) throw new NoSuchElementException();
                synchronized(SafeList.this) { T answer=list.get(now); now++; return answer; }
            }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }
}
