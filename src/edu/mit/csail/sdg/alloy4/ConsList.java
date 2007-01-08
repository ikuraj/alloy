package edu.mit.csail.sdg.alloy4;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * This list is immutable, but allows you to build up longer lists without duplicating the original sublist.
 *
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <T> - the type of element
 */

public final class ConsList<T> implements Iterable<T> {

    /** This points to the next node in this list. */
    private final ConsList<T> next;

    /** The number of elements in this list. */
    private final int size;

    /** If size!=0, this is the element in this node; otherwise, this field is ignored. */
    private final T elem;

    /** Construct a list with the given values. */
    private ConsList(int size, T elem, ConsList<T> next) { this.size=size; this.elem=elem; this.next=next; }

    /** Construct an empty list. */
    public ConsList() { size=0; elem=null; next=null; }

    /** Construct a new list "elem+this" */
    public ConsList<T> concat(T elem) { return new ConsList<T>(size+1,elem,this); }

    /** Construct a new list "elem+this" if elem isn't already in this list; otherwise, the method will return "this". */
    public ConsList<T> concatIfNew(T elem) { if (contains(elem)) return this; else return new ConsList<T>(size+1,elem,this); }

    /** Computes a hash code that is consistent with ConsList's equals() and java.util.List's hashCode() methods. */
    @Override public int hashCode() {
        int answer=1;
        for(Object obj:this) {
            answer=31*answer;
            if (obj!=null) answer=answer+obj.hashCode();
        }
        return answer;
    }

    /** Returns true if (that instanceof ConsList), and that contains the same elements as this list. */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof ConsList)) return false;
        ConsList x=(ConsList)that;
        if (size!=x.size) return false;
        Iterator a=iterator(), b=x.iterator();
        while(a.hasNext()) {
            Object aa=a.next(), bb=b.next();
            if (aa==null) { if (bb!=null) return false; } else { if (!aa.equals(bb)) return false; }
        }
        return true;
    }

    /** Returns true if the list contains the given element. */
    public boolean contains(Object item) {
        for(T entry:this) {
            if (entry==null) { if (item==null) return true; } else { if (entry.equals(item)) return true; }
        }
        return false;
    }

    /** Returns the size of the list. */
    public int size() { return size; }

    /** Returns true if the list is empty. */
    public boolean isEmpty() { return size==0; }

    /**
     * Returns an iterator that iterates over elements in this list
     * (in the order that they were inserted).
     *
     * <p> Note: This iterator's remove() method always throws UnsupportedOperationException.
     */
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            private ConsList<T> ptr=ConsList.this;
            public final boolean hasNext() { return ptr.size>0; }
            public final T next() {
                if (ptr.size==0) throw new NoSuchElementException();
                T answer=ptr.elem;
                ptr=ptr.next;
                return answer;
            }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }
}
