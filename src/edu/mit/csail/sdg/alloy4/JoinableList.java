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

import java.io.Serializable;
import java.util.List;
import java.util.Iterator;
import java.util.NoSuchElementException;

/** Immutable; implements an ordered list where it is cheap to join or append; null values are NOT allowed. */

public final class JoinableList<E> implements Serializable, Iterable<E> {

    // Invariant: pre!=null iff pre.count!=0
    // Invariant: post!=null iff post.count!=0
    // Invariant: count == (pre!=null ? pre.count : 0) + (item!=null ? 1 : 0) + (post!=null ? post.count : 0)

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The number of items stored in this list. */
    private final int count;

    /** The list of items before "this.item"; can be null or even an empty list. */
    private final JoinableList<E> pre;

    /** If nonnull, it stores an item. */
    private final E item;

    /** The list of items after "this.item"; can be null or even an empty list. */
    private final JoinableList<E> post;

    /** Construct a JoinableList object. */
    private JoinableList(int count, JoinableList<E> pre, E item, JoinableList<E> post) {
        this.count = count;
        this.pre = pre;
        this.item = item;
        this.post = post;
    }

    /** Construct a list containing a single item, or return an empty list if item==null. */
    public JoinableList(E item) {
        this.count = (item!=null ? 1 : 0);
        this.pre = null;
        this.item = item;
        this.post = null;
    }

    /** Construct an empty list. */
    public JoinableList() {
        this.count = 0;
        this.item = null;
        this.pre = null;
        this.post = null;
    }

    /** Returns a list that represents the concatenation of this list and that list. */
    public JoinableList<E> join(JoinableList<E> that) {
        if (that==null || that.count==0) return this;
        if (count==0) return that;
        int sum = count + that.count;
        if (sum<count) throw new OutOfMemoryError();
        if (post!=null) return new JoinableList<E>(sum, this, null, that);
        return new JoinableList<E>(sum, pre, item, that);
    }

    /** Returns a list that represents the result of appending newItem onto this list; if newItem==null we return this list as-is. */
    public JoinableList<E> append(E newItem) {
        if (newItem==null) return this; else if (count==0) return new JoinableList<E>(newItem);
        int sum = count + 1;
        if (sum<1) throw new OutOfMemoryError();
        if (post!=null) return new JoinableList<E>(sum, this, newItem, null);
        if (item!=null) return new JoinableList<E>(sum, pre, item, new JoinableList<E>(newItem));
        return new JoinableList<E>(sum, pre, newItem, null);
    }

    /** If the list if nonempty, arbitrarily return one of the item, otherwise throw NoSuchElementException. */
    public E pick() { if (count==0) throw new NoSuchElementException(); else return get(0); }

    /**
     * Return the i-th element
     * @throws ArrayIndexOutOfBoundsException if the given index doesn't exist
     */
    private E get(int i) {
        if (i<0 || i>=count) throw new ArrayIndexOutOfBoundsException();
        JoinableList<E> x = this;
        while(true) {
            int pre = (x.pre==null) ? 0 : x.pre.count;
            if (i<pre) { x=x.pre; continue; }
            if (x.item==null) { i=i-pre; x=x.post; } else if (i!=pre) { i=i-pre-1; x=x.post; } else return x.item;
        }
    }

    /** Returns the number of elements in this list. */
    public int size() { return count; }

    /** Returns true iff the list is empty. */
    public boolean isEmpty() { return count==0; }

    /** Returns a String representation of this list. */
    @Override public String toString() {
        StringBuilder sb = new StringBuilder("[");
        boolean first = true;
        for(Object x: this) {
            if (first) first=false; else sb.append(", ");
            sb.append(x);
        }
        return sb.append(']').toString();
    }

    /** Computes a hash code that is consistent with JoinableList's equals() and java.util.List's hashCode() methods. */
    @Override public int hashCode() {
        int answer = 1;
        for(Object x: this) { answer = 31*answer + x.hashCode(); }
        return answer;
    }

    /** Returns true if that is a List or JoinableList, and contains the same elements as this list. */
    @SuppressWarnings("unchecked")
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        Iterator<?> b;
        if (that instanceof JoinableList) { JoinableList x=(JoinableList)that; if (count!=x.size()) return false; b=x.iterator(); }
           else if (that instanceof List) { List x=(List)that; if (count!=x.size()) return false; b=x.iterator(); }
           else return false;
        Iterator<?> a=iterator();
        for(int i=0; i<count; i++) if (!a.next().equals(b.next())) return false;
        return true;
    }

    /** Returns a readonly iterator that iterates over the elements in this list. */
    public Iterator<E> iterator() {
        return new Iterator<E>() {
            private int i = 0;
            public final E next() { if (i>=count) throw new NoSuchElementException(); E ans=get(i); i++; return ans; }
            public final boolean hasNext() { return i < count; }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }
}
