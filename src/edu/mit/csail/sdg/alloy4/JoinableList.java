package edu.mit.csail.sdg.alloy4;

import java.util.Iterator;

/** Immutable; implements an ordered list where it is very cheap to join two lists or to append an item to a list. */

public final class JoinableList<E> implements Iterable<E> {

    /** The number of items stored in this list. */
    private final int count;

    /** The list of items before "this.item"; can be null or even an empty list. */
    private final JoinableList<E> pre;

    /** If pre.count+post.count!=this.count, then this field stores the middle item; else this field is ignored. */
    private final E item;

    /** The list of items after "this.item"; can be null or even an empty list. */
    private final JoinableList<E> post;

    /** Construct an JoinableList object. */
    private JoinableList(int count, JoinableList<E> pre, E item, JoinableList<E> post) {
        this.count = count;
        this.pre = pre;
        this.item = item;
        this.post = post;
    }

    /** Construct a list containing a single item. */
    public JoinableList(E item) {
        this.count = 1;
        this.item = item;
        this.pre = null;
        this.post = null;
    }

    /** The empty list. */
    public static final JoinableList emptylist = new JoinableList<Object>(0,null,null,null);

    /** Returns the empty list. */
    @SuppressWarnings("unchecked")
    public static<E> JoinableList<E> emptylist() {
        return emptylist;
    }

    /** Returns a list that represents the concatenation of this and that. */
    public JoinableList<E> join(JoinableList<E> that) {
        if (that==null || that.count==0) return this;
        if (count==0) return that;
        if (post!=null && post.count>0) return new JoinableList<E>(count + that.count, this, null, that);
        return new JoinableList<E>(count + that.count, this.pre, this.item, that);
    }

    /** Returns a list that represents the result of appending the given item onto the end of this list. */
    public JoinableList<E> append(E item) {
        if (post!=null && post.count>0) return new JoinableList<E>(count+1, this, item, null);
        int preCount = (this.pre==null) ? 0 : (this.pre.count);
        if (count==preCount) return new JoinableList<E>(count+1, this.pre, item, null);
        return new JoinableList<E>(count+1, this.pre, this.item, new JoinableList<E>(item));
    }

    /**
     * Return the i-th element
     * @throws ArrayIndexOutOfBoundsException if the given index doesn't exist
     */
    public E get(int i) {
        if (i<0 || i>=count) throw new ArrayIndexOutOfBoundsException();
        JoinableList<E> x = this;
        while(true) {
            int a = (x.pre==null) ? 0 : x.pre.count;
            if (i<a) { x=x.pre; continue; }
            int b = (x.post==null) ? 0 : x.post.count;
            if (a+b==x.count) { i=i-a; } else if (i!=a) { i=i-a-1; } else { return x.item; }
            x=x.post;
        }
    }

    /** Returns the number of elements in this list. */
    public int size() { return count; }

    /** Returns true iff the list is empty. */
    public boolean isEmpty() { return count==0; }

    /** Returns a String representation of this list. */
    @Override public String toString() {
        StringBuilder sb=new StringBuilder("[");
        boolean first=true;
        for(Object x:this) {
            if (first) { first=false; } else { sb.append(", "); }
            if (x==null) sb.append("null");
            else if (x==this) sb.append("(this collection)");
            else sb.append(x.toString());
        }
        return sb.append(']').toString();
    }

    /** Computes a hash code that is consistent with JoinableList's equals() and java.util.List's hashCode() methods. */
    @Override public int hashCode() {
        int answer=1;
        for(Object obj:this) {
            answer=31*answer;
            if (obj!=null) answer=answer+obj.hashCode();
        }
        return answer;
    }

    /** Returns true if (that instanceof JoinableList), and that contains the same elements as this list. */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof JoinableList)) return false;
        JoinableList x=(JoinableList)that;
        int n=count;
        if (count!=x.count) return false;
        Iterator a=iterator(), b=x.iterator();
        for(int i=0; i<n; i++) {
            Object aa=a.next(), bb=b.next();
            if (aa==null) {
                if (bb!=null) return false;
            } else {
                if (!aa.equals(bb)) return false;
            }
        }
        return true;
    }

    /** Returns true if the list contains the given element. */
    public boolean contains(Object item) {
        for(E entry:this) {
            if (entry==null) {
                if (item==null) return true;
            } else {
                if (entry.equals(item)) return true;
            }
        }
        return false;
    }

    /** Returns a readonly iterator that iterates over the elements in this list. */
    public Iterator<E> iterator() {
        return new Iterator<E>() {
            private int i=0;
            @SuppressWarnings("unchecked")
            public final E next() { i++; return get(i-1); }
            public final boolean hasNext() { return i < count; }
            public final void remove() { throw new UnsupportedOperationException(); }
        };
    }
}
