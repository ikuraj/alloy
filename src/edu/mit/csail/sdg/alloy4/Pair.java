package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; stores a pair of object references; Pair.equals() compares by value.
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Pair<A,B> {

    /** The first half of the pair. */
    public final A a;

    /** The second half of the pair. */
    public final B b;

    /** Constructs a new Pair object (a,b). */
    public Pair(A a, B b) { this.a=a; this.b=b; }

    /** Returns a hashcode based on (a==null?0:a.hashCode()) and (b==null?0:b.hashCode()). */
    @Override public int hashCode() {
        int i = (a==null) ? 0 : a.hashCode();
        int j = (b==null) ? 0 : b.hashCode();
        return i*173124+j;
    }

    /** Pairs (a1,b1) and (a2,b2) are equal iff a1.equals(a2) and b1.equals(b2). */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Pair)) return false;
        Pair p=(Pair)that;
        if (a==null) { if (p.a!=null) return false; } else { if (!a.equals(p.a)) return false; }
        if (b==null) { if (p.b!=null) return false; } else { if (!b.equals(p.b)) return false; }
        return true;
    }
}
