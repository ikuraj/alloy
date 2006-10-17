package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; stores a pair of object references.
 *
 * <p/><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 *
 * @author Felix Chang
 */

public final class Pair<A,B> {

    /** The first half of the pair. */
    public final A a;

    /** The second half of the pair. */
    public final B b;

    /** Constructs a new Pair object (a,b). */
    public Pair(A a, B b) { this.a=a; this.b=b; }
}
