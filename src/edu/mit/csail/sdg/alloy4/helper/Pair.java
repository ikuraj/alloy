package edu.mit.csail.sdg.alloy4.helper;

/**
 * Immutable; stores a pair of object references.
 *
 * @author Felix Chang
 */

public final class Pair<A,B> {

    /** The first half of the pair. */
    public final A a;

    /** The second half of the pair. */
    public final B b;

    /** Constructs a new Pair object. */
    public Pair(A a, B b) { this.a=a; this.b=b; }
}
