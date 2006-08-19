package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; holds a pair of references.
 *
 * @author Felix Chang
 */

public final class Pair<A,B> {
	
  /** The first value held in this pair. */
  public final A a;

  /** The second value held in this pair. */
  public final B b;

  /** Constructs a new Pair object */
  public Pair(A a, B b) { this.a=a; this.b=b; }
}
