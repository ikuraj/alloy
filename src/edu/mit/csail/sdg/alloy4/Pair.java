package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; holds a pair of references.
 *
 * @author Felix Chang
 */

public final class Pair<X,Y> {

	/** The first value held in this pair. */
	public final X x;

	/** The second value held in this pair. */
	public final Y y;

	/**
	 * Constructs a new Pair object.
	 *
	 * @param x - the first half of the pair
	 * @param y - the second half of the pair
	 */
	public Pair(X x, Y y) { this.x=x; this.y=y; }
}
