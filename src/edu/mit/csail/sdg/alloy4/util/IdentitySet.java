package edu.mit.csail.sdg.alloy4.util;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Mutable; this implements a set based on object reference identity; the null value is allowed.
 *
 * <p/>
 * Since this violates java.util.Set's contract of using object equality,
 * this class intentionally does not implement the java.util.Set interface.
 *
 * @author Felix Chang
 *
 * @param <T> - the type of element
 */

public final class IdentitySet<T> implements Iterable<T> {

	/** This map's key set is used to store the set of elements; the values are ignored. */
	private final Map<T,Object> map = new IdentityHashMap<T,Object>();

	/** Constructs an empty set. */
	public IdentitySet() { }
	
	/** Returns whether the element x is in the set. */
	public boolean contains(T x) { return map.containsKey(x); }
	
	/** Adds the element x into the set (if it isn't in the set already). */
	public void add(T x) { map.put(x,null); }
	
	/** Removes the element x from the set (if it is in the set). */
	public void remove(T x) { map.remove(x); }

	/**
	 * Returns an iterator that iterates over elements in this set;
	 * modification via the iterator has undefined behavior.
	 */ 
	public Iterator<T> iterator() { return map.keySet().iterator(); }
}
