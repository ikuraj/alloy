package edu.mit.csail.sdg.alloy4.util;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * This class generates unique names based on names provided by callers.
 * 
 * @author Felix Chang
 */

public final class UniqueNameGenerator {
	
	private final Set<String> names = new LinkedHashSet<String>();

	/** Construct a blank UniqueNameGenerator. */
	public UniqueNameGenerator() { }
	
	/**
	 * Generate a unique name based on the input name.
	 * 
	 * <p/> If the input name has not been seen already by this generator,
	 * then it is returned as is. Otherwise, we append characters to it
	 * until the name becomes unique.
	 */
	public String make(String name) {
		while(names.contains(name)) name=name+"'";
		names.add(name);
		return name;
	}
}
