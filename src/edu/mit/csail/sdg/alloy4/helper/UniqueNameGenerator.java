package edu.mit.csail.sdg.alloy4.helper;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * This class generates unique names based on names provided by the caller.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @author Felix Chang
 */

public final class UniqueNameGenerator {

    /** This stores the set of names we've generated so far. */
    private final Set<String> names = new LinkedHashSet<String>();

    /** Construct a UniqueNameGenerator with a blank history. */
    public UniqueNameGenerator() { }

    /**
     * Generate a unique name based on the input name.
     *
     * <p/> If the input name has not been generated already by this generator,
     * then it is returned as is. Otherwise, we append characters to it
     * until the name becomes unique.
     */
    public synchronized String make(String name) {
        while(names.contains(name)) name=name+"'";
        names.add(name);
        return name;
    }
}
