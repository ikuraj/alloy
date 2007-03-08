package edu.mit.csail.sdg.alloy4;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * This generates unique names based on names provided by the caller.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class UniqueNameGenerator {

    /** This stores the set of names we've generated so far. */
    private final Set<String> names = new LinkedHashSet<String>();

    /** Construct a UniqueNameGenerator with a blank history. */
    public UniqueNameGenerator() { }

    /**
     * Regard the provided name as "seen".
     * <p> For convenience, it returns the argument as the return value.
     */
    public synchronized String seen(String name) { names.add(name); return name; }

    /**
     * Generate a unique name based on the input name.
     *
     * <p> Specifically: if the name has not been generated/seen already by this generator,
     * then it is returned as is. Otherwise, we append characters to it
     * until the name becomes unique.
     */
    public synchronized String make(String name) {
        if (name.length()==0) name="x";
        while(names.contains(name)) name=name+"'";
        names.add(name);
        return name;
    }
}
