package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Mutable; represents a "fact".
 *
 * <p/> <b>Invariant:</b>  value!=null
 *
 * @author Felix Chang
 */

public final class ParaFact extends Para {

    /** The fact. */
    public Expr value;

    /**
     * Constructs a new ParaFact object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this paragraph
     * @param name - the name of the fact (can be "")
     * @param value - the fact
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name contains '@' or '/'
     * @throws ErrorInternal if pos==null, path==null, name==null, or value==null
     */
    public ParaFact(Pos pos, String path, String name, Expr value) {
        super(pos, path, name);
        this.value=nonnull(value);
    }
}
