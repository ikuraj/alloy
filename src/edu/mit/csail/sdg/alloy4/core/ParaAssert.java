package edu.mit.csail.sdg.alloy4.core;

/**
 * Mutable; represents an "assertion".
 *
 * <br/>
 * <br/> Invariant: value!=null
 *
 * @author Felix Chang
 */

public final class ParaAssert extends Para {

    /** The formula being asserted. */
    public Expr value;

    /**
     * Constructs a new ParaAssert object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this paragraph (can be "" if it's the main unit)
     * @param name - the name of the assertion (can be "")
     * @param value - the formula being asserted
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name contains '@' or '/'
     * @throws ErrorInternal if pos==null, path==null, name==null, or value==null
     */
    public ParaAssert(Pos pos, String path, String name, Expr value) {
        super(pos, path, name);
        this.value=nonnull(value);
    }
}
