package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; this is the abstract super class of the various possible errors.
 *
 * <p><b>Invariant</b>:      msg!=null
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public abstract class Err extends Exception {

    /** This stores the filename/line/row information (null if unknown). */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public Err(Pos pos, String msg) {
        this.pos=(pos==null?Pos.UNKNOWN:pos);
        this.msg=(msg==null?"":msg);
    }

    /** Returns the same Err, but with the position changed. */
    public abstract Err changePosition(Pos pos);

    /** Calls the subclass's "toString()" method to return a textual representation of the error. */
    @Override public final String getMessage() { return toString(); }
}
