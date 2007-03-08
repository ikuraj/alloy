package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; this is the abstract super class of the various possible errors.
 *
 * <p><b>Invariant</b>:       pos!=null and msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public abstract class Err extends Exception {

    /** This stores the filename/line/row information (Pos.UNKNOWN if unknown). */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public Err(Pos pos, String msg) {
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
        this.msg = (msg==null ? "" : msg);
    }

    /** Returns a textual description of the error. */
    @Override public final String getMessage() {
        return toString();
    }

    /**
     * Constructs a new Err object with the same message, but with a new position.
     * @param pos - the new filename/line/row information (can be null if unknown)
     */
    public abstract Err changePosition(Pos pos);
}
