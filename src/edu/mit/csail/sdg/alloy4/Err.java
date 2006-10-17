package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; this is the abstract super class of the various possible errors.
 *
 * <p/><b>Invariant</b>:      msg!=null
 *
 * <p/><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 *
 * @author Felix Chang
 */

public abstract class Err extends RuntimeException {

    /** This stores the filename/line/row information (null if unknown). */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /**
     * Constructs a new error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public Err(Pos pos, String msg) {
        this.pos=pos;
        this.msg=(msg==null?"":msg);
    }
}
