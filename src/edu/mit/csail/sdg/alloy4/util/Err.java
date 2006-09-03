package edu.mit.csail.sdg.alloy4.util;

/**
 * Immutable; this is the abstract super class of the various possible errors.
 *
 * <p/> <b>Invariant</b>: msg!=null
 *
 * @author Felix Chang
 */

public abstract class Err extends RuntimeException {

    /** This stores the filename/line/row information (null if unknown). */
    public final Pos pos;

    /** The object that triggered the error (null if unknown). */
    public final Object obj;

    /** The actual error message. */
    public final String msg;

    /**
     * Constructs a new error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param obj - the object that triggered the error (null if unknown)
     * @param msg - the actual error message
     */
    public Err(Pos pos, Object obj, String msg) {
        this.pos=pos;
        this.obj=obj;
        this.msg=(msg==null?"":msg);
    }
}
