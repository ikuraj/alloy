package edu.mit.csail.sdg.alloy4.core;

/**
 * Immutable;
 * this is the abstract super class of the various possible errors.
 *
 * <br/>
 * <br/> Invariant: msg!=null
 *
 * @author Felix Chang
 */

@SuppressWarnings("serial")
public abstract class Err extends RuntimeException {

    /** This stores the filename/line/row information (null if unknown) */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /** The object that triggered the error (null if unknown) */
    public final Object obj;

    /**
     * Constructs a new exception object.
     * @param pos - the filename/line/row information (null if unknown)
     * @param obj - the object that triggered the error (null if unknown)
     * @param msg - the actual error message.
     */
    public Err(Pos pos, Object obj, String msg) {
        this.pos=pos;
        this.obj=obj;
        this.msg=(msg==null?"":msg);
    }
}
