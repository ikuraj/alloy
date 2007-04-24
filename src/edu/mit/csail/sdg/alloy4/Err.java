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

    /** The additional stack trace information. */
    private final StackTraceElement[] trace;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public Err(Pos pos, String msg, StackTraceElement[] trace) {
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
        this.msg = (msg==null ? "" : msg);
        if (trace==null || trace.length==0) {
            this.trace=null;
        } else {
            this.trace = new StackTraceElement[trace.length];
            for(int i=0; i<this.trace.length; i++) this.trace[i]=trace[i];
        }
    }

    /** Retrieves the complete stack trace as a String */
    public final String getTotalTrace() {
        StringBuilder sb=new StringBuilder();
        if (trace!=null) for(StackTraceElement st: trace) { sb.append(st.toString()); sb.append("\n"); }
        for(StackTraceElement st: getStackTrace()) { sb.append(st.toString()); sb.append("\n"); }
        return sb.toString();
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
