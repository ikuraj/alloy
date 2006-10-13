package edu.mit.csail.sdg.alloy4.helper;

/**
 * Immutable; represents an internal error that should be reported to the developers.
 *
 * <p/><b>Invariant:</b>      msg!=null
 *
 * <p/><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 *
 * @author Felix Chang
 */

public final class ErrorInternal extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new internal error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorInternal(Pos pos, String msg) { super(pos,msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null) return "Internal error: "+msg;
        if (pos.filename.length()>0)
            return "Internal error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+":\n"+msg;
        return "Internal error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
