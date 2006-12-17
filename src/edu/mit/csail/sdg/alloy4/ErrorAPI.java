package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents an API usage error.
 *
 * <p><b>Invariant:</b>      msg!=null
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class ErrorAPI extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new API usage error.
     * @param msg - the actual error message
     */
    public ErrorAPI(String msg) { super(null,msg); }

    /**
     * Constructs a new API usage error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorAPI(Pos pos, String msg) { super(pos,msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null) return "API usage error: "+msg;
        if (pos.filename.length()>0)
            return "API usage error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+":\n"+msg;
        return "API usage error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
