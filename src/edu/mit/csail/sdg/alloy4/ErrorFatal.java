package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a fatal error.
 *
 * <p><b>Invariant:</b>      msg!=null
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class ErrorFatal extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new fatal error.
     * @param msg - the actual error message
     */
    public ErrorFatal(String msg) { super(null,msg); }

    /**
     * Constructs a new fatal error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorFatal(Pos pos, String msg) { super(pos,msg); }

    /** Returns the same Err, but with the position changed. */
    @Override public Err changePosition(Pos pos) { return new ErrorFatal(pos, this.msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null) return "Fatal error: "+msg;
        if (pos.filename.length()>0)
            return "Fatal error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+":\n"+msg;
        return "Fatal error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
