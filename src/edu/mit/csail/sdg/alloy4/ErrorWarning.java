package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a warning that should be reported to the user.
 *
 * <p><b>Invariant:</b>      msg!=null
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class ErrorWarning extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new warning.
     * @param msg - the actual error message
     */
    public ErrorWarning(String msg) { super(null,msg); }

    /**
     * Constructs a new warning.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorWarning(Pos pos, String msg) { super(pos,msg); }

    /** Returns the same Err, but with the position changed. */
    @Override public Err changePosition(Pos pos) { return new ErrorWarning(pos, this.msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null || pos==Pos.UNKNOWN) return msg;
        if (pos.filename.length()>0) return "Line "+pos.y+" column "+pos.x+" in "+pos.filename+":\n"+msg;
        return "Line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
