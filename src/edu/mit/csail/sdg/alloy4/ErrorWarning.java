package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a nonfatal warning that should be reported to the user.
 *
 * <p><b>Invariant:</b>      pos!=null && msg!=null
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
    public ErrorWarning(String msg) {
        super(null,msg);
    }

    /**
     * Constructs a new warning.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public ErrorWarning(Pos pos, String msg) {
        super(pos,msg);
    }

    /**
     * Constructs a new ErrorWarning object with the same message, but with the new position.
     * @param pos - the filename/line/row information (can be null if unknown)
     */
    @Override public ErrorWarning changePosition(Pos pos) {
        return new ErrorWarning(pos, this.msg);
    }

    /** Returns a textual description of the error. */
    @Override public String toString() {
        if (pos==Pos.UNKNOWN) {
            return msg;
        }
        if (pos.filename.length()>0) {
            return "Line "+pos.y+" column "+pos.x+" in "+pos.filename+":\n"+msg;
        }
        return "Line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
