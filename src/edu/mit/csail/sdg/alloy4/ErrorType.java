package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a type error that should be reported to the user.
 *
 * <p><b>Invariant:</b>       pos!=null && msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public final class ErrorType extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new type error.
     * @param msg - the actual error message
     */
    public ErrorType(String msg) {
        super(null,msg);
    }

    /**
     * Constructs a new type error.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public ErrorType(Pos pos, String msg) {
        super(pos,msg);
    }

    /**
     * Constructs a new ErrorType object with the same message, but with the new position.
     * @param pos - the filename/line/row information (can be null if unknown)
     */
    @Override public ErrorType changePosition(Pos pos) {
        return new ErrorType(pos, this.msg);
    }

    /** Returns a textual description of the error. */
    @Override public String toString() {
        if (pos==Pos.UNKNOWN) {
            return "Type error: "+msg;
        }
        if (pos.filename.length()>0) {
            return "Type error in "+pos.filename+" at line "+pos.y+" column "+pos.x+":\n"+msg;
        }
        return "Type error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
