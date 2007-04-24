package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a syntax error that should be reported to the user.
 *
 * <p><b>Invariant:</b>       pos!=null && msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public final class ErrorSyntax extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new syntax error.
     * @param msg - the actual error message
     */
    public ErrorSyntax(String msg) {
        super(null,msg,null);
    }

    /**
     * Constructs a new syntax error.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public ErrorSyntax(Pos pos, String msg) {
        super(pos,msg,null);
    }

    /**
     * Constructs a new ErrorSyntax object with the same message, but with the new position.
     * @param pos - the new filename/line/row information (can be null if unknown)
     */
    @Override public ErrorSyntax changePosition(Pos pos) {
        return new ErrorSyntax(pos, this.msg);
    }

    /** Returns a textual description of the error. */
    @Override public String toString() {
        if (pos==Pos.UNKNOWN) {
            return "Syntax error:\n"+msg;
        }
        if (pos.filename.length()>0) {
            return "Syntax error in "+pos.filename+" at line "+pos.y+" column "+pos.x+":\n"+msg;
        }
        return "Syntax error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
