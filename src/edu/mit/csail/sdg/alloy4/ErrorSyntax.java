package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a syntax error that should be reported to the user.
 *
 * <p><b>Invariant:</b>      msg!=null
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class ErrorSyntax extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new syntax error.
     * @param msg - the actual error message
     */
    public ErrorSyntax(String msg) { super(null,msg); }

    /**
     * Constructs a new syntax error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorSyntax(Pos pos, String msg) { super(pos,msg); }

    /** Returns the same Err, but with the position changed. */
    @Override public Err changePosition(Pos pos) { return new ErrorSyntax(pos, this.msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null) return "Syntax error: "+msg;
        if (pos.filename.length()>0)
            return "Syntax error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+":\n"+msg;
        return "Syntax error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
