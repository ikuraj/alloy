package edu.mit.csail.sdg.alloy4.core;

/**
 * Immutable; represents a syntax error that should be reported to the user.
 *
 * <p/> <b>Invariant:</b> msg!=null
 *
 * @author Felix Chang
 */

@SuppressWarnings("serial")
public final class ErrorSyntax extends Err {

    /**
     * Constructs a new syntax error.
     * @param pos - the filename/line/row information (null if unknown)
     * @param msg - the actual error message
     */
    public ErrorSyntax(Pos pos, String msg) { super(pos,null,msg); }

    /** Returns a human-readable description of the error. */
    @Override public String toString() {
        if (pos==null) return "Syntax error: "+msg;
        if (pos.filename.length()>0)
            return "Syntax error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+": "+msg;
        return "Syntax error at line "+pos.y+" column "+pos.x+": "+msg;
    }
}
