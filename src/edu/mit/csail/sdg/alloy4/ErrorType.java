package edu.mit.csail.sdg.alloy4;

/**
 * Immutable;
 * represents a type error that should be reported to the user.
 *
 * <br/>
 * <br/> Invariant: msg!=null
 *
 * @author Felix Chang
 */

@SuppressWarnings("serial")
public final class ErrorType extends Err {

    /**
     * Constructs a new exception object.
     * @param pos - the filename/line/row information (null if unknown)
     * @param obj - the object that triggered the error (null if unknown)
     * @param msg - the actual error message.
     */
    public ErrorType(Pos pos, Object obj, String msg) { super(pos,obj,msg); }

    /** Returns a human-readable description of the error */
    @Override public String toString() {
        if (pos==null) return "Type error: "+msg;
        if (pos.filename.length()>0)
            return "Type error in "+pos.filename
            +" at line "+pos.y+" column "+pos.x+": "+msg;
        return "Type error at line "+pos.y+" column "+pos.x+": "+msg;
    }
}
