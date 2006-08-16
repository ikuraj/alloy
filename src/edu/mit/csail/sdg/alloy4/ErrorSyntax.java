package edu.mit.csail.sdg.alloy4;


/**
 * Immutable;
 * represents a syntax error that should be reported to the user.
 * @author Felix Chang
 */

public final class ErrorSyntax extends RuntimeException {
	
	/** This silences the javac warning about serialVersionUID not defined. */
	private static final long serialVersionUID=0;
	
	/** This stores the filename/line/row information (null if unknown) */
	public final Pos pos;
	
	/** The actual error message. */
	public final String msg;
	
	/**
	 * Constructs a new exception object.
	 * @param pos - the filename/line/row information (null if unknown)
	 * @param msg - the actual error message.
	 */
	public ErrorSyntax(Pos pos, String msg) {
		this.pos=pos;
		this.msg=msg;
	}
	
	/** Returns a human-readable description of the error */
	@Override public String toString() {
		if (pos==null) return "Syntax error: "+msg;
		if (pos.filename.length()>0)
			return "Syntax error in "+pos.filename
			+" at line "+pos.y+" column "+pos.x+": "+msg;
		return "Syntax error at line "+pos.y+" column "+pos.x+": "+msg;
	}
}
