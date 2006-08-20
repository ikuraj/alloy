package edu.mit.csail.sdg.alloy4;

/**
 * Immutable;
 * represents an internal error that should be reported to the developers.
 *
 * <br/>
 * <br/> Invariant: msg!=null
 *
 * @author Felix Chang
 */

public final class ErrorInternal extends RuntimeException {

	/** This silences the javac warning about serialVersionUID not defined */
	private static final long serialVersionUID=0;

	/** This stores the filename/line/row information (null if unknown) */
	public final Pos pos;

	/** The actual error message. */
	public final String msg;

	/** The object that triggered the error (null if unknown) */
	public final Object obj;

	/**
	 * Constructs a new exception object.
	 * @param pos - the filename/line/row information (null if unknown)
	 * @param obj - the object that triggered the error (null if unknown)
	 * @param msg - the actual error message.
	 */
	public ErrorInternal(Pos pos, Object obj, String msg) {
		this.pos=pos;
		this.obj=obj;
		this.msg=(msg==null?"":msg);
	}

	/** Returns a human-readable description of the error */
	@Override public String toString() {
		if (pos==null) return "Internal error: "+msg;
		if (pos.filename.length()>0)
			return "Internal error in "+pos.filename
			+" at line "+pos.y+" column "+pos.x+": "+msg;
		return "Internal error at line "+pos.y+" column "+pos.x+": "+msg;
	}
}
