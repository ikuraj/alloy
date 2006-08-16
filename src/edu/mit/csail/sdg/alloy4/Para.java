package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a toplevel paragraph in the model.
 * @author Felix Chang
 */

public abstract class Para {
	
	/** The filename, line and column position in the original file (can be null). */
	public final Pos pos;
	
	/** A valid path to the Unit containing this Para (can be an empty string). */
	public final String path;
	
	/** The name of this paragraph (can be an empty string). */
	public final String name;
	
	/**
	 * Constructs a new paragraph node
	 *
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing this paragraph (can be an empty string)
	 * @param name - the name of the paragraph (can be the empty string)
	 *
	 * @throws ErrorSyntax if name contains '@'
	 * @throws ErrorSyntax if name is "none", "iden", "univ", or "Int"
	 * @throws ErrorInternal if path==null or name==null
	 */
	public Para(Pos pos, String path, String name) {
		this.pos=pos;
		this.path=nonnull(path);
		this.name=nonnull(name);
		if (path.indexOf('@')>=0)
			throw syntaxError("Path \""+path+"\" must not contain \'@\' in it.");
		if (name.indexOf('@')>=0)
			throw syntaxError("Name \""+name+"\" must not contain \'@\' in it.");
		if (name.equals("none") ||
			name.equals("iden") ||
			name.equals("univ") ||
			name.equals("Int")) throw syntaxError("Name cannot be \""+name+"\"");
	}
	
	/** Convenience method that constructs a syntax error exception. */
	public final ErrorSyntax syntaxError(String s) {
		return new ErrorSyntax(pos, s);
	}
	
	/** Convenience method that constructs a type error exception. */
	public final ErrorType typeError(String s) {
		return new ErrorType(pos, this, s);
	}
	
	/** Convenience method that constructs an internal error exception. */
	public final ErrorInternal internalError(String s) {
		return new ErrorInternal(pos, this, s);
	}
	
	/**
	 * Convenience method that checks if <b>a</b> is null or not;
	 * (it returns <b>a</b> if nonnull, and throws an exception if null).
	 *
	 * @return <b>a</b> if it is not null
	 * @throws ErrorInternal if <b>a</b> is null
	 */
	public final<T> T nonnull(T a) {
		if (a==null) throw internalError("NullPointerException"); else return a;
	}
}
