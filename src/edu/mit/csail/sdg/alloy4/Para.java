package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a toplevel paragraph in the model.
 * @author Felix Chang
 */

public abstract class Para {
	
	/** The filename, line, and column position in the original file (can be null). */
	public final Pos pos;
	
	/**
	 * A valid path to the Unit containing this Para (can be "" if the Unit is the main unit).
	 *
	 * <p/>
	 * The main unit that the user runs the analyzer on is the "" unit.
	 * 
	 * <p/>
	 * If the main unit imports a file as "a" and imports another file as "b",
	 * then these 2 units can be referred to by "a" and "b", respectively.
	 * 
	 * <p/>
	 * If "a" imports some file as "aa", then the imported file can be referred to by "a/aa".
	 * Etc. Etc. Etc.
	 * 
	 * <p/>
	 * Note: the same unit can be referred to by multiple path.
	 * For example, if a unit X is opened from many places, with different aliases,
	 * but the user has indicated that these places should all import a shared copy of X,
	 * then X will have more than one valid path.
	 * (Hence the wording: the Para.path field has to be ONE of the valid path.)
	 */
	public final String path;
	
	/** The name of this paragraph (can be ""). */
	public final String name;
	
	/**
	 * Constructs a new paragraph node
	 *
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing this paragraph (can be "")
	 * @param name - the name of the paragraph (can be "")
	 *
	 * @throws ErrorSyntax if the name contains '@'
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if the name is "none", "iden", "univ", or "Int"
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
