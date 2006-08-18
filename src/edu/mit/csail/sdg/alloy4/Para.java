package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a toplevel paragraph in the model.
 *
 * <br/>
 * <br/> Invariant: pos!=null && path!=null && name!=null
 * <br/> Invariant: path does not contain '@'
 * <br/> Invariant: name does not contain '@', nor does it contain '/'
 *
 * @author Felix Chang
 */

public abstract class Para {

	/** The filename, line, and column position in the original file (cannot be null). */
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
	 * If "a" imports some file as "aa", then the imported file can be referred to as "a/aa".
	 * Etc. Etc. Etc.
	 *
	 * <p/>
	 * Note: the same unit can be referred to by multiple paths.
	 * For example, if a unit is opened from many places,
	 * but the user has indicated that these places should all import a common shared copy,
	 * then there will be many paths that refer to that same unit.
	 * (Hence the wording above: the Para.path field only has to be ONE of the valid paths.)
	 */
	public final String path;

	/** The name of this paragraph (can be "") (cannot contain '/' nor '@') */
	public final String name;

	/**
	 * Constructs a new paragraph node.
	 *
	 * @param pos - the original position in the file
	 * @param path - a valid path to the Unit containing this paragraph (can be "" if it's the main unit)
	 * @param name - the name of the paragraph (can be "")
	 *
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if the name contains '@' or '/'
	 * @throws ErrorInternal if pos==null, path==null, or name==null
	 */
	public Para(Pos pos, String path, String name) {
		this.pos=nonnull(pos);
		this.path=nonnull(path);
		this.name=nonnull(name);
		if (path.indexOf('@')>=0) throw syntaxError("Path \""+path+"\" must not contain \'@\'");
		if (name.indexOf('@')>=0) throw syntaxError("Name \""+name+"\" must not contain \'@\'");
		if (name.indexOf('/')>=0) throw syntaxError("Name \""+name+"\" must not contain \'/\'");
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
	 * Convenience method that checks if x is null or not;
	 * (it returns x if nonnull, and throws an exception if null).
	 *
	 * @return x if it is not null
	 * @throws ErrorInternal if x is null
	 */
	public final<T> T nonnull(T x) {
		if (x==null) throw internalError("NullPointerException"); else return x;
	}
}
