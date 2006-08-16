package edu.mit.csail.sdg.alloy4;


/**
 * Mutable; represents a "fact".
 * @author Felix Chang
 */

public final class ParaFact extends Para {
	
	/** The fact. */
	public Expr value;
	
	/**
	 * Constructs a new ParaFact object.
	 * 
	 * @param pos - the original position in the file.
	 * @param path - a valid path to the Unit containing the paragraph.
	 * @param n - the name of the assertion (can be an empty string).
	 * @param v - the fact.
	 * 
	 * @throws ErrorSyntax - if n contains '/' or '@'.
	 * @throws ErrorSyntax - if n is equal to "none", "iden", "univ", or "Int".
	 * @throws ErrorInternal - if path==null, n==null, or v==null.
	 */
	public ParaFact(Pos pos, String path, String n, Expr v) {
		super(pos, path, n);
		nonnull(n);
		if (n.indexOf('/')>=0) throw this.syntaxError("Name \""+n+"\" must not contain \'/\' in it.");
		if (n.indexOf('@')>=0) throw this.syntaxError("Name \""+n+"\" must not contain \'@\' in it.");
		if (n.equals("none") ||
			n.equals("iden") ||
			n.equals("univ") ||
			n.equals("Int")) throw this.syntaxError("Name cannot be \""+n+"\"");
		value=nonnull(v);
	}
}
