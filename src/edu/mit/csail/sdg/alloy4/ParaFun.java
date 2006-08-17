package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;

/**
 * Mutable; represents a "predicate" or "function"
 * @author Felix Chang
 */

public final class ParaFun extends Para {
	
	/** The mutable list of parameters (its number of names must match this.argCount) */
	public List<VarDecl> decls;
	
	/** The number of parameters (it must match the number of names in this.decls) */
	public int argCount;
	
	/** The return type (null if this is a "predicate" rather than a "function") */
	public Expr type;
	
	/** The body of the predicate/function */
	public Expr value;
	
	/**
	 * Constructs a new ParaFun object.
	 * 
	 * @param pos - the original position in the file
	 * @param path - a valid path to the Unit containing this paragraph (can be "" if it's the main unit)
	 * @param n - the name of the paragraph (it cannot be "")
	 * @param d - the list of parameters
	 * @param t - the return type (null if this is a predicate rather than a function)
	 * @param v - the body of the predicate/function
	 * 
	 * @throws ErrorSyntax - if n contains '@' or '/'
	 * @throws ErrorSyntax - if n is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax - if d contains duplicate names
	 * @throws ErrorInternal - if pos==null, path==null, n==null, d==null, or v==null
	 */
	public ParaFun(Pos pos, String path, String n, List<VarDecl> d, Expr t, Expr v) {
		super(pos, path, n);
		if (n.length()==0) throw this.syntaxError("Name of a predicate/function cannot be \"\"");
		decls=new ArrayList<VarDecl>(nonnull(d));
		String dup=VarDecl.hasDuplicateName(decls);
		if (dup!=null) throw this.syntaxError("The parameter name \""+dup+"\" appears more than once in this predicate/function declaration.");
		argCount=VarDecl.nameCount(decls);
		// See ExprUnary.java for why we have to call makeMult() here.
		if (t instanceof ExprUnary) t=((ExprUnary)t).makeMult();
		type=t;
		value=nonnull(v);
	}
}
