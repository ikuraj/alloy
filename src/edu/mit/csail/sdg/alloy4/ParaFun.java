package edu.mit.csail.sdg.alloy4;


import java.util.List;
import java.util.ArrayList;

/**
 * Mutable; represents a "predicate" or "function"
 * @author Felix Chang
 */

public final class ParaFun extends Para {
	
	/** The mutable list of parameters */
	public List<VarDecl> decls;
	
	/** The number of parameters */
	public int argCount;
	
	/** The return type (null if this is a "predicate" rather than a "function") */
	public Expr type;
	
	/** The body of the predicate/function */
	public Expr value;
	
	/**
	 * Constructs a new ParaFun object.
	 * 
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing the paragraph
	 * @param n - the name of the function (cannot be an empty string)
	 * @param d - the list of parameters
	 * @param t - the return type (null if this is a predicate rather than a function)
	 * @param v - the body of the predicate/function
	 * 
	 * @throws ErrorSyntax - if n contains '/' or '@'
	 * @throws ErrorSyntax - if n is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax - if d contains duplicate names
	 * @throws ErrorInternal - if path==null, n==null, d==null, or v==null
	 */
	public ParaFun(Pos pos, String path, String n, List<VarDecl> d, Expr t, Expr v) {
		super(pos,path,n);
		nonnull(n);
		if (n.length()==0) throw this.syntaxError("Name must contain at least 1 character.");
		if (n.indexOf('/')>=0) throw this.syntaxError("Name \""+n+"\" must not contain \'/\' in it.");
		if (n.indexOf('@')>=0) throw this.syntaxError("Name \""+n+"\" must not contain \'@\' in it.");
		if (n.equals("none") ||
			n.equals("iden") ||
			n.equals("univ") ||
			n.equals("Int")) throw this.syntaxError("Name cannot be \""+n+"\"");
		decls=new ArrayList<VarDecl>(nonnull(d));
		String dup=VarDecl.hasDuplicateName(decls);
		if (dup!=null) throw this.syntaxError("The parameter name \""+dup+"\" appears more than once in this predicate/function declaration.");
		argCount=VarDecl.nameCount(decls);
		if (t instanceof ExprUnary) t=((ExprUnary)t).makeMult();
		type=t;
		value=nonnull(v);
	}
}
