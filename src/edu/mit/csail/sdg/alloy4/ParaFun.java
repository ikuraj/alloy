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
	 * @param name - the name of the paragraph (it cannot be "")
	 * @param decls - the list of parameters
	 * @param type - the return type (null if this is a predicate rather than a function)
	 * @param value - the body of the predicate/function
	 * 
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if the name contains '@' or '/'
	 * @throws ErrorSyntax if the name is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax if decls contains duplicate names
	 * @throws ErrorInternal if pos==null, path==null, name==null, decls==null, or value==null
	 */
	public ParaFun(Pos pos, String path, String name, List<VarDecl> decls, Expr type, Expr value) {
		super(pos, path, name);
		if (name.length()==0) throw this.syntaxError("Name of a predicate/function cannot be \"\"");
		this.decls=new ArrayList<VarDecl>(nonnull(decls));
		String dup=VarDecl.hasDuplicateName(this.decls);
		if (dup!=null) throw this.syntaxError("The parameter name \""+dup+"\" appears more than once in this predicate/function declaration.");
		argCount=VarDecl.nameCount(this.decls);
		// See ExprUnary.java for why we have to call makeMult() here.
		if (type instanceof ExprUnary) type=((ExprUnary)type).makeMult();
		this.type=type;
		this.value=nonnull(value);
	}
}
