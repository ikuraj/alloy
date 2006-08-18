package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents an identifier in the AST.
 *
 * <br/>
 * <br/> Invariant: name!=null
 * <br/> Invariant: name does not equal "" nor "@"
 * <br/> Invariant: if name contains '@', then '@' must only occur as the first character
 * <br/> Invariant: (object==null)
 *			|| (object instanceof ParaSig)
 *			|| (object instanceof ParaSig.Field)
 *			|| (object instanceof ParaSig.Field.Full)
 *			|| (object instanceof ParaFun)
 *
 * @author Felix Chang
 */

public final class ExprName extends Expr {

	/**
	 * Accepts the return visitor.
	 * @see edu.mit.csail.sdg.alloy4.VisitReturn
	 */
	@Override public Object accept(VisitReturn visitor) {
		return visitor.accept(this);
	}

	/**
	 * Accepts the desugar visitor.
	 * @see edu.mit.csail.sdg.alloy4.VisitDesugar
	 */
	@Override public Expr accept(VisitDesugar visitor) {
		return visitor.accept(this);
	}

	/**
	 * Accepts the desugar2 visitor.
	 * @see edu.mit.csail.sdg.alloy4.VisitDesugar2
	 */
	@Override public Expr accept(VisitDesugar2 visitor, Type type) {
		return visitor.accept(this,type);
	}

	/** The name of the object that this name refers to. */
	public final String name;

	/**
	 *  The object that this name refers to (null if this node hasn't been typechecked).
	 *
	 *  <p/> Note: After typechecking, this field can have the following possibilities:
	 *  <br/> (1) null (meaning it's a quantification/let/function_call_parameter or it's univ/iden/none/Int)
	 *  <br/> (2) ParaSig
	 *  <br/> (3) ParaSig.Field
	 *  <br/> (4) ParaSig.Field.Full
	 *  <br/> (5) parameter-less ParaFun
	 */
	public final Object object;

	/**
	 * Constructs a typechecked ExprName expression.
	 *
	 * @param pos - the original position in the file
	 * @param name - the identifier
	 * @param object - the object being referred to
	 * @param type - the type
	 *
	 * @throws ErrorInternal if pos==null or name==null
	 * @throws ErrorInternal if name is equal to "" or "@"
	 * @throws ErrorInternal if name.lastIndexOf('@')>0
	 * @throws ErrorInternal if object is not one of {null, ParaSig, ParaSig.Field, ParaSig.Field.Full, ParaFun}
	 */
	public ExprName(Pos pos, String name, Object object, Type type) {
		super(pos, type, 0);
		this.name=nonnull(name);
		this.object=object;
		if (name.length()==0)
			throw syntaxError("The name of a variable must not be empty!");
		if (name.length()==1 && name.charAt(0)=='@')
			throw syntaxError("The name of a variable must not be \"@\"");
		if (name.lastIndexOf('@')>0)
			throw syntaxError("If a variable name contains @, it must be the first character!");
		if (object!=null
			&& !(object instanceof ParaSig)
			&& !(object instanceof ParaSig.Field)
			&& !(object instanceof ParaSig.Field.Full)
			&& !(object instanceof ParaFun))
			throw internalError("ExprName object must be Sig, Sig.Field, Sig.Field.Full, Fun, or null!");
	}

	/**
	 * Constructs an untypechecked ExprName expression.
	 *
	 * @param pos - the original position in the file
	 * @param name - the identifier
	 *
	 * @throws ErrorInternal if pos==null or name==null
	 * @throws ErrorInternal if name is equal to "" or "@"
	 * @throws ErrorInternal if name.lastIndexOf('@')>0
	 * @throws ErrorInternal if object is not one of {null, ParaSig, ParaSig.Field, ParaSig.Field.Full, ParaFun}
	 */
	public ExprName(Pos pos, String name) { this(pos, name, null, null); }

	/**
	 * Convenience method that throws a syntax error exception saying the name "n" can't be found.
	 * (In particular, if n is an old Alloy3 keyword, then
	 * the message will tell the user to consult the documentation
	 * on how to migrate old models to the new syntax.)
	 *
	 * @param p - the original position in the file that triggered the error
	 * @param n - the identifier
	 */
	public static void hint (Pos p, String n) {
		String msg="The name \""+n+"\" cannot be found.";
		if ("disj".equals(n) || "disjoint".equals(n) ||
			"exh".equals(n) || "exhaustive".equals(n) ||
			"part".equals(n) || "partition".equals(n) )
			msg=msg+" If you are migrating from Alloy 3, please see the "
				+"online documentation on how to translate models that use the \""
				+n+"\" keyword.";
		throw new ErrorSyntax(p, msg);
	}
}
