package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents an if-then-else expression.
 * @author Felix Chang
 */

public final class ExprITE extends Expr {
	
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
	
	/** The condition formula. */
	public final Expr cond;
	
	/** The then-clause. */
	public final Expr left;
	
	/** The else-clause. */
	public final Expr right;
	
	/**
	 * Constructs a typechecked ExprITE expression.
	 * 
	 * @param pos - the original position in the file
	 * @param cond - the condition formula
	 * @param left - the then-clause
	 * @param right - the else-clause
	 * @param type - the type
	 * 
	 * @throws ErrorInternal if pos==null, cond==null, left==null, or right==null
	 * @throws ErrorSyntax if cond.mult!=0, left.mult!=0, or right.mult!=0
	 */
	public ExprITE(Pos pos, Expr cond, Expr left, Expr right, Type type) {
		super(pos, type, 0);
		if (cond.mult != 0) throw cond.syntaxError("Multiplicity expression not allowed here");
		if (left.mult != 0) throw left.syntaxError("Multiplicity expression not allowed here");
		if (right.mult != 0) throw right.syntaxError("Multiplicity expression not allowed here");
		this.cond=nonnull(cond);
		this.left=nonnull(left);
		this.right=nonnull(right);
	}
	
	/**
	 * Constructs an untypechecked ExprITE expression.
	 *
	 * @param pos - the original position in the file
	 * @param cond - the condition formula
	 * @param left - the then-clause
	 * @param right - the else-clause
	 *
	 * @throws ErrorInternal if pos==null, cond==null, left==null, or right==null
	 * @throws ErrorSyntax if cond.mult!=0, left.mult!=0, or right.mult!=0
	 */
	public ExprITE(Pos pos, Expr cond, Expr left, Expr right) {
		this(pos, cond, left, right, null);
	}
}
