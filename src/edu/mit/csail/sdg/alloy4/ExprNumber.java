package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents an integer constant in the AST.
 *
 * <br/>
 * <br/> Invariant: none
 *
 * @author Felix Chang
 */

public final class ExprNumber extends Expr {

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

	/** The number. */
	public final int num;

	/**
	 * Constructs an ExprNumber expression.
	 *
	 * @param pos - the original position in the file
	 * @param num - the number
	 *
	 * @throws ErrorInternal if pos==null or num==null
	 * @throws ErrorSyntax if num cannot be parsed into a Java integer (eg. it's too large)
	 */
	public ExprNumber(Pos pos, String num) {
		super(pos, Type.INT, 0);
		try {
			this.num=Integer.parseInt(nonnull(num));
		} catch(NumberFormatException e) {
			throw syntaxError("The number "+num
					+"is too small or too large to be stored in a Java integer!");
		}
	}
}
