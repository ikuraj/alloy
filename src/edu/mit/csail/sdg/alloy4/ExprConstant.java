package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a named constant in the AST.
 *
 * <br/>
 * <br/> Invariant: op!=null
 *
 * @author Felix Chang
 */

public final class ExprConstant extends Expr {

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

	/** The constant. */
	public final Op op;

	/**
	 * Constructs an ExprNamedConstant expression.
	 *
	 * @param pos - the original position in the file
	 * @param op - the choice of which constant it is
	 * @param type - the type for this expression
	 *
	 * @throws ErrorInternal if pos==null
	 */
	public ExprConstant(Pos pos, Op op, Type type) {
		super(pos, type, 0);
		this.op=op;
	}

	/** This class contains all possible named constants. */
	public enum Op {
		/** iden */ IDEN("iden"),
		/** univ */ UNIV("univ"),
		/** none */ NONE("none"),
		/** Int  */ SIGINT("Int");

		/** The constructor. */
		Op(String l) {label=l;}

		/** The human readable label for this operator. */
		private final String label;

		/**
		 * Constructs a new ExprNamedConstant expression
		 * with "this" as the operator.
		 *
		 * @param pos - the original position in the file
		 * @throws ErrorInternal if pos==null
		 */
		public final ExprConstant make(Pos pos) {
			if (this==UNIV) return new ExprConstant(pos, this, ParaSig.UNIV.type);
			if (this==NONE) return new ExprConstant(pos, this, ParaSig.NONE.type);
			if (this==SIGINT) return new ExprConstant(pos, this, ParaSig.SIGINT.type);
			return new ExprConstant(pos, this, ParaSig.UNIV.type.product_of_anyEmptyness(ParaSig.UNIV.type));
		}

		/** Returns the human readable label for this operator. */
		@Override public final String toString() { return label; }
	}
}
