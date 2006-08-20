package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a constant in the AST.
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

	private final int num;

	public int num() {
		if (op!=Op.NUMBER) throw internalError("This node is not a number constant");
		return num;
	}

	/**
	 * Constructs an ExprConstant expression.
	 *
	 * @param pos - the original position in the file
	 * @param op - the choice of which constant it is
	 * @param type - the type for this expression
	 *
	 * @throws ErrorInternal if pos==null
	 */
	private ExprConstant(Pos pos, Op op, Type type) {
		super(pos, type, 0);
		this.op=op;
		this.num=0;
	}

	private ExprConstant(Pos pos, String num) {
		super(pos, Type.INT, 0);
		this.op=Op.NUMBER;
		try {
			this.num=Integer.parseInt(nonnull(num));
		} catch(NumberFormatException e) {
			throw syntaxError("The number "+num
					+"is too small or too large to be stored in a Java integer!");
		}
	}

	/** This class contains all possible constants. */
	public enum Op {
		/** the builtin "iden" relation */  IDEN("iden"),
		/** the builtin "univ" sig      */  UNIV("univ"),
		/** the builtin "none" sig      */  NONE("none"),
		/** the builtin "Int"  sig      */  SIGINT("Int"),
		/** an integer constant         */  NUMBER("NUMBER");

		/** The constructor. */
		Op(String l) {label=l;}

		/** The human readable label for this operator. */
		private final String label;

		/**
		 * Constructs a new ExprConstant expression
		 * with "this" as the operator.
		 *
		 * @param pos - the original position in the file
		 * @throws ErrorInternal if pos==null
		 */
		public final ExprConstant make(Pos pos) {
			if (this==UNIV) return new ExprConstant(pos, this, ParaSig.UNIV.type);
			if (this==NONE) return new ExprConstant(pos, this, ParaSig.NONE.type);
			if (this==SIGINT) return new ExprConstant(pos, this, ParaSig.SIGINT.type);
			if (this==IDEN) return new ExprConstant(pos, this, ParaSig.UNIV.type.product_of_anyEmptyness(ParaSig.UNIV.type));
			throw new ErrorInternal(pos, null, "Illegal operator "+this+" encountered in ExprConstant.Op.make(pos)");
		}

		/**
		 * Constructs a new ExprConstant expression
		 * with "this" as the operator.
		 *
		 * @param pos - the original position in the file
		 * @throws ErrorInternal if pos==null
		 */
		public final ExprConstant make(Pos pos, String num) {
			if (this==NUMBER) return new ExprConstant(pos, num);
			throw new ErrorInternal(pos, null, "Illegal operator "+this+" encountered in ExprConstant.Op.make(pos,num)");
		}

		/** Returns the human readable label for this operator. */
		@Override public final String toString() { return label; }
	}
}
