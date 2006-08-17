package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a unary expression of the form (OP subexpression).
 * @author Felix Chang
 */

public final class ExprUnary extends Expr {

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

	/** The unary operator. */
	public final Op op;

	/** The subexpression. */
	public final Expr sub;

	/**
	 * This function is needed to handle a difficult parsing ambiguity.
	 *
	 * <p/>
	 * "some EXPR", "one EXPR", and "lone EXPR"
	 * can be either formulas (saying the EXPR has at least 1, exactly 1, or at most 1 tuple),
	 * or multiplicity constraints (saying something else has this multiplicity).
	 *
	 * <p/>
	 * So we let the parser generate the former by default.
	 * And whenever we construct a VarDecl(x,y) object,
	 * or an ExprBinary.Op.IN(x,y) object, we call this method
	 * on y to convert it into a multiplicity constraint.
	 *
	 * <p/>
	 * This is safe, because in both cases, a formula would be illegal.
	 * So the first form is always wrong.
	 *
	 * <p/>
	 * And this is sufficient, because those are the only two places
	 * where a mulitplicity constraint is allowed to appear.
	 *
	 * @return a newly formed multiplciity constraint (if this.op==SOME or LONE or ONE),
	 * otherwise it just returns the original node.
	 */
	public Expr makeMult() {
		if (op==Op.SOME) return Op.SOMEMULT.make(pos, sub);
		if (op==Op.LONE) return Op.LONEMULT.make(pos, sub);
		if (op==Op.ONE) return Op.ONEMULT.make(pos, sub);
		return this;
	}

	/**
	 * Constructs a new unary expression.
	 *
	 * @param p - the original position in the file
	 * @param o - the operator
	 * @param s - the subexpression
	 * @param t - the type (null if this expression has not been typechecked)
	 *
	 * @throws ErrorInternal if p==null or s==null
	 * @throws ErrorSyntax if o==Op.ALL (since this is no longer supported)
	 * @throws ErrorSyntax if s.mult!=0
	 */
	private ExprUnary(Pos p, Op o, Expr s, Type t) {
		super(p, t, (o==Op.SOMEMULT || o==Op.LONEMULT || o==Op.ONEMULT || o==Op.SETMULT)?1:0);
		op=o;
		sub=nonnull(s);
		if (o==Op.ALL) throw syntaxError("The \"all x\" construct is no longer "
				+"supported. If you know the range of possible values of x, consider "
				+"rewriting it as \"x == set_of_all_possible_values\".");
		if (s.mult != 0) throw s.syntaxError("Multiplicity expression not allowed here");
	}

	/** This class contains all possible unary operators */
	public enum Op {
		/** :some x (where x is a set or relation) */  SOMEMULT(":some"),
		/** :lone x (where x is a set or relation) */  LONEMULT(":lone"),
		/** :one  x (where x is a set or relation) */  ONEMULT(":one"),
		/** :set  x (where x is a set or relation) */  SETMULT(":set"),
		/** not   f (where f is a formula)         */  NOT("not"),
		/** all   x (where x is a set or relation) */  ALL("all"),
		/** no    x (where x is a set or relation) */  NO("no"),
		/** some  x (where x is a set or relation) */  SOME("some"),
		/** lone  x (where x is a set or relation) */  LONE("lone"),
		/** one   x (where x is a set or relation) */  ONE("one"),
		/** transpose                              */  TRANSPOSE("~"),
		/** reflexive closure                      */  RCLOSURE("*"),
		/** closure                                */  CLOSURE("^"),
		/** cardinality                            */  CARDINALITY("#"),
		/** intAtom-to-integer                     */  SUM("sum"),
		/** integer-to-intAtom                     */  INTTOATOM("$");

		/** The constructor */
		Op(String l) {label=l;}

		/** The human readable label for this operator */
		private final String label;

		/** Returns the human readable label for this operator */
		@Override public final String toString() { return label; }

		/**
		 * Constructs an untypechecked ExprUnary expression with "this" as the operator.
		 *
		 * @param p - the original position in the file
		 * @param s - the subexpression
		 *
		 * @throws ErrorInternal if p==null or s==null
		 * @throws ErrorSyntax if o==Op.ALL (since this is no longer supported)
		 * @throws ErrorSyntax if s.mult!=0
		 */
		public final Expr make(Pos p, Expr s) { return new ExprUnary(p,this,s,null); }

		/**
		 * Constructs a typechecked ExprUnary expression
		 * with "this" as the operator, and "type" as the type.
		 *
		 * @param p - the original position in the file
		 * @param s - the subexpression
		 * @param t - the type
		 *
		 * @throws ErrorInternal if p==null or s==null
		 * @throws ErrorSyntax if o==Op.ALL (since this is no longer supported)
		 * @throws ErrorSyntax if s.mult!=0
		 */
		public final Expr make(Pos p, Expr s, Type t) { return new ExprUnary(p,this,s,t); }
	}
}
