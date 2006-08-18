package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a list of formulas joined by the AND operator.
 *
 * <br/>
 * <br/> Invariant: list!=null
 * <br/> Invariant: all x:list | (x!=null && x.mult==0)
 *
 * @author Felix Chang
 */

public final class ExprSequence extends Expr {

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

	/** The unmodifiable list of formulas (The list can be empty, meaning TRUE) */
	public final List<Expr> list;

	/**
	 * Constructs an ExprSequence object.
	 *
	 * @param p - the original position in the file.
	 * @param s - the list of formulas (this can be an empty list, meaning TRUE).
	 *
	 * @throws ErrorInternal if p==null, s==null, or one of the formula is null
	 * @throws ErrorSyntax if one of the formula is a multiplicity constraint
	 */
	public ExprSequence(Pos p, List<Expr> s) {
		super(p, Type.FORMULA, 0);
		list=Collections.unmodifiableList(new ArrayList<Expr>(nonnull(s)));
		for(int i=list.size()-1; i>=0; i--)
			if (nonnull(list.get(i)).mult>0)
				throw list.get(i).syntaxError("Multiplicity expression not allowed here");
	}
}
