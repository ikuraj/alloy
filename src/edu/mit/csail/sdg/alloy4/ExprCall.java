package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a function/predicate call.
 *
 * <br/>
 * <br/> Invariant: name!=null
 * <br/> Invariant: args!=null
 * <br/> Invariant: all x:args | (x!=null && x.mult==0)
 *
 * @author Felix Chang
 */

public final class ExprCall extends Expr {

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

	/** The name of the procedure being called. */
	public final String name;

	/** The procedure being called (null if we haven't typechecked this node yet). */
	public final ParaFun fun;

	/**
	 * The unmodifiable list of arguments.
	 *
	 * <br>If this node has been typechecked,
	 * then the arguments are guaranteed to match the procedure being called.
	 */
	public final List<Expr> args;

	/**
	 * Constructs an ExprCall expression.
	 *
	 * @param pos - the original position in the file
	 * @param name - the name of the procedure
	 * @param fun - the procedure (null if this expression has not been typechecked)
	 * @param args - the list of arguments
	 * @param type - the type (null if this expression has not been typechecked)
	 *
	 * @throws ErrorInternal if pos==null, name==null, args==null, or at least one argument is null
	 * @throws ErrorSyntax if at least one of the argument is a multiplicity constraint
	 */
	public ExprCall(Pos pos, String name, ParaFun fun, List<Expr> args, Type type) {
		super(pos,type,0);
		this.name=nonnull(name);
		this.fun=fun;
		this.args=Collections.unmodifiableList(new ArrayList<Expr>(nonnull(args)));
		for(int i=args.size()-1; i>=0; i--)
			if (nonnull(args.get(i)).mult>0)
				throw args.get(i).syntaxError("Multiplicity expression not allowed here");
	}
}
