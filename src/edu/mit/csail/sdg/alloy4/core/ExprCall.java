package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

import edu.mit.csail.sdg.alloy4.helper.ErrorInternal;
import edu.mit.csail.sdg.alloy4.helper.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.helper.Pos;

/**
 * Immutable; represents a call
 * (Note: only typechecker may make ExprCall nodes; others must make only ExprJoin nodes).
 *
 * <p/>
 * Before typechecking, it's not easy to tell if expressions
 * like "a[b]" or "b.a" are joins or calls.
 *
 * <p/>
 * So instead, everyone (except the typechecker) must create only ExprJoin nodes.
 * The typechecker will convert it to an appropriate ExprCall node if it's a call.
 *
 * <p/> <b>Invariant:</b>  args!=null
 * <p/> <b>Invariant:</b>  all x:args | (x!=null && x.mult==0)
 *
 * @author Felix Chang
 */

public final class ExprCall extends Expr {

    /** Accepts the return visitor. */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.visit(this);
    }

    /** Accepts the typecheck visitor bottom-up. */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.visit(this);
    }

    /** Accepts the typecheck visitor top-down. */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.visit(this,type);
    }

    /** The procedure being called (null if we haven't typechecked this node yet). */
    public final ParaFun fun;

    /**
     * The unmodifiable list of arguments.
     *
     * <br/> If this node has been typechecked,
     * then the arguments will match the procedure being called.
     */
    public final List<Expr> args;

    /**
     * Constructs an ExprCall expression.
     *
     * @param pos - the original position in the file
     * @param fun - the procedure (null if this expression has not been typechecked)
     * @param args - the list of arguments
     * @param type - the type
     *
     * @throws ErrorInternal if pos==null, args==null, or at least one argument is null
     * @throws ErrorSyntax if at least one of the argument is a multiplicity constraint
     */
    public ExprCall(Pos pos, ParaFun fun, List<Expr> args, Type type) {
        super(pos,type,0);
        this.args=Collections.unmodifiableList(new ArrayList<Expr>(nonnull(args)));
        for(int i=args.size()-1; i>=0; i--)
            if (nonnull(args.get(i)).mult!=0)
                throw args.get(i).syntaxError("Multiplicity expression not allowed here");
        this.fun=fun;
        if (type==null)
            throw internalError("ExprCall nodes must only be created by the typechecker.");
    }
}
