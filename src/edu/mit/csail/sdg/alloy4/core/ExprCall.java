package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

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
 * <p/> <b>Invariant:</b>  name!=null.
 * <p/> <b>Invariant:</b>  name does not equal "" nor "@"
 * <p/> <b>Invariant:</b>  if name contains '@', then '@' must only occur as the first character
 * <p/> <b>Invariant:</b>  args!=null
 * <p/> <b>Invariant:</b>  all x:args | (x!=null && x.mult==0)
 *
 * @author Felix Chang
 */

public final class ExprCall extends Expr {

    /** Accepts the return visitor. */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor bottom-up. */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor top-down. */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.accept(this,type);
    }

    /** The name of the procedure being called. */
    public final String name;

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
     * @param name - the name of the procedure
     * @param fun - the procedure (null if this expression has not been typechecked)
     * @param args - the list of arguments
     * @param type - the type
     *
     * @throws ErrorInternal if pos==null, name==null, args==null, or at least one argument is null
     * @throws ErrorInternal if name is equal to "" or "@"
     * @throws ErrorInternal if name.lastIndexOf('@')>0
     * @throws ErrorSyntax if at least one of the argument is a multiplicity constraint
     */
    public ExprCall(Pos pos, String name, ParaFun fun, List<Expr> args, Type type) {
        super(pos,type,0);
        if (type==null)
            throw internalError("ExprCall nodes must only be created by the typechecker.");
        this.name=nonnull(name);
        this.fun=fun;
        this.args=Collections.unmodifiableList(new ArrayList<Expr>(nonnull(args)));
        for(int i=args.size()-1; i>=0; i--)
            if (nonnull(args.get(i)).mult!=0)
                throw args.get(i).syntaxError("Multiplicity expression not allowed here");
        if (name.length()==0)
            throw syntaxError("The name must not be empty!");
        if (name.length()==1 && name.charAt(0)=='@')
            throw syntaxError("The name must not be \"@\"");
        if (name.lastIndexOf('@')>0)
            throw syntaxError("If a variable name contains @, it must be the first character!");
    }
}
