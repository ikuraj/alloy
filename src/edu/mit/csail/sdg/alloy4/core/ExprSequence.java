package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; represents a list of formulas joined by the AND operator.
 *
 * <p/> <b>Invariant:</b>  list!=null
 * <p/> <b>Invariant:</b>  all x:list | (x!=null && x.mult==0)
 *
 * @author Felix Chang
 */

public final class ExprSequence extends Expr {

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

    /** The unmodifiable list of formulas (If empty, then this ExprSequence node simply means TRUE) */
    public final List<Expr> list;

    /**
     * Constructs an ExprSequence object.
     *
     * @param pos - the original position in the file.
     * @param list - the list of formulas (this can be an empty list, meaning TRUE).
     *
     * @throws ErrorInternal if pos==null, list==null, or one of the formula is null
     * @throws ErrorSyntax if one of the formula is a multiplicity constraint
     */
    public ExprSequence(Pos pos, List<Expr> list) {
        super(pos, Type.FORMULA, 0);
        this.list=Collections.unmodifiableList(new ArrayList<Expr>(nonnull(list)));
        for(int i=this.list.size()-1; i>=0; i--)
            if (nonnull(this.list.get(i)).mult>0)
                throw this.list.get(i).syntaxError("Multiplicity expression not allowed here");
    }
}
