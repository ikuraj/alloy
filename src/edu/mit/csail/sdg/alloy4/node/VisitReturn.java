package edu.mit.csail.sdg.alloy4.node;


/**
 * This interface defines what a Return Visitor's interface should be.
 *
 * @author Felix Chang
 */

public interface VisitReturn {

    /** Visits an ExprBinary node. */
    public abstract Object visit(ExprBinary x);

    /** Visits an ExprITE node. */
    public abstract Object visit(ExprITE x);

    /** Visits an ExprJoin node. */
    public abstract Object visit(ExprJoin x);

    /** Visits an ExprCall node. */
    public abstract Object visit(ExprCall x);

    /** Visits an ExprLet node. */
    public abstract Object visit(ExprLet x);

    /** Visits an ExprName node. */
    public abstract Object visit(ExprName x);

    /** Visits an ExprConstant node. */
    public abstract Object visit(ExprConstant x);

    /** Visits an ExprQuant node. */
    public abstract Object visit(ExprQuant x);

    /** Visits an ExprSequence node. */
    public abstract Object visit(ExprSequence x);

    /** Visits an ExprUnary node. */
    public abstract Object visit(ExprUnary x);
}
