package edu.mit.csail.sdg.alloy4.node;

/**
 * This abstract class implements a Query visitor that walks over an Expr and its subnodes.
 * <br/> The visit() methods returns null for false, nonnull for true.
 * <br/> Or, you can call the query() method which will return a boolean value.
 *
 * <p/>  This default implementation will return null on all the leaf Expr nodes.
 * <br/> You need to subclass this class to implement a particular query.
 *
 * @author Felix Chang
 */

public abstract class VisitQuery implements VisitReturn {

  /** Visits an Expr node, then returns either true or false. */
  public final boolean query(Expr x) {
    return x.accept(this)!=null;
  }

  /** Visits an ExprBinary node (A OP B) by calling accept() on A then B. */
  public Object visit(ExprBinary x) {
    if (x.left.accept(this)!=null) return this;
    return x.right.accept(this);
  }

  /** Visits an ExprITE node (A => B else C) by calling accept() on A, B, then C. */
  public Object visit(ExprITE x) {
    if (x.cond.accept(this)!=null) return this;
    if (x.left.accept(this)!=null) return this;
    return x.right.accept(this);
  }

  /** Visits an ExprLet node (let A=B | F) by calling accept() on B then F. */
  public Object visit(ExprLet x) {
    if (x.right.accept(this)!=null) return this;
    return x.sub.accept(this);
  }

  /** Visits an ExprConstant node (this default implementation simply returns null) */
  public Object visit(ExprConstant x) {
    return null;
  }

  /** Visits an ExprQuant node (all a:X1, b:X2... | F) by calling accept() on X1, X2... and then on F. */
  public Object visit(ExprQuant x) {
    for(int i=0; i<x.list.size(); i++)
        if (x.list.get(i).value.accept(this)!=null)
            return this;
    return x.sub.accept(this);
  }

  /** Visits an ExprSequence node {X1,X2,X3...Xn} by calling accept() on X1, X2. ... until Xn. */
  public Object visit(ExprSequence x) {
    for(int i=0; i<x.list.size(); i++) {
      Expr sub=x.list.get(i);
      if (sub.accept(this)!=null) return this;
    }
    return null;
  }

  /** Visits an ExprUnary node (OP X) by calling accept() on X. */
  public Object visit(ExprUnary x) {
    return x.sub.accept(this);
  }

  /** Visits an ExprJoin node (A.B) by calling accept() on A then B. */
  public Object visit(ExprJoin x) {
    if (x.left.accept(this)!=null) return this;
    return x.right.accept(this);
  }

  /** Visits an ExprCall node F[X1,X2..Xn] by calling accept() on X1, X2... until Xn. */
  public Object visit(ExprCall x) {
    for(Expr y:x.args) if (y.accept(this)!=null) return this;
    return null;
  }

  /** Visits an ExprName node (this default implementation simply returns null) */
  public Object visit(ExprName x) {
    return null;
  }
}
