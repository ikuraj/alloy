package edu.mit.csail.sdg.alloy4;


public interface VisitReturn {
  public abstract Object accept(ExprBinary x);
  public abstract Object accept(ExprITE x);
  public abstract Object accept(ExprJoin x);
  public abstract Object accept(ExprCall x);
  public abstract Object accept(ExprLet x);
  public abstract Object accept(ExprName x);
  public abstract Object accept(ExprNumber x);
  public abstract Object accept(ExprQuant x);
  public abstract Object accept(ExprSequence x);
  public abstract Object accept(ExprUnary x);
}
