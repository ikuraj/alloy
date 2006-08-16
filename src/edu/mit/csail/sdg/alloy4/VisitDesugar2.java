package edu.mit.csail.sdg.alloy4;

public interface VisitDesugar2 {
  public Expr accept(ExprBinary x,Type t);
  public Expr accept(ExprCall x,Type t);
  public Expr accept(ExprITE x,Type t);
  public Expr accept(ExprJoin x,Type t);
  public Expr accept(ExprLet x,Type t);
  public Expr accept(ExprName x,Type t);
  public Expr accept(ExprNumber x,Type t);
  public Expr accept(ExprQuant x,Type t);
  public Expr accept(ExprSequence x,Type t);
  public Expr accept(ExprUnary x,Type t);
}
