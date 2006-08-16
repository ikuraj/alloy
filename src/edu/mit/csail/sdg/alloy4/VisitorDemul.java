package edu.mit.csail.sdg.alloy4;

public final class VisitorDemul extends VisitDesugar {

  public static final VisitorDemul instance = new VisitorDemul();

  @Override public Expr accept(ExprBinary x) {
    Expr left=x.left.accept(this);
    Expr right=x.right.accept(this);
    if (x.op!=ExprBinary.Op.ARROW && x.op.isArrow()) return ExprBinary.Op.ARROW.make(x.pos, left, right, x.type);
    if (x.left==left && x.right==right) return x; return x.op.make(x.pos, left, right, x.type);
  }

  @Override public Expr accept(ExprUnary x) {
    Expr sub=x.sub.accept(this);
    if (x.op==ExprUnary.Op.ONEMULT || x.op==ExprUnary.Op.SOMEMULT || x.op==ExprUnary.Op.LONEMULT || x.op==ExprUnary.Op.SETMULT) return sub;
    return x.op.make(x.pos, sub);
  }
}
