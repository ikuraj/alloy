package edu.mit.csail.sdg.alloy4;

public abstract class VisitQuery implements VisitReturn {

  public boolean query(Expr x) { return x.accept(this)!=null; }

  public Object accept(ExprBinary x) {
    if (x.left.accept(this)!=null) return Boolean.TRUE;
    return x.right.accept(this);
  }

  public Object accept(ExprITE x) {
    if (x.cond.accept(this)!=null) return Boolean.TRUE;
    if (x.left.accept(this)!=null) return Boolean.TRUE;
    return x.right.accept(this);
  }

  public Object accept(ExprLet x) {
    if (x.right.accept(this)!=null) return Boolean.TRUE;
    return x.sub.accept(this);
  }

  public Object accept(ExprConstant x) { return Boolean.FALSE; }

  public Object accept(ExprQuant x) {
    for(int i=0; i<x.list.size(); i++)
    	if (x.list.get(i).value.accept(this)!=null)
    		return Boolean.TRUE;
    return x.sub.accept(this);
  }

  public Object accept(ExprSequence x) {
    for(int i=0; i<x.list.size(); i++) {
      Expr sub=x.list.get(i);
      if (sub.accept(this)!=null) return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  public Object accept(ExprUnary x) {
    return x.sub.accept(this);
  }

  public Object accept(ExprJoin x) {
    if (x.left.accept(this)!=null) return Boolean.TRUE;
    return x.right.accept(this);
  }

  public Object accept(ExprCall x) {
    for(Expr y:x.args) if (y.accept(this)!=null) return Boolean.TRUE;
    return Boolean.FALSE;
  }

  public Object accept(ExprName x) { return Boolean.FALSE; }
}
