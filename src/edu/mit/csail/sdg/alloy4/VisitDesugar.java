package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;

public abstract class VisitDesugar {

  public final Unit unit;

  public VisitDesugar() { unit=null; }
  public VisitDesugar(Unit u) { unit=u; }

  public Expr accept(ExprBinary x) {
    Expr left=x.left.accept(this);
    Expr right=x.right.accept(this);
    if (x.left==left && x.right==right) return x;
    return x.op.make(x.pos, left, right, x.type);
  }

  public Expr accept(ExprCall x) {
    List<Expr> list=new ArrayList<Expr>();
    for(Expr oldvalue:x.args) {
      list.add(oldvalue.accept(this));
    }
    return new ExprCall(x.pos, x.name, x.fun, list, x.type);
  }

  public Expr accept(ExprITE x) {
    Expr c=x.cond.accept(this);
    Expr l=x.left.accept(this);
    Expr r=x.right.accept(this);
    if (x.cond==c && x.left==l && x.right==r) return x;
    return new ExprITE(x.pos, c, l, r, x.type);
  }

  public Expr accept(ExprJoin x) {
    Expr left=x.left.accept(this);
    Expr right=x.right.accept(this);
    if (x.left==left && x.right==right) return x;
    return new ExprJoin(x.pos, left, right, x.type);
  }

  public Expr accept(ExprLet x) {
    Expr r=x.right.accept(this);
    Expr s=x.sub.accept(this);
    if (x.right==r && x.sub==s) return x;
    return new ExprLet(x.pos, x.left, r, s, x.type);
  }

  public Expr accept(ExprName x) { return x; }

  public Expr accept(ExprNumber x) { return x; }

  public Expr accept(ExprQuant x) {
    List<VarDecl> list=new ArrayList<VarDecl>();
    for(int i=0; i<x.list.size(); i++) {
      VarDecl d=x.list.get(i);
      Expr newvalue=d.value.accept(this);
      list.add(new VarDecl(d, newvalue));
    }
    Expr sub=x.sub.accept(this);
    return x.op.make(x.pos, list, sub, x.type);
  }

  public Expr accept(ExprSequence x) {
    boolean chg=false;
    List<Expr> list=new ArrayList<Expr>();
    for(int i=0; i<x.list.size(); i++) {
      Expr oldvalue=x.list.get(i);
      if (oldvalue==null) break;
      Expr newvalue=oldvalue.accept(this);
      if (oldvalue!=newvalue) chg=true;
      list.add(newvalue);
    }
    if (!chg) return x;
    return new ExprSequence(x.pos, list);
  }

  public Expr accept(ExprUnary x) {
    Expr sub=x.sub.accept(this);
    if (x.sub==sub) return x;
    return x.op.make(x.pos, sub, x.type);
  }
}
