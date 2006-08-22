package edu.mit.csail.sdg.alloy4;

import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import kodkod.ast.IntExpression;
import kodkod.ast.Decls;
import kodkod.ast.IntConstant;
import kodkod.ast.Variable;
import kodkod.ast.Relation;
import kodkod.ast.Formula;
import kodkod.ast.Expression;
import kodkod.ast.BinaryExpression;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Tuple;
import kodkod.instance.Universe;

public final class VisitorEval implements VisitReturn {

//################################################################################################

private final Formula cform(Object x) { if (x instanceof Formula) return ((Formula)x); throw new ErrorInternal(null,null,"This should have been a formula! Instead it is "+x); }
private final IntExpression cint(Object x) { if (x instanceof IntExpression) return ((IntExpression)x); throw new ErrorInternal(null,null,"This should have been an integer expression! Instead it is "+x); }
private final Expression cset(Object x) { if (x instanceof Expression) return ((Expression)x); throw new ErrorInternal(null,null,"This should have been a set or a relation! Instead it is "+x); }

//################################################################################################

public Object accept(ExprJoin x) {
  Expression a=cset(x.left.accept(this));
  Expression b=cset(x.right.accept(this));
  return a.join(b);
}

//################################################################################################

private Formula isIn(Expression a, Expr right) {
  Expression b;
  if (right instanceof ExprUnary) {
     ExprUnary y=(ExprUnary)(right);
     if (y.op==ExprUnary.Op.ONEMULT) { b=cset(y.sub.accept(this)); return a.one().and(a.in(b)); }
     if (y.op==ExprUnary.Op.SETMULT) { b=cset(y.sub.accept(this)); return a.in(b); }
     if (y.op==ExprUnary.Op.LONEMULT) { b=cset(y.sub.accept(this)); return a.lone().and(a.in(b)); }
     if (y.op==ExprUnary.Op.SOMEMULT) { b=cset(y.sub.accept(this)); return a.some().and(a.in(b)); }
  }
  if (right instanceof ExprBinary) return isInBinary(a, (ExprBinary)right);
  return a.in(cset(right.accept(this)));
}

private Formula isInAM(Expression r, ExprBinary op, Expression a, Expression b) {
  int arity=a.arity();
  if (arity==1) {
    Variable v1=Variable.unary("@");
    Expression x=v1.join(r);
    Formula ans=x.in(b);
    switch(op.op) {
      case ANY_ARROW_SOME: case SOME_ARROW_SOME: case ONE_ARROW_SOME: case LONE_ARROW_SOME:
           ans=x.some().and(ans); break;
      case ANY_ARROW_ONE: case SOME_ARROW_ONE: case ONE_ARROW_ONE: case LONE_ARROW_ONE:
           ans=x.one().and(ans); break;
      case ANY_ARROW_LONE: case SOME_ARROW_LONE: case ONE_ARROW_LONE: case LONE_ARROW_LONE:
           ans=x.lone().and(ans); break;
    }
    return ans.forAll(v1.oneOf(a));
  }
  /*
  List<Variable> vars=new ArrayList<Variable>(arity);
  Expression temp=r;
  for(int i=0; i<arity; i++) { Variable v1=Variable.unary("@"); temp=v1.join(temp); vars.add(v1); }
  */
  throw op.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
}

private Formula isInNB(Expression r, ExprBinary op, Expression a, Expression b) {
  int arity=b.arity();
  if (arity==1) {
    Variable v2=Variable.unary("@");
    Expression x=r.join(v2);
    Formula ans=x.in(a);
    switch(op.op) {
      case ONE_ARROW_LONE: case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE:
           ans=x.one().and(ans); break;
      case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
           ans=x.lone().and(ans); break;
      case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
           ans=x.some().and(ans); break;
    }
    return ans.forAll(v2.oneOf(b));
  }
  throw op.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
}

private Formula isInBinary(Expression r, ExprBinary ab) {
  Expression y0=cset(ab.accept(VisitorDemul.instance).accept(this));
  if (!ab.op.isArrow() || ab.mult==0) return r.in(y0);
  Expr a=ab.left;  Expression aa=cset(a.accept(VisitorDemul.instance).accept(this));
  Expr b=ab.right; Expression bb=cset(b.accept(VisitorDemul.instance).accept(this));
  //if (a.type.arity()>1) throw a.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
  //if (b.type.arity()>1) throw b.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
  Formula pa=isInAM(r,ab,aa,bb);
  Formula pb=isInNB(r,ab,aa,bb);
  return r.in(y0).and(pa).and(pb);
}

//################################################################################################

public Object accept(ExprBinary x) {
  if (x.op==ExprBinary.Op.IN) return isIn(cset(x.left.accept(this)), x.right);
  Expression aa,bb;
  Object a=x.left.accept(this), b=x.right.accept(this);
  switch(x.op) {
    case LT: return cint(a).lt(cint(b));
    case LTE: return cint(a).lte(cint(b));
    case GT: return cint(a).gt(cint(b));
    case GTE: return cint(a).gte(cint(b));
    case AND: return cform(a).and(cform(b));
    case OR: return cform(a).or(cform(b));
    case IFF: return cform(a).iff(cform(b));
    case IMPLIES: return cform(a).implies(cform(b));
    case PLUSPLUS: return cset(a).override(cset(b));
    case PLUS:
      if (x.left.type.isInt) return cint(a).plus(cint(b)); return cset(a).union(cset(b));
    case MINUS:
      if (x.left.type.isInt) return cint(a).minus(cint(b)); return cset(a).difference(cset(b));
    case INTERSECT:
      return cset(a).intersection(cset(b));
    case ARROW:
      return cset(a).product(cset(b));
    case ANY_ARROW_SOME: case ANY_ARROW_ONE: case ANY_ARROW_LONE:
    case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
    case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE: case ONE_ARROW_LONE:
    case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
      throw x.right.typeError("Multiplicity symbols are not allowed here");
    case DOMAIN:
      aa=cset(a);
      bb=cset(b);
      for(int i=1; i<bb.arity(); i++) aa=aa.product(Expression.UNIV);
      return aa.intersection(bb);
    case RANGE:
      aa=cset(a);
      bb=cset(b);
      for(int i=1; i<aa.arity(); i++) bb=Expression.UNIV.product(bb);
      return aa.intersection(bb);
    case EQUALS: if (x.left.type.isInt) return cint(a).eq(cint(b)); return cset(a).eq(cset(b));
  }
  throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprBinary.accept()");
}

//################################################################################################

public Object accept(ExprITE x) {
  Formula c=cform(x.cond.accept(this));
  Object l=x.left.accept(this);
  Object r=x.right.accept(this);
  if (l instanceof Formula) return c.implies(cform(l)).and(c.not().implies(cform(r)));
  if (l instanceof Expression) return c.thenElse(cset(l),cset(r));
  return c.thenElse(cint(l),cint(r));
}

//################################################################################################

public Object accept(ExprLet x) {
  Object r=x.right.accept(this);
  env.put(x.left, r);
  Object ans=x.sub.accept(this);
  env.remove(x.left);
  return ans;
}

//################################################################################################

public Object accept(ExprConstant x) {
  switch(x.op) {
    case NONE: return Expression.NONE;
    case IDEN: return Expression.IDEN;
    case UNIV: return Expression.UNIV;
    case SIGINT: return Expression.INTS;
    case NUMBER: return IntConstant.constant(x.num()); // zzz SHOULD WARN AGAINST SILENT TRUNCATION
  }
  throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprNamedConstant.accept()");
}

//################################################################################################

private Expr stripSetMult(Expr x) {
  if (!(x instanceof ExprUnary)) return x;
  ExprUnary y=(ExprUnary)x;
  if (y.op==ExprUnary.Op.SETMULT) return y.sub;
  if (y.op==ExprUnary.Op.ONEMULT) return y.sub;
  if (y.op==ExprUnary.Op.LONEMULT) return y.sub;
  if (y.op==ExprUnary.Op.SOMEMULT) return y.sub;
  return x;
}

private Formula quantN(ExprQuant x, VarDecl d, Expr dex) {
  // all  a:MULT | F(a)     becomes  all  a  | (a in MULT) => F(a)
  // some a:MULT | F(a)     becomes  some a  | (a in MULT) && F(a)
  // lone a:MULT | F(a)     becomes  all a,b | (a in MULT && b in MULT) => (F(a) && F(b) => a==b)
  Decls dd=null;
  //int ri=0; List<Expression> exprs=new ArrayList<Expression>();
  List<Variable> vars=new ArrayList<Variable>();
  Expression dv=cset(dex.accept(VisitorDemul.instance).accept(this));
  for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
  Formula ans1=cform(x.sub.accept(this));
  for(String n:d.names) { env.remove(n); }
  if (x.op==ExprQuant.Op.LONE) {
    for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
    Formula ans2=cform(x.sub.accept(this));
    for(String n:d.names) { env.remove(n); }
    Formula ans3=Formula.TRUE;
    for(int n=d.names.size(),i=0; i<n; i++) ans3=vars.get(i).eq(vars.get(i+n)).and(ans3);
    ans1=ans1.and(ans2).implies(ans3);
  }
  Formula guard=Formula.TRUE;
  for(Variable v:vars) guard=isIn(v,dex).and(guard);
  for(Variable v:vars) if (dd==null) dd=v.setOf(dv); else dd=dd.and(v.setOf(dv));
  if (x.op==ExprQuant.Op.SOME) return guard.and(ans1).forSome(dd); else return guard.implies(ans1).forAll(dd);
}

private Formula quant1(ExprQuant x, VarDecl d, Expr dex) {
  // lone a:MULT | F(a)   becomes  all a,b:MULT | (F(a) && F(b) => a==b)
  Decls dd=null;
  List<Variable> vars=new ArrayList<Variable>();
  ExprUnary.Op op=ExprUnary.Op.ONEMULT;
  if (dex instanceof ExprUnary) {
    if (((ExprUnary)dex).op==ExprUnary.Op.SETMULT) op=ExprUnary.Op.SETMULT;
    else if (((ExprUnary)dex).op==ExprUnary.Op.SOMEMULT) op=ExprUnary.Op.SOMEMULT;
    else if (((ExprUnary)dex).op==ExprUnary.Op.LONEMULT) op=ExprUnary.Op.LONEMULT;
  }
  Expression dv=cset(stripSetMult(dex).accept(this));
  for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
  Formula ans1=cform(x.sub.accept(this));
  for(String n:d.names) { env.remove(n); }
  if (x.op==ExprQuant.Op.LONE) {
    for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
    Formula ans2=cform(x.sub.accept(this));
    for(String n:d.names) { env.remove(n); }
    Formula ans3=Formula.TRUE;
    for(int n=d.names.size(),i=0; i<n; i++) ans3=vars.get(i).eq(vars.get(i+n)).and(ans3);
    ans1=ans1.and(ans2).implies(ans3);
  }
  for(Variable v:vars) switch(op) {
    case ONEMULT: if (dd==null) dd=v.oneOf(dv); else dd=dd.and(v.oneOf(dv)); break;
    case SETMULT: if (dd==null) dd=v.setOf(dv); else dd=dd.and(v.setOf(dv)); break;
    case LONEMULT: if (dd==null) dd=v.loneOf(dv); else dd=dd.and(v.loneOf(dv)); break;
    case SOMEMULT: if (dd==null) dd=v.someOf(dv); else dd=dd.and(v.someOf(dv)); break;
  }
  if (x.op==ExprQuant.Op.SOME) return ans1.forSome(dd); else return ans1.forAll(dd);
}

public Object accept(ExprQuant x) {
  Decls dd=null;
  int ri=0; List<Expression> exprs=new ArrayList<Expression>();
  int vi=0; List<Variable> vars=new ArrayList<Variable>();
  if (x.op==ExprQuant.Op.LONE || x.op==ExprQuant.Op.ALL || x.op==ExprQuant.Op.SOME) {
    VarDecl d=x.list.get(0);
    Expr dex=d.value;
    if (dex.type.arity()==1) return quant1(x,d,dex); return quantN(x,d,dex);
  }
  if (x.op==ExprQuant.Op.SUM) throw x.internalError("Integer quantification is currently unimplemented."/*zzz*/);
  for(VarDecl d:x.list) {
     Expr dex=d.value;
     if (dex.type.arity()>1) throw x.typeError("Each variable in set comprehension can only range over unary sets");
     if (dex instanceof ExprUnary) {
       if (((ExprUnary)dex).op==ExprUnary.Op.SETMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
       if (((ExprUnary)dex).op==ExprUnary.Op.LONEMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
       if (((ExprUnary)dex).op==ExprUnary.Op.SOMEMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
     }
     Expression dv=cset(stripSetMult(d.value).accept(this));
     exprs.add(dv);
     for(String n:d.names) { Variable var=Variable.nary("VAR<"+n+">",dv.arity()); vars.add(var); env.put(n,var); }
  }
  Formula ans=cform(x.sub.accept(this));
  for(VarDecl d:x.list) {
    Expression r=exprs.get(ri); ri++;
    for(String n:d.names) {
       env.remove(n);
       Variable v=vars.get(vi); vi++;
       if (dd==null) dd=v.oneOf(r); else dd=dd.and(v.oneOf(r));
    }
  }
  return ans.comprehension(dd);
}

//################################################################################################

public Object accept (ExprSequence x) {
  Formula ans=Formula.TRUE;
  for(int i=0; i<x.list.size(); i++) {
    Expr sub=x.list.get(i);
    ans=ans.and(cform(sub.accept(this)));
  }
  return ans;
}

//################################################################################################

public Object accept(ExprUnary x) {
  Object y=x.sub.accept(this);
  switch(x.op) {
    case SOMEMULT: case LONEMULT: case ONEMULT: case SETMULT:
      throw x.sub.typeError("Multiplicity symbols are not allowed here");
    case NOT: return cform(y).not();
    case SOME: return cset(y).some();
    case LONE: return cset(y).lone();
    case ONE: return cset(y).one();
    case NO: return cset(y).no();
    case TRANSPOSE: return cset(y).transpose();
    case RCLOSURE: return cset(y).reflexiveClosure();
    case CLOSURE: return cset(y).closure();
    case CARDINALITY: return cset(y).count();
    case SUM: return cset(y).sum();
    case INTTOATOM: return cint(y).toExpression();
  }
  throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprUnary.accept()");
}

//################################################################################################

public Object accept(ExprName x) {
  Object r = x.object;
  if (r instanceof ParaSig.Field.Full) {
     ParaSig.Field.Full y=(ParaSig.Field.Full)r;
     return rel(y);
  }
  if (r instanceof ParaSig) {
     ParaSig y=(ParaSig)r;
     return rel(y);
  }
  Object ans;
  if (r instanceof ParaFun) {
     ParaFun y=(ParaFun)r;
     if (y.argCount!=0) throw x.internalError("ExprName \""+x.name+"\" is not resolved prior to code gen! Its resolution == "+r);
     Env oldenv=this.env; this.env=new Env(); ans=y.value.accept(this); this.env=oldenv; return ans;
  }
  ans=env.get(x.name);
  if (ans==null) throw x.internalError("ExprName \""+x.name+"\" cannot be found during code gen! r=="+r);
  return ans;
}

//################################################################################################

public Object accept(ExprCall x) {
  ParaFun y=x.fun;
  if (y==null) throw x.internalError("ExprCall should now refer to a Function or Predicate");
  Env newenv=new Env();
  int r=0;
  for(VarDecl d:y.decls) {
    for(String n:d.names) {
        newenv.put(n,cset(x.args.get(r).accept(this))); r++;
    }
  }
  Env oldenv=this.env; this.env=newenv; Object ans=y.value.accept(this); this.env=oldenv; return ans;
}

//################################################################################################

  private final List<Unit> units;
  private Env env=new Env();
  private final Log log;
  public VisitorEval(Log log, List<Unit> units) { this.log=log; this.units=units; }

  private Map<ParaSig,Expression> sig2rel = new LinkedHashMap<ParaSig,Expression>();
  private Expression rel(ParaSig x) {
    if (x==ParaSig.UNIV) return Expression.UNIV;
    if (x==ParaSig.NONE) return Expression.NONE;
    if (x==ParaSig.SIGINT) return Expression.INTS;
    return sig2rel.get(x); }
  private void rel(ParaSig x,Expression y) { sig2rel.put(x,y); }

  private Map<ParaSig.Field.Full, Expression> field2rel = new LinkedHashMap<ParaSig.Field.Full,Expression>();
  private Expression rel(ParaSig.Field.Full x) { return field2rel.get(x); }
  private void rel(ParaSig.Field.Full x,Expression y) { field2rel.put(x,y); }

  private Map<ParaSig,Integer> sig2bound = new LinkedHashMap<ParaSig,Integer>();
  private int sig2bound(ParaSig x) { Integer y=sig2bound.get(x); if (y==null) return -1; return y; }
  private void sig2bound(ParaSig x,int y) { sig2bound.put(x,y); }

  private Map<ParaSig,TupleSet> sig2ts = new LinkedHashMap<ParaSig,TupleSet>();
  private TupleSet sig2ts(ParaSig x) { return sig2ts.get(x); }
  private void sig2ts(ParaSig x,TupleSet y) { sig2ts.put(x,y); }

  private Set<ParaSig> sig2exact = new LinkedHashSet<ParaSig>();
  private boolean exact(ParaSig x) { return sig2exact.contains(x); }
  private void exact(ParaSig x,boolean y) { if (y) sig2exact.add(x); else sig2exact.remove(x); }

  private Map<ParaSig,List<String>> sig2atoms = new LinkedHashMap<ParaSig,List<String>>();
  private List<String> sig2atoms(ParaSig x) {
    List<String> ans=sig2atoms.get(x);
    if (ans==null) { ans=new ArrayList<String>(); sig2atoms.put(x,ans); }
    return ans;
  }

  private void bound(String debug, ParaRuncheck c, ParaSig s, int b) {
    if (b<0)
       throw c.syntaxError("Cannot set a negative bound for signature \""+s.fullname+"\"");
    if (sig2bound(s)>=0)
       throw c.syntaxError("The signature \""+s.fullname+"\" already has a bound of "+sig2bound(s)+", so we cannot set it to "+b);
    if (b>=0 && (s==ParaSig.UNIV || s==ParaSig.NONE))
       throw c.syntaxError("You cannot specify a scope for the builtin signature \""+s.name+"\"");
    sig2bound(s,b);
    log.log(debug+"Sig \""+s.fullname+"\" bound to be <= "+b);
  }

  private boolean derive(ParaRuncheck cmd, List<ParaSig> sigs) {
    for(ParaSig s:sigs) {
      if (s.abs && sig2bound(s)<0 && s.subs.size()>0) {
         int sum=0;
         for(ParaSig c:s.subs) {if (sig2bound(c)<0) {sum=(-1);break;} sum=sum+sig2bound(c);}
         if (sum>=0) {bound("#1: ",cmd,s,sum);return true;}
      }
      if (s.abs && sig2bound(s)>=0 && s.subs.size()>0) {
         int sum=0; ParaSig cc=null;
         for(ParaSig c:s.subs) {if (sig2bound(c)>=0) sum=sum+sig2bound(c); else if (cc==null) cc=c; else {cc=null;break;}}
         if (cc!=null) {bound("#2: ",cmd,cc,(sig2bound(s)<sum)?0:sig2bound(s)-sum); return true;}
      }
      if (sig2bound(s)!=1 && !s.subset && s.one) {bound("#3: ",cmd,s,1); return true;}
      if ((sig2bound(s)<0 || sig2bound(s)>1) && !s.subset && s.lone) {bound("#4: ",cmd,s,1); return true;}
    }
    return false;
  }

  private boolean derive2(ParaRuncheck cmd, List<ParaSig> sigs) {
    boolean chg=false;
    for(ParaSig s:sigs) if (s.sup()!=null && sig2bound(s)<0 && sig2bound(s.sup())>=0) {
      bound("#5: ", cmd, s, sig2bound(s.sup()));
      chg=true;
    }
    return chg;
  }

  public int compute(Unit root, List<ParaSig> sigs, ParaRuncheck cmd) {
    final int overall;
    int bitwidth=(-1); // The bound on "int".
    for(ParaSig s:sigs) sig2bound(s,-1);
    if (cmd.names.size()==0 && cmd.overall<0) overall=3; else overall=cmd.overall;
    for(String n:cmd.names) {
       if (n.equals(ParaSig.BITWIDTH_NAME)) { bitwidth=cmd.getScope(n); continue; }
       Set<Object> set=root.lookup_sigORparam(n);
       Iterator<Object> it=set.iterator();
       if (set.size()>1) {
           ParaSig choice1=(ParaSig)(it.next());
           ParaSig choice2=(ParaSig)(it.next());
           throw cmd.syntaxError("The name \""+n+"\" is ambiguous: it could be "+choice1.fullname+" or "+choice2.fullname);
       }
       if (set.size()<1) throw cmd.syntaxError("The name \""+n+"\" cannot be found");
       ParaSig s=(ParaSig)(it.next());
       if (cmd.isExact(n)==true) exact(s,true);
       if (s.subset) throw cmd.syntaxError("Can not specify a scope for a subset signature \""+s.fullname+"\"");
       if (sig2bound(s)>=0) throw cmd.syntaxError("The signature \""+s.fullname+"\" already has a specified scope");
       bound("#6: ",cmd, s, cmd.getScope(n));
    }
    // Ensure "int" and "Int" are consistent
    if (bitwidth<0) bitwidth=4;
    if (bitwidth>=0) {
       if (bitwidth>30) throw cmd.syntaxError("Can not specify a bitwidth of greater than 30");
       int i=(1<<bitwidth);
       if (sig2bound(ParaSig.SIGINT)>i) throw cmd.syntaxError("With an integer bitwidth of "+bitwidth+" you must have exactly "+i+" Int atoms, thus the bound on Int cannot be "+sig2bound(ParaSig.SIGINT));
       if (sig2bound(ParaSig.SIGINT)<i && exact(ParaSig.SIGINT)) throw cmd.syntaxError("With an integer bitwidth of "+bitwidth+" you must allow exactly "+i+" Int atoms, thus the bound on Int cannot be exactly "+sig2bound(ParaSig.SIGINT));
       sig2bound(ParaSig.SIGINT, i);
    }
    // Derive the implicit bounds
    again: while(true) {
      while(derive(cmd,sigs)) {}
      // TopLevel sigs must have bounds!
      for(ParaSig s:sigs) if (!s.subset && s.sup()==null && sig2bound(s)<0) {
        if (overall<0) throw cmd.syntaxError("The signature \""+s.fullname+"\" needs a specific scope!");
        if (s.lone || s.one) bound("#7: ",cmd, s, 1); else bound("#8: ",cmd, s, overall);
        continue again;
      }
      break;
    }
    while(derive2(cmd,sigs)) {}
    return bitwidth;
  }

/*
Here, we will ignore subsetsigs.

The bounds are determined as follows:

. "run x": every topsig is bound to <= 3 elements.

. "run x for N": every topsif is bound to <= N elements.

. "run x for N but N1 SIG1, N2 SIG2...":
  Every sig following "but" is constrained explicitly.
  Any topsig that is
  a) not listed, and
  b) its bound is not derived implicitly
  will be bound to have <= N elements.

. "run x for N1 SIG1, N2 SIG2..."
  Every sig following "but" is constrained explicitly.
  Any topsig that is
  a) not listed, and
  b) its bound is not derived implicitly
  we will give an error message.

Implicit bounds are determined as follows:

. If an abstract signature X has no explicit bound,
  but its subsignatures have bounds (implicit or explicit),
  then X is implicitly bound to be <= the sum of its subsigs.

. If an abstract signature X has a bound (implicit or explicit),
  and all but one of its subsignatures have bounds (implicit or explicit),
  the remaining subsignature is implicitly bound to
  have <= the difference between the parent and the sibling sigs.

. A signature declared with the multiplicity keyword one has a bound == 1.

. A signature declared with the multiplicity keyword lone has a bound <= 1.
*/

/************************************************************************************************************
Code generation
************************************************************************************************************/

  private TupleSet comp(Type t, TupleFactory factory) {
    int a=t.arity(); if (a<1) throw new ErrorInternal(null,null,"Attempting to create a 0-arity TupleSet!");
    TupleSet ans=factory.noneOf(a);
    for(Type.Rel r:t) {
      TupleSet temp1=null,temp2;
      for(int i=0; i<r.basicTypes.size(); i++) {
         ParaSig b=r.basicTypes.get(i);
         if (b==ParaSig.UNIV) temp2=factory.allOf(1);
         else if (b==ParaSig.NONE) temp2=factory.noneOf(1);
         else temp2=sig2ts(b);
         if (temp1==null) temp1=temp2; else temp1=temp1.product(temp2);
      }
      for(Tuple xx:temp1) ans.add(xx);
    }
    return ans;
  }

  public void codegen(List<ParaSig> sigs)  {
    Formula kfact=Formula.TRUE;
    // Generate the relations for the SIGS.
    for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
      rel(s, Relation.unary(s.fullname));
    }
    // Generate the relations for the FIELDS
    for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
      Unit u=units.get(0).lookupPath(s.path);
      // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 below
      if (s.pos!=null && s.pos.filename!=null
         && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")
         && u.params.get("elem")!=null
         ) {
         Relation first=Relation.unary("first_");
         Relation last=Relation.unary("last_");
         Relation next=Relation.binary("next_");
         ParaSig s2=u.params.get("elem");
         rel(s.fields.get(0).full, rel(s).product(first));
         rel(s.fields.get(1).full, rel(s).product(last));
         rel(s.fields.get(2).full, rel(s).product(next));
         rel(s.fields.get(3).full, rel(s).product(next.transpose()));
         kfact=next.totalOrder((Relation)(rel(s2)), first, last).and(kfact);
         continue;
      }
      // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 above
      int fi=0;
      for(VarDecl fd:s.decls) {
        for(String fn:fd.names) {
           ParaSig.Field f=s.fields.get(fi); fi++;
           int a=fd.value.type.arity()+1;
           rel(f.full, Relation.nary(s.fullname+"."+fn, a));
        }
      }
    }
    // Add the constraints among SIGs
    for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
      if (s.subs.size()>1) {
        // If X and Y both extends P, then X and Y are disjoint
        for(int x1=0; x1<s.subs.size(); x1++)
         for(int x2=x1+1; x2<s.subs.size(); x2++) {
           Expression x11=rel(s.subs.get(x1));
           Expression x22=rel(s.subs.get(x2));
           kfact=x11.intersection(x22).no().and(kfact);
         }
      }
      if (s.abs && s.subs.size()>0) {
         // abstract sig with children == union of all children
         List<ParaSig> subs=s.subs;
         Expression x1=rel(subs.get(0));
         for(int x2=1; x2<subs.size(); x2++) x1=x1.union(rel(subs.get(x2)));
         kfact=x1.eq(rel(s)).and(kfact);
      }
      if (s.sup()!=null && s.sup()!=ParaSig.UNIV && !s.sup().abs) {
         // If X extends Y, then X in Y
         kfact=rel(s).in(rel(s.sup())).and(kfact);
      }
      if (s.sups().iterator().hasNext()) {
         // If X in Y1+..+Yn, then X in Y1+..+Yn
         Expression x1=null;
         for(ParaSig x2:s.sups()) {
            if (x1==null) x1=rel(x2); else x1=x1.union(rel(x2));
         }
         kfact=rel(s).in(x1).and(kfact);
      }
      if (s.lone) kfact=rel(s).lone().and(kfact); // "lone"
      if (s.one) kfact=rel(s).one().and(kfact);   // "one"
      if (s.some) kfact=rel(s).some().and(kfact); // "some"
    }
    // Add the regular facts
    for(Unit u:units) for(Map.Entry<String,ParaFact> e:u.facts.entrySet()) {
      kfact=((Formula)(e.getValue().value.accept(this))).and(kfact);
    }
    // Go thru the commands
    for(ParaRuncheck x:units.get(0).runchecks) {
      log.flush();
      log.log("\n\u001b[31mComputing the bounds for the command \""+x+"\"...\u001b[0m");
      log.flush();
      sig2bound.clear();
      sig2ts.clear();
      sig2exact.clear();
      sig2atoms.clear();
      int bitwidth=compute(units.get(0), sigs, x);
      // zzz ALLOY3 compatiblity hack below
      for(ParaSig s:sigs) if (s!=ParaSig.SIGINT && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) {
        ParaSig s2=units.get(0).lookupPath(s.path).params.get("elem");
        if (sig2bound(s2)<=0) throw x.syntaxError("The signature "+s2.fullname+" must have a bound >= 1, since it is used to instantiate the util/ordering.als module");
        log.log("Compatibility hack: "+s2.fullname+" set to exactly "+sig2bound(s2));
        if (s2!=null) exact(s2,true);
      }
      // zzz ALLOY3 compatiblity hack above
      this.env.clear();
      runcheck(x, units.get(0), bitwidth, sigs, kfact);
    }
  }

  /*
  private int childrenSum(ParaRuncheck cmd, ParaSig s) {
    int n=0;
    for(ParaSig c:s.subs) {
      if (sig2bound(c)<0) throw cmd.syntaxError("The signature \""+c.fullname+"\" must have a bound!");
      n=n+sig2bound(c);
    }
    return n;
  }
  */

  private final Map<String,Integer> unique=new LinkedHashMap<String,Integer>();

  private String makeAtom(String n) {
    int i=0;
    if (unique.containsKey(n)) i=unique.get(n);
    String ans=new String(n+"."+i);
    unique.put(n,i+1);
    return ans;
  }

  public Relation right(Expression x) { return (Relation) (((BinaryExpression)x).right()); }

  public IntConstant makeIntConstant(int bitwidth,int i) {
    // We know 1 <= bitwidth <= 30
    // bitwidth==1 ==> MIN==-1 MAX==0
    // bitwidth==2 ==> MIN==-2 MAX==1
    // bitwidth==3 ==> MIN==-4 MAX==3
    int min = 0-(1<<(bitwidth-1));
    int max = (0-min)-1;
    if (i<min || i>max) throw new ErrorSyntax(null,"The scope of "+i+" is too small to fit in a 2's complement integer with bitwidth=="+bitwidth);
    return IntConstant.constant(i);
  }

  public void runcheck(ParaRuncheck cmd, Unit root, int bitwidth, List<ParaSig> sigs, Formula kfact)  {
    // Make the universe
    unique.clear();
    if (bitwidth<1 || bitwidth>30) throw cmd.syntaxError("The integer bitwidth must be between 1..30");
    List<String> atoms=new ArrayList<String>();
    for(int i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++) {
      String ii=new String(""+i);
      atoms.add(ii); sig2atoms(ParaSig.SIGINT).add(ii);
    }
    for(ParaSig s:sigs) if (s!=ParaSig.SIGINT && !s.subset) {
      int sc=sig2bound(s);
      if (sc<0) throw cmd.syntaxError("The signature \""+s.fullname+"\" must have a bound!");
      if (sc==0) continue;
      /*if (s.sup()!=null && !s.isSubtypeOf(ParaSig.SIGINT) && childrenSum(cmd, s.sup())<=sig2bound(s.sup())) {
        int k=0;
        for(ParaSig child:s.sup().subs) { if (child==s) break; k=k+sig2bound(child); }
        List<String> sa=sig2atoms(s);
        List<String> sua=sig2atoms(s.sup());
        while(sa.size()<sc) { sa.add(sua.get(k)); k++; }
      } else*/ if (s.sup()!=null) {
        if (sig2bound(s.sup())<sc) throw cmd.syntaxError("The parent sig \""+s.sup().fullname+"\" cannot have fewer elements than the subsig \""+s.fullname+"\"");
        sig2atoms(s).addAll(sig2atoms(s.sup()));
      } else {
        List<String> sa=sig2atoms(s);
        while(sa.size()<sc) {String a=makeAtom(s.name); atoms.add(a); sa.add(a);}
      }
    }
    final Universe universe = new Universe(atoms);
    final TupleFactory factory = universe.factory();
    final Bounds bounds = new Bounds(universe);
    sig2ts(ParaSig.UNIV, factory.allOf(1));
    sig2ts(ParaSig.NONE, factory.noneOf(1));
    sig2ts(ParaSig.SIGINT, factory.noneOf(1));
    for(int j=0,i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++,j++) {
      Tuple ii=factory.tuple(sig2atoms(ParaSig.SIGINT).get(j));
      bounds.boundExactly(i,factory.range(ii,ii));
      sig2ts(ParaSig.SIGINT).add(ii);
    }
    // Bound the TOPSIGS and SUBSIGS
    for(int si=sigs.size()-1; si>=0; si--) {
      ParaSig s=sigs.get(si);
      if (s.subset || s==ParaSig.SIGINT) continue;
      List<String> at=sig2atoms(s);
      TupleSet ts;
      if (at.size()==0) ts=factory.noneOf(1);
      else ts=factory.range(factory.tuple(at.get(0)), factory.tuple(at.get(at.size()-1)));
      for(ParaSig sub:s.subs) for(Tuple temp:sig2ts(sub)) ts.add(temp);
      sig2ts(s,ts);
      if (exact(s) && ts.size()==sig2bound(s)) {
        log.log("SIG "+s.fullname+" BOUNDEXACTLY=<"+ts.toString()+">");
        bounds.boundExactly((Relation)(rel(s)),ts);
      }
      else if (exact(s)) {
        log.log("SIG "+s.fullname+" BOUND=<"+ts.toString()+"> and #=="+sig2bound(s));
        bounds.bound((Relation)(rel(s)),ts);
        if (sig2bound(s)==0) kfact=rel(s).no().and(kfact);
        else if (sig2bound(s)==1) kfact=rel(s).one().and(kfact);
        else kfact=rel(s).count().eq(makeIntConstant(bitwidth, sig2bound(s))).and(kfact);
      }
      else if (ts.size()>sig2bound(s)) {
        log.log("SIG "+s.fullname+" BOUND=<"+ts.toString()+"> and #<="+sig2bound(s));
        bounds.bound((Relation)(rel(s)),ts);
        if (sig2bound(s)==0) kfact=rel(s).no().and(kfact);
        else if (sig2bound(s)==1) kfact=rel(s).lone().and(kfact);
        else kfact=rel(s).count().lte(makeIntConstant(bitwidth, sig2bound(s))).and(kfact);
      }
      else {
        log.log("SIG "+s.fullname+" BOUND=<"+ts.toString()+">");
        bounds.bound((Relation)(rel(s)),ts);
      }
    }
    // Bound the SUBSETSIGS
    for(int si=0; si<sigs.size(); si++) {
      ParaSig s=sigs.get(si);
      if (!s.subset) continue;
      TupleSet ts=factory.noneOf(1);
      for(ParaSig sup:s.sups()) for(Tuple temp:sig2ts(sup)) ts.add(temp);
      log.log("SUBSETSIG "+s.fullname+" BOUND=<"+ts.toString()+">");
      bounds.bound((Relation)(rel(s)),ts); sig2ts(s,ts);
    }
    // Bound the FIELDS
    for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
      // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 below
      if (s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) {
         Relation first=null, last=null, next=null;
         TupleSet ts1=null, ts2=null;
         for(ParaSig.Field f:s.fields) {
           if (f.name.equals("first_")) { first=right(rel(f.full)); ts1=comp(f.halftype, factory); }
           if (f.name.equals("last_")) { last=right(rel(f.full)); }
           if (f.name.equals("next_")) { next=right(rel(f.full)); ts2=comp(f.halftype, factory); }
         }
         bounds.bound(first,ts1);
         bounds.bound(last,ts1);
         bounds.bound(next,ts2);
         continue;
      }
      // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 above
      int fi=0;
      for(VarDecl fd:s.decls) {
        for(int fn=fd.names.size(); fn>0; fn--) {
          ParaSig.Field f=s.fields.get(fi); fi++;
          TupleSet ts=comp(s.type.product_of_anyEmptyness(fd.value.type), factory); // fulltype
          bounds.bound((Relation)(rel(f.full)),ts);
        }
      }
    }
    if (cmd.check) {
      ParaAssert e=root.asserts.get(cmd.name);
      if (e==null) throw cmd.syntaxError("The assertion \""+cmd.name+"\" cannot be found.");
      try {
        Formula f=((Formula)(e.value.accept(this))).not().and(kfact);
        Solver solver = new Solver();
        //solver.options().setSolver(SATFactory.DefaultSAT4J);
        solver.options().setSolver(SATFactory.MiniSat);
        //solver.options().setSolver(SATFactory.ZChaffBasic);
        solver.options().setBitwidth(bitwidth);
        solver.options().setIntEncoding(Options.IntEncoding.BINARY);
        log.log0("Bitwidth="+bitwidth+" Checking \""+e.name+"\"...\t ");
        log.flush();
        //new MakeJava(f,bitwidth,bounds);
        Solution sol = solver.solve(f,bounds);
        long t1=sol.stats().translationTime();
        long t2=sol.stats().solvingTime();
        switch(sol.outcome()) {
          case TRIVIALLY_SATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" TRIVIALLY VIOLATED (SAT)");
            break;
          case TRIVIALLY_UNSATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" TRIVIALLY OK (UNSAT)");
            break;
          case SATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" VIOLATED (SAT) TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".");
            if (cmd.expects==0) for(Relation r:sol.instance().relations()) log.log("REL "+r+" = "+sol.instance().tuples(r));
            break;
          case UNSATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" OK (UNSAT) TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".");
            break;
        }
      } catch(HigherOrderDeclException ex) { log.log("Analysis cannot be performed because it contains higher-order quanitifcation that could not be skolemized.");
      } catch(TimeoutException ex) { log.log("Timeout");
      } catch(ErrorInternal ex) { log.log(ex.msg);
      } catch(ErrorType ex) { log.log(ex.msg);
      } catch(ErrorSyntax ex) { log.log(ex.msg); }
    }
    else {
      List<ParaFun> ee=root.funs.get(cmd.name);
      if (ee.size()>1) throw cmd.syntaxError("There are more than 1 predicate with the same name \""+cmd.name+"\"!");
      if (ee.size()<1) throw cmd.syntaxError("The predicate \""+cmd.name+"\" cannot be found.");
      ParaFun e=ee.get(0);
      Expr v=e.value;
      if (e.type!=null) {
         Expr vv=e.type;
         if (vv instanceof ExprUnary) vv=((ExprUnary)vv).makeMult();
         v=ExprBinary.Op.IN.make(v.pos, v, vv, Type.FORMULA);
      }
      if (e.argCount>0) v=ExprQuant.Op.SOME.make(v.pos, e.decls, v, Type.FORMULA);
      try {
        Formula f=((Formula)(v.accept(this))).and(kfact);
        Solver solver = new Solver();
        //solver.options().setSolver(SATFactory.DefaultSAT4J);
        solver.options().setSolver(SATFactory.MiniSat);
        //solver.options().setSolver(SATFactory.ZChaffBasic);
        solver.options().setBitwidth(bitwidth);
        solver.options().setIntEncoding(Options.IntEncoding.BINARY);
        log.log0("Bitwidth="+bitwidth+" Running \""+e.name+"\"...\t ");
        log.flush();
        //new MakeJava(f,bitwidth,bounds);
        Solution sol = solver.solve(f,bounds);
        long t1=sol.stats().translationTime();
        long t2=sol.stats().solvingTime();
        switch(sol.outcome()) {
          case TRIVIALLY_SATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" TRIVIALLY SAT");
            break;
          case TRIVIALLY_UNSATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" TRIVIALLY UNSAT");
            break;
          case SATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" SAT TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".");
            if (cmd.expects==0) for(Relation r:sol.instance().relations()) log.log("REL "+r+" = "+sol.instance().tuples(r));
            break;
          case UNSATISFIABLE: log.log("TIME="+t1+"+"+t2+"="+(t1+t2)+" UNSAT TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".");
            break;
        }
      } catch(HigherOrderDeclException ex) { log.log("Analysis cannot be performed because it contains higher-order quanitifcation that could not be skolemized.");
      } catch(TimeoutException ex) { log.log("Timeout");
      } catch(ErrorInternal ex) { log.log(ex.msg);
      } catch(ErrorType ex) { log.log(ex.msg);
      } catch(ErrorSyntax ex) { log.log(ex.msg); }
    }
    log.flush();
  }

}
