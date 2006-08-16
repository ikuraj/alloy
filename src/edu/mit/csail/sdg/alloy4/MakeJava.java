package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.ExprToIntCast;
import kodkod.ast.IfExpression;
import kodkod.ast.IfIntExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntToExprCast;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.SumExpression;
import kodkod.ast.Variable;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;
import kodkod.instance.Tuple;
import kodkod.ast.visitor.VoidVisitor;

public final class MakeJava implements VoidVisitor {

  public MakeJava(Formula x, int bitwidth, Bounds bounds) {
    System.err.println("import kodkod.ast.IntExpression;");
    System.err.println("import kodkod.ast.Expression;");
    System.err.println("import kodkod.ast.BinaryExpression;");
    System.err.println("import kodkod.ast.Formula;");
    System.err.println("import kodkod.ast.Relation;");
    System.err.println("import kodkod.ast.Variable;");
    System.err.println("import kodkod.ast.IntConstant;");
    System.err.println("import kodkod.ast.Decls;");
    System.err.println("import kodkod.engine.Solution;");
    System.err.println("import kodkod.engine.Solver;");
    System.err.println("import kodkod.engine.TimeoutException;");
    System.err.println("import kodkod.engine.satlab.SATFactory;");
    System.err.println("import kodkod.engine.Options;");
    System.err.println("import kodkod.engine.fol2sat.HigherOrderDeclException;");
    System.err.println("import kodkod.instance.Bounds;");
    System.err.println("import kodkod.instance.TupleFactory;");
    System.err.println("import kodkod.instance.TupleSet;");
    System.err.println("import kodkod.instance.Tuple;");
    System.err.println("import kodkod.instance.Universe;");
    System.err.println();
    System.err.println("public final class Test {");
    System.err.println();
    System.err.println("public static void main(String[] args) throws Exception {");
    System.err.println();
    ArrayList<String> atomlist=new ArrayList<String>();
    LinkedHashSet<String> atomset=new LinkedHashSet<String>();
    for(Relation r:bounds.relations()) {
       String name=jk_newname();
       jk_map.put(r, name);
       System.err.printf("Relation %s = Relation.nary(\"%s\", %d);%n%n", name, r.name(), r.arity());
       TupleSet ts=bounds.upperBound(r);
       for(Tuple t:ts) for(int i=0; i<t.arity(); i++) {
         String atom=(String)(t.atom(i));
         if (!atomset.contains(atom)) {atomset.add(atom); atomlist.add(atom);}
       }
    }
    for(int i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++) {
      String j=""+i;
      if (!atomset.contains(j)) {atomset.add(j); atomlist.add(j);}
    }
    Collections.sort(atomlist);
    System.err.printf("String[] atoms = {%n");
    int j=(-1);
    for(String a:atomlist) {
      if (j!=(-1)) System.err.printf(","); else j=0;
      if (j==5) {System.err.printf("%n "); j=0;}
         else {System.err.printf(" ");j++;}
      System.err.printf("\"%s\"",a);
    }
    System.err.printf("%n};%n%n");
    System.err.printf("java.util.ArrayList<String> atomlist=new java.util.ArrayList<String>();%n");
    System.err.printf("for(String a:atoms) atomlist.add(a);%n");
    System.err.printf("Universe universe = new Universe(atomlist);%n");
    System.err.printf("TupleFactory factory = universe.factory();%n");
    System.err.printf("Bounds bounds = new Bounds(universe);%n%n");
    for(Relation r:bounds.relations()) {
       String n=jk_map.get(r);
       TupleSet upper=bounds.upperBound(r);
       TupleSet lower=bounds.lowerBound(r);
       printTupleset(n+"_upper", upper);
       if (upper.equals(lower)) {
          System.err.printf("bounds.boundExactly(%s, %s_upper);%n%n",n,n);
       }
       else if (lower.size()==0) {
          System.err.printf("bounds.bound(%s, %s_upper);%n%n",n,n);
       }
       else {
          printTupleset(n+"_lower", lower);
          System.err.printf("bounds.bound(%s, x%s_lower, %s_upper);%n%n",n,n,n);
       }
    }
    for(int i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++) {
      System.err.printf("bounds.boundExactly(%d,factory.range("
      +"factory.tuple(\"%d\"),factory.tuple(\"%d\")));%n%n", i, i, i);
    }
    x.accept(this);
    System.err.printf("%nSolver solver = new Solver();");
    System.err.printf("%nsolver.options().setSolver(SATFactory.ZChaffBasic);");
    System.err.printf("%nsolver.options().setBitwidth(%d);",bitwidth);
    System.err.printf("%nsolver.options().setIntEncoding(Options.IntEncoding.BINARY);");
    System.err.printf("%nSolution sol = solver.solve(%s,bounds);",jk_map.get(x));
    System.err.printf("%nSystem.out.println(sol.toString());");
    System.err.printf("%n}}%n");
  }

  private void printTupleset(String n, TupleSet ts) {
    System.err.printf("TupleSet %s = factory.noneOf(%d);%n", n, ts.arity());
    for(Tuple t:ts) {
      System.err.printf("%s.add(",n);
      for(int i=0; i<ts.arity(); i++) {
        if (i!=0) System.err.printf(".product(");
        System.err.printf("factory.tuple(\"%s\")", (String)(t.atom(i)));
        if (i!=0) System.err.printf(")");
      }
      System.err.printf(");%n");
    }
  }

  private int jk_i=0;

  private final LinkedHashMap<Object,String> jk_map=new LinkedHashMap<Object,String>();

  private String jk_newname() {
    String name="x"+jk_i;
    jk_i++;
    return name;
  }

  public void visit(Relation x) {}

  public void visit(BinaryExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case DIFFERENCE: System.err.printf("Expression %s=%s.difference(%s);%n", newname, left, right); break;
      case INTERSECTION: System.err.printf("Expression %s=%s.intersection(%s);%n", newname, left, right); break;
      case JOIN: System.err.printf("Expression %s=%s.join(%s);%n", newname, left, right); break;
      case OVERRIDE: System.err.printf("Expression %s=%s.override(%s);%n", newname, left, right); break;
      case PRODUCT: System.err.printf("Expression %s=%s.product(%s);%n", newname, left, right); break;
      case UNION: System.err.printf("Expression %s=%s.union(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(ComparisonFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case EQUALS: System.err.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
      case SUBSET: System.err.printf("Formula %s=%s.in(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(IntComparisonFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case EQ: System.err.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
      case GT: System.err.printf("Formula %s=%s.gt(%s);%n", newname, left, right); break;
      case GTE: System.err.printf("Formula %s=%s.gte(%s);%n", newname, left, right); break;
      case LT: System.err.printf("Formula %s=%s.lt(%s);%n", newname, left, right); break;
      case LTE: System.err.printf("Formula %s=%s.lte(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(BinaryFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case AND: System.err.printf("Formula %s=%s.and(%s);%n", newname, left, right); break;
      case OR: System.err.printf("Formula %s=%s.or(%s);%n", newname, left, right); break;
      case IMPLIES: System.err.printf("Formula %s=%s.implies(%s);%n", newname, left, right); break;
      case IFF: System.err.printf("Formula %s=%s.iff(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(BinaryIntExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case PLUS: System.err.printf("IntExpression %s=%s.plus(%s);%n", newname, left, right); break;
      case MINUS: System.err.printf("IntExpression %s=%s.minus(%s);%n", newname, left, right); break;
      case MULTIPLY: System.err.printf("IntExpression %s=%s.multiply(%s);%n", newname, left, right); break;
      case DIVIDE: System.err.printf("IntExpression %s=%s.divide(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(UnaryExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.op()) {
      case CLOSURE: System.err.printf("Expression %s=%s.closure();%n", newname, sub); break;
      case REFLEXIVE_CLOSURE: System.err.printf("Expression %s=%s.reflexiveClosure();%n", newname, sub); break;
      case TRANSPOSE: System.err.printf("Expression %s=%s.transpose();%n", newname, sub); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(IfExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.condition().accept(this);  String a=jk_map.get(x.condition());
    x.thenExpr().accept(this);  String b=jk_map.get(x.thenExpr());
    x.elseExpr().accept(this);  String c=jk_map.get(x.elseExpr());
    System.err.printf("Expression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
  }

  public void visit(IfIntExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.condition().accept(this);  String a=jk_map.get(x.condition());
    x.thenExpr().accept(this);  String b=jk_map.get(x.thenExpr());
    x.elseExpr().accept(this);  String c=jk_map.get(x.elseExpr());
    System.err.printf("IntExpression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
  }

  public void visit(NotFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.formula().accept(this); String sub=jk_map.get(x.formula());
    System.err.printf("Formula %s=%s.not();%n", newname, sub);
  }

  public void visit(IntToExprCast x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.intExpr().accept(this); String sub=jk_map.get(x.intExpr());
    System.err.printf("Expression %s=%s.toExpression();%n", newname, sub);
  }

  public void visit(ExprToIntCast x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.op()) {
      case CARDINALITY: System.err.printf("IntExpression %s=%s.count();%n", newname, sub); break;
      case SUM: System.err.printf("IntExpression %s=%s.sum();%n", newname, sub); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(IntConstant x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    System.err.printf("IntExpression %s=IntConstant.constant(%d);%n", newname, x.value());
  }

  public void visit(ConstantFormula x) {
    if (jk_map.containsKey(x)) return;
    String newname=(x.booleanValue() ? "Formula.TRUE" : "Formula.FALSE");
    jk_map.put(x,newname);
  }

  public void visit(ConstantExpression x) {
    if (jk_map.containsKey(x)) return;
    String newname=null;
    if (x==Expression.NONE) newname="Expression.NONE";
    else if (x==Expression.UNIV) newname="Expression.UNIV";
    else if (x==Expression.IDEN) newname="Expression.IDEN";
    else if (x==Expression.INTS) newname="Expression.INTS";
    else throw new ErrorInternal(null,x,"Unknown kodkod ConstantExpression \""+x+"\" encountered");
    jk_map.put(x,newname);
  }

  public void visit(Variable x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    System.err.printf("Variable %s=Variable.nary(\"%s\",%d);%n", newname, x.name(), x.arity());
  }

  public void visit(Comprehension x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.formula().accept(this); String f=jk_map.get(x.formula());
    System.err.printf("Expression %s=%s.comprehension(%s);%n",newname,f,d);
  }

  public void visit(QuantifiedFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.formula().accept(this); String f=jk_map.get(x.formula());
    switch(x.quantifier()) {
      case ALL: System.err.printf("Formula %s=%s.forAll(%s);%n",newname,f,d); break;
      case SOME: System.err.printf("Formula %s=%s.forSome(%s);%n",newname,f,d); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod quantifier \""+x.quantifier()+"\" encountered");
    }
  }

  public void visit(SumExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.intExpr().accept(this); String f=jk_map.get(x.intExpr());
    System.err.printf("IntExpression %s=%s.sum(%s);%n",newname,f,d);
  }

  public void visit(MultiplicityFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.multiplicity()) {
      case LONE: System.err.printf("Formula %s=%s.lone();%n",newname,sub); break;
      case ONE: System.err.printf("Formula %s=%s.one();%n",newname,sub); break;
      case SOME: System.err.printf("Formula %s=%s.some();%n",newname,sub); break;
      case NO: System.err.printf("Formula %s=%s.no();%n",newname,sub); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
    }
  }

  public void visit(Decl x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.variable().accept(this); String v=jk_map.get(x.variable());
    x.expression().accept(this); String e=jk_map.get(x.expression());
    switch(x.multiplicity()) {
      case LONE: System.err.printf("Decls %s=%s.loneOf(%s);%n",newname,v,e); break;
      case ONE: System.err.printf("Decls %s=%s.oneOf(%s);%n",newname,v,e); break;
      case SOME: System.err.printf("Decls %s=%s.someOf(%s);%n",newname,v,e); break;
      case SET: System.err.printf("Decls %s=%s.setOf(%s);%n",newname,v,e); break;
      default: throw new ErrorInternal(null,x,"Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
    }
  }

  public void visit(Decls x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    for (Decl y:x) { y.accept(this); }
    boolean first=true;
    System.err.printf("Decls %s=",newname);
    for(Decl y:x) {
      String z=jk_map.get(y);
      if (first) {System.err.printf("%s",z); first=false;} else System.err.printf(".and(%s)",z);
    }
    System.err.printf(";%n");
  }

  public void visit(RelationPredicate x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.relation().accept(this); String r=jk_map.get(x.relation());
    if (x.name()==RelationPredicate.Name.TOTAL_ORDERING) {
        final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) x;
        tp.ordered().accept(this); String o=jk_map.get(tp.ordered());
        tp.first().accept(this); String f=jk_map.get(tp.first());
        tp.last().accept(this); String l=jk_map.get(tp.last());
        System.err.printf("Formula %s=%s.totalOrder(%s,%s,%s);%n",newname,r,o,f,l);
        return;
    }
    throw new ErrorInternal(null,x,"Unknown RelationPredicate \""+x+"\" encountered");
  }
}
