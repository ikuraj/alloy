package edu.mit.csail.sdg.alloy4.translator;

import java.util.Collections;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.io.PrintWriter;
import java.io.FileNotFoundException;

import edu.mit.csail.sdg.alloy4.helper.ErrorInternal;
import edu.mit.csail.sdg.alloy4.helper.Pos;
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
import kodkod.ast.ProjectExpression;
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
import kodkod.ast.visitor.VoidVisitor;
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;
import kodkod.instance.Tuple;

public final class TranslateKodkodToJava implements VoidVisitor {

  private final PrintWriter file;
  private final Pos pos;

  /**
   * Given an existing Kodkod formula node, generate a Java program that would generate an equivalent node.
   *
   * @param pos - if we throw an exception, this is the location in the Alloy model we want to blame
   * @param x - the formula to convert
   * @param bitwidth - the integer bitwidth
   * @param bounds - the Kodkod bounds object to use
   */
  public static void convert(Pos pos, Formula x, int bitwidth, Bounds bounds) {
      new TranslateKodkodToJava(pos, x, bitwidth, bounds);
  }

  /**
   * Constructor is private, so that the only way to access this class is via the static convert() method.
   *
   * @param pos - if we throw an exception, this is the location in the Alloy model we want to blame
   * @param x - the formula to convert
   * @param bitwidth - the integer bitwidth
   * @param bounds - the Kodkod bounds object to use
   */
  private TranslateKodkodToJava(Pos pos, Formula x, int bitwidth, Bounds bounds) {
    this.pos=pos;
    try {
      file=new PrintWriter("/zweb/zweb/Alloy4/tmp/Test.java");
    } catch(FileNotFoundException e) {
      throw new ErrorInternal(pos,null,"Cannot open the file \"/zweb/zweb/Alloy4/Test.java");
    }
    file.println("import kodkod.ast.IntExpression;");
    file.println("import kodkod.ast.Expression;");
    file.println("import kodkod.ast.BinaryExpression;");
    file.println("import kodkod.ast.Formula;");
    file.println("import kodkod.ast.Relation;");
    file.println("import kodkod.ast.Variable;");
    file.println("import kodkod.ast.IntConstant;");
    file.println("import kodkod.ast.Decls;");
    file.println("import kodkod.engine.Solution;");
    file.println("import kodkod.engine.Solver;");
    file.println("import kodkod.engine.TimeoutException;");
    file.println("import kodkod.engine.satlab.SATFactory;");
    file.println("import kodkod.engine.Options;");
    file.println("import kodkod.engine.fol2sat.HigherOrderDeclException;");
    file.println("import kodkod.instance.Bounds;");
    file.println("import kodkod.instance.TupleFactory;");
    file.println("import kodkod.instance.TupleSet;");
    file.println("import kodkod.instance.Tuple;");
    file.println("import kodkod.instance.Universe;");
    file.println();
    file.println("public final class Test {");
    file.println();
    file.println("public static void main(String[] args) throws Exception {");
    file.println();
    ArrayList<String> atomlist=new ArrayList<String>();
    LinkedHashSet<String> atomset=new LinkedHashSet<String>();
    for(Relation r:bounds.relations()) {
       String name=jk_newname();
       jk_map.put(r, name);
       file.printf("Relation %s = Relation.nary(\"%s\", %d);%n%n", name, r.name(), r.arity());
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
    file.printf("String[] atoms = {%n");
    int j=(-1);
    for(String a:atomlist) {
      if (j!=(-1)) file.printf(","); else j=0;
      if (j==5) {file.printf("%n "); j=0;}
         else {file.printf(" ");j++;}
      file.printf("\"%s\"",a);
    }
    file.printf("%n};%n%n");
    file.printf("java.util.ArrayList<String> atomlist=new java.util.ArrayList<String>();%n");
    file.printf("for(String a:atoms) atomlist.add(a);%n");
    file.printf("Universe universe = new Universe(atomlist);%n");
    file.printf("TupleFactory factory = universe.factory();%n");
    file.printf("Bounds bounds = new Bounds(universe);%n%n");
    for(Relation r:bounds.relations()) {
       String n=jk_map.get(r);
       TupleSet upper=bounds.upperBound(r);
       TupleSet lower=bounds.lowerBound(r);
       printTupleset(n+"_upper", upper);
       if (upper.equals(lower)) {
          file.printf("bounds.boundExactly(%s, %s_upper);%n%n",n,n);
       }
       else if (lower.size()==0) {
          file.printf("bounds.bound(%s, %s_upper);%n%n",n,n);
       }
       else {
          printTupleset(n+"_lower", lower);
          file.printf("bounds.bound(%s, %s_lower, %s_upper);%n%n",n,n,n);
       }
    }
    for(int i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++) {
      file.printf("bounds.boundExactly(%d,factory.range("
      +"factory.tuple(\"%d\"),factory.tuple(\"%d\")));%n%n", i, i, i);
    }
    x.accept(this);
    file.printf("%nSolver solver = new Solver();");
    file.printf("%nsolver.options().setSolver(SATFactory.ZChaffBasic);");
    file.printf("%nsolver.options().setBitwidth(%d);",bitwidth);
    file.printf("%nsolver.options().setIntEncoding(Options.IntEncoding.BINARY);");
    file.printf("%nSolution sol = solver.solve(%s,bounds);",jk_map.get(x));
    file.printf("%nSystem.out.println(sol.toString());");
    file.printf("%n}}%n");
    file.close();
  }

  private void printTupleset(String n, TupleSet ts) {
    file.printf("TupleSet %s = factory.noneOf(%d);%n", n, ts.arity());
    for(Tuple t:ts) {
      file.printf("%s.add(",n);
      for(int i=0; i<ts.arity(); i++) {
        if (i!=0) file.printf(".product(");
        file.printf("factory.tuple(\"%s\")", (String)(t.atom(i)));
        if (i!=0) file.printf(")");
      }
      file.printf(");%n");
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
      case DIFFERENCE: file.printf("Expression %s=%s.difference(%s);%n", newname, left, right); break;
      case INTERSECTION: file.printf("Expression %s=%s.intersection(%s);%n", newname, left, right); break;
      case JOIN: file.printf("Expression %s=%s.join(%s);%n", newname, left, right); break;
      case OVERRIDE: file.printf("Expression %s=%s.override(%s);%n", newname, left, right); break;
      case PRODUCT: file.printf("Expression %s=%s.product(%s);%n", newname, left, right); break;
      case UNION: file.printf("Expression %s=%s.union(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(ComparisonFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case EQUALS: file.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
      case SUBSET: file.printf("Formula %s=%s.in(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(ProjectExpression x) {
    throw new ErrorInternal(pos,x,"ProjectExpression should not have been encountered");
  }

  public void visit(IntComparisonFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case EQ: file.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
      case GT: file.printf("Formula %s=%s.gt(%s);%n", newname, left, right); break;
      case GTE: file.printf("Formula %s=%s.gte(%s);%n", newname, left, right); break;
      case LT: file.printf("Formula %s=%s.lt(%s);%n", newname, left, right); break;
      case LTE: file.printf("Formula %s=%s.lte(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(BinaryFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case AND: file.printf("Formula %s=%s.and(%s);%n", newname, left, right); break;
      case OR: file.printf("Formula %s=%s.or(%s);%n", newname, left, right); break;
      case IMPLIES: file.printf("Formula %s=%s.implies(%s);%n", newname, left, right); break;
      case IFF: file.printf("Formula %s=%s.iff(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(BinaryIntExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.left().accept(this);  String left=jk_map.get(x.left());
    x.right().accept(this); String right=jk_map.get(x.right());
    switch(x.op()) {
      case PLUS: file.printf("IntExpression %s=%s.plus(%s);%n", newname, left, right); break;
      case MINUS: file.printf("IntExpression %s=%s.minus(%s);%n", newname, left, right); break;
      case MULTIPLY: file.printf("IntExpression %s=%s.multiply(%s);%n", newname, left, right); break;
      case DIVIDE: file.printf("IntExpression %s=%s.divide(%s);%n", newname, left, right); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(UnaryExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.op()) {
      case CLOSURE: file.printf("Expression %s=%s.closure();%n", newname, sub); break;
      case REFLEXIVE_CLOSURE: file.printf("Expression %s=%s.reflexiveClosure();%n", newname, sub); break;
      case TRANSPOSE: file.printf("Expression %s=%s.transpose();%n", newname, sub); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(IfExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.condition().accept(this);  String a=jk_map.get(x.condition());
    x.thenExpr().accept(this);  String b=jk_map.get(x.thenExpr());
    x.elseExpr().accept(this);  String c=jk_map.get(x.elseExpr());
    file.printf("Expression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
  }

  public void visit(IfIntExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.condition().accept(this);  String a=jk_map.get(x.condition());
    x.thenExpr().accept(this);  String b=jk_map.get(x.thenExpr());
    x.elseExpr().accept(this);  String c=jk_map.get(x.elseExpr());
    file.printf("IntExpression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
  }

  public void visit(NotFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.formula().accept(this); String sub=jk_map.get(x.formula());
    file.printf("Formula %s=%s.not();%n", newname, sub);
  }

  public void visit(IntToExprCast x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.intExpr().accept(this); String sub=jk_map.get(x.intExpr());
    file.printf("Expression %s=%s.toExpression();%n", newname, sub);
  }

  public void visit(ExprToIntCast x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.op()) {
      case CARDINALITY: file.printf("IntExpression %s=%s.count();%n", newname, sub); break;
      case SUM: file.printf("IntExpression %s=%s.sum();%n", newname, sub); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod operator \""+x.op()+"\" encountered");
    }
  }

  public void visit(IntConstant x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    file.printf("IntExpression %s=IntConstant.constant(%d);%n", newname, x.value());
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
    else throw new ErrorInternal(pos,x,"Unknown kodkod ConstantExpression \""+x+"\" encountered");
    jk_map.put(x,newname);
  }

  public void visit(Variable x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    file.printf("Variable %s=Variable.nary(\"%s\",%d);%n", newname, x.name(), x.arity());
  }

  public void visit(Comprehension x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.formula().accept(this); String f=jk_map.get(x.formula());
    file.printf("Expression %s=%s.comprehension(%s);%n",newname,f,d);
  }

  public void visit(QuantifiedFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.formula().accept(this); String f=jk_map.get(x.formula());
    switch(x.quantifier()) {
      case ALL: file.printf("Formula %s=%s.forAll(%s);%n",newname,f,d); break;
      case SOME: file.printf("Formula %s=%s.forSome(%s);%n",newname,f,d); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod quantifier \""+x.quantifier()+"\" encountered");
    }
  }

  public void visit(SumExpression x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.declarations().accept(this); String d=jk_map.get(x.declarations());
    x.intExpr().accept(this); String f=jk_map.get(x.intExpr());
    file.printf("IntExpression %s=%s.sum(%s);%n",newname,f,d);
  }

  public void visit(MultiplicityFormula x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.expression().accept(this); String sub=jk_map.get(x.expression());
    switch(x.multiplicity()) {
      case LONE: file.printf("Formula %s=%s.lone();%n",newname,sub); break;
      case ONE: file.printf("Formula %s=%s.one();%n",newname,sub); break;
      case SOME: file.printf("Formula %s=%s.some();%n",newname,sub); break;
      case NO: file.printf("Formula %s=%s.no();%n",newname,sub); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
    }
  }

  public void visit(Decl x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.variable().accept(this); String v=jk_map.get(x.variable());
    x.expression().accept(this); String e=jk_map.get(x.expression());
    switch(x.multiplicity()) {
      case LONE: file.printf("Decls %s=%s.loneOf(%s);%n",newname,v,e); break;
      case ONE: file.printf("Decls %s=%s.oneOf(%s);%n",newname,v,e); break;
      case SOME: file.printf("Decls %s=%s.someOf(%s);%n",newname,v,e); break;
      case SET: file.printf("Decls %s=%s.setOf(%s);%n",newname,v,e); break;
      default: throw new ErrorInternal(pos,x,"Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
    }
  }

  public void visit(Decls x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    for (Decl y:x) { y.accept(this); }
    boolean first=true;
    file.printf("Decls %s=",newname);
    for(Decl y:x) {
      String z=jk_map.get(y);
      if (first) {file.printf("%s",z); first=false;} else file.printf(".and(%s)",z);
    }
    file.printf(";%n");
  }

  public void visit(RelationPredicate x) {
    if (jk_map.containsKey(x)) return; String newname=jk_newname(); jk_map.put(x,newname);
    x.relation().accept(this); String r=jk_map.get(x.relation());
    if (x.name()==RelationPredicate.Name.TOTAL_ORDERING) {
        final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) x;
        tp.ordered().accept(this); String o=jk_map.get(tp.ordered());
        tp.first().accept(this); String f=jk_map.get(tp.first());
        tp.last().accept(this); String l=jk_map.get(tp.last());
        file.printf("Formula %s=%s.totalOrder(%s,%s,%s);%n",newname,r,o,f,l);
        return;
    }
    throw new ErrorInternal(pos,x,"Unknown RelationPredicate \""+x+"\" encountered");
  }
}
