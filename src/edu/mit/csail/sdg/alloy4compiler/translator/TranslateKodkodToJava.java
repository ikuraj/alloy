/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import java.util.Collections;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.io.PrintWriter;
import java.io.StringWriter;
import edu.mit.csail.sdg.alloy4.ConstMap;
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
import kodkod.ast.IntExpression;
import kodkod.ast.IntToExprCast;
import kodkod.ast.Node;
import kodkod.ast.ProjectExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.SumExpression;
import kodkod.ast.UnaryIntExpression;
import kodkod.ast.Variable;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.visitor.VoidVisitor;
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;
import kodkod.instance.Tuple;
import kodkod.util.ints.IndexedEntry;

/**
 * Given a Kodkod formula node, this class can generate a Java program that (when compiled and executed)
 * would solve that formula.
 *
 * <p> Requirements: atoms must be String objects (since we cannot possibly
 * output a Java source code that can re-generate arbitrary Java objects).
 */

public final class TranslateKodkodToJava implements VoidVisitor {

    /**
     * Given a Kodkod formula node, return a Java program that (when compiled and executed) would solve that formula.
     *
     * <p> Requirement: atoms must be String objects (since we cannot possibly
     * output a Java source code that can re-generate arbitrary Java objects).
     *
     * @param formula - the formula to convert
     * @param bitwidth - the integer bitwidth
     * @param atoms - an iterator over the set of all atoms
     * @param bounds - the Kodkod bounds object to use
     * @param atomMap - if nonnull, it is used to map the atom name before printing
     */
    public static String convert(Formula formula, int bitwidth, Iterator<Object> atoms, Bounds bounds, ConstMap<Object,String> atomMap) {
        StringWriter string=new StringWriter();
        PrintWriter file=new PrintWriter(string);
        new TranslateKodkodToJava(file, formula, bitwidth, atoms, bounds, atomMap);
        if (file.checkError()) {
            return ""; // shouldn't happen
        } else {
            return string.toString();
        }
    }

    /** The PrintWriter that is receiving the text. */
    private final PrintWriter file;

    /** This caches nodes that we have already generated. */
    private final LinkedHashMap<Node,String> map=new LinkedHashMap<Node,String>();

    /** Given a node, return its name (if no name has been chosen, then make a new name) */
    private String makename(Node obj) {
        if (map.containsKey(obj)) return null;
        String name="x"+(map.size());
        map.put(obj,name);
        return name;
    }

    /** Given a node, call the visitor to dump its text out, then return its name. */
    private String make(Node x) { x.accept(this); return map.get(x); }

    /** Constructor is private, so that the only way to access this class is via the static convert() method. */
    private TranslateKodkodToJava
    (PrintWriter pw, Formula x, int bitwidth, Iterator<Object> atoms, Bounds bounds, Map<Object,String> atomMap) {
        file=pw;
        file.print("import java.util.Arrays;\n");
        file.print("import java.util.List;\n");
        file.print("import kodkod.ast.*;\n");
        file.print("import kodkod.instance.*;\n");
        file.print("import kodkod.engine.*;\n");
        file.print("import kodkod.engine.satlab.SATFactory;\n");
        file.print("import kodkod.engine.config.Options;\n\n");
        file.print("public final class Test {\n\n");
        file.print("public static void main(String[] args) throws Exception {\n\n");
        ArrayList<String> atomlist=new ArrayList<String>();
        while(atoms.hasNext()) {
            Object a = atoms.next();
            String b = atomMap==null ? null : atomMap.get(a);
            atomlist.add(b==null ? a.toString() : b);
        }
        for(Relation r:bounds.relations()) {
            String name=makename(r);
            file.printf("Relation %s = Relation.nary(\"%s\", %d);%n", name, r.name(), r.arity());
        }
        Collections.sort(atomlist);
        file.printf("%nList<String> atomlist = Arrays.asList(%n");
        int j=(-1);
        for(String a:atomlist) {
            if (j!=(-1)) file.printf(","); else j=0;
            if (j==5) {file.printf("%n "); j=0;}
            else {file.printf(" ");j++;}
            file.printf("\"%s\"", a);
        }
        file.printf("%n);%n%n");
        file.printf("Universe universe = new Universe(atomlist);%n");
        file.printf("TupleFactory factory = universe.factory();%n");
        file.printf("Bounds bounds = new Bounds(universe);%n%n");
        for(Relation r:bounds.relations()) {
            String n=map.get(r);
            TupleSet upper=bounds.upperBound(r);
            TupleSet lower=bounds.lowerBound(r);
            printTupleset(n+"_upper", upper, atomMap);
            if (upper.equals(lower)) {
                file.printf("bounds.boundExactly(%s, %s_upper);%n%n",n,n);
            }
            else if (lower.size()==0) {
                file.printf("bounds.bound(%s, %s_upper);%n%n",n,n);
            }
            else {
                printTupleset(n+"_lower", lower, atomMap);
                file.printf("bounds.bound(%s, %s_lower, %s_upper);%n%n",n,n,n);
            }
        }
        for(IndexedEntry<TupleSet> i:bounds.intBounds()) {
            for(Tuple t:i.value()) {
                Object a=t.atom(0);
                String b=atomMap==null ? null : atomMap.get(a);
                String c=(b!=null? b : a.toString());
                file.printf("bounds.boundExactly(%d,factory.range("
                +"factory.tuple(\"%s\"),factory.tuple(\"%s\")));%n", i.index(), c, c);
            }
        }
        file.printf("%n");
        String result=make(x);
        file.printf("%nSolver solver = new Solver();");
        file.printf("%nsolver.options().setLogTranslation(true);");
        file.printf("%nsolver.options().setSolver(SATFactory.DefaultSAT4J); // Or \"SATFactory.MiniSatProver\" if you want unsat core");
        file.printf("%nsolver.options().setBitwidth(%d);",bitwidth);
        file.printf("%nsolver.options().setIntEncoding(Options.IntEncoding.BINARY);");
        file.printf("%nSolution sol = solver.solve(%s,bounds);", result);
        file.printf("%nSystem.out.println(sol.toString());");
        file.printf("%n}}%n");
        file.close();
    }

    private void printTupleset(String n, TupleSet ts, Map<Object,String> atomMap) {
        file.printf("TupleSet %s = factory.noneOf(%d);%n", n, ts.arity());
        for(Tuple t:ts) {
            file.printf("%s.add(",n);
            for(int i=0; i<ts.arity(); i++) {
                if (i!=0) file.printf(".product(");
                Object a=t.atom(i);
                String b=atomMap==null ? null : atomMap.get(a);
                String c=(b!=null? b : a.toString());
                file.printf("factory.tuple(\"%s\")", c);
                if (i!=0) file.printf(")");
            }
            file.printf(");%n");
        }
    }

    public void visit(Relation x) {
        if (!map.containsKey(x)) throw new RuntimeException("Unknown kodkod relation \""+x.name()+"\" encountered");
    }

    public void visit(BinaryExpression x) {
        String newname=makename(x); if (newname==null) return;
        String left=make(x.left());
        String right=make(x.right());
        switch(x.op()) {
           case DIFFERENCE: file.printf("Expression %s=%s.difference(%s);%n", newname, left, right); break;
           case INTERSECTION: file.printf("Expression %s=%s.intersection(%s);%n", newname, left, right); break;
           case JOIN: file.printf("Expression %s=%s.join(%s);%n", newname, left, right); break;
           case OVERRIDE: file.printf("Expression %s=%s.override(%s);%n", newname, left, right); break;
           case PRODUCT: file.printf("Expression %s=%s.product(%s);%n", newname, left, right); break;
           case UNION: file.printf("Expression %s=%s.union(%s);%n", newname, left, right); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(ComparisonFormula x) {
        String newname=makename(x); if (newname==null) return;
        String left=make(x.left());
        String right=make(x.right());
        switch(x.op()) {
           case EQUALS: file.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
           case SUBSET: file.printf("Formula %s=%s.in(%s);%n", newname, left, right); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(ProjectExpression x) {
        String newname=makename(x); if (newname==null) return;
        String expr=make(x.expression());
        List<String> names=new ArrayList<String>();
        for(IntExpression i:x.columns()) names.add(make(i));
        for(int i=0; i<names.size(); i++) {
            if (i==0) file.printf("Expression %s=%s.over(", newname, expr); else file.printf(",");
            file.printf("%s", names.get(i));
        }
        file.printf(";%n");
    }

    public void visit(IntComparisonFormula x) {
        String newname=makename(x); if (newname==null) return;
        String left=make(x.left());
        String right=make(x.right());
        switch(x.op()) {
           case EQ: file.printf("Formula %s=%s.eq(%s);%n", newname, left, right); break;
           case GT: file.printf("Formula %s=%s.gt(%s);%n", newname, left, right); break;
           case GTE: file.printf("Formula %s=%s.gte(%s);%n", newname, left, right); break;
           case LT: file.printf("Formula %s=%s.lt(%s);%n", newname, left, right); break;
           case LTE: file.printf("Formula %s=%s.lte(%s);%n", newname, left, right); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(BinaryFormula x) {
        String newname=makename(x); if (newname==null) return;
        String left=make(x.left());
        String right=make(x.right());
        switch(x.op()) {
           case AND: file.printf("Formula %s=%s.and(%s);%n", newname, left, right); break;
           case OR: file.printf("Formula %s=%s.or(%s);%n", newname, left, right); break;
           case IMPLIES: file.printf("Formula %s=%s.implies(%s);%n", newname, left, right); break;
           case IFF: file.printf("Formula %s=%s.iff(%s);%n", newname, left, right); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(BinaryIntExpression x) {
        String newname=makename(x); if (newname==null) return;
        String left=make(x.left());
        String right=make(x.right());
        switch(x.op()) {
           case PLUS: file.printf("IntExpression %s=%s.plus(%s);%n", newname, left, right); break;
           case MINUS: file.printf("IntExpression %s=%s.minus(%s);%n", newname, left, right); break;
           case MULTIPLY: file.printf("IntExpression %s=%s.multiply(%s);%n", newname, left, right); break;
           case DIVIDE: file.printf("IntExpression %s=%s.divide(%s);%n", newname, left, right); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(UnaryIntExpression x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.expression());
        switch(x.op()) {
           case MINUS: file.printf("IntExpression %s=%s.negate();%n", newname, sub); break;
           case NOT: file.printf("IntExpression %s=%s.not();%n", newname, sub); break;
           case ABS: file.printf("IntExpression %s=%s.abs();%n", newname, sub); break;
           case SGN: file.printf("IntExpression %s=%s.signum();%n", newname, sub); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(UnaryExpression x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.expression());
        switch(x.op()) {
           case CLOSURE: file.printf("Expression %s=%s.closure();%n", newname, sub); break;
           case REFLEXIVE_CLOSURE: file.printf("Expression %s=%s.reflexiveClosure();%n", newname, sub); break;
           case TRANSPOSE: file.printf("Expression %s=%s.transpose();%n", newname, sub); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(IfExpression x) {
        String newname=makename(x); if (newname==null) return;
        String a=make(x.condition());
        String b=make(x.thenExpr());
        String c=make(x.elseExpr());
        file.printf("Expression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
    }

    public void visit(IfIntExpression x) {
        String newname=makename(x); if (newname==null) return;
        String a=make(x.condition());
        String b=make(x.thenExpr());
        String c=make(x.elseExpr());
        file.printf("IntExpression %s=%s.thenElse(%s,%s);%n", newname, a, b, c);
    }

    public void visit(NotFormula x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.formula());
        file.printf("Formula %s=%s.not();%n", newname, sub);
    }

    public void visit(IntToExprCast x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.intExpr());
        file.printf("Expression %s=%s.toExpression();%n", newname, sub);
    }

    public void visit(ExprToIntCast x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.expression());
        switch(x.op()) {
           case CARDINALITY: file.printf("IntExpression %s=%s.count();%n", newname, sub); break;
           case SUM: file.printf("IntExpression %s=%s.sum();%n", newname, sub); break;
           default: throw new RuntimeException("Unknown kodkod operator \""+x.op()+"\" encountered");
        }
    }

    public void visit(IntConstant x) {
        String newname=makename(x); if (newname==null) return;
        file.printf("IntExpression %s=IntConstant.constant(%d);%n", newname, x.value());
    }

    public void visit(ConstantFormula x) {
        if (map.containsKey(x)) return;
        String newname=(x.booleanValue() ? "Formula.TRUE" : "Formula.FALSE");
        map.put(x,newname);
    }

    public void visit(ConstantExpression x) {
        if (map.containsKey(x)) return;
        String newname=null;
        if (x==Expression.NONE) newname="Expression.NONE";
           else if (x==Expression.UNIV) newname="Expression.UNIV";
           else if (x==Expression.IDEN) newname="Expression.IDEN";
           else if (x==Expression.INTS) newname="Expression.INTS";
           else throw new RuntimeException("Unknown kodkod ConstantExpression \""+x+"\" encountered");
        map.put(x,newname);
    }

    public void visit(Variable x) {
        String newname=makename(x); if (newname==null) return;
        file.printf("Variable %s=Variable.nary(\"%s\",%d);%n", newname, x.name(), x.arity());
    }

    public void visit(Comprehension x) {
        String newname=makename(x); if (newname==null) return;
        String d=make(x.declarations());
        String f=make(x.formula());
        file.printf("Expression %s=%s.comprehension(%s);%n",newname,f,d);
    }

    public void visit(QuantifiedFormula x) {
        String newname=makename(x); if (newname==null) return;
        String d=make(x.declarations());
        String f=make(x.formula());
        switch(x.quantifier()) {
           case ALL: file.printf("Formula %s=%s.forAll(%s);%n",newname,f,d); break;
           case SOME: file.printf("Formula %s=%s.forSome(%s);%n",newname,f,d); break;
           default: throw new RuntimeException("Unknown kodkod quantifier \""+x.quantifier()+"\" encountered");
        }
    }

    public void visit(SumExpression x) {
        String newname=makename(x); if (newname==null) return;
        String d=make(x.declarations());
        String f=make(x.intExpr());
        file.printf("IntExpression %s=%s.sum(%s);%n",newname,f,d);
    }

    public void visit(MultiplicityFormula x) {
        String newname=makename(x); if (newname==null) return;
        String sub=make(x.expression());
        switch(x.multiplicity()) {
           case LONE: file.printf("Formula %s=%s.lone();%n",newname,sub); break;
           case ONE: file.printf("Formula %s=%s.one();%n",newname,sub); break;
           case SOME: file.printf("Formula %s=%s.some();%n",newname,sub); break;
           case NO: file.printf("Formula %s=%s.no();%n",newname,sub); break;
           default: throw new RuntimeException("Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
        }
    }

    public void visit(Decl x) {
        String newname=makename(x); if (newname==null) return;
        String v=make(x.variable());
        String e=make(x.expression());
        switch(x.multiplicity()) {
           case LONE: file.printf("Decls %s=%s.loneOf(%s);%n",newname,v,e); break;
           case ONE: file.printf("Decls %s=%s.oneOf(%s);%n",newname,v,e); break;
           case SOME: file.printf("Decls %s=%s.someOf(%s);%n",newname,v,e); break;
           case SET: file.printf("Decls %s=%s.setOf(%s);%n",newname,v,e); break;
           default: throw new RuntimeException("Unknown kodkod multiplicity \""+x.multiplicity()+"\" encountered");
        }
    }

    public void visit(Decls x) {
        String newname=makename(x); if (newname==null) return;
        for (Decl y:x) { y.accept(this); }
        boolean first=true;
        file.printf("Decls %s=",newname);
        for(Decl y:x) {
            String z=map.get(y);
            if (first) {file.printf("%s",z); first=false;} else file.printf(".and(%s)",z);
        }
        file.printf(";%n");
    }

    public void visit(RelationPredicate x) {
        String newname=makename(x); if (newname==null) return;
        String rel=make(x.relation());
        switch(x.name()) {
          case TOTAL_ORDERING: {
             final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) x;
             String o=make(tp.ordered());
             String f=make(tp.first());
             String l=make(tp.last());
             file.printf("Formula %s=%s.totalOrder(%s,%s,%s);%n",newname,rel,o,f,l);
             return;
          }
          case FUNCTION: {
             final RelationPredicate.Function tp = (RelationPredicate.Function) x;
             String domain=make(tp.domain());
             String range=make(tp.range());
             switch(((RelationPredicate.Function)x).targetMult()) {
               case ONE: file.printf("Formula %s=%s.function(%s,%s);%n",newname,rel,domain,range); break;
               case LONE: file.printf("Formula %s=%s.functional(%s,%s);%n",newname,rel,domain,range); break;
               default: throw new RuntimeException("Illegal multiplicity encountered in RelationPredicate.Function");
             }
          }
          case ACYCLIC: file.printf("Formula %s=%s.acyclic();%n",newname,rel);
        }
        throw new RuntimeException("Unknown RelationPredicate \""+x+"\" encountered");
    }
}
