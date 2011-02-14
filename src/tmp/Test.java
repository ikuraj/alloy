package tmp;
import java.util.Arrays;
import java.util.List;
import kodkod.ast.*;
import kodkod.ast.operator.*;
import kodkod.instance.*;
import kodkod.util.nodes.PrettyPrinter;
import kodkod.engine.*;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.config.Options;

public final class Test {

    public static void main(String[] args) throws Exception {

        Relation x0 = Relation.unary("Int/min");
        Relation x1 = Relation.unary("Int/zero");
        Relation x2 = Relation.unary("Int/max");
        Relation x3 = Relation.nary("Int/next", 2);
        Relation x4 = Relation.unary("seq/Int");
        Relation x5 = Relation.unary("String");
        Relation x6 = Relation.unary("this/A");
        Relation x7 = Relation.unary("this/Relation");
        Relation x8 = Relation.nary("this/Relation.r", 5);

        List<String> atomlist = Arrays.asList("-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "0",
                "1", "2", "3", "4", "5", "6", "7", "unused0", "unused1");

        Universe universe = new Universe(atomlist);
        TupleFactory factory = universe.factory();
        Bounds bounds = new Bounds(universe);

        TupleSet x0_upper = factory.noneOf(1);
        x0_upper.add(factory.tuple("-8"));
        bounds.boundExactly(x0, x0_upper);

        TupleSet x1_upper = factory.noneOf(1);
        x1_upper.add(factory.tuple("0"));
        bounds.boundExactly(x1, x1_upper);

        TupleSet x2_upper = factory.noneOf(1);
        x2_upper.add(factory.tuple("7"));
        bounds.boundExactly(x2, x2_upper);

        TupleSet x3_upper = factory.noneOf(2);
        x3_upper.add(factory.tuple("-8").product(factory.tuple("-7")));
        x3_upper.add(factory.tuple("-7").product(factory.tuple("-6")));
        x3_upper.add(factory.tuple("-6").product(factory.tuple("-5")));
        x3_upper.add(factory.tuple("-5").product(factory.tuple("-4")));
        x3_upper.add(factory.tuple("-4").product(factory.tuple("-3")));
        x3_upper.add(factory.tuple("-3").product(factory.tuple("-2")));
        x3_upper.add(factory.tuple("-2").product(factory.tuple("-1")));
        x3_upper.add(factory.tuple("-1").product(factory.tuple("0")));
        x3_upper.add(factory.tuple("0").product(factory.tuple("1")));
        x3_upper.add(factory.tuple("1").product(factory.tuple("2")));
        x3_upper.add(factory.tuple("2").product(factory.tuple("3")));
        x3_upper.add(factory.tuple("3").product(factory.tuple("4")));
        x3_upper.add(factory.tuple("4").product(factory.tuple("5")));
        x3_upper.add(factory.tuple("5").product(factory.tuple("6")));
        x3_upper.add(factory.tuple("6").product(factory.tuple("7")));
        bounds.boundExactly(x3, x3_upper);

        TupleSet x4_upper = factory.noneOf(1);
        x4_upper.add(factory.tuple("0"));
        bounds.boundExactly(x4, x4_upper);

        TupleSet x5_upper = factory.noneOf(1);
        bounds.boundExactly(x5, x5_upper);

        TupleSet x6_upper = factory.noneOf(1);
        x6_upper.add(factory.tuple("unused0"));
        bounds.bound(x6, x6_upper);

        TupleSet x7_upper = factory.noneOf(1);
        x7_upper.add(factory.tuple("unused1"));
        bounds.bound(x7, x7_upper);

        TupleSet x8_upper = factory.noneOf(5);
        x8_upper.add(factory.tuple("unused1").product(factory.tuple("unused0")).product(
                factory.tuple("unused0")).product(factory.tuple("unused0")).product(
                factory.tuple("unused0")));
        bounds.bound(x8, x8_upper);

        bounds.boundExactly(-8, factory.range(factory.tuple("-8"), factory.tuple("-8")));
        bounds.boundExactly(-7, factory.range(factory.tuple("-7"), factory.tuple("-7")));
        bounds.boundExactly(-6, factory.range(factory.tuple("-6"), factory.tuple("-6")));
        bounds.boundExactly(-5, factory.range(factory.tuple("-5"), factory.tuple("-5")));
        bounds.boundExactly(-4, factory.range(factory.tuple("-4"), factory.tuple("-4")));
        bounds.boundExactly(-3, factory.range(factory.tuple("-3"), factory.tuple("-3")));
        bounds.boundExactly(-2, factory.range(factory.tuple("-2"), factory.tuple("-2")));
        bounds.boundExactly(-1, factory.range(factory.tuple("-1"), factory.tuple("-1")));
        bounds.boundExactly(0, factory.range(factory.tuple("0"), factory.tuple("0")));
        bounds.boundExactly(1, factory.range(factory.tuple("1"), factory.tuple("1")));
        bounds.boundExactly(2, factory.range(factory.tuple("2"), factory.tuple("2")));
        bounds.boundExactly(3, factory.range(factory.tuple("3"), factory.tuple("3")));
        bounds.boundExactly(4, factory.range(factory.tuple("4"), factory.tuple("4")));
        bounds.boundExactly(5, factory.range(factory.tuple("5"), factory.tuple("5")));
        bounds.boundExactly(6, factory.range(factory.tuple("6"), factory.tuple("6")));
        bounds.boundExactly(7, factory.range(factory.tuple("7"), factory.tuple("7")));

        Variable x12 = Variable.unary("this");
        Decls x11 = x12.oneOf(x7);
        Expression x16 = x12.join(x8);
        Expression x19 = x6.product(x6);
        Expression x18 = x6.product(x19);
        Expression x17 = x18.product(x6);
        Formula x15 = x16.in(x17);
        Variable x23 = Variable.unary("");
        Decls x22 = x23.oneOf(Expression.UNIV);
        Variable x26 = Variable.unary("");
        Decls x25 = x26.oneOf(Expression.UNIV);
        Variable x28 = Variable.unary("");
        Decls x27 = x28.oneOf(Expression.UNIV);
        Decls x21 = x22.and(x25).and(x27);
        Expression x32 = x28.product(x26);
        Expression x31 = x32.product(x23);
        Expression x34 = x6.product(x6);
        Expression x33 = x6.product(x34);
        Formula x30 = x31.in(x33);
        Expression x39 = x28.join(x16);
        Expression x38 = x26.join(x39);
        Expression x37 = x23.join(x38);
        Formula x36 = x37.one();
        Formula x40 = x37.in(x6);
        Formula x35 = x36.and(x40);
        Formula x29 = x30.implies(x35);
        Formula x20 = x29.forAll(x21);
        Formula x14 = x15.and(x20);
        Variable x43 = Variable.unary("");
        Decls x42 = x43.oneOf(x6);
        Expression x45 = x16.join(x43);
        Expression x47 = x6.product(x6);
        Expression x46 = x6.product(x47);
        Formula x44 = x45.in(x46);
        Formula x41 = x44.forAll(x42);
        Formula x13 = x14.and(x41);
        Formula x10 = x13.forAll(x11);
        Expression x52 = x8.join(Expression.UNIV);
        Expression x51 = x52.join(Expression.UNIV);
        Expression x50 = x51.join(Expression.UNIV);
        Expression x49 = x50.join(Expression.UNIV);
        Formula x48 = x49.in(x7);
        Formula x53 = x0.eq(x0);
        Formula x54 = x1.eq(x1);
        Formula x55 = x2.eq(x2);
        Formula x56 = x3.eq(x3);
        Formula x57 = x4.eq(x4);
        Formula x58 = x5.eq(x5);
        Formula x59 = x6.eq(x6);
        Formula x60 = x7.eq(x7);
        Formula x61 = x8.eq(x8);
        Formula x9 = Formula.compose(FormulaOperator.AND, x10, x48, x53, x54, x55, x56, x57, x58,
                x59, x60, x61);

        Solver solver = new Solver();
        solver.options().setSolver(SATFactory.DefaultSAT4J);
        solver.options().setBitwidth(4);
        solver.options().setFlatten(false);
        solver.options().setIntEncoding(Options.IntEncoding.TWOSCOMPLEMENT);
        solver.options().setSymmetryBreaking(20);
        solver.options().setSkolemDepth(0);
        System.out.println("Solving...");
        System.out.println(PrettyPrinter.print(x9, 2));
        System.out.flush();
        Solution sol = solver.solve(x9, bounds);
        System.out.println(sol.toString());
        System.out.println(new Evaluator(sol.instance()).evaluate(x9));
    }
}
