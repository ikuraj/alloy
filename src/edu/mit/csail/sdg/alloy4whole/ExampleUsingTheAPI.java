///*
// * Alloy Analyzer
// * Copyright (c) 2007 Massachusetts Institute of Technology
// *
// * This program is free software; you can redistribute it and/or
// * modify it under the terms of the GNU General Public License
// * as published by the Free Software Foundation; either version 2
// * of the License, or (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program; if not, write to the Free Software
// * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
// */
//
//package edu.mit.csail.sdg.alloy4whole;
//
//import edu.mit.csail.sdg.alloy4.Err;
//import edu.mit.csail.sdg.alloy4.Pos;
//import edu.mit.csail.sdg.alloy4.Util;
//import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
//import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
//import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
//import edu.mit.csail.sdg.alloy4compiler.parser.Module;
//import edu.mit.csail.sdg.alloy4compiler.ast.Func;
//import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
//import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
//import edu.mit.csail.sdg.alloy4compiler.parser.World;
//import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
//import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
//
//public class ExampleUsingTheAPI {
//
//    @SuppressWarnings("unused")
//    public static void main(String[] args) throws Err {
//
//        // Chooses the Alloy4 options
//        A4Options opt = new A4Options();
//        opt.solver = A4Options.SatSolver.SAT4J;
//
//        // Construct the World; every Sig/Pred/Fun/... must belong to some World.
//        World world = new World();
//
//        // Construct the Root Module of that World
//        Module root = world.establishModule(null, Util.asList(""));
//
//        // abstract sig A {}
//        PrimSig A = root.addSig("A", Sig.UNIV, true, false, false, false);
//
//        // sig B {}
//        PrimSig B = root.addSig("B", Sig.UNIV, false, false, false, false);
//
//        // one sig A1 extends A {}
//        PrimSig A1 = root.addSig("A1", A, false, false, true, false);
//
//        // one sig A2 extends A {}
//        PrimSig A2 = root.addSig("A2", A, false, false, true, false);
//
//        // A { f: B lone->lone B }
//        Expr f = A.addField(Pos.UNKNOWN, "f", B.lone_arrow_lone(B));
//        // Since (B lone->lone B) is not unary,  the default is "setOf",  meaning "f:set (B lone->lone B)"
//
//        // A { g: B }
//        Expr g = A.addField(Pos.UNKNOWN, "g", B);
//        // The line above is the same as:   A.addField("g", bexpr.oneOf())  since B is unary.
//        // If you want "setOf", you need:   A.addField("g", bexpr.setOf())
//
//        // pred someG { some g }
//        Func someG = root.addFun(null, "SomeG", null, null);
//        someG.setBody(g.some());
//
//        // pred atMostThree[x:univ, y:univ] { #(x+y) >= 3 }
//        ExprVar x = UNIV.oneOf("x");
//        ExprVar y = UNIV.oneOf("x");
//        Func atMost3 = root.addFun(null, "atMost3", Util.asList(x,y), null);
//        atMost3.setBody(x.plus(y).cardinality().lte(ExprConstant.makeNUMBER(3)));
//
//        // run { some A && atMostThree[B,B] } for 3 but 3 int, 3 seq
//        Expr expr1 = A.some().and(atMost3.call(B,B));
//        /*
//        Command cmd1 = root.addCommand(expr1, 3, 3, 3, null);
//        A4Solution sol1 = TranslateAlloyToKodkod.execute_command(world, cmd1, opt, null, null);
//        System.out.println("[Solution1]:");
//        System.out.println(sol1.toString());
//
//        // run { some f && SomeG[] } for 3 but 2 int, 1 seq, 5 A, exactly 6 B
//        Map<String,Integer> scope2 = new LinkedHashMap<String,Integer>();
//        scope2.put("A", 5);
//        scope2.put("B", -7); // To say exactly N, use the number "-N-1". So, to say "exactly 6", use "-7".
//        Expr expr2 = f.some().and(someG.call());
//        Command cmd2 = root.addCommand(expr2, 3, 2, 1, scope2);
//        A4Solution sol2 = TranslateAlloyToKodkod.execute_command(world, cmd2, opt, null, null);
//        System.out.println("[Solution2]:");
//        System.out.println(sol2.toString());
//        */
//    }
//}
