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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4whole;

import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;
import static edu.mit.csail.sdg.alloy4.A4Reporter.NOP;

public final class ExampleUsingTheAPI {

    @SuppressWarnings("unused")
    public static void main(String[] args) throws Err {

        // Chooses the Alloy4 options
        A4Options opt = new A4Options();
        opt.solver = A4Options.SatSolver.SAT4J;

        // abstract sig A {}
        PrimSig A = new PrimSig(null, UNIV, "A", true, false, false, false, false);

        // sig B {}
        PrimSig B = new PrimSig(null, UNIV, "B", false, false, false, false, false);

        // one sig A1 extends A {}
        PrimSig A1 = new PrimSig(null, A, "A1", false, false, true, false, false);

        // one sig A2 extends A {}
        PrimSig A2 = new PrimSig(null, A, "A2", false, false, true, false, false);

        // A { f: B lone->lone B }
        Expr f = A.addField(null, "f", B.lone_arrow_lone(B));
        // Since (B lone->lone B) is not unary,  the default is "setOf",  meaning "f:set (B lone->lone B)"

        // A { g: B }
        Expr g = A.addField(null, "g", B);
        // The line above is the same as:   A.addField(null, "g", B.oneOf())  since B is unary.
        // If you want "setOf", you need:   A.addField(null, "g", B.setOf())

        // pred someG { some g }
        Func someG = new Func(null, "SomeG", null, null);
        someG.setBody(g.some());

        // pred atMostThree[x:univ, y:univ] { #(x+y) >= 3 }
        ExprVar x = UNIV.oneOf("x");
        ExprVar y = UNIV.oneOf("y");
        Func atMost3 = new Func(null, "atMost3", Util.asList(x,y), null);
        atMost3.setBody(x.plus(y).cardinality().lte(ExprConstant.makeNUMBER(3)));

        List<Sig> sigs = new ArrayList<Sig>();
        sigs.add(UNIV);
        sigs.add(SIGINT);
        sigs.add(SEQIDX);
        sigs.add(NONE);
        sigs.add(A);
        sigs.add(B);
        sigs.add(A1);
        sigs.add(A2);

        // run { some A && atMostThree[B,B] } for 3 but 3 int, 3 seq
        Expr expr1 = A.some().and(atMost3.call(B,B));
        Command cmd1 = new Command(null, "command1", expr1, false, 3, 3, 3, -1, null);
        A4Solution sol1 = TranslateAlloyToKodkod.execute_command(NOP, sigs, null, cmd1, opt);
        System.out.println("[Solution1]:");
        System.out.println(sol1.toString());

        // run { some f && SomeG[] } for 3 but 2 int, 1 seq, 5 A, exactly 6 B
        List<Pair<Sig,Integer>> scope = new ArrayList<Pair<Sig,Integer>>();
        scope.add(new Pair<Sig,Integer>(A,5));
        scope.add(new Pair<Sig,Integer>(B,-7)); // To say exactly N, use the number "-N-1". So, to say "exactly 6", use "-7".
        Expr expr2 = f.some().and(someG.call());
        Command cmd2 = new Command(null, "command2", expr2, false, 3, 2, 1, -1, scope);
        A4Solution sol2 = TranslateAlloyToKodkod.execute_command(NOP, sigs, null, cmd2, opt);
        System.out.println("[Solution2]:");
        System.out.println(sol2.toString());
    }
}
