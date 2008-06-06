/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4whole;

import java.util.Map;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.A4TupleSet;
import edu.mit.csail.sdg.alloy4compiler.translator.Simplifier;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;

/** This class demonstrates how to use partial instance to do greedy multi-step simulation. */

//
//  1. a distinguished abstract toplevel sig "State"
//  2. State has exactly two subsigs: "one sig pre, post extends State {}"
//  3. State has no subsetsigs
//  4. no field has "State" in its declaration
//  5. global facts directly relate pre to post
//  6. optionally, you can have a parameterless predicate called "init" that must hold for the first <pre,post> pair
//
//  Algorithm:
//  ==========
//  prepare KK universe from scratch
//  inst[0] = KK.solve()
//  print inst[0] projected over "pre"
//  print inst[0] projected over "post"
//  for each i = 1, 2, 3... {
//      prepare KK universe from scratch
//      for each field F in State, let pre.F := the value of post.F in inst[i-1]
//      inst[i] = KK.solve
//      print inst[i] projected over "pre"
//      print inst[i] projected over "post"
//  }
//

public final class ExampleSimulator extends Simplifier {

    private Module world = null;

    private A4Reporter rep = A4Reporter.NOP;

    private A4Options options = new A4Options();

    private PrimSig state = null, pre = null, post = null;

    private Expr init = null;

    private A4Solution partial = null;
    
    private StringBuilder output = new StringBuilder();

    private TupleSet convert(TupleFactory factory, TupleSet old) {
       TupleSet ans = factory.noneOf(old.arity());
       for(Tuple oldT: old) {
          Tuple newT = null;
          for(int i=0; i<oldT.arity(); i++) {
             if (newT==null) newT=factory.tuple(oldT.atom(i)); else newT=newT.product(factory.tuple(oldT.atom(i)));
          }
          ans.add(newT);
       }
       return ans;
    }

    private void debug(A4Solution ans, Sig step) throws Exception {
        StringBuilder sb = new StringBuilder("  ");
        for(Sig s: world.getAllReachableSigs()) for(Field f: s.getFields()) if (f.type.firstColumnOverlaps(state.type)) {
           int n = sb.length();
            sb.append(f.label).append(" ").append(ans.eval(step.join(f))).append(" ");
            while(sb.length()-35 < n) sb.append(" ");
        }
        output(sb.append("\n").toString());
     }

    @Override public boolean simplify(A4Reporter rep, A4Solution sol, Iterable<Formula> unused) throws Err {
       TupleFactory factory = factory(sol);
       Object preAtom = ((A4TupleSet)(partial.eval(pre))).debugGetKodkodTupleset().iterator().next().atom(0);
       for(Sig s: world.getAllReachableSigs()) for(Field f: s.getFields()) if (f.type.firstColumnOverlaps(state.type)) {

          // Retrieve the POST state in the previous solution
          TupleSet oldT = ((A4TupleSet) (partial.eval(pre.product(post.join(f))))).debugGetKodkodTupleset();

          // Convert it into the universe used in the new solution that we are about to solve for.
          // This should always work since the new universe is not yet solved, and so it should have all possible atoms.
          TupleSet newLower = convert(factory, oldT),  newUpper = newLower.clone();

          // Extract the expression corresponding to the given field.
          // This will fail iff f.sig is a singleton; that should not be possible, since "pre" and "post" are disjoint
          Expression rel = (Relation) a2k(sol, f);
          if (!(rel instanceof Relation)) continue;

          // Bind the partial instance
          for(Tuple t: query(sol, false, rel)) if (!t.atom(0).equals(preAtom)) newLower.add(t);
          for(Tuple t: query(sol, true,  rel)) if (!t.atom(0).equals(preAtom)) newUpper.add(t);
          shrink(sol, (Relation)rel, newLower, newUpper);
       }
       return true;
    }

    private void output(String text) { output.append(text); }

    private ExampleSimulator(String filename, Map<String,String> snapshot) throws Exception {
       Command cmd = null;
       Expr fact = null;
       world = CompUtil.parseEverything_fromFile(rep, snapshot, filename);
       for(Sig s: world.getAllSigs()) if (s instanceof PrimSig) {
          if (s.label.equals("this/State")) state = (PrimSig)s;
          if (s.label.equals("this/pre")) pre = (PrimSig)s;
          if (s.label.equals("this/post")) post = (PrimSig)s;
       }
       for(Func f: world.getAllFunc()) if (f.isPred && f.params.size()==0) {
          if (f.label.equals("this/init")) init=f.call().and(init);
       }
       for(Pair<Command,Expr> pair: world.getAllCommandsWithFormulas()) {
           if (!pair.a.check) { cmd=pair.a; fact=pair.b.and(world.getAllReachableFacts()); break; }
       }
       if (cmd==null || fact==null) throw new ErrorSyntax("Must have at least one RUN command");
       if (state==null)             throw new ErrorSyntax("Must have a toplevel sig called \"State\"");
       if (pre==null)               throw new ErrorSyntax("Must have a subsig called \"pre\"");
       if (post==null)              throw new ErrorSyntax("Must have a subsig called \"post\"");
       if (!state.isTopLevel())     throw new ErrorSyntax("sig \"State\" must be toplevel");
       if (state.isAbstract==null)  throw new ErrorSyntax("sig \"State\" must be abstract");
       if (pre.isOne==null)         throw new ErrorSyntax("sig \"pre\" must be a \"one\" sig");
       if (pre.parent!=state)       throw new ErrorSyntax("sig \"pre\" must extend the \"State\" sig");
       if (post.isOne==null)        throw new ErrorSyntax("sig \"post\" must be a \"one\" sig");
       if (post.parent!=state)      throw new ErrorSyntax("sig \"post\" must extend the \"State\" sig");
       // now, try the step-by-step greedy simulation
       partial = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), fact.and(init), cmd, options);
       if (!partial.satisfiable()) { output("Unsatisfiable!\n"); return; } else { output("Start:\n"); }
       debug(partial, pre);
       debug(partial, post);
       for(int i=1; i<20 && partial.satisfiable(); i++) {
           partial = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), fact, cmd, options, this);
           if (!partial.satisfiable()) { output("Unsatisfiable!\n"); return; }
           output("Next:\n");
           debug(partial, pre);
           debug(partial, post);
       }
    }

    public static String run(String filename, Map<String,String> snapshot) throws Exception { return (new ExampleSimulator(filename, snapshot)).output.toString(); }
}
