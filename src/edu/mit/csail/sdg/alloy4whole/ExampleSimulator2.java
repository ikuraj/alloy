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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Triple;
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
//  2. State has no subsigs nor subsetsigs
//  3. a distinguished predicate init[s: State]
//  4. a distinguished predicate step[s, s': State]
//
//  Algorithm:
//  ==========
//  add "one sig s[0], s[1] extends State { }" then construct KK universe from scratch
//  instance = KK.solve()
//  for each i = 2... {
//      add "one sig s[i] extends State { }" then construct KK universe from scratch
//      for each field F in State, let (s[0]..s[i-1]).F := their value from the last instance
//      instance = KK.solve()
//  }
//

public final class ExampleSimulator2 extends Simplifier {

    private Module world = null;

    private A4Options options = new A4Options();

    private Command cmd = null;

    private PrimSig state = null;

    private Func init = null, step = null;

    private A4Solution partial = null;

    private PrimSig makeAtom() throws Err {
        String label = Integer.toString(state.children().size());
        while(label.length()<3) label="0"+label;
        PrimSig ans = new PrimSig(Pos.UNKNOWN, state, "this/State"+label, false, false, true, false, false);
        return ans;
    }

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

    @Override public boolean simplify(A4Reporter rep, A4Solution sol, Iterable<Formula> unused) throws Err {
       TupleFactory factory = factory(sol);
       Set<Object> oldAtoms = new HashSet<Object>();
       for(Tuple t: ((A4TupleSet)(partial.eval(state))).debugGetKodkodTupleset()) oldAtoms.add(t.atom(0));
       for(Sig s: world.getAllReachableSigs()) for(Field f: s.getFields()) {
           if (f.type.firstColumnOverlaps(state.type)) {
               // Retrieve the old states from the previous solution
               TupleSet oldT = ((A4TupleSet) (partial.eval(f))).debugGetKodkodTupleset();
               // Convert it into the universe used in the new solution that we are about to solve for.
               // This should always work since the new universe is not yet solved, and so it should have all possible atoms.
               TupleSet newLower = convert(factory, oldT),  newUpper = newLower.clone();
               // Extract the expression corresponding to the given field.
               Expression rel = (Relation) a2k(sol, f);
               if (!(rel instanceof Relation)) continue; // should not happen, as long as the input model obeys our conventions
               // Bind the partial instance
               for(Tuple t: query(sol, false, rel)) if (!oldAtoms.contains(t.atom(0))) newLower.add(t);
               for(Tuple t: query(sol, true,  rel)) if (!oldAtoms.contains(t.atom(0))) newUpper.add(t);
               shrink(sol, (Relation)rel, newLower, newUpper);
           } else {
               // Retrieve the old states from the previous solution
               TupleSet oldT = ((A4TupleSet) (partial.eval(f))).debugGetKodkodTupleset();
               // Convert it into the universe used in the new solution that we are about to solve for.
               // This should always work since the new universe is not yet solved, and so it should have all possible atoms.
               TupleSet newLower = convert(factory, oldT),  newUpper = newLower.clone();
               // Extract the expression corresponding to the given field.
               Expression rel = (Relation) a2k(sol, f);
               if (!(rel instanceof Relation)) continue; // should not happen, as long as the input model obeys our conventions
               // Bind the partial instance
               shrink(sol, (Relation)rel, newLower, newUpper);
           }
       }
       return true;
    }

    private ExampleSimulator2(A4Reporter rep, String filename, String xmlFilename, Map<String,String> snapshot) throws Exception {
       options.originalFilename = filename;
       Expr fact = null;
       world = CompUtil.parseEverything_fromFile(rep, snapshot, filename);
       List<Sig> allsigs = new ArrayList<Sig>(world.getAllReachableSigs());
       for(Func f: world.getAllFunc()) if (f.isPred) {
           if (f.label.equals("this/init") && f.params.size()==1) init=f;
           if (f.label.equals("this/step") && f.params.size()==2) step=f;
       }
       for(Pair<Command,Expr> pair: world.getAllCommandsWithFormulas()) {
           if (!pair.a.check) { cmd=pair.a; fact=pair.b.and(world.getAllReachableFacts()); break; }
       }
       if (cmd==null || fact==null)                  throw new ErrorSyntax("Must have at least one RUN command");
       if (cmd.scope.size()==0)                      throw new ErrorSyntax("First RUN command's must specify at least one sig in the list of scopes.");
       if (!(cmd.scope.get(0).a instanceof PrimSig)) throw new ErrorSyntax("The first sig mentioned in the first RUN command must be a toplevel abstract sig.");
       int scope = cmd.scope.get(0).b;
       if (scope<0) scope=0-(scope+1);
       if (scope<2) throw new ErrorSyntax("Scope for \"State\" must be 2 or higher");
       state = (PrimSig) (cmd.scope.get(0).a);
       if (!state.isTopLevel())       throw new ErrorSyntax("sig \"" + state + "\" must be toplevel");
       if (state.isAbstract==null)    throw new ErrorSyntax("sig \"" + state + "\" must be abstract");
       if (state.children().size()>0) throw new ErrorSyntax("sig \"" + state + "\" must not have any subsigs");
       if (init==null)                throw new ErrorSyntax("Must have a predicate called \"init\"");
       if (step==null)                throw new ErrorSyntax("Must have a predicate called \"step\"");
       // now, try the step-by-step greedy simulation
       PrimSig s0 = makeAtom(); allsigs.add(s0);
       PrimSig s1 = makeAtom(); allsigs.add(s1);
       partial = TranslateAlloyToKodkod.execute_command(rep, allsigs, fact.and(init.call(s0)).and(step.call(s0,s1)), cmd, options);
       if (!partial.satisfiable()) return;
       for(int i=2; i<scope && partial.satisfiable(); i++) {
           PrimSig pre = state.children().get(state.children().size()-1);
           PrimSig post = makeAtom(); allsigs.add(post);
           A4Solution tmp = TranslateAlloyToKodkod.execute_command(rep, allsigs, fact.and(step.call(pre,post)), cmd, options, this);
           if (!tmp.satisfiable()) break;
           partial = tmp;
       }
       partial.writeXML(rep, xmlFilename, world.getAllFunc(), snapshot);
    }

    public static Triple<Module,Command,A4Solution> run(A4Reporter rep, String filename, String xmlFilename, Map<String,String> snapshot) throws Exception {
        if (rep==null) rep = A4Reporter.NOP;
        if (snapshot==null) snapshot = new HashMap<String,String>();
        long old = System.currentTimeMillis();
        ExampleSimulator2 sim = new ExampleSimulator2(rep, filename, xmlFilename, snapshot);
        long now = System.currentTimeMillis();
        A4Solution sol = sim.partial;
        if (!sol.satisfiable()) rep.resultUNSAT(sim.cmd, now-old, sol); else rep.resultSAT(sim.cmd, now-old, sol);
        return new Triple<Module,Command,A4Solution>(sim.world, sim.cmd, sol);
    }

    public static void main(String[] args) throws Exception { run(null, "/zweb/zweb/absFsys.als", "/tmp/z.xml", new HashMap<String, String>()); }
}
