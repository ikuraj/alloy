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

package edu.mit.csail.sdg.alloy4compiler.translator;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.CommandScope;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import kodkod.ast.BinaryExpression;
import kodkod.ast.Decl;
import kodkod.ast.IntExpression;
import kodkod.ast.Decls;
import kodkod.ast.IntConstant;
import kodkod.ast.IntToExprCast;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Variable;
import kodkod.ast.Relation;
import kodkod.ast.Formula;
import kodkod.ast.Expression;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import static edu.mit.csail.sdg.alloy4.Util.tail;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.ZERO;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.ONE;

/** Translate an Alloy AST into Kodkod AST then attempt to solve it using Kodkod. */

public final class TranslateAlloyToKodkod extends VisitReturn<Object> {

    /**
     * This is used to detect "function recursion" (which we currently do not allow);
     * also, by knowing the current function name, we can provide a more meaningful name for skolem variables
     */
    private final List<Func> current_function = new ArrayList<Func>();

    /** This maps the current local variables (LET, QUANT, Function Param) to the actual Kodkod Expression/IntExpression/Formula. */
    private Env<ExprVar,Object> env = new Env<ExprVar,Object>();

    /** If frame!=null, it stores the scope, bounds, and other settings necessary for performing a solve. */
    private final A4Solution frame;

    /** If frame==null, it stores the mapping from each Sig/Field/Skolem/Atom to its corresponding Kodkod expression. */
    private final ConstMap<Expr,Expression> a2k;

    /** The current reporter. */
    private A4Reporter rep;

    /** If nonnull, it's the current command. */
    private final Command cmd;

    /** The bitwidth. */
    private final int bitwidth;

    /** The minimum allowed integer. */
    private final int min;

    /** The maximum allowed integer. */
    private final int max;

    /** The maximum allowed loop unrolling and recursion. */
    private final int unrolls;

    /**
     * Construct a translator based on the given list of sigs and the given command.
     * @param rep - if nonnull, it's the reporter that will receive diagnostics and progress reports
     * @param opt - the solving options (must not be null)
     * @param sigs - the list of sigs (must not be null, and must be a complete list)
     * @param cmd - the command to solve (must not be null)
     */
    private TranslateAlloyToKodkod (A4Reporter rep, A4Options opt, Iterable<Sig> sigs, Command cmd) throws Err {
        this.unrolls = opt.unrolls;
        this.rep = (rep != null) ? rep : A4Reporter.NOP;
        this.cmd = cmd;
        Pair<A4Solution, ScopeComputer> pair = ScopeComputer.compute(this.rep, opt, sigs, cmd);
        this.frame = pair.a;
        this.bitwidth = pair.a.getBitwidth();
        this.min = pair.a.min();
        this.max = pair.a.max();
        this.a2k = null;
        BoundsComputer.compute(rep, frame, pair.b, sigs);
    }

    /**
     * Construct a translator based on a already-fully-constructed association map.
     * @param bitwidth - the integer bitwidth to use
     * @param unrolls - the maximum number of loop unrolling and recursion allowed
     * @param a2k - the mapping from Alloy sig/field/skolem/atom to the corresponding Kodkod expression
     */
    private TranslateAlloyToKodkod (int bitwidth, int unrolls, Map<Expr,Expression> a2k) throws Err {
        this.unrolls = unrolls;
        if (bitwidth<1)  throw new ErrorSyntax("Cannot specify a bitwidth less than 1");
        if (bitwidth>30) throw new ErrorSyntax("Cannot specify a bitwidth greater than 30");
        this.rep = A4Reporter.NOP;
        this.cmd = null;
        this.frame = null;
        this.bitwidth = bitwidth;
        this.max = (1<<(bitwidth-1)) - 1;
        this.min = 0 - (1<<(bitwidth-1));
        this.a2k = ConstMap.make(a2k);
    }

    /** Associate the given formula with the given expression, then return the formula as-is. */
    private Formula k2pos(Formula f, Expr e) throws Err {
        if (frame!=null) frame.k2pos(f, e);
        return f;
    }

    /** Returns the expression corresponding to the given sig. */
    private Expression a2k(Sig x)     throws Err { if (a2k!=null) return a2k.get(x); else return frame.a2k(x); }

    /** Returns the expression corresponding to the given field. */
    private Expression a2k(Field x)   throws Err { if (a2k!=null) return a2k.get(x); else return frame.a2k(x); }

    /** Returns the expression corresponding to the given skolem/atom. */
    private Expression a2k(ExprVar x) throws Err { if (a2k!=null) return a2k.get(x); else return frame.a2k(x); }

    //==============================================================================================================//

    /** Stores the list of "totalOrder predicates" that we constructed. */
    private final List<Relation> totalOrderPredicates = new ArrayList<Relation>();

    /** Conjoin the constraints for "field declarations" and "fact" paragraphs */
    private void makeFacts(Expr facts) throws Err {
        rep.debug("Generating facts...\n");
        // convert into a form that hopefully gives better unsat core
        facts = (Expr) (new ConvToConjunction()).visitThis(facts);
        // now go over each of them
        ArrayList<Expr> ar = new ArrayList<Expr>();
        makelist(ar, facts);
        again:
        for(Sig sig: frame.getAllReachableSigs()) {
            while(sig.isOne!=null && sig.getFields().size()==2) {
                Field f1 = sig.getFields().get(0); Relation fst = right(a2k(f1)); if (fst==null) break;
                Field f2 = sig.getFields().get(1); Relation nxt = right(a2k(f2)); if (nxt==null) break;
                Sig e = findElem(sig, f1, f2);
                if (e==null || !cmd.additionalExactScopes.contains(e)) break;
                Expression ee = a2k(e);
                if (!(ee instanceof Relation)) break;
                for(int i=0; i+4<ar.size(); i++) {
                    if (findOrder(e,sig,f1,f2, ar.get(i), ar.get(i+1), ar.get(i+2), ar.get(i+3), ar.get(i+4))) {
                        Pos pos = ar.get(i).span().merge(ar.get(i+4).span());
                        rep.debug("Found: util/ordering\n");
                        // Remove ar[i..i+4]; the remaining elements are not re-arranged
                        ar.remove(i+4); ar.remove(i+3); ar.remove(i+2); ar.remove(i+1); ar.remove(i);
                        Relation lst = frame.addRel("", null, frame.query(true, (Relation)ee, false));
                        totalOrderPredicates.add((Relation)ee); totalOrderPredicates.add(fst); totalOrderPredicates.add(lst); totalOrderPredicates.add(nxt);
                        Formula f = nxt.totalOrder((Relation)ee, fst, lst);
                        frame.addFormula(f, pos);
                        continue again;
                    }
                }
                break;
            }
            for(Field f:sig.getFields()) {
                Expression sr=a2k(sig), fr=a2k(f);
                // we could test to see if we can use kodkod's "function" and "partialFunction" predicates, but experimentally it actually performs much worse, so I will disable it
                /*
                if (sig.isOne==null && fr instanceof Relation && f.boundingFormula instanceof ExprQuant) {
                   final ExprVar THIS = ((ExprQuant)(f.boundingFormula)).vars.get(0);
                   final Expr a_in_b = deNOP(((ExprQuant)(f.boundingFormula)).sub);
                   if (a_in_b instanceof ExprBinary && ((ExprBinary)a_in_b).op==ExprBinary.Op.IN) {
                      final Expr sub = deNOP(((ExprBinary)a_in_b).right);
                      if (sub.type.arity()==1 && sub instanceof ExprUnary && ((ExprUnary)sub).op==ExprUnary.Op.ONEOF && !sub.hasVar(THIS)) {
                         final Expression range=cset(((ExprUnary)sub).sub);
                         frame.addFormula(((Relation)fr).function(sr, range), f);
                         rep.debug("Found: kodkod function\n");
                         continue;
                      }
                      if (sub.type.arity()==1 && sub instanceof ExprUnary && ((ExprUnary)sub).op==ExprUnary.Op.LONEOF && !sub.hasVar(THIS)) {
                         final Expression range=cset(((ExprUnary)sub).sub);
                         frame.addFormula(((Relation)fr).partialFunction(sr, range), f);
                         rep.debug("Found: kodkod partialFunction\n");
                         continue;
                      }
                   }
                }
                */
                // Each field f has a boundingFormula that says "all x:s | x.f in SOMEEXPRESSION";
                frame.addFormula(cform(f.boundingFormula), f);
                // Given the above, we can be sure that every column is well-bounded (except possibly the first column).
                // Thus, we need to add a bound that the first column is a subset of s.
                if (sig.isOne==null) {
                    for(int i=f.type.arity(); i>1; i--) fr=fr.join(Relation.UNIV);
                    frame.addFormula(fr.in(sr), f);
                }
            }
        }
        for(Expr e:ar) frame.addFormula(cform(e), e);
    }

    //==============================================================================================================//

    private static final class GreedySimulator extends Simplifier {
        private List<Relation> totalOrderPredicates = null;
        private Iterable<Sig> allSigs = null;
        private ConstList<Sig> growableSigs = null;
        private A4Solution partial = null;
        public GreedySimulator() { }
        private TupleSet convert(TupleFactory factory, Expr f) throws Err {
            TupleSet old = ((A4TupleSet) (partial.eval(f))).debugGetKodkodTupleset();
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
            TupleFactory factory = sol.getFactory();
            TupleSet oldUniv = convert(factory, Sig.UNIV);
            Set<Object> oldAtoms = new HashSet<Object>(); for(Tuple t: oldUniv) oldAtoms.add(t.atom(0));
            for(Sig s: allSigs) {
                // The case below is STRICTLY an optimization; the entire statement can be removed without affecting correctness
                if (s.isOne!=null && s.getFields().size()==2)
                  for(int i=0; i+3<totalOrderPredicates.size(); i=i+4)
                      if (totalOrderPredicates.get(i+1)==right(sol.a2k(s.getFields().get(0))) && totalOrderPredicates.get(i+3)==right(sol.a2k(s.getFields().get(1)))) {
                          TupleSet allelem = sol.query(true, totalOrderPredicates.get(i), true);
                          if (allelem.size()==0) continue;
                          Tuple first=null, prev=null; TupleSet next=factory.noneOf(2);
                          for(Tuple t:allelem) {
                              if (prev==null) first=t; else next.add(prev.product(t));
                              prev=t;
                          }
                          try {
                              sol.shrink(totalOrderPredicates.get(i+1), factory.range(first,first), factory.range(first,first));
                              sol.shrink(totalOrderPredicates.get(i+2), factory.range(prev,prev), factory.range(prev,prev));
                              sol.shrink(totalOrderPredicates.get(i+3), next, next);
                          } catch(Throwable ex) {
                              // Error here is not fatal
                          }
                      }
                // The case above is STRICTLY an optimization; the entire statement can be removed without affecting correctness
                for(Field f: s.getFields()) {
                    Expression rel = sol.a2k(f);
                    if (s.isOne!=null) {
                        rel = right(rel);
                        if (!(rel instanceof Relation)) continue;
                        // Retrieve the old value from the previous solution, and convert it to the new unverse.
                        // This should always work since the new universe is not yet solved, and so it should have all possible atoms.
                        TupleSet newLower = convert(factory, s.join(f)), newUpper = newLower.clone();
                        // Bind the partial instance
                        for(Tuple t: sol.query(false, rel, false)) for(int i=0; i<t.arity(); i++) if (!oldAtoms.contains(t.atom(i))) { newLower.add(t); break; }
                        for(Tuple t: sol.query(true, rel, false)) for(int i=0; i<t.arity(); i++) if (!oldAtoms.contains(t.atom(i))) { newUpper.add(t); break; }
                        sol.shrink((Relation)rel, newLower, newUpper);
                    } else {
                        if (!(rel instanceof Relation)) continue;
                        // Retrieve the old value from the previous solution, and convert it to the new unverse.
                        // This should always work since the new universe is not yet solved, and so it should have all possible atoms.
                        TupleSet newLower = convert(factory, f), newUpper = newLower.clone();
                        // Bind the partial instance
                        for(Tuple t: sol.query(false, rel, false)) for(int i=0; i<t.arity(); i++) if (!oldAtoms.contains(t.atom(i))) { newLower.add(t); break; }
                        for(Tuple t: sol.query(true, rel, false)) for(int i=0; i<t.arity(); i++) if (!oldAtoms.contains(t.atom(i))) { newUpper.add(t); break; }
                        sol.shrink((Relation)rel, newLower, newUpper);
                    }
                }
            }
            return true;
        }
    }

    private static A4Solution execute_greedyCommand(A4Reporter rep, Iterable<Sig> sigs, Command usercommand, A4Options opt) throws Exception {
        // FIXTHIS: if the next command has a *smaller scope* than the last command, we would get a Kodkod exception...
        // FIXTHIS: if the solver is "toCNF" or "toKodkod" then this method will throw an Exception...
    	// FIXTHIS: does solution enumeration still work when we're doing a greedy solve?
        TranslateAlloyToKodkod tr = null;
        try {
            long start = System.currentTimeMillis();
            GreedySimulator sim = new GreedySimulator();
            sim.allSigs = sigs;
            sim.partial = null;
            A4Reporter rep2 = new A4Reporter(rep) {
                private boolean first = true;
                public void translate(String solver, int bitwidth, int maxseq, int skolemDepth, int symmetry) { if (first) super.translate(solver, bitwidth, maxseq, skolemDepth, symmetry); first=false; }
                public void resultSAT(Object command, long solvingTime, Object solution) { }
                public void resultUNSAT(Object command, long solvingTime, Object solution) { }
            };
            // Form the list of commands
            List<Command> commands = new ArrayList<Command>();
            while(usercommand!=null) { commands.add(usercommand); usercommand = usercommand.parent; }
            // For each command...
            A4Solution sol = null;
            for(int i=commands.size()-1; i>=0; i--) {
                Command cmd = commands.get(i);
                sim.growableSigs = cmd.getGrowableSigs();
                while(cmd != null) {
                    rep.debug(cmd.scope.toString());
                    usercommand = cmd;
                    tr = new TranslateAlloyToKodkod(rep2, opt, sigs, cmd);
                    tr.makeFacts(cmd.formula);
                    sim.totalOrderPredicates = tr.totalOrderPredicates;
                    sol = tr.frame.solve(rep2, cmd, sim.partial==null || cmd.check ? new Simplifier() : sim, false);
                    if (!sol.satisfiable() && !cmd.check) {
                        start = System.currentTimeMillis() - start;
                        if (sim.partial==null) { rep.resultUNSAT(cmd, start, sol); return sol; } else { rep.resultSAT(cmd, start, sim.partial); return sim.partial; }
                    }
                    if (sol.satisfiable() && cmd.check) {
                        start = System.currentTimeMillis() - start;
                        rep.resultSAT(cmd, start, sol); return sol;
                    }
                    sim.partial = sol;
                    if (sim.growableSigs.isEmpty()) break;
                    for(Sig s: sim.growableSigs) {
                        CommandScope sc = cmd.getScope(s);
                        if (sc.increment > sc.endingScope - sc.startingScope) {cmd=null; break;}
                        cmd = cmd.change(s, sc.isExact, sc.startingScope+sc.increment, sc.endingScope, sc.increment);
                    }
                }
            }
            if (sol.satisfiable()) rep.resultSAT(usercommand, System.currentTimeMillis()-start, sol); else rep.resultUNSAT(usercommand, System.currentTimeMillis()-start, sol);
            return sol;
        } catch(HigherOrderDeclException ex) {
            Pos p = tr!=null ? tr.frame.kv2typepos(ex.decl().variable()).b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        }
    }

    /**
     * Based on the specified "options", execute one command and return the resulting A4Solution object.
     *
     * @param rep - if nonnull, we'll send compilation diagnostic messages to it
     * @param sigs - the list of sigs; this list must be complete
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     *
     * @return null if the user chose "save to FILE" as the SAT solver,
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_command (A4Reporter rep, Iterable<Sig> sigs, Command cmd, A4Options opt) throws Err {
        if (rep==null) rep = A4Reporter.NOP;
        TranslateAlloyToKodkod tr = null;
        try {
            if (cmd.parent!=null || !cmd.getGrowableSigs().isEmpty()) return execute_greedyCommand(rep, sigs, cmd, opt);
            tr = new TranslateAlloyToKodkod(rep, opt, sigs, cmd);
            tr.makeFacts(cmd.formula);
            return tr.frame.solve(rep, cmd, new Simplifier(), false);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pos p = tr!=null ? tr.frame.kv2typepos(ex.decl().variable()).b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Based on the specified "options", execute one command and return the resulting A4Solution object.
     *
     * <p> Note: it will first test whether the model fits one of the model from the "Software Abstractions" book;
     * if so, it will use the exact instance that was in the book.
     *
     * @param rep - if nonnull, we'll send compilation diagnostic messages to it
     * @param sigs - the list of sigs; this list must be complete
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     *
     * @return null if the user chose "save to FILE" as the SAT solver,
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_commandFromBook (A4Reporter rep, Iterable<Sig> sigs, Command cmd, A4Options opt) throws Err {
        if (rep==null) rep = A4Reporter.NOP;
        TranslateAlloyToKodkod tr = null;
        try {
            if (cmd.parent!=null || !cmd.getGrowableSigs().isEmpty()) return execute_greedyCommand(rep, sigs, cmd, opt);
            tr = new TranslateAlloyToKodkod(rep, opt, sigs, cmd);
            tr.makeFacts(cmd.formula);
            return tr.frame.solve(rep, cmd, new Simplifier(), true);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pos p = tr!=null ? tr.frame.kv2typepos(ex.decl().variable()).b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Translate the Alloy expression into an equivalent Kodkod Expression or IntExpression or Formula object.
     * @param sol - an existing satisfiable A4Solution object
     * @param expr - this is the Alloy expression we want to translate
     */
    public static Object alloy2kodkod(A4Solution sol, Expr expr) throws Err {
        if (expr.ambiguous && !expr.errors.isEmpty()) expr = expr.resolve(expr.type, null);
        if (!expr.errors.isEmpty()) throw expr.errors.pick();
        TranslateAlloyToKodkod tr = new TranslateAlloyToKodkod(sol.getBitwidth(), sol.unrolls(), sol.a2k());
        Object ans;
        try {
            ans = tr.visitThis(expr);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            throw new ErrorType("Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
        if ((ans instanceof IntExpression) || (ans instanceof Formula) || (ans instanceof Expression)) return ans;
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }

    //==============================================================================================================//

    /**
     * Convenience method that evalutes x and casts the result to be a Kodkod Formula.
     * @return the formula - if x evaluates to a Formula
     * @throws ErrorFatal - if x does not evaluate to a Formula
     */
    private Formula cform(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof Formula) return (Formula)y;
        throw new ErrorFatal(x.span(), "This should have been a formula.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod IntExpression.
     * @return the integer expression - if x evaluates to an IntExpression
     * @throws ErrorFatal - if x does not evaluate to an IntExpression
     */
    private IntExpression cint(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof IntExpression) return (IntExpression)y;
        throw new ErrorFatal(x.span(), "This should have been an integer expression.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod Expression.
     * @return the expression - if x evaluates to an Expression
     * @throws ErrorFatal - if x does not evaluate to an Expression
     */
    private Expression cset(Expr x) throws Err {
        if (!x.errors.isEmpty()) throw x.errors.pick();
        Object y=visitThis(x);
        if (y instanceof Expression) return (Expression)y;
        throw new ErrorFatal(x.span(), "This should have been a set or a relation.\nInstead it is "+y);
    }

    //==============================================================================================================//

    /**
     * Given a variable name "name", prepend the current function name to form a meaningful "skolem name".
     * (Note: this function does NOT, and need NOT guarantee that the name it generates is unique)
     */
    private String skolem(String name) {
        if (current_function.size()==0) {
            if (cmd!=null && cmd.label.length()>0 && cmd.label.indexOf('$')<0) return cmd.label+"_"+name; else return name;
        }
        Func last=current_function.get(current_function.size()-1);
        String funcname=tail(last.label);
        if (funcname.indexOf('$')<0) return funcname+"_"+name; else return name;
    }

    //==============================================================================================================//

    /** Break a tree of conjunction into a list of formula. */
    private static void makelist(ArrayList<Expr> list, Expr input) {
       input = deNOP(input);
       while(input instanceof ExprBinary && ((ExprBinary)input).op==ExprBinary.Op.AND) {
          makelist(list, ((ExprBinary)input).left);
          input = ((ExprBinary)input).right;
          input = deNOP(input);
       }
       if (!input.isSame(ExprConstant.TRUE)) list.add(input);
    }

    /** Remove the "ExprUnary NOP" in front of an expression. */
    private static Expr deNOP(Expr x) {
        while(x instanceof ExprUnary && ((ExprUnary)x).op==ExprUnary.Op.NOOP) x=((ExprUnary)x).sub;
        return x;
    }

    /** Returns y if f.boundingFormula is of the form "all SomeVar: one s | SomeVar.f in y" */
    private static Expr findY(Sig s, Field f) {
        Expr x=f.boundingFormula;
        if (!(x instanceof ExprQuant)) return null;
        final ExprQuant q=(ExprQuant)x;
        if (q.op!=ExprQuant.Op.ALL || q.vars.size()!=1) return null;
        final ExprVar v=q.vars.get(0);
        x=deNOP(v.expr);
        if (x instanceof ExprUnary && ((ExprUnary)x).op==ExprUnary.Op.ONEOF) x=((ExprUnary)x).sub;
        if (!x.isSame(s)) return null;
        if (!(q.sub instanceof ExprBinary)) return null;
        ExprBinary in=(ExprBinary)(q.sub);
        if (in.op!=ExprBinary.Op.IN) return null;
        if (!in.left.isSame(v.join(f))) return null;
        x=deNOP(in.right);
        if (x instanceof ExprUnary && ((ExprUnary)x).op==ExprUnary.Op.SETOF) x=deNOP(((ExprUnary)x).sub);
        return x;
    }

    /** Returns the sig "elem" if field1="all this:s | s.f1 in set elem" and field2="... in elem->elem" */
    private static Sig findElem(Sig s, Field field1, Field field2) {
        Expr b1 = findY(s, field1), b2 = findY(s, field2);
        if (b1 instanceof Sig && b2!=null && b2.isSame(b1.product(b1))) return (Sig)b1; else return null;
    }

    /**
     * Returns true if we can determine that "e1 && e2 && e3 && e4 && e5" says "total order on elem".
     *
     * <p> In particular, that means:
     * <br> e1 == elem in First.*Next
     * <br> e2 == no Next.First
     * <br> e3 == all e: one elem | (e = First || one Next.e)
     * <br> e4 == all e: one elem | (e = (elem-(Next.elem)) || one e.Next)
     * <br> e5 == all e: one elem | (e !in e.^Next)
     */
    private static boolean findOrder (Sig elem, Sig ord, Expr first, Expr next, Expr e1, Expr e2, Expr e3, Expr e4, Expr e5) {
        ExprQuant qt;
        ExprVar e;
        first = ord.join(first);
        next = ord.join(next);
        if (!elem.in(first.join(next.reflexiveClosure())).isSame(e1)) return false;
        if (!next.join(first).no().isSame(e2)) return false;
        if (!(e3 instanceof ExprQuant) || !(e4 instanceof ExprQuant) || !(e5 instanceof ExprQuant)) return false;
        //
        qt=(ExprQuant)e3; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
        e=qt.vars.get(0); if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        if (!e.equal(first).or(next.join(e).one()).isSame(qt.sub)) return false;
        //
        qt=(ExprQuant)e4; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
        e=qt.vars.get(0); if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        if (!e.equal(elem.minus(next.join(elem))).or(e.join(next).one()).isSame(qt.sub)) return false;
        //
        qt=(ExprQuant)e5; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
        e=qt.vars.get(0); if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        if (!e.in(e.join(next.closure())).not().isSame(qt.sub)) return false;
        //
        return true;
    }

    /** If x = SOMETHING->RELATION where SOMETHING.arity==1, then return the RELATION, else return null. */
    private static Relation right(Expression x) {
        if (!(x instanceof BinaryExpression)) return null;
        BinaryExpression bin = (BinaryExpression)x;
        if (bin.op() != BinaryExpression.Operator.PRODUCT) return null;
        if (bin.left().arity()==1 && bin.right() instanceof Relation) return (Relation)(bin.right()); else return null;
    }

    //==============================================================================================================//

    /*============================*/
    /* Evaluates an ExprITE node. */
    /*============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprITE x) throws Err {
        Formula c = cform(x.cond);
        Object l = visitThis(x.left);
        if (l instanceof Formula) {
            Formula c1 = c.implies((Formula)l);
            Formula c2 = c.not().implies(cform(x.right));
            return k2pos(c1.and(c2), x);
        }
        if (l instanceof Expression) {
            return c.thenElse((Expression)l, cset(x.right));
        }
        return c.thenElse((IntExpression)l, cint(x.right));
    }

    /*============================*/
    /* Evaluates an ExprLet node. */
    /*============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprLet x) throws Err {
        env.put(x.var, visitThis(x.var.expr));
        Object ans = visitThis(x.sub);
        env.remove(x.var);
        return ans;
    }

    /*=================================*/
    /* Evaluates an ExprConstant node. */
    /*=================================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprConstant x) throws Err {
        switch(x.op) {
          case NUMBER:
            int n=x.num();
            if (n<min) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
            if (n>max) throw new ErrorType(x.pos, "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
            return IntConstant.constant(n);
          case TRUE: return Formula.TRUE;
          case FALSE: return Formula.FALSE;
          case IDEN: return Expression.IDEN.intersection(a2k(UNIV).product(Relation.UNIV));
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprConstant.accept()");
    }

    /*==============================*/
    /* Evaluates an ExprUnary node. */
    /*==============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprUnary x) throws Err {
        switch(x.op) {
            case SOMEOF: case LONEOF: case ONEOF: case SETOF: return cset(x.sub);
            case NOOP: return visitThis(x.sub);
            case NOT:  return k2pos( cform(x.sub).not() , x );
            case SOME: return k2pos( cset(x.sub).some() , x);
            case LONE: return k2pos( cset(x.sub).lone() , x);
            case ONE:  return k2pos( cset(x.sub).one() , x);
            case NO:   return k2pos( cset(x.sub).no() , x);
            case TRANSPOSE:   return cset(x.sub).transpose();
            case CARDINALITY: return cset(x.sub).count();
            case CAST2SIGINT: return cint(x.sub).toExpression();
            case CAST2INT:    return sum(cset(x.sub));
            case RCLOSURE:
                Expression iden=Expression.IDEN.intersection(a2k(UNIV).product(Relation.UNIV));
                return cset(x.sub).closure().union(iden);
            case CLOSURE: return cset(x.sub).closure();
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprUnary.visit()");
    }

    /** Performs int[x]; contains an efficiency shortcut that simplifies int[Int[x]] to x. */
    private IntExpression sum(Expression x) {
        if (x instanceof IntToExprCast) return ((IntToExprCast)x).intExpr(); else return x.sum();
    }

    /*============================*/
    /* Evaluates an ExprVar node. */
    /*============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprVar x) throws Err {
        Object ans=env.get(x);
        if (ans==null) ans=a2k(x);
        if (ans==null) throw new ErrorFatal(x.pos, "Variable \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /*=========================*/
    /* Evaluates a Field node. */
    /*=========================*/

    /** {@inheritDoc} */
    @Override public Object visit(Field x) throws Err {
        Expression ans = a2k(x);
        if (ans==null) throw new ErrorFatal(x.pos, "Field \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /*=======================*/
    /* Evaluates a Sig node. */
    /*=======================*/

    /** {@inheritDoc} */
    @Override public Object visit(Sig x) throws Err {
        Expression ans = a2k(x);
        if (ans==null) throw new ErrorFatal(x.pos, "Sig \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /*=============================*/
    /* Evaluates an ExprCall node. */
    /*=============================*/

    private boolean is_mul(Func f) {
        // fun mul [n1, n2: Int] : Int {
        //     let s1 = { p:Int | (n1<0 && p<0 && p>=n1) || (n1>0 && p>0 && p<=n1) } |
        //     let s2 = { q:Int | (n2<0 && q<0 && q>=n2) || (n2>0 && q>0 && q<=n2) } |
        //     #(s1->s2)
        // }
        if (f.params.size()!=2 || !(f.label.equals("mul") || f.label.endsWith("/mul"))) return false;
        final Expr cast = deNOP(f.getBody());
        if (!(cast instanceof ExprUnary) || ((ExprUnary)cast).op != ExprUnary.Op.CAST2SIGINT) return false;
        final Expr let1 = deNOP(((ExprUnary)cast).sub); if (!(let1 instanceof ExprLet)) return false;
        final Expr let2 = deNOP(((ExprLet)let1).sub);   if (!(let2 instanceof ExprLet)) return false;
        final Expr card = deNOP(((ExprLet)let2).sub);
        if (!(card instanceof ExprUnary) || ((ExprUnary)card).op != ExprUnary.Op.CARDINALITY) return false;
        final Expr product = deNOP(((ExprUnary)card).sub);
        if (!(product instanceof ExprBinary) || ((ExprBinary)product).op != ExprBinary.Op.ARROW) return false;
        final ExprVar s1 = ((ExprLet)let1).var; if (!((ExprBinary)product).left.isSame(s1)) return false;
        final ExprVar s2 = ((ExprLet)let2).var; if (!((ExprBinary)product).right.isSame(s2)) return false;
        final Expr q1 = deNOP(s1.expr); if (!(q1 instanceof ExprQuant)) return false;
        final Expr q2 = deNOP(s2.expr); if (!(q2 instanceof ExprQuant)) return false;
        final ExprQuant qt1 = (ExprQuant)q1; if (qt1.op != ExprQuant.Op.COMPREHENSION || qt1.vars.size()!=1) return false;
        final ExprQuant qt2 = (ExprQuant)q2; if (qt2.op != ExprQuant.Op.COMPREHENSION || qt2.vars.size()!=1) return false;
        final ExprVar n1=f.params.get(0), p=qt1.vars.get(0); if (!p.expr.isSame(Sig.SIGINT.oneOf())) return false;
        final ExprVar n2=f.params.get(1), q=qt2.vars.get(0); if (!q.expr.isSame(Sig.SIGINT.oneOf())) return false;
        if (!qt1.sub.isSame(n1.lt(ZERO).and(p.lt(ZERO)).and(p.gte(n1)).or(n1.gt(ZERO).and(p.gt(ZERO)).and(p.lte(n1))))) return false;
        if (!qt2.sub.isSame(n2.lt(ZERO).and(q.lt(ZERO)).and(q.gte(n2)).or(n2.gt(ZERO).and(q.gt(ZERO)).and(q.lte(n2))))) return false;
        return true;
    }

    private boolean is_div(Func f) {
        // fun div [a, b: Int] : Int {
        //     let sn = (((a>=0 && b>=0) || (a<0 && b<0)) => 1 else 0-1) |
        //     let na = (a>0 => 0-a else int[a]) |
        //     let nb = (b>0 => 0-b else int[b]) |
        //     let r = div[na-nb, nb] |
        //     (na=0 || na>nb) => 0 else
        //     nb=0 => 0-sn else
        //     na=nb => sn else (sn>0 => 1+r else (0-1)-r)
        // }
        if (f.params.size()!=2 || !(f.label.equals("div") || f.label.endsWith("/div"))) return false;
        final ExprVar a = f.params.get(0), b = f.params.get(1);
        final Expr cast = deNOP(f.getBody());
        if (!(cast instanceof ExprUnary) || ((ExprUnary)cast).op != ExprUnary.Op.CAST2SIGINT) return false;
        final Expr let1 = deNOP(((ExprUnary)cast).sub); if (!(let1 instanceof ExprLet)) return false;
        final Expr let2 = deNOP(((ExprLet)let1).sub);   if (!(let2 instanceof ExprLet)) return false;
        final Expr let3 = deNOP(((ExprLet)let2).sub);   if (!(let3 instanceof ExprLet)) return false;
        final Expr let4 = deNOP(((ExprLet)let3).sub);   if (!(let4 instanceof ExprLet)) return false;
        final Expr if1  = deNOP(((ExprLet)let4).sub);   if (!(if1  instanceof ExprITE)) return false;
        final Expr if2  = deNOP(((ExprITE)if1).right);  if (!(if2  instanceof ExprITE)) return false;
        final Expr if3  = deNOP(((ExprITE)if2).right);  if (!(if3  instanceof ExprITE)) return false;
        final Expr if4  = deNOP(((ExprITE)if3).right);  if (!(if4  instanceof ExprITE)) return false;
        final ExprVar sn = ((ExprLet)let1).var; if (!a.gte(ZERO).and(b.gte(ZERO)).or(a.lt(ZERO).and(b.lt(ZERO))).ite(ONE,ZERO.minus(ONE)).isSame(sn.expr)) return false;
        final ExprVar na = ((ExprLet)let2).var; if (!a.gt(ZERO).ite(ZERO.minus(a), a).isSame(na.expr)) return false;
        final ExprVar nb = ((ExprLet)let3).var; if (!b.gt(ZERO).ite(ZERO.minus(b), b).isSame(nb.expr)) return false;
        final ExprVar r  = ((ExprLet)let4).var; if (!f.call(na.minus(nb),nb).isSame(r.expr)) return false;
        if (!na.equal(ZERO).or(na.gt(nb)).isSame(((ExprITE)if1).cond)) return false; if (!ZERO.isSame(((ExprITE)if1).left)) return false;
        if (!nb.equal(ZERO)              .isSame(((ExprITE)if2).cond)) return false; if (!ZERO.minus(sn).isSame(((ExprITE)if2).left)) return false;
        if (!na.equal(nb)                .isSame(((ExprITE)if3).cond)) return false; if (!sn.isSame(((ExprITE)if3).left)) return false;
        if (!sn.gt(ZERO)                 .isSame(((ExprITE)if4).cond)) return false; if (!ONE.plus(r).isSame(((ExprITE)if4).left)) return false;
        return ZERO.minus(ONE).minus(r).isSame(((ExprITE)if4).right);
    }

    private boolean is_rem(Func f) {
        // fun rem [a, b: Int] : Int {
        //     int[a] - (a.div[b].mul[b])
        // }
        if (f.params.size()!=2 || !(f.label.equals("rem") || f.label.endsWith("/rem"))) return false;
        final ExprVar a = f.params.get(0), b = f.params.get(1);
        final Expr cast = deNOP(f.getBody());
        if (!(cast instanceof ExprUnary) || ((ExprUnary)cast).op != ExprUnary.Op.CAST2SIGINT) return false;
        final Expr sub = deNOP(((ExprUnary)cast).sub); if (!(sub instanceof ExprBinary)) return false;
        final ExprBinary bin = (ExprBinary)sub;
        if (bin.op != ExprBinary.Op.MINUS || !a.cast2int().isSame(bin.left)) return false;
        final Expr tmp0 = deNOP(bin.right);
        if (!(tmp0 instanceof ExprUnary) || ((ExprUnary)tmp0).op != ExprUnary.Op.CAST2INT) return false;
        final Expr tmp1 = deNOP(((ExprUnary)tmp0).sub);
        if (!(tmp1 instanceof ExprCall)) return false;
        final ExprCall call1 = (ExprCall)tmp1;
        if (!is_mul(call1.fun) || !call1.args.get(1).isSame(b)) return false;
        final Expr tmp2 = deNOP(call1.args.get(0));
        if (!(tmp2 instanceof ExprCall)) return false;
        final ExprCall call2 = (ExprCall)tmp2;
        return is_div(call2.fun) && call2.args.get(0).isSame(a) && call2.args.get(1).isSame(b);
    }

    /** Caches functions that we have matched to be one of the builtin ones. */
    private final Map<Func,Pair<Expr,String>> cacheForExprCall = new IdentityHashMap<Func,Pair<Expr,String>>();

    /** Caches parameter-less functions to a Kodkod Expression, Kodkod IntExpression, or Kodkod Formula. */
    private final Map<Func,Object> cacheForConstants = new IdentityHashMap<Func,Object>();

    /** {@inheritDoc} */
    @Override public Object visit(ExprCall x) throws Err {
        final Func f = x.fun;
        final Object candidate = f.params.size()==0 ? cacheForConstants.get(f) : null;
        if (candidate!=null) return candidate;
        final Pair<Expr,String> cache = cacheForExprCall.get(f);
        final Expr body = f.getBody();
        final int n = f.params.size();
        int maxRecursion = unrolls;
        for(Func ff:current_function) if (ff==f) {
            if (maxRecursion<0) {
                throw new ErrorSyntax(x.span(), ""+f+" cannot call itself recursively!");
            }
            if (maxRecursion==0) {
                Type t = f.returnDecl.type;
                if (t.is_bool) return Formula.FALSE;
                if (t.is_int) return IntConstant.constant(0);
                int i = t.arity();
                Expression ans = Expression.NONE;
                while(i>1) { ans = ans.product(Expression.NONE); i--; }
                return ans;
            }
            maxRecursion--;
        }
        if (n==2) if ((cache!=null && cache.a==body && cache.b=="util/integer/mul") || is_mul(f)) {
            cacheForExprCall.put(f, new Pair<Expr,String>(body, "util/integer/mul"));
            rep.debug("Found: util/integer/mul\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.multiply(b).toExpression();
        }
        if (n==2) if ((cache!=null && cache.a==body && cache.b=="util/integer/div") || is_div(f)) {
            cacheForExprCall.put(f, new Pair<Expr,String>(body, "util/integer/div"));
            rep.debug("Found: util/integer/div\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.divide(b).toExpression();
        }
        if (n==2) if ((cache!=null && cache.a==body && cache.b=="util/integer/rem") || is_rem(f)) {
            cacheForExprCall.put(f, new Pair<Expr,String>(body, "util/integer/rem"));
            rep.debug("Found: util/integer/rem\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.modulo(b).toExpression();
        }
        Env<ExprVar,Object> newenv = new Env<ExprVar,Object>();
        for(int i=0; i<n; i++) newenv.put(f.params.get(i), cset(x.args.get(i)));
        Env<ExprVar,Object> oldenv = env;
        env = newenv;
        current_function.add(f);
        Object ans = visitThis(body);
        env = oldenv;
        current_function.remove(current_function.size()-1);
        if (ans instanceof Formula) k2pos((Formula)ans, x);
        if (f.params.size()==0) cacheForConstants.put(f, ans);
        return ans;
    }

    /*================================*/
    /* Evaluates an ExprBuiltin node. */
    /*================================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprBuiltin x) throws Err {
        // This says  no(a&b) and no((a+b)&c) and no((a+b+c)&d)...
        // Emperically this seems to be more efficient than "no(a&b) and no(a&c) and no(b&c)"
        Formula answer=null;
        Expression a=null;
        for(Expr arg:x.args) {
            Expression b=cset(arg);
            if (a==null) {a=b;continue;}
            if (answer==null) answer=a.intersection(b).no(); else answer=a.intersection(b).no().and(answer);
            a=a.union(b);
        }
        if (answer!=null) return k2pos(answer, x); else return Formula.TRUE;
    }

    /*===============================*/
    /* Evaluates an ExprBinary node. */
    /*===============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        Expression s, s2; IntExpression i; Formula f; Object obj;
        switch(x.op) {
            case IN:  return k2pos(isIn(cset(a),b), x);
            case LT:  i=cint(a);  f=i.lt(cint(b));   return k2pos(f,x);
            case LTE: i=cint(a);  f=i.lte(cint(b));  return k2pos(f,x);
            case GT:  i=cint(a);  f=i.gt(cint(b));   return k2pos(f,x);
            case GTE: i=cint(a);  f=i.gte(cint(b));  return k2pos(f,x);
            case AND: f=cform(a); f=f.and(cform(b)); return k2pos(f,x);
            case OR:  f=cform(a); f=f.or(cform(b));  return k2pos(f,x);
            case IFF: f=cform(a); f=f.iff(cform(b)); return k2pos(f,x);
            case PLUSPLUS: s=cset(a); return s.override(cset(b));
            case SHL: i=cint(a); return i.shl(cint(b));
            case SHR: i=cint(a); return i.shr(cint(b));
            case SHA: i=cint(a); return i.sha(cint(b));
            case PLUS:
                obj = visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; return i.plus(cint(b)); }
                s = (Expression)obj; return s.union(cset(b));
            case MINUS:
                // Special exception to allow "0-8" to not throw an exception, where 7 is the maximum allowed integer (when bitwidth==4)
                // (likewise, when bitwidth==5, then +15 is the maximum allowed integer, and we want to allow 0-16 without throwing an exception)
                if (a instanceof ExprConstant && ((ExprConstant)a).op==ExprConstant.Op.NUMBER && ((ExprConstant)a).num()==0)
                   if (b instanceof ExprConstant && ((ExprConstant)b).op==ExprConstant.Op.NUMBER && ((ExprConstant)b).num()==max+1)
                      return IntConstant.constant(min);
                obj=visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; return i.minus(cint(b));}
                s=(Expression)obj; return s.difference(cset(b));
            case INTERSECT:
                s=cset(a); return s.intersection(cset(b));
            case ANY_ARROW_SOME: case ANY_ARROW_ONE: case ANY_ARROW_LONE:
            case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
            case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE: case ONE_ARROW_LONE:
            case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
            case ISSEQ_ARROW_LONE:
            case ARROW:
                s=cset(a); return s.product(cset(b));
            case JOIN:
                s=cset(a); return s.join(cset(b));
            case EQUALS:
                obj=visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; f=i.eq(cint(b));}
                else { s=(Expression)obj; f=s.eq(cset(b)); }
                return k2pos(f,x);
            case DOMAIN:
                a=deNOP(a);
                b=deNOP(b);
                if (a instanceof Sig && b instanceof Field && ((Field)b).sig==a) return cset(b);
                s=cset(a);
                s2=cset(b);
                for(int j=s2.arity(); j>1; j--) s=s.product(Expression.UNIV);
                return s.intersection(s2);
            case RANGE:
                s=cset(a);
                s2=cset(b);
                for(int j=s.arity(); j>1; j--) s2=Expression.UNIV.product(s2);
                return s.intersection(s2);
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprBinary.accept()");
    }

    /** Helper method that translates the formula "a in b" into a Kodkod formula. */
    private Formula isIn(Expression a, Expr right) throws Err {
        Expression b;
        if (right instanceof ExprUnary) {
            // Handles possible "unary" multiplicity
            ExprUnary y=(ExprUnary)(right);
            if (y.op==ExprUnary.Op.ONEOF) { b=cset(y.sub); return a.one().and(a.in(b)); }
            if (y.op==ExprUnary.Op.SETOF) { b=cset(y.sub); return a.in(b); }
            if (y.op==ExprUnary.Op.LONEOF) { b=cset(y.sub); return a.lone().and(a.in(b)); }
            if (y.op==ExprUnary.Op.SOMEOF) { b=cset(y.sub); return a.some().and(a.in(b)); }
        }
        if (right instanceof ExprBinary && right.mult!=0 && ((ExprBinary)right).op.isArrow) {
            // Handles possible "binary" or higher-arity multiplicity
            return isInBinary(a, (ExprBinary)right);
        }
        return a.in(cset(right));
    }

    /** Helper method that translates the formula "r in (a ?->? b)" into a Kodkod formula. */
    private Formula isInBinary(Expression r, ExprBinary ab) throws Err {
        final Expression a=cset(ab.left), b=cset(ab.right);
        Decls d=null, d2=null;
        Formula ans1, ans2;
        // "R in A ->op B" means for each tuple a in A, there are "op" tuples in r that begins with a.
        Expression atuple=null, ar=r;
        for(int i=a.arity(); i>0; i--) {
           Variable v=Variable.unary("");
           if (a.arity()==1) d=v.oneOf(a); else if (d==null) d=v.oneOf(Relation.UNIV); else d=v.oneOf(Relation.UNIV).and(d);
           ar=v.join(ar);
           if (atuple==null) atuple=v; else atuple=atuple.product(v);
        }
        ans1=isIn(ar, ab.right);
        switch(ab.op) {
           case ISSEQ_ARROW_LONE:
           case ANY_ARROW_LONE: case SOME_ARROW_LONE: case ONE_ARROW_LONE: case LONE_ARROW_LONE: ans1=ar.lone().and(ans1); break;
           case ANY_ARROW_ONE:  case SOME_ARROW_ONE:  case ONE_ARROW_ONE:  case LONE_ARROW_ONE:  ans1=ar.one().and(ans1);  break;
           case ANY_ARROW_SOME: case SOME_ARROW_SOME: case ONE_ARROW_SOME: case LONE_ARROW_SOME: ans1=ar.some().and(ans1); break;
        }
        if (a.arity()>1) { Formula tmp=isIn(atuple, ab.left); if (tmp!=Formula.TRUE) ans1=tmp.implies(ans1); }
        ans1=ans1.forAll(d);
        // "R in A op-> B" means for each tuple b in B, there are "op" tuples in r that end with b.
        Expression btuple=null, rb=r;
        for(int i=b.arity(); i>0; i--) {
           Variable v=Variable.unary("");
           if (b.arity()==1) d2=v.oneOf(b); else if (d2==null) d2=v.oneOf(Relation.UNIV); else d2=v.oneOf(Relation.UNIV).and(d2);
           rb=rb.join(v);
           if (btuple==null) btuple=v; else btuple=v.product(btuple);
        }
        ans2=isIn(rb, ab.left);
        switch(ab.op) {
           case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE: ans2=rb.lone().and(ans2); break;
           case ONE_ARROW_ANY:  case ONE_ARROW_SOME:  case ONE_ARROW_ONE:  case ONE_ARROW_LONE:  ans2=rb.one().and(ans2);  break;
           case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE: ans2=rb.some().and(ans2); break;
        }
        if (b.arity()>1) { Formula tmp=isIn(btuple, ab.right); if (tmp!=Formula.TRUE) ans2=tmp.implies(ans2); }
        ans2=ans2.forAll(d2);
        // Now, put everything together
        Formula ans=r.in(a.product(b)).and(ans1).and(ans2);
        if (ab.op==ExprBinary.Op.ISSEQ_ARROW_LONE) {
            Expression rr=r;
            while(rr.arity()>1) rr=rr.join(Relation.UNIV);
            ans=rr.difference(rr.join(A4Solution.SIGINT_NEXT)).in(A4Solution.SIGINT_ZERO).and(ans);
        }
        return ans;
    }

    /*==============================*/
    /* Evaluates an ExprQuant node. */
    /*==============================*/

    /** Adds a "one of" in front of X if X is unary and does not have a declared multiplicity. */
    private static Expr addOne(Expr x) {
        if (x instanceof ExprUnary) switch(((ExprUnary)x).op) {
            case SETOF: case ONEOF: case LONEOF: case SOMEOF: return x;
        }
        return (x.type.arity()!=1) ? x : ExprUnary.Op.ONEOF.make(x.span(), x);
    }

    /** Helper method that translates the quantification expression "op vars | sub" */
    private Object visit_qt(final ExprQuant.Op op, final ConstList<ExprVar> xvars, final Expr sub) throws Err {
        if (op == ExprQuant.Op.NO) {
            return visit_qt(ExprQuant.Op.ALL, xvars, sub.not());
        }
        if (op == ExprQuant.Op.ONE || op == ExprQuant.Op.LONE) {
            for(int i=0; ;i++) {
                if (i>=xvars.size() && op==ExprQuant.Op.ONE)
                   return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub)).one();
                if (i>=xvars.size() && op==ExprQuant.Op.LONE)
                   return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub)).lone();
                Expr v = deNOP(addOne(xvars.get(i).expr));
                if (v.type.arity()>1 || v.mult!=1 || !(v instanceof ExprUnary) || ((ExprUnary)v).op!=ExprUnary.Op.ONEOF) break;
            }
        }
        if (op == ExprQuant.Op.ONE) {
            Formula f1 = (Formula) visit_qt(ExprQuant.Op.LONE, xvars, sub);
            Formula f2 = (Formula) visit_qt(ExprQuant.Op.SOME, xvars, sub);
            return f1.and(f2);
        }
        if (op == ExprQuant.Op.LONE) {
            QuantifiedFormula p1 = (QuantifiedFormula) visit_qt(ExprQuant.Op.ALL, xvars, sub);
            QuantifiedFormula p2 = (QuantifiedFormula) visit_qt(ExprQuant.Op.ALL, xvars, sub);
            Formula f1=p1.formula(), f2=p2.formula(), eqv=Formula.TRUE;
            Iterator<Decl> s1=p1.declarations().iterator(), s2=p2.declarations().iterator();
            Decls decls=null;
            while(s1.hasNext() && s2.hasNext()) {
                Decl d1=s1.next(), d2=s2.next();
                eqv = d1.variable().eq(d2.variable()).and(eqv);
                if (decls==null) decls=d1.and(d2); else decls=decls.and(d1).and(d2);
            }
            return f1.and(f2).implies(eqv).forAll(decls);
        }
        List<Decl> decls=new ArrayList<Decl>();
        Decls dd=null;
        Formula guard=Formula.TRUE;
        Expr lastExpr=null;
        Expression lastValue=null;
        for(ExprVar dex:xvars) {
            final Variable v = Variable.nary(skolem(dex.label), dex.type.arity());
            final Decl newd;
            final Expression dv = (dex.expr==lastExpr) ? lastValue : cset(dex.expr);
            env.put(dex, v);
            lastExpr=dex.expr;
            lastValue=dv;
            final Expr dexexpr=addOne(lastExpr);
            if (dex.type.arity()==1) {
                ExprUnary.Op mt=ExprUnary.Op.ONEOF;
                if (dexexpr instanceof ExprUnary) {
                    if (((ExprUnary)dexexpr).op==ExprUnary.Op.SETOF) mt=ExprUnary.Op.SETOF;
                    else if (((ExprUnary)dexexpr).op==ExprUnary.Op.SOMEOF) mt=ExprUnary.Op.SOMEOF;
                    else if (((ExprUnary)dexexpr).op==ExprUnary.Op.LONEOF) mt=ExprUnary.Op.LONEOF;
                }
                switch(mt) {
                  case SETOF: newd=v.setOf(dv); break;
                  case SOMEOF: newd=v.someOf(dv); break;
                  case LONEOF: newd=v.loneOf(dv); break;
                  default: newd=v.oneOf(dv);
                }
            } else {
                guard=isIn(v, dexexpr).and(guard);
                newd=v.setOf(dv);
            }
            if (frame!=null) frame.kv2typepos(v, dex.type, dex.pos);
            if (dd==null) dd=newd; else dd=dd.and(newd);
            decls.add(newd);
        }
        final Formula ans = (op==ExprQuant.Op.SUM) ? null : cform(sub) ;
        final IntExpression ians = (op!=ExprQuant.Op.SUM) ? null : cint(sub) ;
        for(ExprVar dex:xvars) env.remove(dex);
        if (op==ExprQuant.Op.COMPREHENSION) return ans.comprehension(dd); // guard==Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SUM) return ians.sum(dd);                    // guard==Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SOME) {
            return guard==Formula.TRUE ? ans.forSome(dd) : guard.and(ans).forSome(dd);
        } else {
            return guard==Formula.TRUE ? ans.forAll(dd) : guard.implies(ans).forAll(dd);
        }
    }

    /** {@inheritDoc} */
    @Override public Object visit(ExprQuant x) throws Err {
        // Special translation that are useful for util/integer.als (and any other places where this expression shows up)
        if (x.op==ExprQuant.Op.COMPREHENSION && x.vars.size()==1) {
            ExprVar a=x.vars.get(0);
            if (a.expr.isSame(Sig.SIGINT.oneOf())) {
                if (a.gte(ZERO).and(a.plus(ONE).lt(ZERO)).isSame(x.sub)) {
                    rep.debug("Found: util/integer/max\n");
                    return A4Solution.SIGINT_MAX;
                }
                if (a.lt(ZERO).and(a.minus(ONE).gte(ZERO)).isSame(x.sub)) {
                    rep.debug("Found: util/integer/min\n");
                    return A4Solution.SIGINT_MIN;
                }
            }
        }
        // Special translation that are useful for util/integer.als (and any other places where this expression shows up)
        if (x.op==ExprQuant.Op.COMPREHENSION && x.vars.size()==2) {
            ExprVar a=x.vars.get(0), b=x.vars.get(1);
            if (a.expr.isSame(Sig.SIGINT.oneOf()) && b.expr.isSame(Sig.SIGINT.oneOf())) {
                if (b.gt(a).and(b.equal(ONE.plus(a))).isSame(x.sub)) {
                    rep.debug("Found: util/integer/next\n");
                    return A4Solution.SIGINT_NEXT;
                }
            }
        }
        // All else, invoke the helper method to translate this quantification expression
        Object ans = visit_qt(x.op, x.vars, x.sub);
        if (ans instanceof Formula) k2pos((Formula)ans, x);
        return ans;
    }
}
