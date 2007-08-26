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

package edu.mit.csail.sdg.alloy4compiler.translator;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.IdentityHashMap;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
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
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import kodkod.ast.BinaryExpression;
import kodkod.ast.Decl;
import kodkod.ast.IntExpression;
import kodkod.ast.Decls;
import kodkod.ast.IntConstant;
import kodkod.ast.IntToExprCast;
import kodkod.ast.Variable;
import kodkod.ast.Relation;
import kodkod.ast.Formula;
import kodkod.ast.Expression;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.MinTopStrategy;
import kodkod.engine.config.AbstractReporter;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import static edu.mit.csail.sdg.alloy4.Util.tail;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.ZERO;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.ONE;
import static kodkod.engine.Solution.Outcome.UNSATISFIABLE;

/** Translate an Alloy AST into Kodkod AST then attempt to solve it using Kodkod. */

public final class TranslateAlloyToKodkod extends VisitReturn {

    /** The reporter that does nothing. */
    private static AbstractReporter blankReporter = new AbstractReporter(){};

    /**
     * This is used to detect "function recursion" (which we currently do not allow);
     * also, by knowing the current function name, we can provide a more meaningful name for skolem variables
     */
    private final List<Func> current_function = new ArrayList<Func>();

    /** This maps the current local variables (LET, QUANT, Function Param) to the actual Kodkod Expression/IntExpression/Formula. */
    private Env<ExprVar,Object> env = new Env<ExprVar,Object>();

    /** This maps each Kodkod formula we generate to a Alloy Pos or Alloy Expr. */
    private final IdentityHashMap<Formula,Object> fmap = new IdentityHashMap<Formula,Object>();

    /** This maps each Kodkod Decl we generate to an Alloy Type and Alloy Pos. */
    private final Map<Decl,Pair<Type,Pos>> decl2type = new IdentityHashMap<Decl,Pair<Type,Pos>>();

    /** This maps each Kodkod skolem relation we generate to an Alloy Type. */
    private final Map<Relation,Type> rel2type = new IdentityHashMap<Relation,Type>();

    //==============================================================================================================//

    /** The current reporter. */
    private final A4Reporter rep;

    /** The list of all Sigs. */
    private final ConstList<Sig> sigs;

    /** If nonnull, it's the current command. */
    private final Command cmd;

    /**
     * Step1: Initialize the list of sigs and the command to check.
     *
     * @param rep - if nonnull, it's the reporter that will receive diagnostics and progress reports
     * @param sigs - the list of sigs; must be complete, must contain UNIV+SIGINT+SEQIDX+NONE, and must not contain duplicates
     * @param cmd - the command to solve
     *
     * <p> Reads: none
     * <p> Writes: rep, sigs, cmd
     */
    private TranslateAlloyToKodkod (A4Reporter rep, Iterable<Sig> sigs, Command cmd) {
        this.rep = (rep != null) ? rep : A4Reporter.NOP;
        this.sigs = ConstList.make(sigs);
        this.cmd = cmd;
    }

    //==============================================================================================================//

    /** The integer bitwidth. */
    private int bitwidth;

    /** The maximum sequence length. */
    private int maxseq;

    /** This maps each AlloySig, each AlloyField, and possibly even some parameterless AlloyFunc to a Kodkod Expression. */
    private ConstMap<Object,Expression> bcc;

    /** This maps each KodkodRelation to an upperbound and a lowerbound. */
    private Bounds bounds;

    /** This is the formula we want to satisfy. */
    private Formula goal;

    /**
     * Step2: construct the bounds and the formula we want to satisfy.
     * <p> Reads: rep, sigs, cmd
     * <p> Writes: bitwidth, maxseq, bcc, bounds, goal
     */
    private void makeFormula (Iterable<Module> modules) throws Err {
        rep.debug("Generating bounds...");
        final ScopeComputer sc = new ScopeComputer(rep,sigs,cmd);
        this.bitwidth = sc.getBitwidth();
        this.maxseq = sc.getMaxSeq();
        final Pair<Pair<Bounds,Formula>,ConstMap<Object,Expression>> bc = BoundsComputer.compute(sc,rep,sigs,fmap);
        this.bcc = bc.b;
        this.bounds = bc.a.a;
        goal = bc.a.b;
        rep.debug("Generating facts...");
        for(Module u:modules) goal = makeFacts(u, goal);
        goal = fmap(cform(cmd.formula), cmd.formula).and(goal);
    }

    //==============================================================================================================//

    /** The Kodkod solver object. */
    private Solver solver;

    /** The temporary file that may receive the CNF (or null if we are not writing the CNF out) */
    private File tmpCNF = null;

    /**
     * Step3: construct the Kodkod solver object, and chooses the temporary file name
     * <p> Reads: rep, cmd, bitwidth, maxseq
     * <p> Writes: solver
     */
    private void makeSolver (A4Options opt) throws Err, IOException {
        rep.debug("Assigning kodkod options...");
        int sym = (cmd.expects==1 ? 0 : opt.symmetry);
        solver = new Solver();
        if (opt.solver.external()!=null) {
            String ext = opt.solver.external();
            if (opt.solverDirectory.length()>0 && ext.indexOf(File.separatorChar)<0) ext=opt.solverDirectory+File.separatorChar+ext;
            tmpCNF = File.createTempFile("tmp", ".cnf", new File(opt.tempDirectory));
            solver.options().setSolver(SATFactory.externalFactory(ext, "", tmpCNF.getAbsolutePath(), ""));
        } else if (opt.solver.equals(A4Options.SatSolver.ZChaffJNI)) {
            solver.options().setSolver(SATFactory.ZChaff);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatJNI)) {
            solver.options().setSolver(SATFactory.MiniSat);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatProverJNI)) {
            sym=20;
            solver.options().setSolver(SATFactory.MiniSatProver);
            solver.options().setLogTranslation(true);
        } else if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            tmpCNF = File.createTempFile("tmp", ".cnf", new File(opt.tempDirectory));
            String name = System.getProperty("user.home")+File.separatorChar+"nosuchprogram";
            solver.options().setSolver(SATFactory.externalFactory(name, "", tmpCNF.getAbsolutePath(), ""));
        } else {
            solver.options().setSolver(SATFactory.DefaultSAT4J);
        }
        solver.options().setSymmetryBreaking(sym);
        solver.options().setSkolemDepth(opt.skolemDepth);
        solver.options().setBitwidth(bitwidth);
        solver.options().setIntEncoding(Options.IntEncoding.BINARY);
        solver.options().setReporter(new AbstractReporter() {
            @Override public void skolemizing(Decl decl, Relation skolem, List<Decl> predecl) {
                try {
                    Pair<Type,Pos> p=decl2type.get(decl);
                    if (p==null) return;
                    Type t=p.a;
                    for(int i=(predecl==null ? -1 : predecl.size()-1); i>=0; i--) {
                        Pair<Type,Pos> pp=decl2type.get(predecl.get(i));
                        if (pp==null) return; else t=(pp.a).product(t);
                    }
                    while(t.arity() > skolem.arity()) t=UNIV.type.join(t); // Should not happen, but just to be safe...
                    rel2type.put(skolem,t);
                } catch(Throwable ex) { } // Exception here is not fatal
            }
            @Override public void solvingCNF(int primaryVars, int vars, int clauses) { rep.solve(primaryVars, vars, clauses); }
        });
        rep.debug("Simplifying the bounds...");
        if (!Simplifier.simplify(bounds, goal, solver.options())) goal=Formula.FALSE;
        rep.translate(opt.solver.id(), bitwidth, maxseq, opt.skolemDepth, sym);
    }

    //==============================================================================================================//

    /**
     * Step4: solve for the solution
     * <p> Reads: all
     * <p> Writes: all
     */
    private A4Solution solve (boolean tryBookExamples, A4Options opt)
    throws SaveToFileException, Err {
        rep.debug("Generating the solution...");
        Iterator<Solution> sols;
        Solution sol = null;
        IdentitySet<Formula> kCore = null;
        if (tryBookExamples && solver.options().solver()!=SATFactory.MiniSatProver) {
            try { sol=BookExamples.trial(sigs, bcc, bounds, goal, solver, cmd.check); } catch(Throwable ex) { }
        }
        if (solver.options().solver()==SATFactory.ZChaff || !solver.options().solver().incremental()) {
            sols=null;
            if (sol==null) sol=solver.solve(goal, bounds);
        } else {
            sols=solver.solveAll(goal, bounds);
            if (sol==null) sol=sols.next();
        }
        final Instance inst = sol.instance();
        if (inst==null && solver.options().solver()==SATFactory.MiniSatProver) {
            rep.minimizing(cmd);
            try {
                kCore=new IdentitySet<Formula>();
                Proof p=sol.proof();
                if (sol.outcome()==UNSATISFIABLE) {
                    try { p.minimize(new MinTopStrategy(p.log())); } catch(UnsupportedOperationException ex) {}
                }
                for(Iterator<TranslationRecord> it=p.core(); it.hasNext();) {
                    Object n=it.next().node();
                    if (n instanceof Formula) kCore.add((Formula)n);
                }
            } catch(Throwable th) {
                kCore=null;
            }
        }
        solver.options().setReporter(blankReporter); // To ensure no more output during SolutionEnumeration
        if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            // The formula is trivial, but since the user wants it in CNF format, so we manually generate
            // a trivial satisfiable (or unsatisfiable) CNF file.
            String txt = inst!=null ? "p cnf 1 1\n1 0\n" : "p cnf 1 2\n1 0\n-1 0\n";
            Util.writeAll(tmpCNF.getAbsolutePath(), txt);
            throw new RuntimeException("CNF file successfully generated (using \"nosuchprogram\" as the solver name)");
        }
        return new A4Solution(sigs, bcc, opt.originalFilename, cmd.toString(), sols, (opt.recordKodkod ? goal : null), bounds, bitwidth, inst, rel2type, fmap, kCore);
    }

    //==============================================================================================================//

    public static Object alloy2kodkod(ConstMap<Object,Expression> bcc, int bitwidth, Expr expr) throws Err {
        if (bitwidth<1 || bitwidth>30) throw new ErrorType("The integer bitwidth must be between 1 and 30.");
        if (!expr.errors.isEmpty() && expr.ambiguous) expr = expr.resolve(expr.type, new ArrayList<ErrorWarning>());
        if (!expr.errors.isEmpty()) throw expr.errors.get(0);
        TranslateAlloyToKodkod tr = new TranslateAlloyToKodkod(A4Reporter.NOP, null, null);
        tr.bcc = bcc;
        tr.bitwidth = bitwidth;
        Object ans = tr.visitThis(expr);
        if ((ans instanceof IntExpression) || (ans instanceof Formula) || (ans instanceof Expression)) return ans;
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }

    /**
     * Based on the specified "options", execute a command from the given "world", then optionally write the result as an XML file.
     *
     * @param rep - if nonnull, we'll send compilation diagnostic messages to it
     * @param world - the World that the command comes from
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     * @param xmlFileName - if it's a nonempty String, and the command is satisfiable, then we'll write the solution into it
     * @param tempFileName - this is the name of a temporary file where the solver MIGHT dump temporary formulas into
     *
     * <p> (In particular, if solver is not "FILE", and solver is not "EXTERNAL", and "recordKodkod==false",
     * then we promise we will not write into "tempFileName", so you can pass "null" as the argument in this case.)
     *
     * @return null if the solver wrote the CNF to "tempFileName",
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_command (A4Reporter rep, Module world, Command cmd, A4Options opt) throws Err {
        if (rep==null) rep=A4Reporter.NOP;
        TranslateAlloyToKodkod tr = null;
        try {
            tr = new TranslateAlloyToKodkod(rep, world.getAllReachableSigs(), cmd);
            tr.makeFormula(world.getAllReachableModules());
            tr.makeSolver(opt);
            long time = System.currentTimeMillis();
            A4Solution sol = tr.solve(false, opt);
            time = System.currentTimeMillis()-time;
            if (!sol.satisfiable()) rep.resultUNSAT(cmd, time, sol); else rep.resultSAT(cmd, time, sol);
            return sol;
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x = tr!=null ? tr.decl2type.get(ex.decl()) : null;
            Pos p = x!=null ? x.b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            if (ex.toString().contains("nosuchprogram") && tr!=null && tr.tmpCNF!=null) {
                rep.resultCNF(tr.tmpCNF.getAbsolutePath());
                return null;
            }
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Based on the specified "options", execute a command from the given "world", then optionally write the result as an XML file.
     * Note: it will first test whether the model fits one of the model from the "Software Abstractions" book;
     * if so, it will use the exact instance that was in the book.
     *
     * @param rep - if nonnull, we'll send compilation diagnostic messages to it
     * @param world - the World that the command comes from
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     * @param xmlFileName - if it's a nonempty String, and the command is satisfiable, then we'll write the solution into it
     * @param tempFileName - this is the name of a temporary file where the solver MIGHT dump temporary formulas into
     *
     * <p> (In particular, if solver is not "FILE", and solver is not "EXTERNAL", and "recordKodkod==false",
     * then we promise we will not write into "tempFileName", so you can pass "null" as the argument in this case.)
     *
     * @return null if the solver wrote the CNF to "tempFileName",
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_commandFromBook (A4Reporter rep, Module world, Command cmd, A4Options opt) throws Err {
        if (rep==null) rep=A4Reporter.NOP;
        TranslateAlloyToKodkod tr = null;
        try {
            tr = new TranslateAlloyToKodkod(rep, world.getAllReachableSigs(), cmd);
            tr.makeFormula(world.getAllReachableModules());
            tr.makeSolver(opt);
            long time = System.currentTimeMillis();
            A4Solution sol = tr.solve(false, opt);
            time = System.currentTimeMillis()-time;
            if (!sol.satisfiable()) rep.resultUNSAT(cmd, time, sol); else rep.resultSAT(cmd, time, sol);
            return sol;
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x = tr!=null ? tr.decl2type.get(ex.decl()) : null;
            Pos p = x!=null ? x.b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            if (ex.toString().contains("nosuchprogram") && tr!=null && tr.tmpCNF!=null) {
                rep.resultCNF(tr.tmpCNF.getAbsolutePath());
                return null;
            }
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    //==============================================================================================================//

    /**
     * Convenience method that evalutes x and casts the result to be a Kodkod Formula.
     * @return the formula - if x evaluates to a Formula
     * @throws ErrorFatal - if x does not evaluate to a Formula
     */
    private final Formula cform(Expr x) throws Err {
        Object y=visitThis(x);
        if (y instanceof Formula) return (Formula)y;
        throw new ErrorFatal(x.span(), "This should have been a formula.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod IntExpression.
     * @return the integer expression - if x evaluates to an IntExpression
     * @throws ErrorFatal - if x does not evaluate to an IntExpression
     */
    private final IntExpression cint(Expr x) throws Err {
        Object y=visitThis(x);
        if (y instanceof IntExpression) return (IntExpression)y;
        throw new ErrorFatal(x.span(), "This should have been an integer expression.\nInstead it is "+y);
    }

    /**
     * Convenience method that evalutes x and cast the result to be a Kodkod Expression.
     * @return the expression - if x evaluates to an Expression
     * @throws ErrorFatal - if x does not evaluate to an Expression
     */
    private final Expression cset(Expr x) throws Err {
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
            if (cmd.label.length()>0 && cmd.label.indexOf('$')<0) return cmd.label+"_"+name; else return name;
        }
        Func last=current_function.get(current_function.size()-1);
        if (last!=null) {
            String funcname=tail(last.label);
            if (funcname.indexOf('$')<0) return funcname+"_"+name;
        }
        return name;
    }

    //==============================================================================================================//

    private Formula fmap(Object f, Expr x) {
        if (!(f instanceof Formula)) return null;
        if (!fmap.containsKey(f)) fmap.put((Formula)f, x);
        return (Formula)f;
    }

    private Formula fmap(Object f, Pos x) {
        if (!(f instanceof Formula)) return null;
        if (x!=null && x!=Pos.UNKNOWN && !fmap.containsKey(f)) fmap.put((Formula)f, x);
        return (Formula)f;
    }

    //==============================================================================================================//

    /** Remove the NOOP in front of an expression. */
    private static Expr deNOP(Expr x) {
        while(x instanceof ExprUnary && ((ExprUnary)x).op==ExprUnary.Op.NOOP) x=((ExprUnary)x).sub;
        return x;
    }

    //==============================================================================================================//

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

    /** Returns the sig "elem" if field1="all this:s | s.f1 in set elem", field2=same, field3="... in elem->elem" */
    private static Sig findElem(Sig s, Field field1, Field field2, Field field3) {
        Expr b1=findY(s,field1), b2=findY(s,field2), b3=findY(s,field3);
        if (!(b1 instanceof Sig) || b1!=b2 || b3==null) return null;
        if (!(b3.isSame(b1.product(b1)))) return null;
        return (Sig)b1;
    }

    /**
     * Returns true if we can determine that "fact" says "total order on elem".
     *
     * <p> In particular, that means fact == e1 && (e2 && (e3 && (all e: one elem | (e4 && (e5 && e6))))) where
     * <br> e1 = elem in First.*Next
     * <br> e2 = no First.(~Next)
     * <br> e3 = no Last.Next
     * <br> e4 = (e = First || one e.(~Next))
     * <br> e5 = (e = Last || one e.Next)
     * <br> e6 = (e !in e.^Next)
     */
    private static boolean chkFact(Sig elem, Sig ord, Expr first, Expr last, Expr next, Expr fact) {
        Expr prev;
        ExprBinary bin;
        first = ord.join(first);
        last = ord.join(last);
        next = ord.join(next);
        prev = next.transpose();
        if (!(fact instanceof ExprBinary)) return false;
        bin=(ExprBinary)fact;
        if (!(bin.right instanceof ExprBinary)) return false;
        if (!elem.in(first.join(next.reflexiveClosure())).isSame(bin.left)) return false;
        bin=(ExprBinary)(bin.right);
        if (!(bin.right instanceof ExprBinary)) return false;
        if (!first.join(prev).no().isSame(bin.left)) return false;
        bin=(ExprBinary)(bin.right);
        if (!(bin.right instanceof ExprQuant)) return false;
        if (!last.join(next).no().isSame(bin.left)) return false;
        ExprQuant qt=(ExprQuant)(bin.right);
        if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1 || !(qt.sub instanceof ExprBinary)) return false;
        ExprVar e=qt.vars.get(0);
        if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        bin=(ExprBinary)(qt.sub);
        if (!(bin.right instanceof ExprBinary)) return false;
        if (!e.equal(first).or(e.join(prev).one()).isSame(bin.left)) return false;
        bin=(ExprBinary)(bin.right);
        if (!e.equal(last).or(e.join(next).one()).isSame(bin.left)) return false;
        if (!e.in(e.join(next.closure())).not().isSame(bin.right)) return false;
        return true;
    }

    /** If x = SOMETHING->RELATION where SOMETHING.arity==1, then return the RELATION, else return null. */
    private Relation right(Expression x) {
        if (!(x instanceof BinaryExpression)) return null;
        BinaryExpression bin = (BinaryExpression)x;
        if (bin.op() != BinaryExpression.Operator.PRODUCT) return null;
        if (bin.left().arity()==1 && bin.right() instanceof Relation) return (Relation)(bin.right()); else return null;
    }

    /** Construct the constraints for "field declarations" and "appended fact paragraphs" and "fact" paragraphs */
    private Formula makeFacts(Module module, Formula kfact) throws Err {
        SafeList<Sig> sigs = module.getAllSigs();
        SafeList<Pair<String,Expr>> facts = module.getAllFacts();
        for(Sig sig: sigs) {
            if (sig.isOne!=null && sigs.size()==1 && sig.getFields().size()==3 && facts.size()==1) {
                Field f1 = sig.getFields().get(0); Relation fst = right(bcc.get(f1));
                Field f2 = sig.getFields().get(1); Relation lst = right(bcc.get(f2));
                Field f3 = sig.getFields().get(2); Relation nxt = right(bcc.get(f3));
                Sig e = findElem(sig,f1,f2,f3);
                if (e!=null && e.isOrdered!=null && fst!=null && lst!=null && nxt!=null) {
                    Expression ee = bcc.get(e);
                    if (ee instanceof Relation && chkFact(e, sig, f1, f2, f3, facts.get(0).b)) {
                        Formula f = ((Relation)nxt).totalOrder((Relation)ee, (Relation)fst, (Relation)lst);
                        return fmap(f, e.isOrdered).and(kfact);
                    }
                }
            }
            for(Field f:sig.getFields()) {
                // Each field f has a boundingFormula that says "all x:s | x.f in SOMEEXPRESSION";
                kfact = fmap(cform(f.boundingFormula), f).and(kfact);
                // Given the above, we can be sure that every column is well-bounded (except possibly the first column).
                // Thus, we need to add a bound that the first column is a subset of s.
                if (sig.isOne==null) {
                    Expression sr=bcc.get(sig), fr=bcc.get(f);
                    for(int i=f.type.arity(); i>1; i--) fr=fr.join(Relation.UNIV);
                    kfact = fmap(fr.in(sr), f).and(kfact);
                }
            }
        }
        for(Pair<String,Expr> e:module.getAllFacts()) kfact = fmap(cform(e.b), e.b).and(kfact);
        return kfact;
    }

    //==============================================================================================================//

    /*============================*/
    /* Evaluates an ExprITE node. */
    /*============================*/

    @Override public Object visit(ExprITE x) throws Err {
        Formula c=cform(x.cond);
        Object l=visitThis(x.left);
        if (l instanceof Formula) {
            Formula c1 = fmap( c.implies((Formula)l) , x );
            Formula c2 = fmap( c.not().implies(cform(x.right)) , x );
            return fmap(c1.and(c2) , x );
        }
        if (l instanceof Expression) {
            return c.thenElse((Expression)l,cset(x.right));
        }
        return c.thenElse((IntExpression)l,cint(x.right));
    }

    /*============================*/
    /* Evaluates an ExprLet node. */
    /*============================*/

    @Override public Object visit(ExprLet x) throws Err {
        Object r=visitThis(x.var.expr);
        env.put(x.var, r);
        Object ans=visitThis(x.sub);
        env.remove(x.var);
        return ans;
    }

    /*=================================*/
    /* Evaluates an ExprConstant node. */
    /*=================================*/

    @Override public Object visit(ExprConstant x) throws Err {
        switch(x.op) {
          case NUMBER:
              int n=x.num();
              int b=bitwidth;
              int max=(b==1 ? 0 : (1<<(b-1))-1);
              int min=(0-max)-1;
              if (n<min) throw new ErrorType(x.pos,
                 "Current bitwidth is set to "+b+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
              if (n>max) throw new ErrorType(x.pos,
                 "Current bitwidth is set to "+b+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
              return IntConstant.constant(n);
          case TRUE: return Formula.TRUE;
          case FALSE: return Formula.FALSE;
          case IDEN: return Expression.IDEN.intersection(bcc.get(UNIV).product(Relation.UNIV));
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprConstant.accept()");
    }

    /*==============================*/
    /* Evaluates an ExprUnary node. */
    /*==============================*/

    @Override public Object visit(ExprUnary x) throws Err {
        switch(x.op) {
            case NOOP:
                return visitThis(x.sub);
            case SOMEOF: case LONEOF: case ONEOF: case SETOF:
                return cset(x.sub);
            case NOT:
                if (x.sub instanceof ExprBinary && ((ExprBinary)(x.sub)).op==ExprBinary.Op.OR) {
                    // This transformation is not required; but it should give you better precision unsat core
                    Expr left = ((ExprBinary)(x.sub)).left;
                    Expr right = ((ExprBinary)(x.sub)).right;
                    Formula leftF = fmap( cform(left.not()) , left );
                    Formula rightF = fmap( cform(right.not()) , right );
                    return fmap( leftF.and(rightF) , x );
                }
                return fmap( cform(x.sub).not() , x );
            case SOME: return fmap( cset(x.sub).some() , x);
            case LONE: return fmap( cset(x.sub).lone() , x);
            case ONE: return fmap( cset(x.sub).one() , x);
            case NO: return fmap( cset(x.sub).no() , x);
            case CAST2INT:
                // Efficiency shortcut that simplifies int[Int[x]] to x.
                Expression sub=cset(x.sub);
                if (sub instanceof IntToExprCast) return ((IntToExprCast)sub).intExpr();
                return sub.sum();
            case CAST2SIGINT: return cint(x.sub).toExpression();
            case TRANSPOSE: return cset(x.sub).transpose();
            case RCLOSURE:
                Expression iden=Expression.IDEN.intersection(bcc.get(UNIV).product(Relation.UNIV));
                return cset(x.sub).closure().union(iden);
            case CLOSURE: return cset(x.sub).closure();
            case CARDINALITY: return cset(x.sub).count();
        }
        throw new ErrorFatal(x.pos, "Unsupported operator ("+x.op+") encountered during ExprUnary.visit()");
    }

    /*============================*/
    /* Evaluates an ExprVar node. */
    /*============================*/

    @Override public Object visit(ExprVar x) throws Err {
        Object ans=env.get(x);
        if (ans==null) throw new ErrorFatal(x.pos, "Variable \""+x+"\" cannot be resolved during translation.\n");
        return ans;
    }

    /*==============================================*/
    /* Evaluates a Field node or an ExprField node. */
    /*==============================================*/

    @Override public Object visit(Field x) {
        return bcc.get(x);
    }

    /*==========================================*/
    /* Evaluates a Sig node or an ExprSig node. */
    /*==========================================*/

    @Override public Object visit(Sig x) {
        return bcc.get(x);
    }

    /*=============================*/
    /* Evaluates an ExprCall node. */
    /*=============================*/

    @Override public Object visit(ExprCall x) throws Err {
        Func y=x.fun;
        if (y.params.size()==0) {
            Object ans=bcc.get(y);
            if (ans!=null) return ans;
        }
        if (current_function.contains(y)) throw new ErrorSyntax(x.span(), ""+y+" cannot call itself recursively!");
        Env<ExprVar,Object> newenv=new Env<ExprVar,Object>();
        int r=0;
        for(ExprVar d:y.params) {
            newenv.put(d, cset(x.args.get(r)));
            r++;
        }
        Env<ExprVar,Object> oldenv=env;
        env=newenv;
        int oldfunc=current_function.size();
        current_function.add(y);
        Object ans=visitThis(y.getBody());
        env=oldenv;
        while(current_function.size()>oldfunc) current_function.remove(current_function.size()-1);
        if (ans instanceof Formula) fmap(ans, x);
        return ans;
    }

    /*================================*/
    /* Evaluates an ExprBuiltin node. */
    /*================================*/

    @Override public Object visit(ExprBuiltin x) throws Err {
        // This says  no(a&b) and no((a+b)&c) and no((a+b+c)&d)
        // Emperically this seems to be more efficient than "no(a&b) and no(a&c) and no(b&c)"
        Formula answer=null;
        Expression a=null;
        for(Expr arg:x.args) {
            Expression b=cset(arg);
            if (a==null) {a=b;continue;}
            if (answer==null) answer=a.intersection(b).no(); else answer=a.intersection(b).no().and(answer);
            a=a.union(b);
        }
        if (answer!=null) return fmap(answer,x); else return Formula.TRUE;
    }

    /*===============================*/
    /* Evaluates an ExprBinary node. */
    /*===============================*/

    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        Expression s, s2; IntExpression i; Formula f; Object obj;
        switch(x.op) {
            case IN: f=isIn(cset(a),b); return fmap(f,x);
            case LT: i=cint(a); f=i.lt(cint(b)); return fmap(f,x);
            case LTE: i=cint(a); f=i.lte(cint(b)); return fmap(f,x);
            case GT: i=cint(a); f=i.gt(cint(b)); return fmap(f,x);
            case GTE: i=cint(a); f=i.gte(cint(b)); return fmap(f,x);
            case AND: f=cform(a); f=f.and(cform(b)); return fmap(f,x);
            case OR: f=cform(a); f=f.or(cform(b)); return fmap(f,x);
            case IFF: f=cform(a); f=f.iff(cform(b)); return fmap(f,x);
            case PLUSPLUS: s=cset(a); return s.override(cset(b));
            case PLUS:
                obj=visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; return i.plus(cint(b)); }
                s=(Expression)obj; return s.union(cset(b));
            case MINUS:
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
                return fmap(f,x);
            case DOMAIN:
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
        if (right instanceof ExprBinary) {
            // Handles possible "binary" or higher-arity multiplicity
            return isInBinary(a, (ExprBinary)right);
        }
        return a.in(cset(right));
    }

    private Formula isInBinary(Expression r, ExprBinary ab) throws Err {
        if (!ab.op.isArrow || ab.mult==0) return r.in(cset(ab));
        // "R in A ->op B" means for each tuple a in A, there are "op" tuples in r that begins with a.
        // "R in A op-> B" means for each tuple b in B, there are "op" tuples in r that end with b.
        Decls d=null; Expression a=cset(ab.left), atuple=null, ar=r;
        for(int i=a.arity(); i>0; i--) {
          Variable v=Variable.unary("[discard]");
          if (a.arity()==1) d=v.oneOf(a); else if (d==null) d=v.oneOf(Relation.UNIV); else d=v.oneOf(Relation.UNIV).and(d);
          ar=v.join(ar);
          if (atuple==null) atuple=v; else atuple=atuple.product(v);
        }
        Formula ans1=isIn(ar, ab.right);
        switch(ab.op) {
          case ISSEQ_ARROW_LONE:
          case ANY_ARROW_LONE: case SOME_ARROW_LONE: case ONE_ARROW_LONE: case LONE_ARROW_LONE: ans1=ar.lone().and(ans1); break;
          case ANY_ARROW_ONE:  case SOME_ARROW_ONE:  case ONE_ARROW_ONE:  case LONE_ARROW_ONE:  ans1=ar.one().and(ans1);  break;
          case ANY_ARROW_SOME: case SOME_ARROW_SOME: case ONE_ARROW_SOME: case LONE_ARROW_SOME: ans1=ar.some().and(ans1); break;
        }
        // TODO: We should allow manual grounding
        if (a.arity()==1) ans1=ans1.forAll(d); else ans1=isIn(atuple, ab.left).implies(ans1).forAll(d);
        //
        Decls d2=null; Expression b=cset(ab.right), btuple=null, rb=r;
        for(int i=b.arity(); i>0; i--) {
          Variable v=Variable.unary("[discard]");
          if (b.arity()==1) d2=v.oneOf(b); else if (d2==null) d2=v.oneOf(Relation.UNIV); else d2=v.oneOf(Relation.UNIV).and(d2);
          rb=rb.join(v);
          if (btuple==null) btuple=v; else btuple=v.product(btuple);
        }
        Formula ans2=isIn(rb, ab.left);
        switch(ab.op) {
          case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE: ans2=rb.lone().and(ans2); break;
          case ONE_ARROW_LONE: case ONE_ARROW_ANY:   case ONE_ARROW_SOME: case ONE_ARROW_ONE:   ans2=rb.one().and(ans2);  break;
          case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE: ans2=rb.some().and(ans2); break;
        }
        if (b.arity()==1) ans2=ans2.forAll(d2); else ans2=isIn(btuple, ab.right).implies(ans2).forAll(d2);
        //
        Formula ans=r.in(a.product(b)).and(ans1).and(ans2);
        if (ab.op==ExprBinary.Op.ISSEQ_ARROW_LONE) {
            Expression rr=r;
            while(rr.arity()>1) rr=rr.join(Relation.UNIV);
            ans=rr.difference(rr.join(BoundsComputer.SIGINT_NEXT)).in(BoundsComputer.SIGINT_ZERO).and(ans);
        }
        return ans;
    }

    /*==============================*/
    /* Evaluates an ExprQuant node. */
    /*==============================*/

    private static Expr addOne(Expr x) {
        if (x instanceof ExprUnary) switch(((ExprUnary)x).op) {
            case SETOF: case ONEOF: case LONEOF: case SOMEOF: return x;
        }
        return (x.type.arity()!=1) ? x : ExprUnary.Op.ONEOF.make(x.span(), x);
    }

    private Object visit_qt(final ExprQuant.Op op, final ConstList<ExprVar> xvars, final Expr sub, final boolean split) throws Err {
        if (op == ExprQuant.Op.NO) {
            return visit_qt(ExprQuant.Op.ALL, xvars, sub.not(), false);
        }
        if (op == ExprQuant.Op.ONE || op == ExprQuant.Op.LONE) {
            for(int i=0; ;i++) {
                if (i>=xvars.size() && op==ExprQuant.Op.ONE)
                   return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub, false)).one();
                if (i>=xvars.size() && op==ExprQuant.Op.LONE)
                   return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub, false)).lone();
                Expr v=addOne(xvars.get(i).expr);
                if (v.type.arity()>1) break;
                if (v.mult!=1) break;
                if (!(v instanceof ExprUnary)) break;
                if (((ExprUnary)v).op!=ExprUnary.Op.ONEOF) break;
            }
        }
        if (op == ExprQuant.Op.ONE) {
            Formula f1 = (Formula) visit_qt(ExprQuant.Op.LONE, xvars, sub, false);
            Formula f2 = (Formula) visit_qt(ExprQuant.Op.SOME, xvars, sub, false);
            return f1.and(f2);
        }
        if (op == ExprQuant.Op.LONE) {
            Pair p1 = (Pair) visit_qt(ExprQuant.Op.ALL, xvars, sub, true);
            Pair p2 = (Pair) visit_qt(ExprQuant.Op.ALL, xvars, sub, true);
            Formula f1 = (Formula)(p1.a), f2 = (Formula)(p2.a), eqv = Formula.TRUE;
            List s1 = (List)(p1.b), s2 = (List)(p2.b);
            Decls decls = null;
            for(int i=0; i<s1.size(); i++) {
                Decl d1=(Decl)(s1.get(i)), d2=(Decl)(s2.get(i));
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
            decl2type.put(newd, new Pair<Type,Pos>(dex.type, dex.pos));
            if (dd==null) dd=newd; else dd=dd.and(newd);
            decls.add(newd);
        }
        final Formula ans = (op==ExprQuant.Op.SUM) ? null : cform(sub) ;
        final IntExpression ians = (op!=ExprQuant.Op.SUM) ? null : cint(sub) ;
        for(ExprVar dex:xvars) env.remove(dex);
        if (split) return new Pair<Formula,List>(guard.implies(ans), decls);
        if (op==ExprQuant.Op.COMPREHENSION) return ans.comprehension(dd); // guard==Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SUM) return ians.sum(dd);                    // guard==Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SOME) return guard.and(ans).forSome(dd); else return guard.implies(ans).forAll(dd);
    }

    @Override public Object visit(ExprQuant x) throws Err {
        // Special translation that are useful for util/integer.als (and any other places where this expression shows up)
        if (x.op==ExprQuant.Op.COMPREHENSION && x.vars.size()==1) {
            ExprVar a=x.vars.get(0);
            Expr max=a.gte(ZERO).and(a.plus(ONE).lt(ZERO));
            Expr min=a.lt(ZERO).and(a.minus(ONE).gte(ZERO));
            if (x.sub.isSame(max)) return BoundsComputer.SIGINT_MAX;
            if (x.sub.isSame(min)) return BoundsComputer.SIGINT_MIN;
        }
        if (x.op==ExprQuant.Op.COMPREHENSION && x.vars.size()==2) {
            ExprVar a=x.vars.get(0), b=x.vars.get(1);
            Expr next=b.gt(a).and(b.equal(a.plus(ONE)));
            if (x.sub.isSame(next)) return BoundsComputer.SIGINT_NEXT;
        }
        Object ans = visit_qt(x.op, x.vars, x.sub, false);
        if (ans instanceof Formula) fmap.put((Formula)ans, x);
        return ans;
    }
}
