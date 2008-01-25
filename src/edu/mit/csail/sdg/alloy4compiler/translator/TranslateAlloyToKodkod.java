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
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
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
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.HybridStrategy;
import kodkod.engine.ucore.RCEStrategy;
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

    /** This maps each Kodkod Variable we generate to an Alloy Type and Alloy Pos. */
    private final Map<Variable,Pair<Type,Pos>> decl2type = new IdentityHashMap<Variable,Pair<Type,Pos>>();

    /** This maps each Kodkod skolem relation we generate to an Alloy Type. */
    private final Map<Relation,Type> rel2type = new IdentityHashMap<Relation,Type>();

    //==============================================================================================================//

    /** The current reporter. */
    private A4Reporter rep;

    /** The list of all Sigs. */
    private final ConstList<Sig> sigs;

    /** If nonnull, it's the current command. */
    private final Command cmd;

    /**
     * Step1: Initialize the list of sigs and the command to check.
     *
     * @param rep - if nonnull, it's the reporter that will receive diagnostics and progress reports
     * @param sigs - the list of sigs (this list must be complete)
     * @param cmd - the command to solve
     *
     * <p> Reads: none
     * <p> Writes: rep, sigs, cmd
     */
    private TranslateAlloyToKodkod (A4Reporter rep, Iterable<Sig> sigs, Command cmd) {
        this.rep = (rep != null) ? rep : A4Reporter.NOP;
        IdentitySet<Sig> set = new IdentitySet<Sig>();
        set.add(Sig.UNIV); set.add(Sig.SIGINT); set.add(Sig.SEQIDX); set.add(Sig.NONE);
        if (sigs!=null) for(Sig s:sigs) set.add(s);
        this.sigs = ConstList.make(set);
        this.cmd = cmd;
    }

    //==============================================================================================================//

    /** The integer bitwidth. */
    private int bitwidth;

    /** The maximum sequence length. */
    private int maxseq;

    /** The maximum possible integer with the given bitwidth. */
    private int max;

    /** The minimum possible integer with the given bitwidth. */
    private int min;

    /** This maps each AlloySig, each AlloyField, and possibly even some parameterless AlloyFunc to a Kodkod Expression. */
    private ConstMap<Object,Expression> bcc;

    /** This maps each KodkodRelation to an upperbound and a lowerbound. */
    private Bounds bounds;

    /** This is the formula we want to satisfy. */
    private final List<Formula> goal = new ArrayList<Formula>();

    /** Conjoin the constraints for "field declarations" and "fact" paragraphs */
    private void makeFacts(Expr facts) throws Err {
        ArrayList<Expr> ar = new ArrayList<Expr>();
        makelist(ar, facts);
        again:
        for(Sig sig: sigs) {
            while(sig.isOne!=null && sig.getFields().size()==3) {
                Field f1 = sig.getFields().get(0); Relation fst = right(bcc.get(f1)); if (fst==null) break;
                Field f2 = sig.getFields().get(1); Relation lst = right(bcc.get(f2)); if (lst==null) break;
                Field f3 = sig.getFields().get(2); Relation nxt = right(bcc.get(f3)); if (nxt==null) break;
                Sig e = findElem(sig,f1,f2,f3);
                if (e==null || e.isOrdered==null) break;
                Expression ee = bcc.get(e);
                if (!(ee instanceof Relation)) break;
                for(int i=0; i+5<ar.size(); i++) {
                    if (findOrder(e,sig,f1,f2,f3, ar.get(i), ar.get(i+1), ar.get(i+2), ar.get(i+3), ar.get(i+4), ar.get(i+5))) {
                        rep.debug("Found: util/ordering\n");
                        // Remove ar[i..i+5]; the remaining elements are not re-arranged
                        ar.remove(i+5); ar.remove(i+4); ar.remove(i+3); ar.remove(i+2); ar.remove(i+1); ar.remove(i);
                        Formula f = nxt.totalOrder((Relation)ee, fst, lst);
                        goal.add(fmap(f, e.isOrdered));
                        continue again;
                    }
                }
                break;
            }
            for(Field f:sig.getFields()) {
                // Each field f has a boundingFormula that says "all x:s | x.f in SOMEEXPRESSION";
                goal.add(fmap(cform(f.boundingFormula), f));
                // Given the above, we can be sure that every column is well-bounded (except possibly the first column).
                // Thus, we need to add a bound that the first column is a subset of s.
                if (sig.isOne==null) {
                    Expression sr=bcc.get(sig), fr=bcc.get(f);
                    for(int i=f.type.arity(); i>1; i--) fr=fr.join(Relation.UNIV);
                    goal.add(fmap(fr.in(sr), f));
                }
            }
        }
        for(Expr e:ar) goal.add(fmap(cform(e), e));
    }

    /**
     * Step2: construct the bounds and the formula we want to satisfy.
     * <p> Reads: rep, sigs, cmd
     * <p> Writes: bitwidth, maxseq, bcc, bounds, goal
     */
    private void makeFormula (Expr facts) throws Err {
        facts = (Expr) (new ConvToConjunction()).visitThis(facts);
        rep.debug("Generating bounds...\n");
        final ScopeComputer sc = new ScopeComputer(rep,sigs,cmd);
        bitwidth = sc.getBitwidth();
        max=(1<<(bitwidth-1))-1;
        min=(0-max)-1;
        maxseq = sc.getMaxSeq();
        final Pair<Pair<Bounds,List<Formula>>,ConstMap<Object,Expression>> bc = BoundsComputer.compute(sc,rep,sigs,fmap);
        bcc = bc.b;
        bounds = bc.a.a;
        goal.addAll(bc.a.b);
        rep.debug("Generating facts...\n");
        makeFacts(facts);
        // Kodkod sometimes refuses to enlarge a Relation during solution enumeration
        // if that Relation is never mentioned in the GOAL formula; so, this ensures that
        // the said relation is mentioned (and the R==R is optimized away very efficiently, so we don't incur runtime cost)
        for(Relation r: bounds.relations()) goal.add(r.eq(r));
    }

    //==============================================================================================================//

    /** The Kodkod solver object. */
    private Solver solver;

    /** The temporary file that may receive the CNF (or null if we are not writing the CNF out) */
    private File tmpCNF = null;

    /**
     * Step3: construct the Kodkod solver object, and chooses the temporary file name
     * <p> Reads: rep, cmd, bitwidth, maxseq
     * <p> Writes: solver, tmpCNF
     */
    private void makeSolver (A4Options opt) throws Err, IOException {
        rep.debug("Assigning kodkod options...\n");
        int sym = (cmd.expects==1 ? 0 : opt.symmetry);
        solver = new Solver();
        solver.options().setFlatten(false); // added for now, since multiplication and division circuit takes forever to flatten
        if (opt.solver.external()!=null) {
            String ext = opt.solver.external();
            if (opt.solverDirectory.length()>0 && ext.indexOf(File.separatorChar)<0) ext=opt.solverDirectory+File.separatorChar+ext;
            File tmp = File.createTempFile("tmp", ".cnf", new File(opt.tempDirectory));
            solver.options().setSolver(SATFactory.externalFactory(ext, "", tmp.getAbsolutePath(), ""));
        } else if (opt.solver.equals(A4Options.SatSolver.ZChaffJNI)) {
            solver.options().setSolver(SATFactory.ZChaff);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatJNI)) {
            solver.options().setSolver(SATFactory.MiniSat);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatProverJNI)) {
            sym=20;
            solver.options().setSolver(SATFactory.MiniSatProver);
            solver.options().setLogTranslation(2);
        } else if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            tmpCNF = File.createTempFile("tmp", ".cnf", new File(opt.tempDirectory));
            String name = System.getProperty("user.home")+File.separatorChar+"nosuchprogram"+File.separatorChar;
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
                    Pair<Type,Pos> p=decl2type.get(decl.variable());
                    if (p==null) return;
                    Type t=p.a;
                    for(int i=(predecl==null ? -1 : predecl.size()-1); i>=0; i--) {
                        Pair<Type,Pos> pp=decl2type.get(predecl.get(i).variable());
                        if (pp==null) return; else t=(pp.a).product(t);
                    }
                    while(t.arity() > skolem.arity()) t=UNIV.type.join(t); // Should not happen, but just to be safe...
                    rel2type.put(skolem,t);
                } catch(Throwable ex) { } // Exception here is not fatal
            }
            @Override public void solvingCNF(int primaryVars, int vars, int clauses) {
                if (rep!=null) rep.solve(primaryVars, vars, clauses);
            }
        });
        rep.debug("Simplifying the bounds...\n");
        if (!Simplifier.simplify(bounds, goal, solver.options())) { goal.clear(); goal.add(Formula.FALSE); }
        rep.translate(opt.solver.id(), bitwidth, maxseq, opt.skolemDepth, sym);
    }

    //==============================================================================================================//

    /** Given a list of Formula, construct a balanced binary tree (actually a "binary heap" where node X's two children are node 2X and node 2X+1) then conjoin into a single formula. */
    private static Formula make(List<Formula> list, int i) {
        if (i<1 || i>list.size()) return Formula.TRUE;
        Formula me = list.get(i-1);
        int child1=i+i, child2=child1+1;
        if (child1<i || child1>list.size()) return me;
        me = me.and(make(list, child1));
        if (child2<child1 || child2>list.size()) return me;
        me = me.and(make(list, child2));
        return me;
    }

    private static final class Peeker<T> implements Iterator<T> {
        private Iterator<T> iterator;
        private boolean hasFirst;
        private T first;
        private Peeker(Iterator<T> it) {
            iterator = it;
            hasFirst = it.hasNext();
            if (hasFirst) first=it.next(); else first=null;
        }
        public boolean hasNext() {
            return hasFirst || iterator.hasNext();
        }
        public T next() {
            if (hasFirst) { hasFirst=false; T ans=first; first=null; return ans; } else return iterator.next();
        }
        public void remove() { throw new UnsupportedOperationException(); }
    }

    /**
     * Step4: solve for the solution
     * <p> Reads: all
     * <p> Writes: all
     */
    private A4Solution solve (boolean tryBookExamples, A4Options opt) throws Err {
        Formula fgoal = make(goal,1);
        rep.debug("Generating the solution...\n");
        long time = System.currentTimeMillis();
        Iterator<Solution> sols;
        Solution sol=null;
        IdentitySet<Formula> lCore = null, hCore = null;
        if (tryBookExamples) {
            A4Reporter r = "yes".equals(System.getProperty("debug")) ? rep : null;
            A4Reporter save = rep;
            rep = null;
            try { sol=BookExamples.trial(rel2type, r, sigs, bcc, bounds, fgoal, solver, cmd.check); } catch(Throwable ex) { }
            rep = save;
        }
        if (solver.options().solver()==SATFactory.ZChaff || !solver.options().solver().incremental()) {
            sols=null;
            if (sol==null) sol=solver.solve(fgoal, bounds);
        } else {
            sols=new Peeker<Solution>(solver.solveAll(fgoal, bounds));
            if (sol==null) sol=sols.next();
        }
        final Instance inst = sol.instance();
        if (inst==null && solver.options().solver()==SATFactory.MiniSatProver) {
            try {
                lCore=new IdentitySet<Formula>();
                Proof p=sol.proof();
                if (sol.outcome()==UNSATISFIABLE) {
                    int i = p.highLevelCore().size();
                    rep.minimizing(cmd, i);
                    if (opt.coreMinimization==0) try { p.minimize(new RCEStrategy(p.log())); } catch(Throwable ex) {}
                    if (opt.coreMinimization==1) try { p.minimize(new HybridStrategy(p.log())); } catch(Throwable ex) {}
                    rep.minimized(cmd, i, p.highLevelCore().size());
                }
                for(Iterator<TranslationRecord> it=p.core(); it.hasNext();) {
                    Object n=it.next().node();
                    if (n instanceof Formula) lCore.add((Formula)n);
                }
                hCore=new IdentitySet<Formula>(p.highLevelCore());
            } catch(Throwable ex) {
                lCore=hCore=null;
            }
        }
        solver.options().setReporter(blankReporter); // To ensure no more output during SolutionEnumeration
        if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            // The formula is trivial! (otherwise, since we used a nonexistent solver name, it would have thrown an exception.
            // Since the user wants it in CNF format, we manually generate
            // a trivially satisfiable (or unsatisfiable) CNF file.
            String txt = inst!=null ? "p cnf 1 1\n1 0\n" : "p cnf 1 2\n1 0\n-1 0\n";
            String out = tmpCNF.getAbsolutePath();
            Util.writeAll(out, txt);
            rep.resultCNF(out);
            return null;
        }
        A4Solution answer = new A4Solution(sigs, bcc, opt.originalFilename, cmd.toString(),
           sols, (opt.recordKodkod ? fgoal : null), bounds, bitwidth, inst, rel2type, fmap, lCore, hCore);
        time = System.currentTimeMillis() - time;
        if (answer.satisfiable()) rep.resultSAT(cmd, time, answer); else rep.resultUNSAT(cmd, time, answer);
        return answer;
    }

    //==============================================================================================================//

    /**
     * Based on the specified "options", execute one command and return the resulting A4Solution object.
     *
     * @param rep - if nonnull, we'll send compilation diagnostic messages to it
     * @param sigs - the list of sigs; this list must be complete
     * @param fact - a formula that must be satisfied by the solution
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     *
     * @return null if the user chose "save to FILE" as the SAT solver,
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_command (A4Reporter rep, Iterable<Sig> sigs, Expr fact, Command cmd, A4Options opt)
    throws Err {
        if (rep==null) rep=A4Reporter.NOP;
        if (fact==null) fact=ExprConstant.TRUE;
        TranslateAlloyToKodkod tr = null;
        try {
            tr = new TranslateAlloyToKodkod(rep, sigs, cmd);
            tr.makeFormula(fact);
            tr.makeSolver(opt);
            return tr.solve(false, opt);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x = tr!=null ? tr.decl2type.get(ex.decl().variable()) : null;
            Pos p = x!=null ? x.b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order " +
               "quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            if (tr!=null && tr.tmpCNF!=null && ex.toString().contains("nosuchprogram")) {
                rep.resultCNF(tr.tmpCNF.getAbsolutePath());
                return null;
            }
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
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
     * @param fact - a formula that must be satisfied by the solution
     * @param cmd - the Command to execute
     * @param opt - the set of options guiding the execution of the command
     *
     * @return null if the user chose "save to FILE" as the SAT solver,
     * and nonnull if the solver finishes the entire solving and is either satisfiable or unsatisfiable.
     * <p> If the return value X is satisfiable, you can call X.next() to get the next satisfying solution X2;
     * and you can call X2.next() to get the next satisfying solution X3... until you get an unsatisfying solution.
     */
    public static A4Solution execute_commandFromBook (A4Reporter rep, List<Sig> sigs, Expr fact, Command cmd, A4Options opt)
    throws Err {
        if (rep==null) rep=A4Reporter.NOP;
        if (fact==null) fact=ExprConstant.TRUE;
        TranslateAlloyToKodkod tr = null;
        try {
            tr = new TranslateAlloyToKodkod(rep, sigs, cmd);
            tr.makeFormula(fact);
            tr.makeSolver(opt);
            return tr.solve(true, opt);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x = tr!=null ? tr.decl2type.get(ex.decl().variable()) : null;
            Pos p = x!=null ? x.b : Pos.UNKNOWN;
            throw new ErrorType(p, "Analysis cannot be performed since it requires higher-order " +
               "quantification that could not be skolemized.");
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            if (tr!=null && tr.tmpCNF!=null && ex.toString().contains("nosuchprogram")) {
                rep.resultCNF(tr.tmpCNF.getAbsolutePath());
                return null;
            }
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Translate the Alloy expression into an equivalent Kodkod Expression or IntExpression or Formula object.
     * @param bcc - this must map every Sig and every Field to an equivalent Kodkod Expression
     * @param bitwidth - this specifies the integer bitwidth and must be between 1 and 30
     * @param expr - this is the Alloy expression we want to translate
     */
    public static Object alloy2kodkod(ConstMap<Object,Expression> bcc, int bitwidth, Expr expr)
    throws Err {
        if (bitwidth<1 || bitwidth>30) throw new ErrorType("The integer bitwidth must be between 1 and 30.");
        if (expr.ambiguous && !expr.errors.isEmpty()) expr = expr.resolve(expr.type, new ArrayList<ErrorWarning>());
        if (!expr.errors.isEmpty()) throw expr.errors.get(0);
        TranslateAlloyToKodkod tr = new TranslateAlloyToKodkod(null, null, null);
        tr.bcc = bcc;
        tr.bitwidth = bitwidth;
        tr.max=(1<<(bitwidth-1))-1;
        tr.min=(0-tr.max)-1;
        Object ans;
        try {
            ans = tr.visitThis(expr);
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(HigherOrderDeclException ex) {
            throw new ErrorType("Analysis cannot be performed since it requires higher-order " +
               "quantification that could not be skolemized.");
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

    /** If f is a formula, and it hasn't been associated with any location information, then associate it with x. */
    private Formula fmap(Object f, Expr x) {
        if (!(f instanceof Formula)) return null;
        Formula ff=(Formula)f;
        if (fmap.containsKey(ff)) return ff;
        fmap.put(ff, x);
        if (ff instanceof BinaryFormula) {
            BinaryFormula b=(BinaryFormula)ff;
            if (b.op() == BinaryFormula.Operator.AND) { fmap(b.left(), x); fmap(b.right(), x); }
        }
        return ff;
    }

    /** If f is a formula, and it hasn't been associated with any location information, then associate it with x. */
    private Formula fmap(Object f, Pos x) {
        if (!(f instanceof Formula)) return null;
        Formula ff=(Formula)f;
        if (x==null || x==Pos.UNKNOWN || fmap.containsKey(f)) return ff;
        fmap.put(ff, x);
        if (ff instanceof BinaryFormula) {
            BinaryFormula b=(BinaryFormula)ff;
            if (b.op() == BinaryFormula.Operator.AND) { fmap(b.left(), x); fmap(b.right(), x); }
        }
        return ff;
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

    /** Returns the sig "elem" if field1="all this:s | s.f1 in set elem", field2=same, field3="... in elem->elem" */
    private static Sig findElem(Sig s, Field field1, Field field2, Field field3) {
        Expr b1=findY(s,field1), b2=findY(s,field2), b3=findY(s,field3);
        if (!(b1 instanceof Sig) || b1!=b2 || b3==null) return null;
        if (!(b3.isSame(b1.product(b1)))) return null;
        return (Sig)b1;
    }

    /**
     * Returns true if we can determine that "e1 && e2 && e3 && e4 && e5 && e6" says "total order on elem".
     *
     * <p> In particular, that means:
     * <br> e1 == elem in First.*Next
     * <br> e2 == no First.(~Next)
     * <br> e3 == no Last.Next
     * <br> e4 == all e: one elem | (e = First || one e.(~Next))
     * <br> e5 == all e: one elem | (e = Last || one e.Next)
     * <br> e6 == all e: one elem | (e !in e.^Next)
     */
    private static boolean findOrder
    (Sig elem, Sig ord, Expr first, Expr last, Expr next, Expr e1, Expr e2, Expr e3, Expr e4, Expr e5, Expr e6) {
        Expr prev;
        ExprQuant qt;
        ExprVar e;
        first = ord.join(first);
        last = ord.join(last);
        next = ord.join(next);
        prev = next.transpose();
        if (!elem.in(first.join(next.reflexiveClosure())).isSame(e1)) return false;
        if (!first.join(prev).no().isSame(e2)) return false;
        if (!last.join(next).no().isSame(e3)) return false;
        if (!(e4 instanceof ExprQuant)) return false;
        //
        qt=(ExprQuant)e4; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
        e=qt.vars.get(0); if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        if (!e.equal(first).or(e.join(prev).one()).isSame(qt.sub)) return false;
        //
        qt=(ExprQuant)e5; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
        e=qt.vars.get(0); if (!e.expr.isSame(ExprUnary.Op.ONEOF.make(null,elem))) return false;
        if (!e.equal(last).or(e.join(next).one()).isSame(qt.sub)) return false;
        //
        qt=(ExprQuant)e6; if (qt.op!=ExprQuant.Op.ALL || qt.vars.size()!=1) return false;
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
        Formula c=cform(x.cond);
        Object l=visitThis(x.left);
        if (l instanceof Formula) {
            Formula c1 = fmap( c.implies((Formula)l) , x );
            Formula c2 = fmap( c.not().implies(cform(x.right)) , x );
            return fmap(c1.and(c2) , x );
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
        Object ans=visitThis(x.sub);
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
            if (n<min) throw new ErrorType(x.pos,
               "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
            if (n>max) throw new ErrorType(x.pos,
               "Current bitwidth is set to "+bitwidth+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
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

    /** {@inheritDoc} */
    @Override public Object visit(ExprUnary x) throws Err {
        switch(x.op) {
            case SOMEOF: case LONEOF: case ONEOF: case SETOF: return cset(x.sub);
            case NOOP: return visitThis(x.sub);
            case NOT: return fmap( cform(x.sub).not() , x );
            case SOME: return fmap( cset(x.sub).some() , x);
            case LONE: return fmap( cset(x.sub).lone() , x);
            case ONE: return fmap( cset(x.sub).one() , x);
            case NO: return fmap( cset(x.sub).no() , x);
            case TRANSPOSE: return cset(x.sub).transpose();
            case CARDINALITY: return cset(x.sub).count();
            case CAST2SIGINT: return cint(x.sub).toExpression();
            case CAST2INT: return sum(cset(x.sub));
            case RCLOSURE:
                Expression iden=Expression.IDEN.intersection(bcc.get(UNIV).product(Relation.UNIV));
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
        if (ans==null) throw new ErrorFatal(x.pos, "Variable \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /*=========================*/
    /* Evaluates a Field node. */
    /*=========================*/

    /** {@inheritDoc} */
    @Override public Object visit(Field x) throws Err {
        Expression ans=bcc.get(x);
        if (ans==null) throw new ErrorFatal(x.pos, "Field \""+x+"\" is not bound to a legal value during translation.\n");
        return ans;
    }

    /*=======================*/
    /* Evaluates a Sig node. */
    /*=======================*/

    /** {@inheritDoc} */
    @Override public Object visit(Sig x) throws Err {
        Expression ans=bcc.get(x);
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

    /** {@inheritDoc} */
    @Override public Object visit(ExprCall x) throws Err {
        Func f=x.fun;
        int n=f.params.size();
        if (n==0) {
            Object ans=bcc.get(f); // Try looking it up; it may have been pre-bound to some value
            if (ans!=null) return ans;
        }
        //int maxRecursion=12;
        for(Func ff:current_function) if (ff==f) {
            throw new ErrorSyntax(x.span(), ""+f+" cannot call itself recursively!");
            /*
            maxRecursion--;
            if (maxRecursion==0) {
                Type t = f.returnDecl.type;
                if (t.is_bool) return Formula.FALSE;
                if (t.is_int) return IntConstant.constant(0);
                int i = t.arity();
                Expression ans = Expression.NONE;
                while(i>1) { ans = ans.product(Expression.NONE); i--; }
                return ans;
            }
            */
        }
        if (is_mul(f)) {
            rep.debug("Found: util/integer/mul\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.multiply(b).toExpression();
        }
        if (is_div(f)) {
            rep.debug("Found: util/integer/div\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.divide(b).toExpression();
        }
        if (is_rem(f)) {
            rep.debug("Found: util/integer/rem\n");
            final IntExpression a = sum(cset(x.args.get(0))), b = sum(cset(x.args.get(1)));
            return a.modulo(b).toExpression();
        }
        Env<ExprVar,Object> newenv=new Env<ExprVar,Object>();
        for(int i=0; i<n; i++) newenv.put(f.params.get(i), cset(x.args.get(i)));
        Env<ExprVar,Object> oldenv=env;
        env=newenv;
        current_function.add(f);
        Object ans=visitThis(f.getBody());
        env=oldenv;
        current_function.remove(current_function.size()-1);
        if (ans instanceof Formula) fmap(ans, x);
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
            if (answer==null) answer=fmap(a.intersection(b).no(), x); else answer=fmap(a.intersection(b).no(), x).and(answer);
            a=a.union(b);
        }
        if (answer!=null) return fmap(answer,x); else return Formula.TRUE;
    }

    /*===============================*/
    /* Evaluates an ExprBinary node. */
    /*===============================*/

    /** {@inheritDoc} */
    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        Expression s, s2; IntExpression i; Formula f; Object obj;
        switch(x.op) {
            case IN: return fmap(isIn(cset(a),b), x);
            case LT: i=cint(a); f=i.lt(cint(b)); return fmap(f,x);
            case LTE: i=cint(a); f=i.lte(cint(b)); return fmap(f,x);
            case GT: i=cint(a); f=i.gt(cint(b)); return fmap(f,x);
            case GTE: i=cint(a); f=i.gte(cint(b)); return fmap(f,x);
            case AND: f=cform(a); f=f.and(cform(b)); return fmap(f,x);
            case OR: f=cform(a); f=f.or(cform(b)); return fmap(f,x);
            case IFF: f=cform(a); f=f.iff(cform(b)); return fmap(f,x);
            case PLUSPLUS: s=cset(a); return s.override(cset(b));
            case SHL: i=cint(a); return i.shl(cint(b));
            case SHR: i=cint(a); return i.shr(cint(b));
            case SHA: i=cint(a); return i.sha(cint(b));
            case PLUS:
                obj=visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; return i.plus(cint(b)); }
                s=(Expression)obj; return s.union(cset(b));
            case MINUS:
                // Special exception to allow "0-8" to not throw an exception, where 7 is the maximum allowed integer (when bitwidth==4)
                // (likewise, when bitwidth==5, then +15 is the maximum allowed integer, and we want to allow 0-16 without throwing an exception)
                if (a instanceof ExprConstant && ((ExprConstant)a).op==ExprConstant.Op.NUMBER && ((ExprConstant)a).num()==0)
                   if (b instanceof ExprConstant && ((ExprConstant)b).op==ExprConstant.Op.NUMBER && ((ExprConstant)b).num()==(1+max))
                      return IntConstant.constant(0-max-1);
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
            ans=rr.difference(rr.join(BoundsComputer.SIGINT_NEXT)).in(BoundsComputer.SIGINT_ZERO).and(ans);
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
            List<Decl> s1=p1.declarations().declarations(), s2=p2.declarations().declarations();
            Decls decls=null;
            for(int i=0; i<s1.size(); i++) {
                Decl d1=s1.get(i), d2=s2.get(i);
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
            decl2type.put(v, new Pair<Type,Pos>(dex.type, dex.pos));
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
                    return BoundsComputer.SIGINT_MAX;
                }
                if (a.lt(ZERO).and(a.minus(ONE).gte(ZERO)).isSame(x.sub)) {
                    rep.debug("Found: util/integer/min\n");
                    return BoundsComputer.SIGINT_MIN;
                }
            }
        }
        // Special translation that are useful for util/integer.als (and any other places where this expression shows up)
        if (x.op==ExprQuant.Op.COMPREHENSION && x.vars.size()==2) {
            ExprVar a=x.vars.get(0), b=x.vars.get(1);
            if (a.expr.isSame(Sig.SIGINT.oneOf()) && b.expr.isSame(Sig.SIGINT.oneOf())) {
                if (b.gt(a).and(b.equal(ONE.plus(a))).isSame(x.sub)) {
                    rep.debug("Found: util/integer/next\n");
                    return BoundsComputer.SIGINT_NEXT;
                }
            }
        }
        // All else, invoke the helper method to translate this quantification expression
        Object ans = visit_qt(x.op, x.vars, x.sub);
        if (ans instanceof Formula) fmap(ans, x);
        return ans;
    }
}
