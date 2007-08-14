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

import java.io.File;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
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
import edu.mit.csail.sdg.alloy4compiler.parser.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.ast.VisitReturn;
import edu.mit.csail.sdg.alloy4compiler.parser.World;
import kodkod.ast.BinaryFormula;
import kodkod.ast.Decl;
import kodkod.ast.IntExpression;
import kodkod.ast.Decls;
import kodkod.ast.IntConstant;
import kodkod.ast.IntToExprCast;
import kodkod.ast.Variable;
import kodkod.ast.Relation;
import kodkod.ast.Formula;
import kodkod.ast.Expression;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.config.AbstractReporter;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4.Util.tail;

/** Given a World object, solve one or more commands using Kodkod. */

public final class TranslateAlloyToKodkod extends VisitReturn {

    /**
     * This is used to detect "function recursion" (which we currently do not allow);
     * also, by knowing the current function name, we can provide a more meaningful name for skolem variables
     */
    private final List<Func> current_function=new ArrayList<Func>();

    /**
     * This is used to provide a more meaningful name for skolem variables (when we are not inside a function);
     * when we are inside a function, then the function's name is used to provide the prefix for skolem variables.
     */
    private String current_command="";

    /** This map keeps track of the mapping from local variables (LET, QUANT, Function Param) to the actual value. */
    private Env<ExprVar,Object> env=new Env<ExprVar,Object>();

    /** This map keeps track of the mapping from Kodkod Decl to the original Union Type and the original Pos. */
    private final Map<Decl,Pair<Type,Pos>> skolemType;

    /** This map keeps track of the mapping from Kodkod Skolem to the original Union Type. */
    private final Map<Relation,Type> skolemRelType=new IdentityHashMap<Relation,Type>();

    /** The Kodkod-to-Alloy map. */
    final Map<Formula,List<Object>> core;
    private Formula core(Formula f, Expr x) {
        if (core==null || x==null) return f;
        if (f instanceof BinaryFormula && ((BinaryFormula)f).op()==BinaryFormula.Operator.AND) {
            core(((BinaryFormula)f).left(), x);
            core(((BinaryFormula)f).right(), x);
            return f;
        }
        List<Object> list=core.get(f);
        if (list==null) { list=new ArrayList<Object>(3); core.put(f,list); }
        list.add(x);
        return f;
    }

    /**
     * True if we want to silently ignore "multiplicity constraints".
     * <br> It must be off by default (for correctness).
     * <br> However, somtimes we might turn this on, evaluate some expression, then turn it off again.
     */
    private boolean demul=false;

    /** The integer bitwidth. */
    private final int bitwidth;

    /**
     * If nonnull, this holds the BoundsComputer object which computed the bounds for each sig and each field.
     */
    final BoundsComputer bc;

    /**
     * If nonnull, this holds a map from Sig, Field, and String to a Kodkod Expression
     */
    private final Map<Object,Expression> bcc;

    /** Constructs a TranslateAlloyKodkod object based on the BoundsComputer object. */
    TranslateAlloyToKodkod(BoundsComputer bc, Map<Decl,Pair<Type,Pos>> skolemType, Map<Formula,List<Object>> core) {
        if (skolemType==null) skolemType=new IdentityHashMap<Decl,Pair<Type,Pos>>();
        this.skolemType=skolemType; this.bc=bc; this.bcc=null; this.bitwidth=bc.getBitwidth(); this.core=core;
    }

    /** Constructs a TranslateAlloyKodkod object. */
    TranslateAlloyToKodkod(Map<Object,Expression> bcc, int bitwidth, Map<Decl,Pair<Type,Pos>> skolemType, Map<Formula,List<Object>> core) {
        if (skolemType==null) skolemType=new IdentityHashMap<Decl,Pair<Type,Pos>>();
        this.skolemType=skolemType; this.bc=null; this.bcc=bcc; this.bitwidth=bitwidth; this.core=core;
    }

    //==============================================================================================================//

    private static A4Solution helper
    (final Map<Decl,Pair<Type,Pos>> skolemType, final World world, Command cmd, final A4Options opt,
    Map<String,String> originalSources, String xmlFileName, String tempFileName, boolean tryBookExamples)
    throws Err {
        ConstList<Sig> sigs = world.all();
        final Map<Formula,List<Object>> core=new LinkedHashMap<Formula,List<Object>>();
        final A4Reporter rep=A4Reporter.getReporter();
        rep.debug("Generating bounds...");
        final TranslateAlloyToKodkod tr = new TranslateAlloyToKodkod(new BoundsComputer(world, opt, cmd, core), skolemType, core);
        Formula kfact = tr.bc.getFacts();
        rep.debug("Generating facts...");
        for(Sig s:sigs) kfact = tr.makeFieldAndAppendedConstraints(world,s,kfact);
        for(Module u:world.getAllModules())
            for(Pair<String,Expr> e:u.getAllFacts())
                kfact = tr.core(tr.cform(e.b), e.b).and(kfact);
        Formula mainformula;
        tr.current_command=cmd.label;
        tr.current_function.clear();
        if (cmd.check) {
            mainformula = tr.core(tr.cform(cmd.formula.not()), cmd.formula).and(kfact);
        } else {
            mainformula=tr.core(tr.cform(cmd.formula), cmd.formula).and(kfact);
        }
        tr.current_command="";
        tr.current_function.clear();
        rep.debug("Assigning kodkod options...");
        Solver solver = new Solver();
        if (opt.solver.external()!=null) {
            String ext = opt.solver.external();
            if (opt.solverDirectory.length()>0 && ext.indexOf(File.separatorChar)<0) ext=opt.solverDirectory+File.separatorChar+ext;
            solver.options().setSolver(SATFactory.externalFactory(ext, "", tempFileName, ""));
        } else if (opt.solver.equals(A4Options.SatSolver.ZChaffJNI)) {
            solver.options().setSolver(SATFactory.ZChaff);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatJNI)) {
            solver.options().setSolver(SATFactory.MiniSat);
        } else if (opt.solver.equals(A4Options.SatSolver.MiniSatProverJNI)) {
            solver.options().setSolver(SATFactory.MiniSatProver);
        } else if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            solver.options().setSolver(SATFactory.externalFactory(
            System.getProperty("user.home")+File.separatorChar+"nosuchprogram", "", tempFileName, ""));
        } else {
            solver.options().setSolver(SATFactory.DefaultSAT4J);
        }
        solver.options().setBitwidth(tr.bitwidth);
        solver.options().setIntEncoding(Options.IntEncoding.BINARY);
        int sym=1;
        int skolemDepth=opt.skolemDepth;
        if (cmd.expects==1) sym=0;
        if (cmd.options.contains("nosym")) sym=0;
        if (cmd.options.contains("sym")) sym=1;
        sym = sym * opt.symmetry;
        if (opt.solver.equals(A4Options.SatSolver.MiniSatProverJNI)) {
            sym=20;
            solver.options().setLogTranslation(true);
        }
        solver.options().setSymmetryBreaking(sym);
        solver.options().setSkolemDepth(skolemDepth);
        rep.translate(opt.solver.id(), tr.bitwidth, tr.bc.getMaxSeq(), skolemDepth, sym);
        rep.debug("Assigning kodkod reporter...");
        solver.options().setReporter(new AbstractReporter() {
            @Override public final void skolemizing(Decl decl, Relation skolem, List<Decl> predecl) {
                try {
                    Pair<Type,Pos> p=tr.skolemType.get(decl);
                    if (p==null) return;
                    Type t=p.a;
                    if (predecl!=null) for(int i=predecl.size()-1; i>=0; i--) {
                        Pair<Type,Pos> pp=tr.skolemType.get(predecl.get(i));
                        if (pp==null) return;
                        t=(pp.a).product(t);
                    }
                    while(t.arity() > skolem.arity()) t=UNIV.type.join(t);
                    tr.skolemRelType.put(skolem,t);
                } catch(Throwable ex) { }
            }
            @Override public final void solvingCNF(int primaryVars, int vars, int clauses) {
                rep.solve(primaryVars, vars, clauses);
            }
        });
        rep.debug("Simplifying the bounds...");
        if (!tr.bc.simplify(mainformula, solver.options())) mainformula=Formula.FALSE;
        rep.debug("Generating the solution...");
        A4Solution mainResult;
        try {
            mainResult=A4Solution.make(
                tr,
                world,
                opt,
                tr.skolemRelType, // You cannot turn it Constant, since it needs to be modified DURING KODKOD TRANSLATION!
                solver,
                mainformula,
                originalSources,
                cmd,
                tryBookExamples);
        } catch(SaveToFileException ex) {
            rep.resultCNF(tempFileName);
            return null;
        }
        if (opt.solver.equals(A4Options.SatSolver.FILE)) {
            rep.debug("Solution trivial...");
            // If the formula wasn't trivial, we should have entered the "RuntimeException" clause above
            // (since we specified a non-sensical SAT solver name); but we didn't, hence the formula was trivial.
            // Now, since the user wants it in CNF format, so we manually generate
            // a trivial satisfiable (or unsatisfiable) CNF file.
            String txt = mainResult.satisfiable() ? "p cnf 1 1\n1 0\n" : "p cnf 1 2\n1 0\n-1 0\n";
            Util.writeAll(tempFileName, txt);
            rep.debug("Solution return...");
            rep.resultCNF(tempFileName);
            return null;
        }
        rep.debug("Solution nontrivial...");
        long t2=mainResult.solvingTime();
        if (!mainResult.satisfiable()) {
            rep.resultUNSAT(cmd, t2, mainResult.kInput, mainResult.core());
        } else {
            if (xmlFileName!=null && xmlFileName.length()>0) mainResult.writeXML(xmlFileName,true);
            rep.resultSAT(cmd, t2, mainResult.kInput, xmlFileName);
        }
        return mainResult;
    }

    /**
     * Based on the specified "options", execute a command from the given "world", then optionally write the result as an XML file.
     *
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
    public static A4Solution execute_command
    (World world, Command cmd, A4Options opt, String xmlFileName, String tempFileName)
    throws Err {
        Map<Decl,Pair<Type,Pos>> skolemType=new IdentityHashMap<Decl,Pair<Type,Pos>>();
        try {
            A4Solution ans=helper(skolemType,world,cmd,opt,null,xmlFileName,tempFileName,false);
            return ans;
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x=skolemType.get(ex.decl());
            Pos p=(x!=null ? x.b : Pos.UNKNOWN);
            throw new ErrorType(p,"Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Based on the specified "options", execute a command from the given "world", then optionally write the result as an XML file.
     * Note: it will first test whether the model fits one of the model from the "Software Abstractions" book;
     * if so, it will use the exact instance that was in the book.
     *
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
    public static A4Solution execute_commandFromBook
    (World world, Command cmd, A4Options opt, Map<String,String> originalSources, String xmlFileName, String tempFileName)
    throws Err {
        Map<Decl,Pair<Type,Pos>> skolemType=new IdentityHashMap<Decl,Pair<Type,Pos>>();
        try {
            A4Solution ans=helper(skolemType,world,cmd,opt,originalSources, xmlFileName,tempFileName,true);
            return ans;
        } catch(HigherOrderDeclException ex) {
            Pair<Type,Pos> x=skolemType.get(ex.decl());
            Pos p=(x!=null ? x.b : Pos.UNKNOWN);
            throw new ErrorType(p,"Analysis cannot be performed since it requires higher-order quantification that could not be skolemized.");
        } catch(UnsatisfiedLinkError ex) {
            throw new ErrorFatal("The required JNI library cannot be found: "+ex.toString().trim());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
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

    /**
     * Convenience method that evalutes x (ignoring all multiplicity) and cast the result to be a Kodkod Expression.
     * @return the expression - if x evaluates to an Expression
     * @throws ErrorFatal - if x does not evaluate to an Expression
     */
    private final Expression csetIgnoreMultiplicity(Expr x) throws Err {
        boolean old=demul;
        demul=true;
        Object y=visitThis(x);
        demul=old;
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
            if (current_command!=null && current_command.length()>0) return current_command+"_"+name;
            return name;
        }
        Func last=current_function.get(current_function.size()-1);
        if (last!=null) return tail(last.label)+"_"+name; else return name;
    }

    //==============================================================================================================//

    /** Construct the constraints for "field declarations" and "appended fact paragraphs" for the given sig. */
    private Formula makeFieldAndAppendedConstraints(World world, final Sig sig, Formula kfact) throws Err {
        if (sig.isOrd() != null) return kfact;
        for(Field f:sig.getFields()) {
            // Each field f has a boundingFormula that says "all x:s | x.f in SOMEEXPRESSION";
            kfact=core(cform(f.boundingFormula), f).and(kfact);
            // Given the above, we can be sure that every column is well-bounded (except possibly the first column).
            // Thus, we need to add a bound that the first column is a subset of s.
            Expression sr=bc.expr(sig), fr=bc.expr(f);
            for(int i=f.type.arity(); i>1; i--) fr=fr.join(Relation.UNIV);
            kfact=core(fr.in(sr), f).and(kfact);
        }
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
            Formula c1=core( c.implies((Formula)l) , x );
            Formula c2=core( c.not().implies(cform(x.right)) , x );
            return core(c1.and(c2) , x );
        }
        if (l instanceof Expression)
            return c.thenElse((Expression)l,cset(x.right));
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
              if (n<min) throw new ErrorType(x.pos, "Current bitwidth is set to "+b+", thus this integer constant "+n+" is smaller than the minimum integer "+min);
              if (n>max) throw new ErrorType(x.pos, "Current bitwidth is set to "+b+", thus this integer constant "+n+" is bigger than the maximum integer "+max);
              return IntConstant.constant(n);
          case TRUE: return Formula.TRUE;
          case FALSE: return Formula.FALSE;
          case IDEN:
              if (bc==null) return Relation.IDEN;
              return Expression.IDEN.intersection(bc.expr(UNIV).product(Relation.UNIV));
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
                if (demul) return cset(x.sub);
                throw new ErrorType(x.sub.span(), "Multiplicity symbols are not allowed here.");
            case NOT:
                if (x.sub instanceof ExprBinary && ((ExprBinary)(x.sub)).op==ExprBinary.Op.OR) {
                    // This transformation is not required; but it should give you better precision unsat core
                    Expr left=((ExprBinary)(x.sub)).left;
                    Expr right=((ExprBinary)(x.sub)).right;
                    Formula leftF = core( cform(left.not()) , left );
                    Formula rightF = core( cform(right.not()) , right );
                    return core( leftF.and(rightF) , x );
                }
                return cform(x.sub).not();
            case SOME: return cset(x.sub).some();
            case LONE: return cset(x.sub).lone();
            case ONE: return cset(x.sub).one();
            case NO: return cset(x.sub).no();
            case CAST2INT:
                // Efficiency shortcut that simplifies int[Int[x]] to x.
                Expression sub=cset(x.sub);
                if (sub instanceof IntToExprCast) return ((IntToExprCast)sub).intExpr();
                return sub.sum();
            case CAST2SIGINT: return cint(x.sub).toExpression();
            case TRANSPOSE: return cset(x.sub).transpose();
            case RCLOSURE:
                if (bc==null) return cset(x.sub).reflexiveClosure();
                Expression iden=Expression.IDEN.intersection(bc.expr(UNIV).product(Relation.UNIV));
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

    @Override public Object visit(Field x) throws Err {
        if (bc!=null) return bc.expr(x); else return bcc.get(x);
    }

    /*==========================================*/
    /* Evaluates a Sig node or an ExprSig node. */
    /*==========================================*/

    @Override public Object visit(Sig x) throws Err {
        if (bc!=null) return bc.expr(x); else return bcc.get(x);
    }

    /*=============================*/
    /* Evaluates an ExprCall node. */
    /*=============================*/

    @Override public Object visit(ExprCall x) throws Err {
        Func y=x.fun;
        if (current_function.contains(y))
            throw new ErrorSyntax(x.span(), ""+y+" cannot call itself recursively!");
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
        if (ans instanceof Formula) core((Formula)ans, x);
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
        return answer==null ? Formula.TRUE : answer;
    }

    /*===============================*/
    /* Evaluates an ExprBinary node. */
    /*===============================*/

    @Override public Object visit(ExprBinary x) throws Err {
        Expr a=x.left, b=x.right;
        Expression s, s2; IntExpression i; Formula f; Object obj;
        switch(x.op) {
            case IN: return isIn(cset(a),b);
            case LT: i=cint(a); return i.lt(cint(b));
            case LTE: i=cint(a); return i.lte(cint(b));
            case GT: i=cint(a); return i.gt(cint(b));
            case GTE: i=cint(a); return i.gte(cint(b));
            case AND: f=cform(a); return f.and(cform(b));
            case OR: f=cform(a); return f.or(cform(b));
            case IFF: f=cform(a); return f.iff(cform(b));
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
                if (demul) { s=cset(a); return s.product(cset(b)); }
                throw new ErrorType(x.right.span(), "Multiplicity symbols are not allowed here");
            case ARROW:
                s=cset(a); return s.product(cset(b));
            case JOIN:
                s=cset(a); return s.join(cset(b));
            case EQUALS:
                obj=visitThis(a);
                if (obj instanceof IntExpression) { i=(IntExpression)obj; return i.eq(cint(b));}
                s=(Expression)obj; return s.eq(cset(b));
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
        if (!ab.op.isArrow || ab.mult==0) return r.in(csetIgnoreMultiplicity(ab));
        // "R in A ->op B" means for each tuple a in A, there are "op" tuples in r that begins with a.
        // "R in A op-> B" means for each tuple b in B, there are "op" tuples in r that end with b.
        Decls d=null; Expression a=csetIgnoreMultiplicity(ab.left), atuple=null, ar=r;
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
        if (a.arity()==1) ans1=ans1.forAll(d); else ans1=isIn(atuple, ab.left).implies(ans1).forAll(d); // TODO We should allow manual grounding
        //
        Decls d2=null; Expression b=csetIgnoreMultiplicity(ab.right), btuple=null, rb=r;
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

    private Variable var(int arity, String name, Type type) {Variable v=Variable.nary(name,arity); return v;}

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
                if (i>=xvars.size() && op==ExprQuant.Op.ONE) return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub, false)).one();
                if (i>=xvars.size() && op==ExprQuant.Op.LONE) return ((Expression) visit_qt(ExprQuant.Op.COMPREHENSION, xvars, sub, false)).lone();
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
            final Variable v=var(dex.type.arity(), skolem(dex.label), dex.type);
            final Decl newd;
            final Expression dv = (dex.expr==lastExpr) ? lastValue : csetIgnoreMultiplicity(dex.expr);
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
            skolemType.put(newd, new Pair<Type,Pos>(dex.type, dex.pos));
            if (dd==null) dd=newd; else dd=dd.and(newd);
            decls.add(newd);
        }
        final Formula ans = (op==ExprQuant.Op.SUM) ? null : cform(sub) ;
        final IntExpression ians = (op!=ExprQuant.Op.SUM) ? null : cint(sub) ;
        for(ExprVar dex:xvars) env.remove(dex);
        if (split) return new Pair<Formula,List>(guard.implies(ans), decls);
        if (op==ExprQuant.Op.COMPREHENSION) return ans.comprehension(dd); // "guard" will always be Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SUM) return ians.sum(dd);                    // "guard" will always be Formula.TRUE, since each var has to be unary
        if (op==ExprQuant.Op.SOME) return guard.and(ans).forSome(dd); else return guard.implies(ans).forAll(dd);
    }

    @Override public Object visit(ExprQuant x) throws Err {
        return visit_qt(x.op, x.vars, x.sub, false);
    }
}
