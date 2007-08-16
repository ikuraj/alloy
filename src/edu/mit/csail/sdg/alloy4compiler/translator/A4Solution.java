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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import nanoxml_2_2_3.XMLElement;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.engine.Evaluator;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import static kodkod.engine.Solution.Outcome.UNSATISFIABLE;
import static kodkod.engine.Solution.Outcome.TRIVIALLY_UNSATISFIABLE;
import kodkod.engine.Solver;
import kodkod.engine.config.AbstractReporter;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.MinTopStrategy;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IndexedEntry;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.UniqueNameGenerator;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4compiler.parser.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;
import static edu.mit.csail.sdg.alloy4.Util.tail;

/**
 * This represents a solution (Can be either satisfiable or unsatisfiable); this class is immutable.
 */

public final class A4Solution {

    private static final class Parent {
        /** It's the root module that these solutions belong to. */
        private final Module world;
        /** If not null, it's the original BoundsComputer that maps Alloy sigs/fields to Kodkod expressions. NOTE: bc==null iff bcc!=null. */
        private final BoundsComputer bc;
        /** If not null, it's a map from Sig/Field/String to Kodkod expressions. NOTE: bc==null iff bcc!=null. */
        private final Map<Object,Expression> bcc;
        /** If not "", it's the original filename where the model came from (it may be included in the XML file as a comment). */
        private final String originalFileName;
        /** If not null, it's an immutable image of all source files that were used in the construction of this A4Solution. */
        private final Map<String,String> originalSources;
        /** If not "", it's the original command where the solution came from (it may be included in the XML file as a comment). */
        private final String originalCommand;
        /** If not null, it's the original Kodkod input. */
        private final Formula kInputFormula;
        /** If not null, it's the original Kodkod bounds. */
        private final Bounds kBounds;
        /** If not null, you can ask it to get another solution. */
        private final Iterator<Solution> next;
        /** Whether the solver supports solution enumeration. */
        private final boolean canEnumerate;
        /** The bitwidth. */
        private final int bitwidth;
        /** Constructor is private. */
        private Parent(Module world, BoundsComputer bc, Map<Object,Expression> bcc, String originalFileName,
        Map<String,String> originalSources, String originalCommand,
        Iterator<Solution> moreSolutions, Formula originalFormula, Bounds originalBounds,
        boolean canEnumerate, int bitwidth) {
            this.world=world;
            this.bc=bc;
            this.bcc=bcc;
            this.originalFileName=originalFileName;
            this.originalSources=(originalSources==null ? null : new LinkedHashMap<String,String>(originalSources));
            this.originalCommand=originalCommand;
            this.next=moreSolutions;
            this.kInputFormula=originalFormula;
            this.kBounds=originalBounds;
            this.canEnumerate=canEnumerate;
            this.bitwidth=bitwidth;
        }
    }

    /** The A4Solutions parent that spawned this A4Solution object. */
    private final Parent parent;

    /** The Kodkod input (or "" if the user did not ask to capture the Kodkod input). */
    public final String kInput;

    /** The Kodkod output if satisfiable (or null if unsatisfiable). */
    private final Instance kInstance;

    /** The map from kodkod unsat core back to Alloy AST (or null if the map is unavailable) */
    private final Map<Formula,List<Object>> core;

    /** The kodkod unsat core if unsatisfiable (or null if satisfiable, or the unsat core is unavailable). */
    private final IdentitySet<Formula> proof;

    /** The time it took to solve for this solution. */
    private final long solvingTime;

    /** The kodkod evaluator constructed specifically for this kodkod solution (or null if the kodkod solution was unsatisfiable) */
    private final Evaluator kEval;

    /** Old AtomName to New AtomName map. */
    private final ConstMap<Object,String> map;

    /** Old AtomName to its Most Specific Sig. */
    private final ConstMap<Object,PrimSig> map2sig;

    /** Modifiable Skolem->Type map. */
    private final Map<Relation,Type> skolem2type;

    /** If nonnull, it caches the result of calling "next()" */
    private A4Solution next=null;

    /** Returns true if this solution was generated by an incremental SAT solver. */
    public synchronized boolean isIncremental() { return parent.canEnumerate; }

    /**
     * If this solution is UNSAT, return itself; else return the next solution (which could be SAT or UNSAT).
     * @throws ErrorAPI if the solver was not an incremental solver
     */
    public synchronized A4Solution next() throws Err {
        if (kEval==null) return this; // If UNSAT, then return myself.
        if (next!=null) return next; // If result is already cached, then return it.
        if (!parent.canEnumerate)
            throw new ErrorAPI("This solution was not generated by an incremental SAT solver.\n"
            +"Solution enumeration is currently only implemented for MiniSat and SAT4J.");
        Solution sol=parent.next.next();
        next=new A4Solution(parent, sol.stats().solvingTime(), sol.instance(), skolem2type, core, null);
        return next;
    }

    /** The reporter that does nothing. */
    private static AbstractReporter blankReporter = new AbstractReporter(){};

    /** Construct the first solution from a formula; after this, user would call A4Solution.next() to get the next solution. */
    static A4Solution make (TranslateAlloyToKodkod tr, Module world, A4Options opt, Map<Relation,Type> skolem2type, Solver solver, Formula formula,
    Map<String,String> originalSources, Command originalCommand, boolean tryBookExamples) throws Err, SaveToFileException {
        final A4Reporter rep=A4Reporter.getReporter();
        final BoundsComputer bc=tr.bc;
        final Bounds bounds=bc.getBounds();
        final String originalFileName=opt.originalFilename;
        Iterator<Solution> sols;
        Solution sol = null;
        IdentitySet<Formula> proof = null;
        // try { Util.writeAll("/tmp/debug", TranslateKodkodToJava.convert(formula, solver.options().bitwidth(), bounds.universe().iterator(), bounds, null)); } catch(Throwable ex) { }
        if (tryBookExamples && solver.options().solver()!=SATFactory.MiniSatProver) {
            rep.debug("A4Solution.make() #1...");
            try {
                sol = BookExamples.trial(world, bc, bounds, formula, solver, originalCommand.toString(), originalFileName);
            } catch(Throwable ex) {
                sol=null;
            }
        }
        if (solver.options().solver()==SATFactory.ZChaff || !solver.options().solver().incremental()) {
            rep.debug("A4Solution.make() #2...");
            sols=null;
            if (sol==null) {
                try {
                    sol=solver.solve(formula,bounds);
                } catch(Throwable ex) {
                    if (ex instanceof UnsatisfiedLinkError) throw (UnsatisfiedLinkError)ex;
                    if (ex instanceof HigherOrderDeclException) throw (HigherOrderDeclException)ex;
                    if (ex.toString().contains("nosuchprogram") && opt.solver.equals(A4Options.SatSolver.FILE)) throw new SaveToFileException();
                    throw new ErrorFatal("Solver fatal exception: "+ex,ex);
                }
            }
            rep.debug("A4Solution.make() #3...");
        } else {
            rep.debug("A4Solution.make() #4...");
            sols=solver.solveAll(formula,bounds);
            rep.debug("A4Solution.make() #5...");
            if (sol==null) {
                rep.debug("A4Solution.make() #6...");
                try {
                    sol=sols.next();
                } catch(Throwable ex) {
                    rep.debug("A4Solution.make()ext: "+ex);
                    if (ex instanceof UnsatisfiedLinkError) throw (UnsatisfiedLinkError)ex;
                    if (ex instanceof HigherOrderDeclException) throw (HigherOrderDeclException)ex;
                    if (ex.toString().contains("nosuchprogram") && opt.solver.equals(A4Options.SatSolver.FILE)) throw new SaveToFileException();
                    throw new ErrorFatal("Solver fatal exception: "+ex,ex);
                }
                rep.debug("A4Solution.make() #7...");
                if (sol.outcome()==TRIVIALLY_UNSATISFIABLE || sol.outcome()==UNSATISFIABLE) {
                    if (solver.options().solver()==SATFactory.MiniSatProver) {
                        rep.minimizing(originalCommand);
                        rep.debug("A4Solution.make() #8a...");
                        try {
                            rep.debug("A4Solution.make() #8b...");
                            proof=new IdentitySet<Formula>();
                            Proof p=sol.proof();
                            if (sol.outcome()==UNSATISFIABLE) {
                                rep.debug("A4Solution.make() #8c...");
                                try { p.minimize(new MinTopStrategy(p.log())); }
                                catch(UnsupportedOperationException ex) {}
                            }
                            for(Formula f:p.highLevelCore()) proof.add(f);
                        } catch(Throwable th) {
                            rep.debug("A4Solution.make() exception: "+th);
                            proof=null;
                        }
                        if (proof==null) rep.debug("A4Solution.make() #9 (proof==null)");
                        else rep.debug("A4Solution.make() #9 (proof!=null)");
                    }
                }
            }
        }
        rep.debug("A4Solution.make() #10...");
        solver.options().setReporter(blankReporter); // To ensure no more output during SolutionEnumeration
        if (!opt.recordKodkod) formula=null;
        Parent p=new Parent(world,
                bc,
                null,
                originalFileName,
                originalSources,
                originalCommand.toString(),
                sols,
                formula,
                bounds,
                solver.options().solver()!=SATFactory.ZChaff && solver.options().solver().incremental(),
                solver.options().bitwidth()
            );
        rep.debug("A4Solution.make() #11...");
        return new A4Solution(p, sol.stats().solvingTime(), sol.instance(), skolem2type, tr.core, proof);
    }

    /** Private constructor to ensure TranslateAlloyToKodkod is the only one who can construct this. */
    private A4Solution(Parent parent, long solvingTime, Instance originalKodkodInstance, Map<Relation,Type> skolem2type, Map<Formula,List<Object>> core, IdentitySet<Formula> proof) throws Err {
        final A4Reporter rep=A4Reporter.getReporter();
        rep.debug("A4Solution() #1...");
        TempMap<Object,String> m1=new TempMap<Object,String>();
        TempMap<Object,PrimSig> m2=new TempMap<Object,PrimSig>();
        this.core=core;
        this.proof=proof;
        this.skolem2type=skolem2type;
        this.parent=parent;
        this.kInstance=originalKodkodInstance;
        this.solvingTime=solvingTime;
        rep.debug("A4Solution() #2...");
        if (this.kInstance!=null) {
            Options options = new Options();
            options.setBitwidth(parent.bitwidth);
            rep.debug("A4Solution() #3...");
            this.kEval=new Evaluator(kInstance.clone(), options);
            rep.debug("A4Solution() #4...");
            if (parent.bc!=null) {
                rep.debug("A4Solution() #5...");
                for(Sig s:parent.world.getAllSigsInTheWorld()) if (!s.builtin && s.isTopLevel()) rename((PrimSig)s,m1,m2);
                rep.debug("A4Solution() #6...");
                int unused=0;
                for(Object atom: parent.bc.factory().universe()) {
                    String atomstr = atom.toString();
                    if (atomstr.indexOf('[')<0) { m2.put(atom, SIGINT); continue; }
                    if (m1.containsKey(atom)) continue;
                    m1.put(atom, "unused"+unused);
                    unused++;
                }
                rep.debug("A4Solution() #7...");
            }
        } else {
            rep.debug("A4Solution() #8...");
            this.kEval=null;
        }
        if (parent.bc!=null && parent.kInputFormula!=null && parent.kBounds!=null) {
            rep.debug("A4Solution() #9...");
            this.kInput=TranslateKodkodToJava.convert(
                    parent.kInputFormula,
                    parent.bitwidth,
                    parent.bc.factory().universe().iterator(),
                    parent.kBounds,
                    m1.makeConst());
        } else {
            rep.debug("A4Solution() #10...");
            this.kInput="";
        }
        rep.debug("A4Solution() #11...");
        this.map = m1.makeConst();
        this.map2sig = m2.makeConst();
        rep.debug("A4Solution() #12...");
    }

    /** Map old atom names to new atom names. */
    private String a2s(Object atom) {
        String ans=map.get(atom);
        if (ans==null) return atom.toString(); else return ans;
    }

    /** Recursively rename all atoms to be of the form "SIGNAME[INDEX]" where SIGNAME is the most-specific-sig. */
    private void rename(PrimSig s, TempMap<Object,String> map, TempMap<Object,PrimSig> map2sig) throws Err {
        for(PrimSig c:s.children()) rename(c, map, map2sig);
        List<String> list=null;
        for(Tuple t:kEval.evaluate(parent.bc.expr(s))) {
            String a=t.atom(0).toString();
            if (map.containsKey(a)) continue; // This means one of the subsig has already claimed this atom.
            if (list==null) list=new ArrayList<String>();
            list.add(a);
        }
        if (list==null) return;
        // Compute the width of the index (eg. width("0")=1,  width("7")=1,  width("23")=2, etc)
        int width=1;
        for(int i=list.size()-1; i>=10; i=i/10) width++;
        // Now, generate the mappings
        // By prepending enough 0 to make the numbers line up, we ensure the atoms will sort lexicographically.
        StringBuilder sb=new StringBuilder();
        String signame=s.toString();
        if (signame.startsWith("this/")) signame=signame.substring(5);
        if (signame.charAt(0)=='/') signame=signame.substring(1);
        for(int i=0; i<list.size(); i++) {
            sb.delete(0, sb.length());
            sb.append(signame);
            sb.append('[');
            String x=Integer.toString(i);
            int xlen=x.length();
            while(xlen<width) {sb.append('0'); xlen++;}
            sb.append(x);
            sb.append(']');
            map2sig.put(list.get(i), s);
            map.put(list.get(i), sb.toString());
        }
    }

    /** Returns true iff the problem is satisfiable. */
    public synchronized boolean satisfiable() { return kEval!=null; }

    /** Returns the number of milliseconds it took to solve this. */
    public synchronized long solvingTime() { return solvingTime; }

    /** Returns the World that this solution is from. */
    public synchronized Module getWorld() { return parent.world; }

    public synchronized IdentitySet<Pos> core() {
        IdentitySet<Pos> ans = new IdentitySet<Pos>();
        if (proof!=null) {
            for(Formula f: proof) {
                List<Object> x = core.get(f);
                if (x==null) continue;
                for(Object y:x) {
                    if (y instanceof Pos) ans.add( (Pos)y );
                    if (y instanceof Expr) {
                        Expr expr = (Expr)y;
                        ans.add(expr.span());
                        for(Func func:expr.findAllFunctions()) ans.add(func.getBody().span());
                    }
                }
            }
        }
        return ans;
    }

    /**
     * Evaluates the given expression based on the solution, and returns an A4TupleSet, a java Integer, or a java Boolean.
     * @throws Err if the expression has syntax error, type error, or other errors, or is not fully typechecked
     * @throws ErrorAPI if the expression is not from the same world
     * @throws ErrorAPI if this solution is not a satisfiable solution
     */
    public synchronized Object eval(Expr expr) throws Err {
        if (!satisfiable()) throw new ErrorAPI("This solution is unsatisfiable, so no eval() is allowed.");
        Object result;
        if (parent.bc!=null)
            result=(new TranslateAlloyToKodkod(parent.bc, null, null)).visitThis(expr);
        else
            result=(new TranslateAlloyToKodkod(parent.bcc, parent.bitwidth, null, null)).visitThis(expr);
        if (result instanceof IntExpression) return kEval.evaluate((IntExpression)result);
        if (result instanceof Formula) return kEval.evaluate((Formula)result);
        if (result instanceof Expression) {
            TupleSet tps = (TupleSet) (kEval.evaluate((Expression)result));
            return new A4TupleSet(tps, map, map2sig);
        }
        throw new ErrorFatal("Unknown internal error encountered in the evaluator.");
    }

    /** Convenience helper method that returns a short but unique name for each sig. */
    private static String shorten(Sig sig) {
        String string=sig.toString();
        if (string.startsWith("/")) string=string.substring(1);
        if (string.startsWith("this/")) string=string.substring(5);
        return string;
    }

    /** Convenience helper method to sheild us from possible "Higher Order Quantification" exceptions. */
    private static TupleSet eval(Evaluator evaluator, Expression expr) {
        TupleSet answer;
        try {
            answer=evaluator.evaluate(expr);
        } catch(Throwable ex) {
            answer=null;
        }
        return answer;
    }

    /** Convenience helper method that writes out sig "s" and all its fields. */
    private void process_each_sig(PrintWriter out, Sig s, IdentitySet<Expression> rels, UniqueNameGenerator un) throws Err {
        // "univ" and "none" do not need to be generated; SIGINT and SEQIDX will be generated explicitly later.
        if (s.builtin) return;
        Expression r=parent.bc.expr(s);
        rels.add(r);
        TupleSet ts = eval(kEval,r);
        if (ts!=null) {
            if (s instanceof SubsetSig) {
                writeTS(new A4TupleSet(ts, map, map2sig), out, shorten(s), s.type, un);
            } else {
                Util.encodeXMLs(out, "\n<sig name=\"", un.seen(shorten(s)), "\"");
                if (!s.isTopLevel()) Util.encodeXMLs(out, " extends=\"", shorten(((PrimSig)s).parent), "\"");
                if (s.isOne!=null) out.printf(" isOne=\"true\"");
                if (s.isAbstract!=null) out.printf(" isAbstract=\"true\"");
                if (s.builtin) out.printf(" isBuiltin=\"true\"");
                if (s.isOrdered!=null) out.printf(" isOrdered=\"true\"");
                out.printf(">\n");
                for(Tuple t:ts) Util.encodeXMLs(out, "  <atom name=\"", a2s(t.atom(0)), "\"/>\n");
                out.printf("</sig>\n");
            }
        }
        for(Field f:s.getFields()) {
            r=parent.bc.expr(f);
            rels.add(r);
            ts=eval(kEval,r);
            if (ts!=null) writeTS(new A4TupleSet(ts, map, map2sig), out, f.label, f.type, un);
        }
    }

    /** Dumps the Kodkod solution into String. */
    @Override public String toString() {
        if (kInstance==null)
            return "---OUTCOME---\nUnsatisfiable.\n";
        return dump(kInstance);
    }

    /** Dumps a Kodkod solution into String. */
    private String dump(Instance sol) {
        StringBuilder sb=new StringBuilder();
        sb.append("---INSTANCE---\n");
        Iterator<IndexedEntry<TupleSet>> it=sol.intTuples().iterator();
        if (it.hasNext()) {
            sb.append("integers={");
            boolean firstTuple=true;
            while(it.hasNext()) {
                if (firstTuple) firstTuple=false; else sb.append(", ");
                IndexedEntry<TupleSet> e=it.next();
                sb.append(e.index());
                sb.append('=');
                sb.append(a2s(e.value().iterator().next().atom(0)));
            }
            sb.append("}\n");
        }
        for(Map.Entry<Relation,TupleSet> e:sol.relationTuples().entrySet()) {
            if (e.getKey().name().contains("[discard]")) continue;
            sb.append(e.getKey().name());
            sb.append("={");
            boolean firstTuple=true;
            for(Tuple t:e.getValue()) {
                if (firstTuple) firstTuple=false; else sb.append(", ");
                for(int i=0; i<t.arity(); i++) {
                    if (i>0) sb.append("->");
                    sb.append(a2s(t.atom(i)));
                }
            }
            sb.append("}\n");
        }
        sb.append("\n");
        return sb.toString();
    }

    private void writeTS(A4TupleSet r, PrintWriter out, String name, Type type, UniqueNameGenerator un) throws Err {
        int n=r.arity();
        List<PrimSig> list = new ArrayList<PrimSig>(n);
        if (type==null) {
            type=Type.EMPTY;
            again:
            for(A4Tuple t:r) {
                list.clear();
                for(int i=0; i<n; i++) {
                    PrimSig s=t.sig(i);
                    if (s==null) continue again;
                    list.add(s);
                }
                type=type.merge(list);
            }
        }
        if (type.size()==0) return;
        for(List<PrimSig> sigs:type.fold()) {
            if (n>1) {
                Util.encodeXMLs(out, "\n<field name=\"", un.seen(name), "\">\n");
                out.print("    <type>");
                for(int i=0; i<n; i++) Util.encodeXMLs(out, " <sig name=\"", un.seen(shorten(sigs.get(i))), "\"/>");
                out.print(" </type>\n");
            } else {
                Util.encodeXMLs(out, "\n<set name=\"", un.seen(name), "\" type=\"", shorten(sigs.get(0)), "\">\n");
            }
            again2:
            for(A4Tuple t:r) {
                for(int i=0; i<n; i++) {
                    PrimSig s=t.sig(i);
                    if (s==null) continue again2;
                    if (!s.intersects(sigs.get(i))) continue again2;
                }
                if (n>1) {
                    out.print("    <tuple>");
                    for(int i=0; i<n; i++) Util.encodeXMLs(out, " <atom name=\"", t.atom(i), "\"/>");
                    out.print(" </tuple>\n");
                } else {
                    Util.encodeXMLs(out, "  <atom name=\"", t.atom(0), "\"/>\n");
                }
            }
            out.print(n>1 ? "</field>\n" : "</set>\n");
        }
    }

    /**
     * If this solution is a satisfiable solution, write it to an XML file.
     *
     * <p>  We first write every Sig and every Field into the XML file;
     * <br> after that, we look through the additional Relation(s) generated by Kodkod.
     * <br> For each additional relation X:
     * <br> (1) If X hasn't already been written out to the XML file (as a Sig or as a Field)
     * <br> (2) and if X's Kodkod name does not contain "[discard]"
     * <br> Then we write X into the XML file as a Skolem value.
     *
     * @param destfilename - the XML filename (It will be overwritten if it exists)
     * @param evaluateAllMacros - if true, we will also write the value of every parameter-less function into the XML file
     *
     * @throws ErrorAPI if this solution is not a satisfiable solution
     */
    public synchronized void writeXML(String destfilename, boolean evaluateAllMacros) throws Err {
        if (kInstance==null)
            throw new ErrorAPI("This solution is unsatisfiable, so there is nothing to write to an XML file.");
        if (parent.bc==null)
            throw new ErrorAPI("This solution was reconstructed from readXML, so we cannot call writeXML on it.");
        PrintWriter out;
        try {
            out=new PrintWriter(destfilename,"UTF-8");
        } catch(IOException ex) {
            throw new ErrorAPI("writeXML failed: "+ex);
        }
        final UniqueNameGenerator un=new UniqueNameGenerator();
        final Instance inst=kInstance;
        final IdentitySet<Expression> rels=parent.bc.getDiscards();
        // Write out all user-defined Sig(s) and their Field(s)
        Util.encodeXMLs(out, "\n<alloy builddate=\"",
            Version.buildDate(), "\">\n\n<instance filename=\"",
            parent.originalFileName, "\" bitwidth=\"",
            Integer.toString(parent.bitwidth), "\" command=\"", parent.originalCommand,"\">\n");
        for(Sig s:parent.world.getAllSigsInTheWorld()) process_each_sig(out, s, rels, un);
        // Write out SIGINT
        out.print("\n<sig name=\"Int\" isBuiltin=\"true\">\n");
        un.seen("Int");
        for(Tuple t:kEval.evaluate(Expression.INTS)) Util.encodeXMLs(out, "  <atom name=\"", a2s(t.atom(0)), "\"/>\n");
        out.print("</sig>\n");
        // Write out SEQIDX
        out.print("\n<sig name=\"seq/Int\" extends=\"Int\" isBuiltin=\"true\">\n");
        un.seen("seq/Int");
        for(Tuple t:kEval.evaluate(BoundsComputer.SEQ_SEQIDX)) Util.encodeXMLs(out, "  <atom name=\"", a2s(t.atom(0)), "\"/>\n");
        out.print("</sig>\n");
        // Write out any Skolem relations that were generated by Kodkod
        for(final Relation r:inst.relations()) if (!rels.contains(r)) if (!r.name().contains("[discard]")) {
            Type t=skolem2type.get(r);
            if (t==null) continue; // That means we don't know its type
            while (t.arity() < r.arity()) t=UNIV.type.product(t);
            if (t.arity() > r.arity()) continue; // That means something terrible has happened, so let's skip it
            String rn=r.name();
            while(rn.length()>0 && rn.charAt(0)=='$') rn=rn.substring(1);
            writeTS(new A4TupleSet(inst.tuples(r),map,map2sig), out, un.make("$"+rn), t, un);
        }
        // Write out all parameter-less Function in the main module
        for(final Func pf:parent.world.getAllFunc()) if (!pf.isPred && pf.params.size()==0) {
            A4TupleSet ts;
            try {
                final Object obj=eval(pf.getBody());
                if (!(obj instanceof A4TupleSet)) continue;
                ts=(A4TupleSet)obj;
            } catch(Throwable ex) { continue; } // This is not fatal
            String rname=tail(pf.label);
            while(rname.length()>0 && rname.charAt(0)=='$') rname=rname.substring(1);
            writeTS(ts, out, un.make("$"+rname), pf.returnDecl.type, un);
        }
        // Done!
        out.print("\n</instance>\n");
        if (kInput.length()>0) {
            String kOutput=dump(kInstance);
            Util.encodeXMLs(out, "\n<koutput value=\"", kOutput, "\"/>\n\n<kinput value=\"", kInput, "\"/>\n");
        }
        if (parent.originalSources!=null) {
            for(Map.Entry<String,String> e: parent.originalSources.entrySet()) {
                Util.encodeXMLs(out, "\n<source filename=\"", e.getKey(), "\" content=\"", e.getValue(), "\"/>\n");
            }
        }
        out.print("\n</alloy>\n");
        if (!Util.close(out)) throw new ErrorAPI("writeXML failed!");
    }

    //============================================================================================================================//

    /** Use the XML library to parse the file into an XMLElement object. */
    private static XMLElement readElement(File file) {
        FileInputStream fis=null;
        InputStreamReader reader=null;
        try {
            fis = new FileInputStream(file);
            reader = new InputStreamReader(fis,"UTF-8");
            XMLElement xml = new XMLElement(new Hashtable(),true,false);
            xml.parseFromReader(reader);
            return xml;
        } catch(IOException ex) {
            throw new RuntimeException("I/O error: "+ex);
        } finally {
            Util.close(reader);
            Util.close(fis);
        }
    }

    /**
     * Parse the file into an AlloyInstance if possible.
     * @throws ErrorFatal - if an error occurred in reading of the XML file.
     * @throws ErrorSyntax - if there is a syntax error in the XML file.
     */
    public static A4Solution readXML(final String file, final String alloyHome) throws Err {
        final XMLElement xml=readElement(new File(file));
        if (!xml.is("alloy")) throw new ErrorSyntax("The XML file's root node must be <alloy>.");
        A4Solution instance=null;
        String kinput="", koutput="";
        TempMap<String,String> fc = new TempMap<String,String>();
        for(XMLElement sub: xml.getChildren()) {
            if (sub.is("kinput")) kinput = sub.getAttribute("value");
            else if (sub.is("koutput")) koutput = sub.getAttribute("value");
            else if (sub.is("source")) {
                String name = sub.getAttribute("filename");
                String content = sub.getAttribute("content");
                fc.put(name,content);
            }
        }
        if (fc.size()==0) throw new RuntimeException("The original source files were not embedded in the saved instance file.");
        ConstMap<String,String> cfc=fc.makeConst();
        for(XMLElement sub: xml.getChildren("instance")) { instance=parseInstance(alloyHome, cfc, sub, kinput, koutput); break; }
        if (instance==null) throw new ErrorSyntax("The XML file does not have an <instance> element.");
        return instance;
    }

    /**
     * Parse the XML element into an AlloyInstance if possible
     * @param x - the XML element labeled "instance"
     * @param kinput - the kodkod input we want to include with the AlloyInstance object
     * @param koutput - the kodkod output we want to include with the AlloyInstance object
     */
    private static A4Solution parseInstance(final String alloyHome, final ConstMap<String,String> fc, XMLElement x, String kinput, String koutput) throws Err {
        final String filename = x.getAttribute("filename");
        final String command = x.getAttribute("command");
        Module world;
        try {
            world = CompUtil.parseEverything_fromFile(fc, alloyHome, filename);
        } catch(Throwable ex) {
            throw new RuntimeException("The original source files failed to be reconstructed.");
        }
        LinkedHashSet<String> atoms=new LinkedHashSet<String>();
        LinkedHashMap<String,List<String>> sig2atoms=new LinkedHashMap<String,List<String>>(); // The list may contain duplicates
        LinkedHashMap<String,List<String>> set2atoms=new LinkedHashMap<String,List<String>>(); // The list may contain duplicates
        for(XMLElement sub:x.getChildren()) {
            boolean isSig=sub.is("sig");
            if (isSig || sub.is("set")) {
                String name=sub.getAttribute("name");
                if (name.indexOf('/')<0 && !name.equals("univ") && !name.equals("Int")) name="this/"+name;
                List<String> array = isSig ? sig2atoms.get(name) : set2atoms.get(name);
                if (array==null) array = new ArrayList<String>();
                for(XMLElement atom:sub.getChildren("atom")) { array.add(atom.getAttribute("name")); }
                atoms.addAll(array);
                if (isSig) sig2atoms.put(name,array); else set2atoms.put(name,array);
            }
        }
        Universe u=new Universe(atoms);
        TupleFactory tf=u.factory();
        Instance i=new Instance(u);
        LinkedHashMap<Object,Expression> obj2expr = new LinkedHashMap<Object,Expression>();
        obj2expr.put(UNIV, Relation.UNIV);
        obj2expr.put(SIGINT, Relation.INTS);
        obj2expr.put(NONE, Relation.NONE);
        for(Sig s:world.getAllSigsInTheWorld()) if (s!=NONE && s!=UNIV) {
            List<String> atms = (s instanceof PrimSig) ? sig2atoms.remove(s.label) : set2atoms.remove(s.label);
            TupleSet ts=tf.noneOf(1);
            if (atms!=null) for(String a:atms) {
                Tuple ta=tf.tuple(a);
                if (s==SIGINT) i.add(Integer.parseInt(a), tf.range(ta,ta));
                ts.add(ta);
            }
            if (s!=SIGINT) {
                Relation r = (s==SEQIDX) ? BoundsComputer.SEQ_SEQIDX : Relation.unary(s.label);
                i.add(r,ts);
                obj2expr.put(s,r);
            }
        }
        LinkedHashMap<String,TupleSet> field2expr = new LinkedHashMap<String,TupleSet>();
        for(XMLElement sub:x.getChildren("field")) {
            int arity=0;
            String fn=null;
            for(XMLElement t:sub.getChildren()) if (t.is("type")) {
                for(XMLElement s:t.getChildren()) if (s.is("sig")) {
                    arity++;
                    if (arity==1) fn=s.getAttribute("name")+"["+sub.getAttribute("name")+"]";
                }
            }
            if (arity<1 || fn==null) continue;
            TupleSet ts=field2expr.get(fn);
            if (ts==null) ts=tf.noneOf(arity);
            if (ts.arity()!=arity) continue; // If we encounter an illegal value, we skip it.
            for(XMLElement t:sub.getChildren()) if (t.is("tuple")) {
                Tuple tuple=null;
                for(XMLElement a:t.getChildren()) if (a.is("atom")) {
                    Tuple temp=tf.tuple(a.getAttribute("name"));
                    if (tuple==null) tuple=temp; else tuple=tuple.product(temp);
                }
                if (tuple!=null && tuple.arity()==arity) ts.add(tuple);
            }
            field2expr.put(fn, ts);
        }
        for(Sig s:world.getAllSigsInTheWorld()) if (!s.builtin) {
            for(Field f:s.getFields()) {
                TupleSet ts=tf.noneOf(f.type.arity());
                getField(s, f.label, field2expr, ts);
                Relation fr=Relation.nary(s.label+" <: "+f.label, ts.arity());
                obj2expr.put(f, fr);
                i.add(fr, ts);
            }
        }
        // Bound SIGINT_{NEXT,MAX,ZERO,MIN}
        final int bitwidth=Integer.parseInt(x.getAttribute("bitwidth"));
        if (bitwidth<1 || bitwidth>30) throw new RuntimeException("Bitwidth of "+bitwidth+" is not allowed.");
        int min=0-(1<<(bitwidth-1));
        int max=(1<<(bitwidth-1))-1;
        TupleSet next=tf.noneOf(2);
        for(int h=min; h<max; h++) next.add(tf.tuple(""+h).product(tf.tuple(""+(h+1))));
        i.add(BoundsComputer.SIGINT_NEXT, next);
        i.add(BoundsComputer.SIGINT_MAX, tf.range(tf.tuple(""+max), tf.tuple(""+max)));
        i.add(BoundsComputer.SIGINT_ZERO, tf.range(tf.tuple("0"), tf.tuple("0")));
        i.add(BoundsComputer.SIGINT_MIN, tf.range(tf.tuple(""+min), tf.tuple(""+min)));
        // Add the skolem sets
        for(Map.Entry<String,List<String>> e: set2atoms.entrySet()) {
            TupleSet ts=tf.noneOf(1);
            for(String a: e.getValue()) ts.add(tf.tuple(a));
            String n=e.getKey();
            if (n.startsWith("this/")) n=n.substring(5);
            if (n.length()==0 || n.charAt(0)!='$') n="$"+n;
            Relation r=Relation.unary(n);
            obj2expr.put(n, r);
            i.add(r, ts);
        }
        // Add the skolem relations
        for(Map.Entry<String,TupleSet> e: field2expr.entrySet()) {
            String n=e.getKey();
            if (n.startsWith("this/")) n=n.substring(5);
            if (n.length()==0 || n.charAt(0)!='$') n="$"+n;
            Relation r=Relation.nary(n, e.getValue().arity());
            obj2expr.put(n, r);
            i.add(r, e.getValue());
        }
        // Done
        Parent p=new Parent(world, null, obj2expr, filename, fc, command, null, null, null, false, bitwidth);
        return new A4Solution(p, 0, i, ConstMap.make((Map<Relation,Type>)null), null, null);
    }

    private static void getField(Sig s, String fieldName, Map<String,TupleSet> cache, TupleSet ans) {
        if (s instanceof PrimSig) {
            String sn=s.label;
            if (sn.startsWith("this/")) sn=sn.substring(5);
            TupleSet tmp=cache.remove(sn+"["+fieldName+"]");
            if (tmp!=null && tmp.arity()==ans.arity()) ans.addAll(tmp);
            return;
        }
        for(Sig c: ((SubsetSig)s).parents) getField(c, fieldName, cache, ans);
    }
}
