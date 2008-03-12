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

import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.BOLD;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.CLICK;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.DECLARE_INSTANCE;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.DELETE_ON_EXIT;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.FLUSH;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.INDENTLONG;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.INDENTSHORT;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.LINK;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.RESTORE2;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.RESTORE3;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SAVE1;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SAVE2;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SAVE3;
import static edu.mit.csail.sdg.alloy4whole.SwingLogPanel.SETLINK;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.ConstSet;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.MailBug;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.XMLNode;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.A4SolutionReader;
import edu.mit.csail.sdg.alloy4compiler.translator.A4SolutionWriter;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;

/** This helper method is used by SimpleGUI. */

final class SimpleReporter extends A4Reporter {

    /** The PrintStream that will display the messages (or null if we want to write to System.err instead) */
    private final PrintStream out;

    /** Emit one of the action code (SAVE1, RESTORE1, FLUSH...) */
    private void log(byte code) {
        if (out!=null) { out.printf("%c",code); out.flush(); }
    }

    /** Emit the given message. */
    private void log(String msg) {
        if (out==null) System.err.print(msg); else out.print(msg);
    }

    /** Emit the given message in both long and short versions; the GUI will choose whether to display the long version or the short version. */
    private void logIndented(String full, String shortText) {
        if (out==null) System.err.println(full);
        else { out.printf("%c%s%c%s%c", FLUSH, full, INDENTLONG, shortText, INDENTSHORT); out.flush(); }
    }

    /** Emit the given message using bold font. */
    private void logBold(String msg) {
        if (out==null) System.err.print(msg);
        else { out.printf("%c%s%c", FLUSH, msg, BOLD); out.flush(); }
    }

    /** Emit the given hyperlink (with msg as the hyperlink label) (with target as the hyperlink destination) */
    private void logLink(String msg, String target) {
        if (out==null || target==null || target.length()==0) log(msg);
        else { out.printf("%c%s%c%s%c", FLUSH, target, SETLINK, msg, LINK); out.flush(); }
    }

    /** Tell the GUI that it should delete the given file when the GUI exits. */
    private void deleteOnExit(String filename) {
        if (out==null) (new File(filename)).deleteOnExit();
        else { out.printf("%c%s%c", FLUSH, filename, DELETE_ON_EXIT); out.flush(); }
    }

    /** Tell the GUI the filename of the the most recently generated XML solution. */
    private void declareInstance(String filename) {
        if (out!=null) { out.printf("%c%s%c", FLUSH, filename, DECLARE_INSTANCE); out.flush(); }
    }

    /** Tell the GUI to highlight the text corresponding to the given Pos object. */
    private void highlight(Pos e) {
        if (out!=null && e!=Pos.UNKNOWN) { out.printf("%c%s%c", FLUSH, "POS: "+e.x+" "+e.y+" "+e.x2+" "+e.y2+" "+e.filename, CLICK); out.flush(); }
    }

    //========== These fields should be set each time we execute a set of commands

    /** The main Alloy filename. */
    private String mainAlloyFileName=null;

    /** The message verbosity level (0, 1, 2, or higher) */
    private int verbosity=0;

    /** Whether we should record Kodkod input/output. */
    private boolean recordKodkod=false;

    /** The time that the last action began; we subtract it from System.currentTimeMillis() to determine the elapsed time. */
    private long lastTime=0;

    /** If we performed unsat core minimization, then this is the start of the minimization, else this is 0. */
    private long minimized = 0;

    /** The unsat core size before minimization. */
    private int minimizedBefore;

    /** The unsat core size after minimization. */
    private int minimizedAfter;

    /** The set of warnings */
    private final LinkedHashSet<ErrorWarning> warnings=new LinkedHashSet<ErrorWarning>();

    /** The filename where we can write a temporary Java file or Core file. */
    private String tempfile=null;

    //========== These fields may be altered as each successful command generates a Kodkod or Metamodel instance

    /** The A4Solution corresponding to the latest solution generated by Kodkod; this field must be synchronized. */
    private static A4Solution latestKodkod=null;

    /** The root Module corresponding to this.latestKodkod; this field must be synchronized. */
    private static Module latestModule=null;

    /** The source code corresponding to the latest solution generated by Kodkod; this field must be synchronized. */
    private static ConstMap<String,String> latestKodkodSRC = null;

    /** The XML filename corresponding to the latest solution generated by Kodkod; this field must be synchronized. */
    private static String latestKodkodXML=null;

    /** The XML filename corresponding to the latest metamodel generated by TranslateAlloyToMetamodel; this field must be synchronized. */
    private static String latestMetamodelXML=null;

    /**
     * Constructs an object which can execute Alloy commands and report progress.
     * @param out - the PrintStream that shall receive the progress messages (or null if we want to write to System.err)
     */
    private SimpleReporter(PrintStream out) { this.out=out; }

    /** Helper method to write out a full XML file. */
    private static void writeXML(A4Reporter rep, Module mod, String filename, A4Solution sol, Map<String,String> sources) throws IOException, Err {
        sol.writeXML(rep, filename, mod.getAllFunc(), sources);
        if ("yes".equals(System.getProperty("debug"))) {
            // When in debug mode, try reading the file right back so that we can catch some errors right away
            A4SolutionReader.read(new ArrayList<Sig>(), new XMLNode(new File(filename))).toString();
        }
    }

    /**
     * Perform solution enumeration.
     * @param out - the PrintStream that shall receive the progress messages (or null if we want to write to System.err)
     * @param filename - the XML file that contains the current solution
     * @return "" if we succeed and we overwrite the XML file with the new solution (or a nonempty String containing an error message)
     */
    static String performEnumeration(PrintStream out, String filename) throws Exception {
        SimpleReporter rep = new SimpleReporter(out);
        rep.logBold("Enumerating...\n");
        rep.log(FLUSH);
        A4Solution sol;
        Module mod;
        synchronized(SimpleReporter.class) {
            if (latestMetamodelXML!=null && latestMetamodelXML.equals(filename))
                return "You cannot enumerate a metamodel.\n";
            if (latestKodkodXML==null || !latestKodkodXML.equals(filename))
                return "You can only enumerate the solutions of the most-recently-solved command.";
            if (latestKodkod==null || latestModule==null || latestKodkodSRC==null)
                return "Error: the SAT solver that generated the instance has exited,\nso we cannot enumerate unless you re-solve that command.\n";
            sol=latestKodkod;
            mod=latestModule;
        }
        if (!sol.satisfiable())
            return "Error: This command is unsatisfiable,\nso there are no solutions to enumerate.";
        if (!sol.isIncremental())
            return "Error: This solution was not generated by an incremental SAT solver.\n" +
            "Currently only MiniSat and SAT4J are supported.";
        sol=sol.next();
        if (!sol.satisfiable())
            return "There are no more satisfying instances.\n\n" +
            "Note: due to symmetry breaking and other optimizations,\n" +
            "some equivalent solutions may have been omitted.";
        synchronized(SimpleReporter.class) { writeXML(null, mod, filename, sol, latestKodkodSRC); latestKodkod=sol; }
        rep.declareInstance(filename);
        return "";
    }

    /**
     * Perform one command.
     * @param out - the PrintStream that shall receive the progress messages (or null if we want to write to System.err)
     * @param bundleCache - the pre-cached list of Alloy files and their corresponding cached contents (if a needed file isn't in here, the compiler will read it from disk)
     * @param bundleIndex - the command to execute (or -1 meaning every command) (or -2 meaning to generate the metamodel)
     * @param options - the Alloy compiler options
     * @param bundleWarningNonFatal - if false, compilation warnings will be treated as fatal errors
     * @param tempdir - a temporary directory where we can create/modify/delete any file in it
     * @param verbosity - the verbosity level (0, 1, 2...)
     */
    static void performRegularCommand(
        final PrintStream out, final Map<String,String> bundleCache, final int bundleIndex, final A4Options options,
        final boolean bundleWarningNonFatal, final String tempdir, final int verbosity)
        throws Exception {
        SimpleReporter rep = new SimpleReporter(out);
        rep.verbosity = verbosity;
        rep.recordKodkod = options.recordKodkod;
        rep.mainAlloyFileName = Util.canon(options.originalFilename);
        rep.log(SAVE2);
        rep.logBold("Starting the solver...\n\n");
        final Module world = CompUtil.parseEverything_fromFile(rep, bundleCache, rep.mainAlloyFileName);
        /*
        if (1==1) {
            for(Pair<String,Expr> p: world.getAllAssertions()) {
                System.out.println("Assertion "+p.a+" ("+p.b.pos.y+","+p.b.pos.x+")..("+p.b.pos.y2+","+p.b.pos.x2+")");
            }
            for(Pair<String,Expr> p: world.getAllFacts()) {
                System.out.println("Fact "+p.a+" ("+p.b.pos.y+","+p.b.pos.x+")..("+p.b.pos.y2+","+p.b.pos.x2+")");
            }
            for(Func p: world.getAllFunc()) {
                System.out.println("Func "+p.label+" ("+p.pos.y+","+p.pos.x+")..("+p.pos.y2+","+p.pos.x2+")");
            }
            for(Pair<Command,Expr> pp: world.getAllCommandsWithFormulas()) {
                Command p=pp.a;
                System.out.print("Cmd "+p.label+" ("+p.pos.y+","+p.pos.x+")..("+p.pos.y2+","+p.pos.x2+")");
                Expr b=pp.b;
                System.out.println(" ("+b.pos.y+","+b.pos.x+")..("+b.pos.y2+","+b.pos.x2+")");
            }
            for(Sig p: world.getAllSigs()) {
                System.out.println("Sig "+p.label+" ("+p.pos.y+","+p.pos.x+")..("+p.pos.y2+","+p.pos.x2+")");
            }
            System.out.flush();
        }
        */
        final List<Sig> sigs = world.getAllReachableSigs();
        final ConstList<Pair<Command,Expr>> cmds = world.getAllCommandsWithFormulas();
        if (rep.warnings.size()>0) {
            if (rep.warnings.size()>1)
                rep.logBold("Note: There were "+rep.warnings.size()+" compilation warnings. Please scroll up to see them.\n\n");
            else
                rep.logBold("Note: There was 1 compilation warning. Please scroll up to see them.\n\n");
            if (!bundleWarningNonFatal) {
                rep.highlight(rep.warnings.iterator().next().pos);
                rep.logBold("Warnings often indicate errors in the model.\n"
                        +"Some warnings can affect the soundness of the analysis.\n"
                        +"To proceed despite the warnings, go to the Options menu.\n");
                return;
            }
        } else {
            rep.log(RESTORE2);
        }
        List<String> result = new ArrayList<String>(cmds.size());
        if (bundleIndex==-2) {
            final String outf=tempdir+File.separatorChar+"m.xml";
            rep.log(SAVE2);
            rep.logBold("Generating the metamodel...\n");
            PrintWriter of=new PrintWriter(outf, "UTF-8");
            Util.encodeXMLs(of, "\n<alloy builddate=\"", Version.buildDate(), "\">\n\n");
            A4SolutionWriter.writeMetamodel(ConstList.make(sigs), options.originalFilename, of);
            Util.encodeXMLs(of, "\n</alloy>");
            Util.close(of);
            rep.log(RESTORE2);
            rep.deleteOnExit(outf);
            rep.declareInstance(outf);
            rep.logLink("Metamodel", "XML: "+outf);
            rep.log(" successfully generated.\n\n");
            synchronized(SimpleReporter.class) { latestMetamodelXML=outf; }
        } else for(int i=0; i<cmds.size(); i++) if (bundleIndex<0 || i==bundleIndex) {
            synchronized(SimpleReporter.class) { latestModule=world; latestKodkodSRC=ConstMap.make(bundleCache); }
            final String tempXML=tempdir+File.separatorChar+i+".cnf.xml";
            final String tempCNF=tempdir+File.separatorChar+i+".cnf";
            final Pair<Command,Expr> cmd=cmds.get(i);
            rep.tempfile=tempCNF;
            rep.logBold("Executing \""+cmd.a+"\"\n");
            Expr facts = ExprConstant.TRUE;
            for(Module m:world.getAllReachableModules()) for(Pair<String,Expr> f:m.getAllFacts()) facts=facts.and(f.b);
            A4Solution ai=TranslateAlloyToKodkod.execute_commandFromBook(rep, world.getAllReachableSigs(), facts.and(cmd.b), cmd.a, options);
            if (ai==null) {
                result.add(null);
            }
            else if (!ai.satisfiable()) {
                if (ai.highLevelCore().a.size()>0) { rep.deleteOnExit(tempCNF+".core"); result.add(tempCNF+".core"); } else result.add("");
            }
            else {
                rep.deleteOnExit(tempXML);
                rep.declareInstance(tempXML);
                result.add(tempXML);
            }
        }
        (new File(tempdir)).delete(); // In case it was UNSAT, or canceled...
        if (result.size()>1) {
            rep.logBold("" + result.size() + " commands were executed. The results are:\n");
            for(int i=0; i<result.size(); i++) {
                Command r=world.getAllCommands().get(i);
                if (result.get(i)==null) { rep.log("   #"+(i+1)+": Unknown.\n"); continue; }
                if (result.get(i).endsWith(".xml")) {
                    rep.log("   #"+(i+1)+": ");
                    rep.logLink(r.check?"Counterexample found. ":"Instance found. ", "XML: "+result.get(i));
                    rep.log(r.label+(r.check?" is invalid":" is consistent"));
                    if (r.expects==0) rep.log(", contrary to expectation");
                    else if (r.expects==1) rep.log(", as expected");
                } else if (result.get(i).endsWith(".core")) {
                    rep.log("   #"+(i+1)+": ");
                    rep.logLink(r.check?"No counterexample found. ":"No instance found. ", "CORE: "+result.get(i));
                    rep.log(r.label+(r.check?" may be valid":" may be inconsistent"));
                    if (r.expects==1) rep.log(", contrary to expectation");
                    else if (r.expects==0) rep.log(", as expected");
                } else {
                    if (r.check) rep.log("   #"+(i+1)+": No counterexample found. "+r.label+" may be valid");
                    else rep.log("   #"+(i+1)+": No instance found. "+r.label+" may be inconsistent");
                    if (r.expects==1) rep.log(", contrary to expectation");
                    else if (r.expects==0) rep.log(", as expected");
                }
                rep.log(".\n");
            }
            rep.log("\n");
        }
        if (rep.warnings.size()>1) rep.logBold("Note: There were "+rep.warnings.size()+" compilation warnings. Please scroll up to see them.\n");
        if (rep.warnings.size()==1) rep.logBold("Note: There was 1 compilation warning. Please scroll up to see it.\n");
    }

    /** {@inheritDoc} */
    @Override public void warning(final ErrorWarning e) {
        if (!warnings.add(e)) return;
        Pos p=e.pos;
        logLink("Warning #"+warnings.size(), "POS: "+p.x+" "+p.y+" "+p.x2+" "+p.y2+" "+p.filename);
        log("\n");
        logIndented(e.toString().trim(), e.msg.trim());
        log("\n\n");
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void scope(final String msg) {
        if (verbosity>0) log("   "+msg);
    }

    /** {@inheritDoc} */
    @Override public void bound(final String msg) {
        if (verbosity>1) log("   "+msg);
    }

    /** {@inheritDoc} */
    @Override public void translate (String solver, int bitwidth, int maxseq, int skolemDepth, int symmetry) {
        log("   Solver="+solver+" Bitwidth="+bitwidth+" MaxSeq="+maxseq
                +(skolemDepth==0?"":" SkolemDepth="+skolemDepth)
                +" Symmetry="+(symmetry>0 ? (""+symmetry) : "OFF")+'\n'
        );
        log(SAVE3);
        lastTime = System.currentTimeMillis();
        logBold("   Generating CNF...\n");
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void solve(final int primaryVars, final int totalVars, final int clauses) {
        minimized=0;
        log(RESTORE3);
        log("   "+totalVars+" vars. "+primaryVars+" primary vars. "+clauses+" clauses. "
                +(System.currentTimeMillis()-lastTime)+"ms.\n");
        log(SAVE3);
        lastTime = System.currentTimeMillis();
        logBold("   Solving...\n");
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void debug(final String msg) {
        if (verbosity>2) { log("   "+(msg.trim())+"\n"); log(SAVE1); log(SAVE2); log(SAVE3); log(FLUSH); }
    }

    /** {@inheritDoc} */
    @Override public void write(Object obj) {
        String label;
        if (obj instanceof Sig) { label="   Writing sig "+((Sig)obj).label; }
        else if (obj instanceof Field) { label="   Writing field "+((Field)obj).sig.label+"."+((Field)obj).label; }
        else { label="   Writing skolem "+obj; }
        log(RESTORE3);
        log(label);
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void resultSAT(Object command, long solvingTime, Object solution) {
        if (!(solution instanceof A4Solution)) return;
        if (!(command instanceof Command)) return;
        A4Solution sol = (A4Solution)solution;
        Command cmd = (Command)command;
        log(RESTORE3);
        log("   Writing the XML file...");
        log(FLUSH);
        String formula = recordKodkod ? sol.debugExtractKInput() : "";
        String filename = tempfile+".xml";
        synchronized(SimpleReporter.class) {
            try {
                writeXML(this, latestModule, filename, sol, latestKodkodSRC);
            } catch(Throwable ex) {
                logBold(Util.indent(ex.toString().trim() + "\nStackTrace:\n" + (MailBug.dump(ex).trim()), "   "));
                log("\n");
                log(FLUSH);
                return;
            }
            latestKodkod=sol;
            latestKodkodXML=filename;
        }
        log(RESTORE3);
        String formulafilename = "";
        if (formula.length()>0 && tempfile!=null) {
            formulafilename = tempfile+".java";
            try { Util.writeAll(formulafilename,formula); formulafilename="CNF: "+formulafilename; } catch(Throwable ex) { formulafilename=""; }
        }
        logLink(cmd.check ? "   Counterexample" : "   Instance", (filename==null||filename.length()==0)?"":("XML: "+filename));
        log(" found. ");
        if (cmd.check) {
            logLink("Assertion", formulafilename); log(" is invalid");
        } else {
            logLink("Predicate", formulafilename); log(" is consistent");
        }
        if (cmd.expects==0) log(", contrary to expectation"); else if (cmd.expects==1) log(", as expected");
        log(". "+(System.currentTimeMillis()-lastTime)+"ms.\n\n");
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void minimizing(Object command, int before) {
        if (!(command instanceof Command)) return;
        Command cmd = (Command)command;
        log(RESTORE3);
        log(cmd.check ? "   No counterexample found." : "   No instance found.");
        if (cmd.check) log(" Assertion may be valid"); else log(" Predicate may be inconsistent");
        if (cmd.expects==1) log(", contrary to expectation"); else if (cmd.expects==0) log(", as expected");
        minimized=System.currentTimeMillis();
        log(". "+(minimized-lastTime)+"ms.\n");
        logBold("   Minimizing the unsat core of "+before+" entries...\n");
        log(FLUSH);
    }

    /** {@inheritDoc} */
    @Override public void minimized(Object command, int before, int after) {
        minimizedBefore=before;
        minimizedAfter=after;
    }

    /** {@inheritDoc} */
    @Override public void resultUNSAT(Object command, long solvingTime, Object solution) {
        if (!(solution instanceof A4Solution)) return;
        if (!(command instanceof Command)) return;
        A4Solution sol = (A4Solution)solution;
        Command cmd = (Command)command;
        log(RESTORE3);
        String originalFormula = recordKodkod ? sol.debugExtractKInput() : "";
        String corefilename="", formulafilename="";
        if (originalFormula.length()>0 && tempfile!=null) {
            formulafilename=tempfile+".java";
            try { Util.writeAll(formulafilename, originalFormula); formulafilename="CNF: "+formulafilename; } catch(Throwable ex) { formulafilename=""; }
        }
        Pair<ConstSet<Pos>,ConstSet<Pos>> core = sol.highLevelCore();
        if ((core.a.size()>0 || core.b.size()>0) && tempfile!=null) {
            corefilename=tempfile+".core";
            OutputStream fs=null;
            ObjectOutputStream os=null;
            try {
                fs=new FileOutputStream(corefilename);
                os=new ObjectOutputStream(fs);
                os.writeObject(core);
                os.writeObject(sol.lowLevelCore());
                corefilename="CORE: "+corefilename;
            } catch(Throwable ex) {
                corefilename="";
            } finally {
                Util.close(os);
                Util.close(fs);
            }
        }
        log(cmd.check ? "   No counterexample found. " : "   No instance found. ");
        logLink(cmd.check ? "Assertion" : "Predicate", formulafilename);
        log(cmd.check? " may be valid" : " may be inconsistent");
        if (cmd.expects==1) log(", contrary to expectation"); else if (cmd.expects==0) log(", as expected");
        if (minimized==0) {
            log(". "+(System.currentTimeMillis()-lastTime)+"ms.\n\n");
        } else {
            log(". "+(minimized-lastTime)+"ms.\n");
            if (corefilename.length()==0) {
                log("   No unsat core is available in this case");
            } else {
                logLink("   Core", corefilename);
                if (minimizedBefore<=minimizedAfter)
                    log(" contains "+minimizedAfter+" top-level formulas");
                else
                    log(" reduced from "+minimizedBefore+" to "+minimizedAfter+" top-level formulas");
            }
            log(". "+(System.currentTimeMillis()-minimized)+"ms.\n\n");
        }
    }

    /** {@inheritDoc} */
    @Override public void resultCNF(final String filename) {
        log(RESTORE3);
        log("   File written to "+filename+"\n\n");
    }
}
