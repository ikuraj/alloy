package edu.mit.csail.sdg.alloy4;

/**
 * This class receives diagnostic, progress, and warning messages from Alloy4.
 * (This default implementation ignores all calls; you should subclass it to do the appropriate screen output)
 */

public class A4Reporter {

    /** This is a read-only pre-constructed instance that simply ignores all calls. */
    public static final A4Reporter NOP = new A4Reporter();

    /** Constructs a default A4Reporter object that does nothing. */
    public A4Reporter() {}

    /** This method is called by the parser to report parser events. */
    public void parse(String msg) {}

    /** This method is called by the typechecker to report the type for each field/function/predicate/assertion, etc. */
    public void typecheck(String msg) {}

    /** This method is called by the typechecker to report a nonfatal type error. */
    public void warning(ErrorWarning msg) {}

    /** This method is called by the ScopeComputer to report the scope chosen for each sig. */
    public void scope(String msg) {}

    /** This method is called by the BoundsComputer to report the bounds chosen for each sig and each field. */
    public void bound(String msg) {}

    /** This method is called by the translator just before it begins generating CNF. */
    public void translate(String solver, int bitwidth, int maxseq, int skolemDepth, int symmetry) {}

    /** This method is called by the translator just after it generated the CNF. */
    public void solve(int primaryVars, int totalVars, int clauses) {}

    /** If solver==FILE, this method is called by the translator after it constructed the CNF file. */
    public void resultCNF(String filename) {}

    /**
     * If solver!=FILE, this method is called by the translator if the formula is satisfiable.
     * Furthermore, if filename!=null && filename.length()>0, then filename contains the solution (in XML format)
     */
    public void resultSAT(Object command, long solvingTime, String filename) {}

    /**
     * If solver!=FILE, this method is called by the translator if the formula is unsatisfiable.
     * Furthermore, if filename!=null && filename.length()>0, then filename contains the original Kodkod formula (in JAVA format)
     */
    public void resultUNSAT(Object command, long solvingTime, String filename) {}
}
