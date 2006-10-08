package edu.mit.csail.sdg.alloy4.translator;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import edu.mit.csail.sdg.alloy4util.Subprocess;
import kodkod.engine.satlab.SATSolver;

public final class ViaPipe implements SATSolver {

    /** The number of variables and clauses given to us so far. */
    private int vars=0,clauses=0;

    /** The binary's file name. */
    private final String binary;

    /** The temporary file name. */
    private final String name;

    /** The RandomAccessFile handle to the temporary file. */
    private RandomAccessFile file;

    /** This buffers the write to the file; whenever it reaches a certain size, we will call flush() to flush it. */
    private final StringBuilder sb=new StringBuilder();

    /** If nonnull, that means an error had occurred. */
    private Exception error=null;

    /** The latest subprocess. */
    private static volatile Subprocess latest=null;
    /** Terminate the latest subprocess. */
    public static void forceTerminate() {
        Subprocess pro=latest;
        if (pro!=null) pro.terminate();
    }

    /** Constructs a new instance of SATSolver using "name" as the temporary file for writing the CNF clauses. */
    public ViaPipe(String binaryFilename, String temporaryFilename) {
        binary=binaryFilename;
        name=temporaryFilename;
        (new File(temporaryFilename)).deleteOnExit();
        try {
            file=new RandomAccessFile(name, "rw");
            file.setLength(0);
            sb.append("p cnf 0 0                                      \n");
            // This reserves enough spaces so that we can come back and
            // write out the actual number of variables and clauses.
        }
        catch(IOException ex) { error=ex; cleanup(); }
    }

    public int numberOfVariables() { return vars; }

    public int numberOfClauses() { return clauses; }

    public void addVariables(int numVars) {
        if (numVars < 0)
            throw new IllegalArgumentException("numVars < 0: " + numVars);
        else if (numVars > 0)
            vars += numVars;
    }

    public void addClause(int[] lits) {
        clauses++;
        if (error!=null) return; // If an error has already occurred, there's no point in writing any further
        for(int i=0; i<lits.length; i++) { sb.append(lits[i]); sb.append(' '); if (sb.length()>8192) flush(); }
        sb.append("0\n");
    }

    private void flush() {
        if (error!=null || sb.length()==0) return;
        try {file.writeBytes(sb.toString());} catch(IOException ex) {error=ex; cleanup();}
        sb.setLength(0);
    }

    private String solution="";

    public boolean solve() {
        flush();
        if (error==null) try {
            file.seek(0);
            file.writeBytes("p cnf "+vars+" "+clauses);
            file.close();
            file=null;
        } catch(IOException ex) {
            error=ex;
            cleanup();
            throw new RuntimeException("Error occurred in writing to file \""+name+"\": "+ex.getMessage());
        }
        for(String attempt: new String[]{"", "exe", "6", "4"}) {
            Subprocess sp=new Subprocess(new String[]{binary+attempt,name});
            // We don't care about race condition here, since the only reason "latest" is used
            // is in the GUI, where only 1 solving can occur at a time.
            latest=sp;
            int r=sp.waitFor();
            latest=null;
            if (r==0) {
                solution=sp.getOutput();
                cleanup();
                if (solution.startsWith("SAT:")) return true;
                if (solution.startsWith("UNSAT:")) return false;
            }
        }
        cleanup();
        throw new RuntimeException("An error has occurred in calling the SAT solver...");
    }

    public boolean valueOf(int variable) { // variable = 1..N where N is the number of primary variables
        // 4 is the number of characters in "SAT:"
        if (variable+4-1 < solution.length()) return solution.charAt(variable+4-1)=='1'; else return false;
    }

    private void cleanup() {
        try { if (file!=null) file.close(); } catch(IOException ex) { error=ex; }
        file=null;
        (new File(name)).delete(); // Removes the temporary file
    }

    public void free() { cleanup(); solution=""; }
}
