package edu.mit.csail.sdg.alloy4.translator;

import java.io.IOException;
import java.io.RandomAccessFile;

import edu.mit.csail.sdg.alloy4util.Subprocess;
import kodkod.engine.satlab.SATSolver;

public final class ViaBerkMin implements SATSolver {

    private RandomAccessFile file;
    private final String name;
    private int vars=0;
    private int clauses=0;
    private final StringBuilder sb=new StringBuilder();
    private Exception error=null;

    public ViaBerkMin(String name) {
        this.name=name;
        try {
            file=new RandomAccessFile(name, "rw");
            file.setLength(0);
            sb.append("p cnf 0 0                                      \n");
            // This reserves enough spaces so that we can come back and
            // write out the actual number of variables and clauses.
        }
        catch(IOException ex) {
            error=ex;
        }
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
        if (error!=null) return;
        for(int i=0; i<lits.length; i++) { sb.append(lits[i]); sb.append(' '); if (sb.length()>8192) flush(); }
        sb.append("0\n");
        clauses++;
    }

    private void flush() {
        try {file.writeBytes(sb.toString());} catch(IOException ex) {error=ex;}
        sb.setLength(0);
    }

    public boolean solve() {
        flush();
        if (error==null) try {
            file.seek(0);
            file.writeBytes("p cnf "+vars+" "+clauses);
            file.close();
            System.out.println(""+vars+" variables and "+clauses+" clauses written to "+name+"...");
            System.out.flush();
            Subprocess sp=new Subprocess(new String[]{"/tmp/berkmin",name});
            int r=sp.waitFor();
            if (r!=0) throw new RuntimeException("BerkMin failed...");
            System.out.println("Result = "+sp.getOutput());
            return true;
        } catch(IOException ex) {
            error=ex;
        }
        throw new RuntimeException("Error occurred in writing to file \""+name+"\": "+error.getMessage());
    }

    public boolean valueOf(int variable) { return false; }

    public void free() { }
}
