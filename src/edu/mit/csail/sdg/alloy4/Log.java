package edu.mit.csail.sdg.alloy4;

import java.io.PrintWriter;
import java.io.FileNotFoundException;

public final class Log {

    private PrintWriter file;

    public Log() throws FileNotFoundException {
      file=new PrintWriter(".alloy.tmp");
    }

    public void log(String x) { file.println(x); }
    public void log0(String x) { file.print(x); }
    public void flush() { file.flush(); }
    public void close() { file.close(); file=null; }
}
