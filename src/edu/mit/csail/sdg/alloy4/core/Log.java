package edu.mit.csail.sdg.alloy4.core;

public interface Log {
    public void log(String x);
    public void log0(String x);
    public void log0Green(String x);
    public void flush();
    public void close();
}
