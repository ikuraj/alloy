package edu.mit.csail.sdg.alloy4.backend;

import edu.mit.csail.sdg.alloy4.core.Log;

public final class BlankLogger implements Log {
	public void log0Green(String x) { }
	public void log0(String x) { }
	public void log(String x) { }
	public void flush() { }
	public void close() { }
}
