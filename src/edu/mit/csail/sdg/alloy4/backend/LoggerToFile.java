package edu.mit.csail.sdg.alloy4.backend;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import edu.mit.csail.sdg.alloy4.core.Logger;

public final class LoggerToFile extends Logger {
	
	private PrintWriter file;
	
	public LoggerToFile(String filename) throws FileNotFoundException {
		file=new PrintWriter(filename);
	}
	
	public void log(String x) {
		if (file!=null) file.print(x);
	}
	
	public void logBold(String x) {
		if (file!=null) file.print(x);
	}

	public void flush() {
		if (file!=null) file.flush();
	}
	
	public void close() {
		if (file!=null) { file.flush(); file.close(); file=null; }
	}
}
