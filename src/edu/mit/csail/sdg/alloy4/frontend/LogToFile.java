package edu.mit.csail.sdg.alloy4.frontend;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import edu.mit.csail.sdg.alloy4.core.Log;

public final class LogToFile implements Log {
	
	private PrintWriter file;
	
	public LogToFile(String filename) throws FileNotFoundException {
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
