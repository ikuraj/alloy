package edu.mit.csail.sdg.alloy4.backend;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import edu.mit.csail.sdg.alloy4.core.Log;

public final class FileLogger implements Log {
	
	private PrintWriter file=null;
	
	public FileLogger(String filename) throws FileNotFoundException {
		file=new PrintWriter(filename);
	}
	
	public void log(String x) {
		if (file!=null) file.println(x);
	}
	
	public void log0(String x) {
		if (file!=null) file.print(x);
	}
	
	public void log0Green(String x) {
		if (file!=null) file.print(x);
	}
	
	public void flush() {
		if (file!=null) file.flush();
	}
	
	public void close() {
		if (file!=null) { file.flush(); file.close(); file=null; }
	}
}
