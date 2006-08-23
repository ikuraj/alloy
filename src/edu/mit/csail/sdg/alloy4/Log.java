package edu.mit.csail.sdg.alloy4;

import java.io.PrintWriter;
import java.io.FileNotFoundException;
import javax.swing.JTextArea;

public final class Log {

    private PrintWriter file=null;
    private JTextArea textarea=null;

    public Log() throws FileNotFoundException {
      this.file=new PrintWriter(".alloy.tmp");
    }
    
    public Log(JTextArea textarea) {
      this.textarea=textarea;
    }

    public void log(String x) {
      if (file!=null) file.println(x);
      if (textarea!=null) {
    	  textarea.append(x+"\n");
    	  textarea.setCaretPosition(textarea.getDocument().getLength());
      }
    }
    
    public void log0(String x) {
      if (file!=null) file.print(x);
      if (textarea!=null) {
    	  textarea.append(x);
    	  textarea.setCaretPosition(textarea.getDocument().getLength());
      }
    }
    
    public void flush() {
      if (file!=null) file.flush();
    }
    
    public void close() {
      if (file!=null) { file.flush(); file.close(); file=null; }
    }
}
