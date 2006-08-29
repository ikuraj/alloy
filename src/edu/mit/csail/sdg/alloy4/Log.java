package edu.mit.csail.sdg.alloy4;

import java.io.PrintWriter;
import java.io.FileNotFoundException;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

public final class Log {

    private PrintWriter file=null;
    private JTextComponent textarea=null;
    private Style defaultStyle=null, greenStyle=null;

    public Log() throws FileNotFoundException {
      this.file=new PrintWriter(".alloy.tmp");
    }
    
    public Log(JTextComponent textarea, Style defaultStyle, Style greenStyle) {
      this.textarea=textarea;
      this.defaultStyle=defaultStyle;
      this.greenStyle=greenStyle;
    }

    public void log(String x) {
      if (file!=null) file.println(x);
      if (textarea!=null) log0(x+"\n");
    }
    
    public void log0Green(String x) {
        if (file!=null) file.print(x);
        if (textarea instanceof JTextArea) {
      	  JTextArea jt=(JTextArea)textarea;
            jt.append(x);
            jt.setCaretPosition(jt.getDocument().getLength());
        }
        if (textarea instanceof JTextPane) {
            StyledDocument doc=((JTextPane)textarea).getStyledDocument();
    		  try { doc.insertString(doc.getLength(), x, greenStyle); } catch (BadLocationException e) { }
            textarea.setCaretPosition(doc.getLength());
        }
    }

    public void log0(String x) {
      if (file!=null) file.print(x);
      if (textarea instanceof JTextArea) {
    	  JTextArea jt=(JTextArea)textarea;
          jt.append(x);
          jt.setCaretPosition(jt.getDocument().getLength());
      }
      if (textarea instanceof JTextPane) {
          StyledDocument doc=((JTextPane)textarea).getStyledDocument();
  		  try { doc.insertString(doc.getLength(), x, defaultStyle); } catch (BadLocationException e) { }
          textarea.setCaretPosition(doc.getLength());
      }
    }
    
    public void flush() {
      if (file!=null) file.flush();
    }
    
    public void close() {
      if (file!=null) { file.flush(); file.close(); file=null; }
    }
}
