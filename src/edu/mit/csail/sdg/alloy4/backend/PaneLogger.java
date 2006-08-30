package edu.mit.csail.sdg.alloy4.backend;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

import edu.mit.csail.sdg.alloy4.core.Log;

public final class PaneLogger implements Log {
	
	private JTextPane textarea=null;
	private Style defaultStyle=null, greenStyle=null;
	
	public PaneLogger(JTextPane textarea, Style defaultStyle, Style greenStyle) {
		this.textarea=textarea;
		this.defaultStyle=defaultStyle;
		this.greenStyle=greenStyle;
	}
	
	public void log0Green(String x) {
		StyledDocument doc=textarea.getStyledDocument();
		try { doc.insertString(doc.getLength(), x, greenStyle); } catch (BadLocationException e) { }
		textarea.setCaretPosition(doc.getLength());
	}
	
	public void log0(String x) {
		StyledDocument doc=textarea.getStyledDocument();
		try { doc.insertString(doc.getLength(), x, defaultStyle); } catch (BadLocationException e) { }
		textarea.setCaretPosition(doc.getLength());
	}
	
	public void log(String x) { log0(x+"\n"); }

	public void flush() { }

	public void close() { }
}
