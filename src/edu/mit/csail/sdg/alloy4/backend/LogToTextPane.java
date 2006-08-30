package edu.mit.csail.sdg.alloy4.backend;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

import edu.mit.csail.sdg.alloy4.core.Log;

public final class LogToTextPane extends Log {
	
	private JTextPane textarea=null;
	private Style defaultStyle=null, boldStyle=null;
	
	public LogToTextPane(JTextPane textarea, Style defaultStyle, Style boldStyle) {
		this.textarea=textarea;
		this.defaultStyle=defaultStyle;
		this.boldStyle=boldStyle;
	}
	
	public void log(String x) {
		StyledDocument doc=textarea.getStyledDocument();
		try { doc.insertString(doc.getLength(), x, defaultStyle); } catch (BadLocationException e) { }
		textarea.setCaretPosition(doc.getLength());
	}

    public void logBold(String x) {
		StyledDocument doc=textarea.getStyledDocument();
		try { doc.insertString(doc.getLength(), x, boldStyle); } catch (BadLocationException e) { }
		textarea.setCaretPosition(doc.getLength());
	}
}
