package edu.mit.csail.sdg.alloy4.helper;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import edu.mit.csail.sdg.alloy4util.Func1;
import edu.mit.csail.sdg.alloy4util.OurUtil;
import edu.mit.csail.sdg.alloy4util.Util;

public final class LogToJTextPane extends Log {

    private final JTextPane log;
    private final Color background;
    private final Style styleRegular, styleBold, styleRed;
    private final Func1 action;
    private int lastSize=0;

    public LogToJTextPane(JScrollPane parent, Color background, Color regular, Color bold, Color red, Func1 action) {
        this.action=action;
        log=new JTextPane();
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(this.background=background);
        log.setEditable(false);
        log.setFont(OurUtil.getFont());
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        styleRegular=doc.addStyle("regular", old);
        StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        StyleConstants.setForeground(styleBold,bold);
        styleRed=doc.addStyle("red", styleBold);
        StyleConstants.setForeground(styleRed,red);
        parent.setViewportView(log);
    }

    /** Print a text message into the log window using the provided style. */
    private void log(final String msg, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { log(msg,style); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        try { doc.insertString(doc.getLength(), msg, style); }
        catch (BadLocationException e) { Util.harmless("log()",e); }
        log.setCaretPosition(doc.getLength());
    }

    /** Print msg1 into the log window using the provided style, then print msg2 using the default style.
     * <br/> NOTE: this method can be called by any thread (hence the extra precaution) */
    public void log(final String msg1, final String msg2) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { log(msg1,msg2); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        try {
            doc.insertString(doc.getLength(),msg1,styleRed);
            doc.insertString(doc.getLength(),msg2,styleRegular);
        }
        catch (BadLocationException e) {Util.harmless("log()",e);}
        log.setCaretPosition(doc.getLength());
    }

    @Override public void logLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JButton b=new JButton("Click here to see the result");
        if (Util.onMac()) b.setBackground(background);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont());
        b.setForeground(Color.BLUE);
        b.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { action.run(link); }
        });
        StyleConstants.setComponent(s,b);
        log(" ",s);
        lastSize=doc.getLength();
    }

    public void logTextLink(final String link) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { logTextLink(link); } });
            return;
        }
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        final JLabel b=new JLabel("[Click here to see the result]");
        final Color linkColor=new Color(0.3f, 0.3f, 0.9f), hoverColor=new Color(.9f, .3f, .3f);
        b.setAlignmentY(0.8f);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont().deriveFont(Font.BOLD));
        b.setForeground(linkColor);
        b.addMouseListener(new MouseListener(){
            public final void mouseClicked(MouseEvent e) { action.run(link); }
            public final void mousePressed(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { b.setForeground(hoverColor); }
            public final void mouseExited(MouseEvent e) { b.setForeground(linkColor); }
        });
        StyleConstants.setComponent(s, b);
        log(" ",s);
        lastSize=doc.getLength();
    }

    @Override public void log(String msg) { log(msg,styleRegular); lastSize=log.getStyledDocument().getLength(); }

    @Override public void logBold(String msg) { log(msg,styleBold); lastSize=log.getStyledDocument().getLength(); }

    public void logRed(String msg) { log(msg,styleRed); }

    public void clear() { log.setText(""); lastSize=0; }

    private void clearError() {
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=lastSize) return;
        try {doc.remove(lastSize,n-lastSize);}
        catch (BadLocationException e) {Util.harmless("log",e);}
    }

    @Override public void flush() { }
}
