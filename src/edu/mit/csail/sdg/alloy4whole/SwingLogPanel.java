/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4whole;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4viz.VizGUI;

/** Only the AWT Event Thread may call methods in this class. */

final class SwingLogPanel {

    /** This defines the escape code for CLICK (see Alloy4 Developer's Guide). */
    public static final byte CLICK=1;

    /** This defines the escape code for BOLD (see Alloy4 Developer's Guide). */
    public static final byte BOLD=2;

    /** This defines the escape code for RED (see Alloy4 Developer's Guide). */
    public static final byte RED=3;

    /** This defines the escape code for INDENT (see Alloy4 Developer's Guide). */
    public static final byte INDENTSHORT=4;
    public static final byte INDENTLONG=5;

    /** This defines the escape code for LINK (see Alloy4 Developer's Guide). */
    public static final byte LINK=6;
    public static final byte SETLINK=7;

    /** This defines the escape code for DIVIDER (see Alloy4 Developer's Guide). */
    public static final byte DIVIDER=12;

    /** This defines the escape code for FLUSH (see Alloy4 Developer's Guide). */
    public static final byte FLUSH=21;

    /** This defines the escape code for SAVE#1 (see Alloy4 Developer's Guide). */
    public static final byte SAVE1=22;

    /** This defines the escape code for RESTORE#1 (see Alloy4 Developer's Guide). */
    public static final byte RESTORE1=23;

    /** This defines the escape code for SAVE#2 (see Alloy4 Developer's Guide). */
    public static final byte SAVE2=24;

    /** This defines the escape code for RESTORE#2 (see Alloy4 Developer's Guide). */
    public static final byte RESTORE2=25;

    /** This defines the escape code for SAVE#3 (see Alloy4 Developer's Guide). */
    public static final byte SAVE3=26;

    /** This defines the escape code for RESTORE#3 (see Alloy4 Developer's Guide). */
    public static final byte RESTORE3=27;

    /** This defines the escape code for CLEARERROR (see Alloy4 Developer's Guide). */
    public static final byte CLEARERROR=28;

    /** This defines the escape code for DONE (see Alloy4 Developer's Guide). */
    public static final byte DONE=29;

    /** This defines the escape code for FAIL (see Alloy4 Developer's Guide). */
    public static final byte FAIL=20;

    /** This defines the escape code for VIZMSG (see Alloy4 Developer's Guide). */
    public static final byte VIZMSG=19;

    /** This defines the escape code for DELETE_ON_EXIT (see Alloy4 Developer's Guide). */
    public static final byte DELETE_ON_EXIT=30;

    /** This defines the escape code for DECLARE_INSTANCE (see Alloy4 Developer's Guide). */
    public static final byte DECLARE_INSTANCE=31;

    /** This stores the serialized writings that are not yet processed. */
    private byte[] escSb = new byte[65536];
    private int escSn = 0;
    private String escLink = "";

    /** This stores the document length previously saved by the "SAVE#1" escape code. */
    private int escLength1 = 0;

    /** This stores the document length previously saved by the "SAVE#2" escape code. */
    private int escLength2 = 0;

    /** This stores the document length previously saved by the "SAVE#3" escape code. */
    private int escLength3 = 0;

    /** This stores the current process that is allowed to call esc(..) on this logpanel. */
    private Process escProcess=null;

    /** Sets the current process that is allowed to call esc(...) on this logpanel. */
    public void escSetProcess(Process process) { escProcess=process; escLink=""; escSn=escLength1=escLength2=escLength3=0; }

    /** Discard any serialized writings that were not yet processed (This method can be called by any thread) */
    public void escReset() {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(new Runnable() {
                public final void run() {
                    escReset();
                }
            });
            return;
        }
        escLink="";
        escSn=escLength1=escLength2=escLength3=0;
    }

    /**
     * Write "msg" into the serialized writing buffer (This method can be called by any thread).
     * NOTE: this method may return BEFORE it reads the arguments, so you must give up control of the byte[] array!
     */
    public void esc(final Process process, final byte[] msg, final int msglen) {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(new Runnable() {
                public final void run() {
                    esc(process, msg, msglen);
                }
            });
            return;
        }
        if (process!=escProcess) return;
        while(msglen > (escSb.length - escSn)) {
            byte[] escSb2 = new byte[escSb.length+4096];
            for(int i=0; i<escSn; i++) escSb2[i]=escSb[i];
            escSb=escSb2;
        }
        for(int i=0; i<msglen; i++) {
            byte c=msg[i];
            if (c>0 && c<32 && c!=10 && c!=13 && c!=8 && c!=9) {
                String txt="";
                try {txt=new String(escSb, 0, escSn, "UTF-8");} catch(UnsupportedEncodingException ex) {}
                if (c==CLICK) handler.run(SimpleGUI.evs_visualize, txt);
                else if (c==BOLD) logBold(txt);
                else if (c==RED) logRed(txt);
                else if (c==INDENTLONG) { if (handler.isUsingExternalEditor()) logIndented(txt); }
                else if (c==INDENTSHORT) { if (!handler.isUsingExternalEditor()) logIndented(txt); }
                else if (c==SETLINK) escLink=txt;
                else if (c==LINK) logLink(txt,escLink);
                else if (c==DELETE_ON_EXIT) (new File(txt)).deleteOnExit();
                else if (c==DECLARE_INSTANCE) handler.run(SimpleGUI.evs_setLatest, txt);
                else if (c==DIVIDER) { log(txt); logDivider(); }
                else if (c==SAVE1) { log(txt); escLength3=(escLength2=(escLength1=getLength())); }
                else if (c==RESTORE1) { log(txt); setLength(escLength1); }
                else if (c==SAVE2) { log(txt); escLength3=(escLength2=getLength()); }
                else if (c==RESTORE2) { log(txt); setLength(escLength2); }
                else if (c==SAVE3) { log(txt); escLength3=getLength(); }
                else if (c==RESTORE3) { log(txt); setLength(escLength3); }
                else if (c==CLEARERROR) { log(txt); clearError(); }
                else if (c==DONE) { log(txt); handler.run(SimpleGUI.ev_done); }
                else if (c==FAIL) { log(txt); handler.run(SimpleGUI.ev_fail); }
                else if (c==VIZMSG) { viz.run(VizGUI.evs_alert, txt); }
                else { log(txt); } // Everything else is treated as FLUSH
                flush();
                escSn=0;
            } else {
                escSb[escSn]=c;
                escSn++;
            }
        }
    }

    /** Try to wrap the input to about 60 characters per line; however, if a token is too long, we won't break it. */
    private static void linewrap(StringBuilder sb, String msg) {
        StringTokenizer tokenizer=new StringTokenizer(msg,"\r\n\t ");
        final int max=60;
        int now=0;
        while(tokenizer.hasMoreTokens()) {
            String x=tokenizer.nextToken();
            if (now+1+x.length() > max) {
                if (now>0) { sb.append('\n'); }
                sb.append(x);
                now=x.length();
            } else {
                if (now>0) { now++; sb.append(' '); }
                sb.append(x);
                now=now+x.length();
            }
        }
    }

    /**
     * This field buffers previous calls to log() so that we can write them out later in a single Swing call
     * (If there is nothing buffered, this field can be an empty list or even null).
     */
    private final List<String> batch = new ArrayList<String>();

    /** The newly created JTextPane object that will display the log; null if this log has been destroyed. */
    private JTextPane log;

    /** The style to use when writing regular messages. */
    private final Style styleRegular;

    /** The style to use when writing bold messages. */
    private final Style styleBold;

    /** The style to use when writing red messages. */
    private final Style styleRed;

    /** This stores the JLabels used for displaying hyperlinks. */
    private final List<JLabel> links=new ArrayList<JLabel>();

    /**
     * When the window gains focus, we'll call handler.run(ev_logFocused);
     * When a hyperlink is clicked, we'll call handler.run(evs_visualize, linkURL).
     */
    private final SimpleGUI handler;

    /** The VizGUI handler. */
    private final VizGUI viz;

    /** The current length of the log, not counting any "red" error message at the end of the log. */
    private int lastSize=0;

    /** The current font name. */
    private String fontName;

    /** The current font size. */
    private int fontSize;

    /** The color to use for hyperlinks when the mouse is not hovering over it. */
    private static final Color linkColor = new Color(0.3f, 0.3f, 0.9f);

    /** The color to use for a hyperlink when the mouse is hovering over it. */
    private static final Color hoverColor = new Color(0.9f, 0.3f, 0.3f);

    /** This stores a default ViewFactory that will handle the View requests that we don't care to override. */
    private static final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();

    /**
     * Constructs a new JTextPane logger, and put it inside an existing JScrollPane.
     * @param parent - the existing JScrollPane to insert the JTextPane into
     * @param fontName - the font family to use
     * @param fontSize - the font size to use
     * @param background - the background color to use
     * @param regular - the color to use for regular messages
     * @param red - the color to use for red messages
     * @param handler - the SimpleGUI parent
     * @param viz - the VizGUI parent
     */
    public SwingLogPanel(
        final JScrollPane parent, String fontName, int fontSize,
        final Color background, final Color regular, final Color red,
        final SimpleGUI handler, final VizGUI viz) {
        this.handler=handler;
        this.viz=viz;
        this.fontName=fontName;
        this.fontSize=fontSize;
        this.log=new JTextPane() {
            private static final long serialVersionUID = 1L;
            @Override public void paintComponent(Graphics g) {
                if (g instanceof Graphics2D) {
                    Graphics2D g2 = (Graphics2D)g;
                    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                }
                super.paintComponent(g);
            }
        };
        // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
        // 30000 is a good number; value higher than about 32768 will cause errors.
        this.log.setEditorKit(new StyledEditorKit() {
            private static final long serialVersionUID = 1L;
            @Override public final ViewFactory getViewFactory() {
                return new ViewFactory() {
                    public final View create(Element x) {
                        if (!AbstractDocument.SectionElementName.equals(x.getName())) {
                            return defaultFactory.create(x);
                        }
                        return new BoxView(x, View.Y_AXIS) {
                            @Override public final float getMinimumSpan(int axis) {
                                return super.getPreferredSpan(axis);
                            }
                            @Override public final void layout(int width,int height) {
                                super.layout(30000, height);
                            }
                        };
                    }
                };
            }
        });
        log.setBorder(new EmptyBorder(1,1,1,1));
        log.setBackground(background);
        log.setEditable(false);
        log.setFont(new Font(fontName, Font.PLAIN, fontSize));
        log.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { if (handler!=null) handler.run(SimpleGUI.ev_logFocused); }
            public final void focusLost(FocusEvent e) { }
        });
        StyledDocument doc=log.getStyledDocument();
        styleRegular=doc.addStyle("regular", null);
        StyleConstants.setFontFamily(styleRegular, fontName);
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setForeground(styleRegular, regular);
        styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        styleRed=doc.addStyle("red", styleBold);
        StyleConstants.setForeground(styleRed, red);
        parent.setViewportView(log);
        parent.setBackground(background);
    }

    /** Write a horizontal separator into the log window. */
    public void logDivider() {
        if (log==null) return;
        clearError();
        StyledDocument doc = log.getStyledDocument();
        Style dividerStyle = doc.addStyle("bar", styleRegular);
        JPanel jpanel = new JPanel();
        jpanel.setBackground(Color.LIGHT_GRAY);
        jpanel.setPreferredSize(new Dimension(300,1)); // 300 is arbitrary, since it will auto-stretch
        StyleConstants.setComponent(dividerStyle, jpanel);
        reallyLog(".", dividerStyle); // Any character would do; "." will be replaced by the JPanel
        reallyLog("\n\n", styleRegular);
        log.setCaretPosition(doc.getLength());
        lastSize = doc.getLength();
    }

    /** Write a clickable link into the log window. */
    public void logLink(final String link, final String linkDestination) {
        if (log==null || link.length()==0) return;
        clearError();
        StyledDocument doc=log.getStyledDocument();
        Style linkStyle=doc.addStyle("link", styleRegular);
        final JLabel label=OurUtil.label(linkColor, link);
        label.setAlignmentY(0.8f);
        label.setMaximumSize(label.getPreferredSize());
        label.setFont(new Font(fontName, Font.BOLD, fontSize));
        label.addMouseListener(new MouseListener(){
            public final void mousePressed(MouseEvent e) { if (handler!=null) handler.run(SimpleGUI.evs_visualize, linkDestination); }
            public final void mouseClicked(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { label.setForeground(hoverColor); }
            public final void mouseExited(MouseEvent e) { label.setForeground(linkColor); }
        });
        StyleConstants.setComponent(linkStyle, label);
        links.add(label);
        reallyLog(".", linkStyle); // Any character would do; the "." will be replaced by the JLabel
        log.setCaretPosition(doc.getLength());
        lastSize = doc.getLength();
    }

    /** Write "msg" in regular style. */
    public void log(String msg) { if (log!=null && msg.length()>0) batch.add(msg); }

    /** Write "msg" in bold style. */
    public void logBold(String msg) { if (msg.length()>0) {clearError(); reallyLog(msg, styleBold);} }

    private void reallyLog(String text, Style style) {
        if (log==null || text.length()==0) return;
        int i=text.lastIndexOf('\n'), j=text.lastIndexOf('\r');
        if (i>=0 && i<j) { i=j; }
        StyledDocument doc=log.getStyledDocument();
        try {
            if (i<0) {
                doc.insertString(doc.getLength(), text, style);
            } else {
                // Performs intelligent caret positioning
                doc.insertString(doc.getLength(), text.substring(0,i+1), style);
                log.setCaretPosition(doc.getLength());
                if (i<text.length()-1) {
                    doc.insertString(doc.getLength(), text.substring(i+1), style);
                }
            }
        } catch (BadLocationException e) {
            // Harmless
        }
        if (style!=styleRed) { lastSize=doc.getLength(); }
    }

    /** Write "msg" in red style (with automatic line wrap). */
    public void logRed (String msg) {
        if (log==null || msg.length()==0) return;
        StringBuilder sb=new StringBuilder();
        while (msg.length()>0) {
            int i = msg.indexOf('\n');
            if (i>=0) {
                linewrap(sb, msg.substring(0,i));
                sb.append('\n');
                msg=msg.substring(i+1);
            } else {
                linewrap(sb, msg);
                break;
            }
        }
        clearError();
        reallyLog(sb.toString(), styleRed);
    }

    /** Write "msg" in regular style (with automatic line wrap). */
    public void logIndented (String msg) {
        if (log==null || msg.length()==0) return;
        StringBuilder sb=new StringBuilder();
        while(msg.length()>0) {
            int i = msg.indexOf('\n');
            if (i>=0) {
                linewrap(sb, msg.substring(0,i));
                sb.append('\n');
                msg=msg.substring(i+1);
            } else {
                linewrap(sb, msg);
                break;
            }
        }
        clearError();
        reallyLog(sb.toString(), styleRegular);
    }

    /** Set the font name. */
    public void setFontName(String fontName) {
        if (log==null) return;
        this.fontName = fontName;
        log.setFont(new Font(fontName, Font.PLAIN, fontSize));
        StyleConstants.setFontSize(styleRegular, fontSize);
        StyleConstants.setFontSize(styleBold, fontSize);
        StyleConstants.setFontSize(styleRed, fontSize);
        // Changes all existing text
        StyledDocument doc=log.getStyledDocument();
        Style temp=doc.addStyle("temp", null);
        StyleConstants.setFontFamily(temp, fontName);
        StyleConstants.setFontSize(temp, fontSize);
        doc.setCharacterAttributes(0, doc.getLength(), temp, false);
        // Changes all existing hyperlinks
        Font newFont = new Font(fontName, Font.BOLD, fontSize);
        for(JLabel link: links) { link.setFont(newFont); }
    }

    /** Set the font size. */
    public void setFontSize(int fontSize) {
        if (log==null) return;
        this.fontSize = fontSize;
        setFontName(this.fontName);
    }

    /** Set the background color. */
    public void setBackground(Color background) {
        if (log==null) return;
        log.setBackground(background);
    }

    /** Query the current length of the log. */
    private int getLength() {
        if (log==null) return 0;
        clearError();
        return log.getStyledDocument().getLength();
    }

    /** Truncate the log to the given length; if the log is shorter than the number given, then nothing happens. */
    private void setLength(int newLength) {
        if (log==null) return;
        clearError();
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n<=newLength) return;
        try {
            doc.remove(newLength, n-newLength);
        } catch (BadLocationException e) {
            // Harmless
        }
        if (lastSize>doc.getLength()) { lastSize=doc.getLength(); }
    }

    /** This method copies the currently selected text in the log (if any) into the clipboard. */
    public void copy() {
        if (log==null) return;
        log.copy();
    }

    /** Removes any messages writtin in "red" style. */
    public void clearError() {
        if (log==null) return;
        // Since this class always removes "red" messages prior to writing anything,
        // that means if there are any red messages, they will always be at the end of the JTextPane.
        StyledDocument doc=log.getStyledDocument();
        int n=doc.getLength();
        if (n>lastSize) {
            try {doc.remove(lastSize, n-lastSize);} catch (BadLocationException e) {}
        }
        if (batch.size()>0) {
            for(String msg: batch) { reallyLog(msg, styleRegular); }
            batch.clear();
        }
    }

    /** Commits all outstanding writes (if the messages are buffered). */
    public void flush() {
        if (log==null) return;
        if (batch.size()>0) clearError();
    }
}
