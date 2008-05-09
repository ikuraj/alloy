/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.AWTKeyStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.KeyboardFocusManager;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;

/**
 * Graphical syntax-highlighting editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

public final class OurTextArea extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** If in!=null, then this OurTextArea is merely a wrapper around an actual JTextArea. */
    private JTextArea in;

    /** If in==null, this stores the text buffer content (always contains at least one String). */
    private final List<String> lines = new ArrayList<String>();

    /** If in==null, this is the color to use for normal text. */
    private final Color colorNormal = Color.BLACK;

    /** If in==null, this is the color to use for numbers. */
    private final Color colorNumber = Color.RED;

    /** If in==null, this is the color to use for identifiers. */
    private final Color colorIden = Color.BLACK;

    /** If in==null, this is the color to use for keywords. */
    private final Color colorKeyword = new Color(30, 30, 168);

    /** If in==null, this is the color to use for "until end of line" comments. */
    private final Color colorComment = new Color(30, 168, 30);

    /** If in==null, this is the color to use for block comments. */
    private final Color colorBlockComment = new Color(30, 168, 30);

    /** If in==null, this is the color to use for javadoc comments. */
    private final Color colorJavadoc = new Color(210, 30, 30);

    /** If in==null, this is the ammount of padding to use around the text. */
    private static final int pad = 5;

    /** If in==null, this is the cursor's x position. */
    private int x = 0;

    /** If in==null, this is the cursor's y position. */
    private int y = 0;

    /** If in==null, this is the current font height. */
    private int h = 15; // FIXTHIS

    /** If in==null, this is the current font ascent. */
    private int asc = 11; // FIXTHIS

    /** If in==null, this is the amount of pixel to use per tab. */
    private int tab = 30;

    /** If in==null, this stores the non-bold font to use. */
    private Font font = new Font(false ? "Arial" : "Monospaced", Font.PLAIN, 14);

    /** If in==null, this stores the bold font to use. */
    private Font bold = font.deriveFont(Font.BOLD);

    /** If in==null, this stores the set of DocumentListeners. */
    private List<DocumentListener> docListeners = new ArrayList<DocumentListener>();

    /** If in==null, this stores the set of CaretListeners. */
    private List<CaretListener> caretListeners = new ArrayList<CaretListener>();

    /** If in==null, this stores the set of UndoableEditListeners. */
    private List<UndoableEditListener> undoListeners = new ArrayList<UndoableEditListener>();

    /** This stores the currently recognized set of keywords. */
    private final String[] keywords = new String[] {"abstract", "all", "and", "as", "assert", "but", "check", "disj", "disjoint",
      "else", "enum", "exactly", "exh", "exhaustive", "expect", "extends", "fact", "for", "fun", "iden", "iff", "implies", "in",
      "Int", "int", "let", "lone", "module", "no", "none", "not", "one", "open", "or", "part", "partition", "pred", "private",
      "run", "seq", "set", "sig", "some", "sum", "this", "univ"
    };

    /** Returns true if "c" can be the start of an identifier. */
    private static boolean myIdenStart(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$'; }

    /** Returns true if "c" can be in the middle or the end of an identifier. */
    private boolean myIden(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9') || c=='$' || c=='\'' || c=='\"'; }

    /** Returns true if c[start..start+len-1] matches one of the reserved keyword. */
    private boolean myIsKeyword(char[] array, int start, int len) {
      again:
      for(int i=keywords.length-1; i>=0; i--) {
         String str=keywords[i];
         if (str.length() != len) continue;
         for(int j=0; j<len; j++) if (str.charAt(j)!=array[start+j]) continue again;
         return true;
      }
      return false;
    }

    /** Returns the number of lines. */
    public int getLineCount() { if (in!=null) return in.getLineCount(); else return lines.size(); }

    /** Add a documentListener. */
    public void addDocumentListener(DocumentListener doc) {
        if (in!=null) in.getDocument().addDocumentListener(doc); else docListeners.add(doc);
    }

    /** Add a caretListener. */
    public void addCaretListener(CaretListener caret) {
        if (in!=null) in.addCaretListener(caret); else caretListeners.add(caret);
    }

    /** Add an undoableEditListener. */
    public void addUndoableEditListener(UndoableEditListener undo) {
        if (in!=null) in.getDocument().addUndoableEditListener(undo); else undoListeners.add(undo);
    }

    /** Handles a keyTyped event. */
    private void myType(KeyEvent e) {
        if (e.getModifiers() != 0 && e.getModifiers() != KeyEvent.SHIFT_MASK) return;
        final String text = lines.get(y);
        final int oldHeight = lines.size();
        final char c = e.getKeyChar();
        if (c=='\t' || (c>=32 && c<=126)) {
           // Add a character
           lines.set(y, text.substring(0,x)+c+text.substring(x));  x++;
        } else if (c==10 || c==13) {
           // Add a line break
           lines.set(y, text.substring(0,x));  lines.add(y+1, text.substring(x));  x=0;  y++;
        } else {
           return;
        }
        e.consume();
        if (oldHeight != lines.size()) setSize(getPreferredSize());
        myRepaint(); myCaretChanged(); myChanged();
    }

    /** Handles a keyPressed event. */
    private void myPress(KeyEvent e) {
        boolean chg = false;
        String text = lines.get(y);
        final int c=e.getKeyCode(), jump=10, len=text.length(), oldHeight=lines.size(), oldX=x, oldY=y;
        if (e.getModifiers()!=0 || e.getModifiersEx()!=0) return;
        else if (c==KeyEvent.VK_LEFT)              { if (x > 0) x--; else if (y > 0) { y--; x=lines.get(y).length(); } }
        else if (c==KeyEvent.VK_RIGHT)             { if (x < len) x++; else if (y < oldHeight-1) { x=0; y++; } }
        else if (c==KeyEvent.VK_HOME)              { x = 0; }
        else if (c==KeyEvent.VK_END)               { x = len; }
        else if (c==KeyEvent.VK_UP)                { if (y > 0) y--; }
        else if (c==KeyEvent.VK_DOWN)              { if (y < oldHeight-1) y++; }
        else if (c==KeyEvent.VK_PAGE_UP)           { if (y > jump) y=y-jump; else y=0; }
        else if (c==KeyEvent.VK_PAGE_DOWN)         { if (y+jump < oldHeight) y=y+jump; else y=oldHeight-1; }
        else if (c==KeyEvent.VK_BACK_SPACE && x>0) { chg=true; lines.set(y, text.substring(0,x-1)+text.substring(x)); x--; }
        else if (c==KeyEvent.VK_BACK_SPACE)        { if (y>0) { chg=true; String old=lines.get(y-1); lines.remove(y); y--; x=old.length(); lines.set(y, old+text); } }
        else if (c==KeyEvent.VK_DELETE && x<len)   { chg=true; lines.set(y, text.substring(0,x)+text.substring(x+1)); }
        else if (c==KeyEvent.VK_DELETE)            { if (y<oldHeight-1) { chg=true; lines.set(y, text+lines.remove(y+1)); } }
        else return;
        if (y<0) y=0; else if (y>=lines.size()) y=lines.size()-1;
        if (x<0) x=0; else if (x>lines.get(y).length()) x=lines.get(y).length();
        e.consume();
        if (oldHeight != lines.size()) setSize(getPreferredSize());
        if (x!=oldX || y!=oldY || chg) { myRepaint(); myCaretChanged(); }
        if (chg) myChanged();
    }

    /** Helper method to repaint this widget. */
    private void myRepaint() { invalidate(); repaint(); validate(); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getCaretPosition() getCaretPosition} method. */
    public int getCaretPosition() {
        if (in!=null) return in.getCaretPosition();
        int ans = 0;
        for(int i=0; i<y; i++) ans = ans + lines.get(i).length() + 1;
        return ans + x;
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setCaretPosition(int) setCaretPosition} method. */
    public void setCaretPosition(int offset) throws IllegalArgumentException {
        if (in!=null) { in.setCaretPosition(offset); return; }
        int newY=(offset<=0 ? 0 : lines.size()-1), newX=(offset<=0 ? 0 : lines.get(newY).length());
        if (offset>0) for(int i=0; i<lines.size(); i++) {
            int n=lines.get(i).length();
            if (offset<=n) { newY=i; newX=offset; break; }
            offset=offset-n-1;
        }
        if (y!=newY || x!=newX) { y=newY; x=newX; myRepaint(); myCaretChanged(); }
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setSelectionStart(int) setSelectionStart} method. */
    public void setSelectionStart(int offset) { if (in!=null) in.setSelectionStart(offset); else setCaretPosition(offset); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setSelectionEnd(int) setSelectionEnd} method. */
    public void setSelectionEnd(int offset) { if (in!=null) in.setSelectionEnd(offset); else setCaretPosition(offset); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#moveCaretPosition(int) moveCaretPosition} method. */
    public void moveCaretPosition(int offset) { if (in!=null) in.moveCaretPosition(offset); else setCaretPosition(offset); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineOfOffset(int) getLineOfOffset} method. */
    public int getLineOfOffset(int offset) throws BadLocationException {
        if (in!=null) return in.getLineOfOffset(offset);
        if (offset>=0) for(int i=0; i<lines.size(); i++) {
            int n=lines.get(i).length();
            if (offset<=n) return i;
            offset=offset-n-1;
        }
        throw new BadLocationException("", 0);
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineStartOffset(int) getLineStartOffset} method. */
    public int getLineStartOffset(int line) throws BadLocationException {
        if (in!=null) return in.getLineStartOffset(line);
        if (line<0 || line>=lines.size()) throw new BadLocationException("", 0);
        int ans = 0;
        for(int i=0; i<line; i++) ans = ans + lines.get(i).length() + 1;
        return ans;
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineEndOffset(int) getLineEndOffset} method. */
    public int getLineEndOffset(int line) throws BadLocationException {
        if (in!=null) return in.getLineEndOffset(line);
        if (line<0 || line>=lines.size()) throw new BadLocationException("", 0);
        int ans = 0;
        for(int i=0; i<line; i++) ans = ans + lines.get(i).length() + 1;
        return ans + lines.get(line).length();
    }

    /** This method sends out change notifications. */
    private void myChanged() { for(DocumentListener doc: docListeners) { doc.changedUpdate(null); } }

    /** This method sends out caretUpdate() notifications. */
    private void myCaretChanged() { for(CaretListener caret: caretListeners) { caret.caretUpdate(null); } }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getText() getText} method. */
    public String getText() {
        if (in!=null) return in.getText();
        StringBuilder sb = new StringBuilder();
        for(int i=0; i<lines.size(); i++) { sb.append(lines.get(i)); sb.append("\n"); }
        return sb.toString();
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setText(java.lang.String) setText} method. */
    public void setText(String text) {
        if (in!=null) { in.setText(text); return; }
        lines.clear();
        x=0;
        y=0;
        for(int n=text.length(), i=0; i<n;) {
           int na=text.indexOf('\r', i), nb=text.indexOf('\n', i);
           if (na<0 && nb<0) { lines.add(text.substring(i)); break; }
           if (na<0) na=nb; else if (na>nb) { int tmp=na; na=nb; nb=tmp; }
           lines.add(text.substring(i,na));
           i=na+1;
           if (i==nb) i=i+1;
        }
        if (lines.size()==0 || lines.get(lines.size()-1).length()>0) lines.add(""); // Make it feel like there is a ending LINE BREAK
        myChanged();
        myCaretChanged();
    }

    /** This stores the latest caret's X position. */
    private int caretX = 0;

    /** This stores the latest caret's Y position. */
    private int caretY = 0;

    /** Helper method that draws the caret. */
    private void myDrawCaret(Graphics gr, int px, int py) {
        caretX = px;
        caretY = py;
        gr.setColor(Color.BLUE);
        gr.drawLine(px-1, py, px-1, py+h-1);
        gr.drawLine(px  , py, px  , py+h-1);
    }

    /** Helper method that draws a String, then return the horizontal distance we traveled as a result of drawing the String. */
    private int myDraw(Graphics gr, Color color, char[] buf, int start, int len, int x, int y) {
        gr.setColor(color);
        if (len>1 || (len==1 && buf[start]!=' ' && buf[start]!='\t')) gr.drawChars(buf, start, len, x, y+asc);
        if (y==pad+this.y*h && this.x>=start && this.x<start+len) {
            myDrawCaret(gr, x + (int) (gr.getFontMetrics().getStringBounds(buf, start, this.x, gr).getWidth()), y);
        }
        if (len==1 && buf[start]=='\t') return tab-(x%tab); else return (int) (gr.getFontMetrics().getStringBounds(buf, start, start+len, gr).getWidth());
    }

    /** {@inheritDoc} */
    @Override public void paintComponent(final Graphics gr) {
        super.paintComponent(gr);
        if (in!=null) return;
        if (!(1==1) && gr instanceof Graphics2D) {
            Graphics2D gr2 = (Graphics2D)gr;
            gr2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        final Font oldFont = gr.getFont(); gr.setFont(font);
        final Color oldColor = gr.getColor(); gr.setColor(colorNormal);
        int maxWidth = 0;
        int comment = 0; // 0:none   1:/*..*/   2:/**..*/
        for(int i=0; i<lines.size(); i++) {
            char[] ln = lines.get(i).toCharArray();
            int px = pad, py = pad + i*h; // the current horizontal and vertical DRAWING position
            for(int p=0; p<ln.length; p++) {
                char c = ln[p];
                if (c==' ' || c=='\t') { px += myDraw(gr, colorNormal, ln, p, 1, px, py); continue; }
                if (comment==0 && (c=='/' || c=='-') && p<ln.length-1 && ln[p+1]==c) {
                    px += myDraw(gr, colorComment, ln, p, ln.length-p, px, py);
                    break;
                }
                if ((comment==0 && c=='/' && p<ln.length-3 && ln[p+1]=='*' && ln[p+2]=='*' && ln[p+3]!='/')
                  ||(comment==0 && c=='/' && p==ln.length-3 && ln[p+1]=='*' && ln[p+2]=='*')) {
                    px += myDraw(gr, colorJavadoc, ln, p, 3, px, py);
                    p=p+2; comment=2;
                } else if (comment==0 && c=='/' && p<ln.length-1 && ln[p+1]=='*') {
                    px += myDraw(gr, colorBlockComment, ln, p, 2, px, py);
                    p=p+1; comment=1;
                } else if (comment>0) {
                    Color color = comment==1 ? colorBlockComment : colorJavadoc;
                    if (c=='*' && p<ln.length-1 && ln[p+1]=='/') { px+=myDraw(gr, color, ln, p, 2, px, py); p++; comment=0; } else px+=myDraw(gr, color, ln, p, 1, px, py);
                } else if (c>='0' && c<='9') {
                    int old=p; p++; while(p<ln.length) if (myIden(ln[p])) p++; else break;
                    px += myDraw(gr, colorNumber, ln, old, p-old, px, py); p--;
                } else if (myIdenStart(c)) {
                    int old=p; p++; while(p<ln.length) if (myIden(ln[p])) p++; else break;
                    boolean isKey = myIsKeyword(ln, old, p-old);
                    if (isKey) gr.setFont(bold);
                    px += myDraw(gr, isKey ? colorKeyword : colorIden, ln, old, p-old, px, py); p--;
                    if (isKey) gr.setFont(font);
                } else { gr.setFont(bold); px += myDraw(gr, colorNormal, ln, p, 1, px, py); gr.setFont(font); }
            }
            if (y==i && x==ln.length) myDrawCaret(gr, px, py);
            if (maxWidth < px) maxWidth = px;
        }
        if (boxwidth != maxWidth+pad || boxheight != lines.size()*h+pad+pad) { boxwidth=(maxWidth+pad); boxheight=(lines.size()*h+pad+pad); invalidate(); }
        gr.setColor(oldColor);
        gr.setFont(oldFont);
        final int caretX = this.caretX, caretY = this.caretY;
        SwingUtilities.invokeLater(new Runnable() {
          public void run() {
            scrollRectToVisible(new Rectangle((caretX>50 ? caretX-50 : 0), (caretY>50 ? caretY-50 : 0), 100, 100));
          }
        });
    }

    /** {@inheritDoc} */
    @Override public boolean requestFocusInWindow() { return in!=null ? in.requestFocusInWindow() : super.requestFocusInWindow(); }

    private int boxwidth = 100;

    private int boxheight = 100;

    /** {@inheritDoc} */
    @Override public Dimension getMinimumSize() { return in!=null ? in.getMinimumSize() : getPreferredSize(); }

    /** {@inheritDoc} */
    @Override public Dimension getMaximumSize() { return in!=null ? in.getMaximumSize() : getPreferredSize(); }

    /** {@inheritDoc} */
    @Override public Dimension getPreferredSize() { return in!=null ? in.getPreferredSize() : new Dimension(boxwidth, boxheight); }

    /** {@inheritDoc} */
    @Override public void setFont(Font font) { if (in!=null) in.setFont(font); else {this.font=font; this.bold=font.deriveFont(Font.BOLD); repaint();} }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setTabSize(int) setTabSize} method. */
    public void setTabSize(int tab) { if (in!=null) in.setTabSize(tab); else if (this.tab!=font.getSize()*tab) { this.tab=font.getSize()*tab; myRepaint(); } }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getHighlighter() getHighlighter} method. */
    public Highlighter getHighlighter() { if (in!=null) return in.getHighlighter(); else return null; }

    /** Implements JTextArea's {@link javax.swing.JTextArea#copy() copy} method. */
    public void copy() { if (in!=null) in.copy(); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#cut() cut} method. */
    public void cut() { if (in!=null) in.cut(); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#paste() paste} method. */
    public void paste() {
        if (in!=null) { in.paste(); return; }
        String clip;
        try { clip = (String) (getToolkit().getSystemClipboard().getData(DataFlavor.stringFlavor)); } catch(Throwable ex) { return; }
        clip = Util.convertLineBreak(clip);
        if (clip.length()==0) return;
        while(clip.length()>0) {
           final int i = clip.indexOf('\n');
           final String chunk = (i<0) ? clip : clip.substring(0, i);
           final String txt = lines.get(y);
           if (i<0) {
              lines.set(y, txt.substring(0,x) + chunk + txt.substring(x));
              x = x + chunk.length();
              break;
           } else {
              lines.set(y, txt.substring(0,x) + chunk);
              lines.add(y+1, txt.substring(x));
              y = y + 1;
              x = 0;
              clip = clip.substring(i+1);
           }
        }
        myRepaint(); myCaretChanged(); myChanged();
    }

    /** Construct a new OurTextArea object displaying the given String. */
    public OurTextArea(String text) {
        lines.add(""); // since we list this as one of the invariant for this class
        text = Util.convertLineBreak(text);
        if (1==1 && !("yes".equals(System.getProperty("debug")))) {
           in = new JTextArea(text);
           in.setHighlighter(new DefaultHighlighter());
           setLayout(new BorderLayout());
           add(in, BorderLayout.CENTER);
           return;
        }
        setText(text);
        setBackground(Color.WHITE);
        setOpaque(true);
        setFocusable(true);
        Set<AWTKeyStroke> emptyset = Collections.unmodifiableSet(new HashSet<AWTKeyStroke>(1));
        setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, emptyset);
        setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, emptyset);
        addKeyListener(new KeyListener() {
           public void keyPressed(KeyEvent e) { myPress(e); }
           public void keyTyped(KeyEvent e) { myType(e); }
           public void keyReleased(KeyEvent e) { }
        });
        addMouseListener(new MouseAdapter() {
           public void mouseClicked(MouseEvent e) { requestFocusInWindow(); }
           public void mousePressed(MouseEvent e) { requestFocusInWindow(); }
        });
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                try {
                String text = Util.readAll("/zweb/zweb/w/p/public/def.als");
                JFrame jf = new JFrame("Demo");
                jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                jf.setLayout(new BorderLayout());
                Component area = new OurTextArea(text);
                JScrollPane scroll = new JScrollPane(area, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                jf.add(scroll);
                jf.pack();
                jf.setLocation(50,50);
                jf.setSize(1200,650);
                jf.setVisible(true);
                area.requestFocusInWindow();
                } catch(IOException ex) { }
            }
        });
    }
}
