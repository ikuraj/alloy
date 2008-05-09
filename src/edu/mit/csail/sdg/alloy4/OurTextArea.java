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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JViewport;
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

    private static final long serialVersionUID = 1L;
    private JTextArea in;
    private final List<String> lines = new ArrayList<String>(); // always contains at least one String
    private Font font = new Font("Monospaced", Font.PLAIN, 14);
    private Font bold = new Font("Monospaced", Font.BOLD, 14);
    private int w = 8, h = 15, asc = 11;
    private int x=0, y=0;
    private final int pad = 5;

    private void myRepaint() { invalidate(); repaint(); validate(); }

    @Override public void setFont(Font font) {
        if (in!=null) { in.setFont(font); return; }
    }

    private int myMaxWidth() {
        int max=0;
        for(int i=0; i<lines.size(); i++) { int n=lines.get(i).length(); if (max<n) max=n; }
        return max;
    }

    public Highlighter myMakeDefaultHighlighter() { if (in!=null) return new DefaultHighlighter(); else return null; }

    @Override public boolean requestFocusInWindow() {
        if (in!=null) return in.requestFocusInWindow(); else return super.requestFocusInWindow();
    }

    @Override public Dimension getPreferredSize() {
        if (in!=null) return in.getPreferredSize();
        return new Dimension(myMaxWidth()*w+w+pad+pad, lines.size()*h+pad+pad); // FIXTHIS
    }

    @Override public Dimension getMinimumSize() {
        if (in!=null) return in.getMinimumSize();
        return getPreferredSize();
    }

    @Override public Dimension getMaximumSize() {
        if (in!=null) return in.getMaximumSize();
        return getPreferredSize();
    }

    private boolean myFindVisible(Rectangle rect) {
        Container p = getParent();
        while(p!=null && !(p instanceof JViewport)) p=p.getParent();
        if (p instanceof JViewport) {
           Point topleft = ((JViewport)p).getViewPosition();
           int pw = p.getWidth(), ph = p.getHeight();
           rect.x = topleft.x;
           rect.y = topleft.y;
           rect.width = pw;
           rect.height = ph;
           return true;
        }
        return false;
    }

    private void myScroll() {
        Rectangle r = new Rectangle();
        if (myFindVisible(r))
           if (!(r.x>x*w || r.x+r.width<x*w+w+pad+pad || r.y>y*h || r.y+r.height<y*h+h+pad+pad)) return;
        r.x=x*w; r.y=y*h; r.width=w+pad+pad; r.height=h+pad+pad;
        scrollRectToVisible(r);
    }

    private void myType(KeyEvent e) {
        if (e.getModifiers()!=0 && e.getModifiers()!=KeyEvent.SHIFT_MASK) return;
        final int on = lines.size();
        char c = e.getKeyChar();
        if (c>=32 && c<=126) {
            String txt=lines.get(y); if (x==0) txt=c+txt; else if (x==txt.length()) txt=txt+c; else txt=txt.substring(0,x)+c+txt.substring(x); x++; lines.set(y,txt);
        }
        else if (c==10 || c==13) {
            String txt=lines.get(y); if (x==0) lines.add(y,""); else if (x==txt.length()) lines.add(y+1,""); else {lines.set(y,txt.substring(0,x)); lines.add(y+1,txt.substring(x));} x=0; y++;
        }
        else {
            return;
        }
        e.consume();
        if (on != lines.size()) setSize(getPreferredSize());
        myRepaint(); myScroll(); myCaretChanged(); myChanged();
    }

    private void myPress(KeyEvent e) {
        boolean chg = false;
        final int on = lines.size();
        String text = lines.get(y);
        int c = e.getKeyCode();
        int jump = 10;
        int len = text.length();
        if (e.getModifiers()!=0 || e.getModifiersEx()!=0) return;
        else if (c==KeyEvent.VK_LEFT && x>0) { x--; }
        else if (c==KeyEvent.VK_LEFT && y>0) { y--; x=lines.get(y).length(); }
        else if (c==KeyEvent.VK_RIGHT && x<len) { x++; }
        else if (c==KeyEvent.VK_RIGHT && y<lines.size()-1) { x=0; y++; }
        else if (c==KeyEvent.VK_HOME && x!=0) { x=0; }
        else if (c==KeyEvent.VK_END && x!=len) { x=len; }
        else if (c==KeyEvent.VK_UP && y>0) { y--; }
        else if (c==KeyEvent.VK_DOWN && y<lines.size()-1) { y++; }
        else if (c==KeyEvent.VK_PAGE_UP && y>0) { y=y-jump; }
        else if (c==KeyEvent.VK_PAGE_DOWN && y<lines.size()-1) { y=y+jump; }
        else if (c==KeyEvent.VK_DELETE && x<len) { chg=true; if (x==0) text=text.substring(1); else text=text.substring(0,x)+text.substring(x+1); lines.set(y,text); }
        else if (c==KeyEvent.VK_DELETE && y<lines.size()-1) { chg=true; text=text+lines.remove(y+1); lines.set(y,text); }
        else if (c==KeyEvent.VK_BACK_SPACE && x>0) { chg=true; text=text.substring(0,x-1)+text.substring(x); lines.set(y,text); x--; }
        else if (c==KeyEvent.VK_BACK_SPACE && y>0) { chg=true; String old=lines.get(y-1); text=old+text; lines.remove(y); y--; lines.set(y,text); x=old.length(); }
        else if (c==KeyEvent.VK_LEFT || c==KeyEvent.VK_RIGHT) { e.consume(); return; }
        else if (c==KeyEvent.VK_UP || c==KeyEvent.VK_DOWN) { e.consume(); return; }
        else if (c==KeyEvent.VK_HOME || c==KeyEvent.VK_END) { e.consume(); return; }
        else if (c==KeyEvent.VK_PAGE_UP || c==KeyEvent.VK_PAGE_DOWN) { e.consume(); return; }
        else return;
        if (y<0) y=0; else if (y>=lines.size()) y=lines.size()-1;
        if (x<0) x=0; else if (x>lines.get(y).length()) x=lines.get(y).length();
        e.consume();
        if (on != lines.size()) setSize(getPreferredSize());
        myRepaint(); myScroll(); myCaretChanged(); if (chg) myChanged();
    }

    public int getLineCount() { if (in!=null) return in.getLineCount(); else return lines.size(); }

    public void setLineWrap(boolean flag) { if (in!=null) in.setLineWrap(flag); }

    public void setEditable(boolean flag) { if (in!=null) in.setEditable(flag); }

    private List<DocumentListener> docs = new ArrayList<DocumentListener>();
    public void addDocumentListener(DocumentListener doc) {
        if (in!=null) in.getDocument().addDocumentListener(doc); else docs.add(doc);
    }

    private List<CaretListener> carets = new ArrayList<CaretListener>();
    public void addCaretListener(CaretListener caret) {
        if (in!=null) in.addCaretListener(caret); else carets.add(caret);
    }

    private List<UndoableEditListener> undos = new ArrayList<UndoableEditListener>();
    public void addUndoableEditListener(UndoableEditListener undo) {
        if (in!=null) in.getDocument().addUndoableEditListener(undo); else undos.add(undo);
    }

    public void setSelectionStart(int i) { if (in!=null) in.setSelectionStart(i); else setCaretPosition(i); }

    public void setSelectionEnd(int i) { if (in!=null) in.setSelectionEnd(i); else setCaretPosition(i); }

    public void moveCaretPosition(int c) { if (in!=null) in.moveCaretPosition(c); else setCaretPosition(c); }

    public void setCaretPosition(int c) {
        if (in!=null) { in.setCaretPosition(c); return; }
        int newY=(c<=0 ? 0 : lines.size()-1), newX=(c<=0 ? 0 : lines.get(newY).length());
        if (c>0) for(int i=0; i<lines.size(); i++) {
            int n=lines.get(i).length();
            if (c<=n) { newY=i; newX=c; break; }
            c=c-n-1;
        }
        if (y!=newY || x!=newX) { y=newY; x=newX; myRepaint(); myScroll(); myCaretChanged(); }
    }

    public int getCaretPosition() {
        if (in!=null) return in.getCaretPosition();
        int ans = 0;
        for(int i=0; i<y; i++) ans = ans + lines.get(i).length() + 1;
        return ans + x;
    }

    public int getLineOfOffset(int c) throws BadLocationException {
        if (in!=null) return in.getLineOfOffset(c);
        if (c<=0) return 0;
        for(int i=0; i<lines.size(); i++) {
            int n=lines.get(i).length();
            if (c<=n) return i;
            c=c-n-1;
        }
        return lines.size()-1;
    }

    public int getLineStartOffset(int line) throws BadLocationException {
        if (in!=null) return in.getLineStartOffset(line);
        if (line<0 || line>=lines.size()) throw new BadLocationException("",0);
        int ans = 0;
        for(int i=0; i<line; i++) ans = ans + lines.get(i).length() + 1;
        return ans;
    }

    public int getLineEndOffset(int line) throws BadLocationException {
        if (in!=null) return in.getLineEndOffset(line);
        if (line<0 || line>=lines.size()) throw new BadLocationException("",0);
        int ans = 0;
        for(int i=0; i<line; i++) ans = ans + lines.get(i).length() + 1;
        return ans + lines.get(line).length();
    }

    public void setTabSize(int tab) { if (in!=null) in.setTabSize(tab); }

    public void setHighlighter(Highlighter ht) { if (in!=null) in.setHighlighter(ht); }

    public void copy() { if (in!=null) in.copy(); }

    public void cut() { if (in!=null) in.cut(); }

    private void myChanged() {
        for(DocumentListener doc: docs) { doc.changedUpdate(null); }
    }

    private void myCaretChanged() {
        for(CaretListener caret: carets) { caret.caretUpdate(null); }
    }

    public void paste() {
        if (in!=null) { in.paste(); return; }
        String clip;
        try { clip = (String) (getToolkit().getSystemClipboard().getData(DataFlavor.stringFlavor)); }
        catch(Throwable ex) { return; }
        if (clip.length()==0) return;
        // FIXTHIS: what if clip contains "\r" or "\n"?
        String txt = lines.get(y);
        txt = txt.substring(0,x) + clip + txt.substring(x);
        x = x + clip.length();
        lines.set(y, txt);
        myRepaint(); myScroll(); myCaretChanged(); myChanged();
    }

    public String getText() {
        if (in!=null) return in.getText();
        StringBuilder sb = new StringBuilder();
        for(int i=0; i<lines.size(); i++) { sb.append(lines.get(i)); sb.append("\n"); }
        return sb.toString();
    }

    public void setText(String text) {
        if (in!=null) { in.setText(text); return; }
        lines.clear();
        x=0;
        y=0;
        int i=0, n=text.length();
        while(i<n) {
           int na=text.indexOf('\r', i), nb=text.indexOf('\n', i);
           if (na<0 && nb<0) { lines.add(text.substring(i)); break; }
           if (na<0) na=nb; else if (na>nb) { int tmp=na; na=nb; nb=tmp; }
           lines.add(text.substring(i,na));
           i=na+1;
           if (i==nb) i=i+1;
        }
        if (lines.size()==0) lines.add("");
        if (lines.get(lines.size()-1).length()>0) lines.add(""); // Make it feel like there is a ending LINE BREAK
        myChanged(); myCaretChanged();
    }

    public OurTextArea(String text) {
        lines.add(""); // since we list this as one of the invariant for this class
        if (!("yes".equals(System.getProperty("debug")))) {
            in = new JTextArea(text);
            setLayout(new BorderLayout());
            add(in, BorderLayout.CENTER);
            return;
        }
        setText(text);
        setBackground(Color.WHITE);
        setOpaque(true);
        setFocusable(true);
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

    private boolean myIdenStart(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$'; }

    private boolean myIden(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9') || c=='$' || c=='\'' || c=='\"'; }

    private final String[] keywords = new String[] {"abstract", "all", "and", "as", "assert", "but", "check", "disj", "disjoint",
      "else", "enum", "exactly", "exh", "exhaustive", "expect", "extends", "fact", "for", "fun", "iden", "iff", "implies", "in",
      "Int", "int", "let", "lone", "module", "no", "none", "not", "one", "open", "or", "part", "partition", "pred", "private",
      "run", "seq", "set", "sig", "some", "sum", "this", "univ"
    };

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

    private final Color colorNormal = Color.BLACK;
    private final Color colorNumber = Color.RED;
    private final Color colorIden = Color.BLACK;
    private final Color colorKeyword = new Color(30, 30, 168);
    private final Color colorComment = new Color(30, 168, 30);
    private final Color colorBlockComment = new Color(30, 168, 30);
    private final Color colorJavadoc = new Color(210, 30, 30);

    @Override public void paint(Graphics gr) {
        // FIXTHIS: use "findVisible" to avoid drawing in irrelevant area
        super.paint(gr);
        if (in!=null) return;
        if (false) if (gr instanceof Graphics2D) {
            Graphics2D gr2 = (Graphics2D)gr;
            gr2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        final Font oldFont = gr.getFont(); gr.setFont(font);
        final Color oldColor = gr.getColor(); gr.setColor(colorNormal);
        int comment = 0; // 0:none   1:/*..*/   2:/**..*/
        again:
        for(int i=0; i<lines.size(); i++) {
            char[] ln = lines.get(i).toCharArray();
            for(int p=0; p<ln.length; p++) {
                char c = ln[p];
                if (c<=32) continue;
                if (comment==0 && (c=='/' || c=='-') && p<ln.length-1 && ln[p+1]==c) {
                    gr.setColor(colorComment);
                    gr.drawChars(ln, p, ln.length-p, pad+p*w, pad+i*h+asc);
                    gr.setColor(colorNormal);
                    continue again;
                }
                if ((comment==0 && c=='/' && p<ln.length-3 && ln[p+1]=='*' && ln[p+2]=='*' && ln[p+3]!='/')
                  ||(comment==0 && c=='/' && p==ln.length-3 && ln[p+1]=='*' && ln[p+2]=='*')) {
                    gr.setColor(colorJavadoc);
                    gr.drawChars(ln, p, 3, pad+p*w, pad+i*h+asc);
                    p=p+2; comment=2; continue;
                }
                if (comment==0 && c=='/' && p<ln.length-1 && ln[p+1]=='*') {
                    gr.setColor(colorBlockComment);
                    gr.drawChars(ln, p, 2, pad+p*w, pad+i*h+asc);
                    p=p+1; comment=1; continue;
                }
                if (comment>0) {
                    gr.setColor(comment==1 ? colorBlockComment : colorJavadoc);
                    if (c=='*' && p<ln.length-1 && ln[p+1]=='/') {
                       gr.drawChars(ln, p, 2, pad+p*w, pad+i*h+asc);
                       p=p+1; comment=0; gr.setColor(colorNormal);
                    } else {
                       gr.drawChars(ln, p, 1, pad+p*w, pad+i*h+asc);
                    }
                    continue;
                }
                if (c>='0' && c<='9') {
                    int old=p; p++; while(p<ln.length) if (ln[p]>='0' && ln[p]<='9') p++; else break;
                    gr.setColor(colorNumber);
                    gr.drawChars(ln, old, p-old, pad+old*w, pad+i*h+asc);
                    gr.setColor(colorNormal);
                    p--;
                    continue;
                }
                if (myIdenStart(c)) {
                    int old=p; p++; while(p<ln.length) if (myIden(ln[p])) p++; else break;
                    boolean isKey = myIsKeyword(ln,old,p-old);
                    if (isKey) gr.setFont(bold);
                    gr.setColor(isKey ? colorKeyword : colorIden);
                    gr.drawChars(ln, old, p-old, pad+old*w, pad+i*h+asc);
                    gr.setColor(colorNormal);
                    if (isKey) gr.setFont(font);
                    p--;
                    continue;
                }
                gr.setFont(bold); gr.drawChars(ln, p, 1, pad+p*w, pad+i*h+asc); gr.setFont(font);
            }
        }
        gr.setColor(Color.BLUE);
        gr.drawLine(pad+x*w-1, pad+y*h, pad+x*w-1, pad+y*h+h-1);
        gr.drawLine(pad+x*w  , pad+y*h, pad+x*w  , pad+y*h+h-1);
        gr.setColor(oldColor);
        gr.setFont(oldFont);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                try {
                String text = Util.readAll("/zweb/zweb/w/p/public/def.als");
                JFrame jf = new JFrame("Demo");
                jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                jf.setLayout(new BorderLayout());
                Component area = (1==1) ? new OurTextArea(text) : new JTextArea(text);
                JScrollPane scroll = new JScrollPane(area, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
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
