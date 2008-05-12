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

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;
import javax.swing.JTextPane;
import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.undo.UndoableEdit;

/**
 * Graphical syntax-highlighting editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

public final class OurTextArea extends JTextPane {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** This flag indicates whether syntax highlighting is currently enabled or not. */
    private static boolean enabled = true;

    /** This stores the set of all OurTextArea objects ever constructed; stale objects will be removed automatically. */
    private static final WeakHashMap<OurTextArea,Object> all = new WeakHashMap<OurTextArea,Object>();

    /** This stores the timer that performs timer-based syntax highlighting. */
    private static Timer timer = null;

    /** The most recent keyboard activity in any TextArea widget. */
    private static long latest = 0;

    /** Disabled syntax highlighting. */
    public static void myDisabledHighlighting() {
        if (!enabled) return;
        enabled = false;
        for(OurTextArea x:all.keySet()) if (x.doc!=null) {
            x.doc.setCharacterAttributes(0, x.doc.getLength(), x.styleNormal, true);
            x.needReApply = false;
        }
    }

    /** Enable syntax highlighting; the flag indicates whether we will use a timer or an EventQueue for the coloring. */
    public static void myEnableHighlighting() {
        if (timer==null) {
           timer = new Timer(400, new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                if (!enabled || e.getWhen()<latest || e.getWhen()-latest < 800) return;
                for(OurTextArea x:all.keySet()) if (x.doc!=null && x.needReApply) x.myReapplyAll();
              }
           });
           timer.start();
        }
        if (enabled) return;
        enabled = true;
        for(OurTextArea x:all.keySet()) if (x.doc!=null) x.myReapplyAll();
    }

    /** This stores the currently recognized set of keywords. */
    private final String[] keywords = new String[] {"abstract", "all", "and", "as", "assert", "but", "check", "disj",
      "disjoint", "else", "enum", "exactly", "exh", "exhaustive", "expect", "extends", "fact", "for", "fun", "iden",
      "iff", "implies", "in", "Int", "int", "let", "lone", "module", "no", "none", "not", "one", "open", "or", "part",
      "partition", "pred", "private", "run", "seq", "set", "sig", "some", "sum", "this", "univ"
    };

    /** Returns true if "c" can be the start of an identifier. */
    private static boolean myIdenStart(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || c=='$'; }

    /** Returns true if "c" can be in the middle or the end of an identifier. */
    private static boolean myIden(char c) { return (c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9') || c=='$' || c=='\'' || c=='\"'; }

    /** Returns true if c[start..start+len-1] matches one of the reserved keyword. */
    private boolean myIsKeyword(String array, int start, int len) {
        again:
        for(int i=keywords.length-1; i>=0; i--) {
            String str=keywords[i];
            if (str.length() != len) continue;
            for(int j=0; j<len; j++) if (str.charAt(j)!=array.charAt(start+j)) continue again;
            return true;
        }
        return false;
    }

    /** The attribute set for defining TAB stops. */
    private SimpleAttributeSet tabAttribute = null;

    /** The various style to use when displaying text in the text area. */
    private final Style styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleIden, styleSymbol;

    /** The styled document being displayed. */
    private final StyledDocument doc;

    /** Stores the list of UndoableEditListeners registered with this widget. */
    private final List<UndoableEditListener> undoListeners = new ArrayList<UndoableEditListener>();

    /** Add the given listener to the list of UndoableEditListeners registered with this widget. */
    public void myAddUndoableEditListener(UndoableEditListener listener) {
        for(int i=0; i<undoListeners.size(); i++) if (undoListeners.get(i)==listener) return;
        undoListeners.add(listener);
    }

    /** Implements the core traversal logic in getLineStartOffset, getLineCount, and getLineOfOffset. */
    private static int myHelper(String text, int action, int target) throws BadLocationException {
        final int n=text.length();
        if (action==3) if (target<0 || target>text.length()) throw new BadLocationException("", target);
        for(int i=0, line=0; i<=n; line++) {
            // invariant #1:  line == the number of lines we've seen already before text[i]
            // invariant #2:  i==0 or text[i-1]=='\n'
            int j = (i>=n) ? n : text.indexOf('\n',i); if (j<0) j=n; // offset of the end of this line
            if (action==1 && line==target) return i;
            if (action==2 && j==n) return line+1;
            if (action==3 && target>=i && target<=j) return line;
            i=j+1;
        }
        throw new BadLocationException("", target);
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineStartOffset(int) getLineStartOffset} method. */
    public int getLineStartOffset(int line) throws BadLocationException { return myHelper(getText(), 1, line); }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineCount() getLineCount} method. */
    public int getLineCount() { try {return myHelper(getText(), 2, 0);} catch(BadLocationException ex) {return 0;} }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineOfOffset(int) getLineOfOffset} method. */
    public int getLineOfOffset(int offset) throws BadLocationException { return myHelper(getText(), 3, offset); }

    /** Constructs a text area widget. */
    public OurTextArea(String text, String fontFamily, int fontSize, int tabSize) {
       super();
       all.put(this, Boolean.FALSE);
       // This customized StyledEditorKit prevents line-wrapping up to 30000 pixels wide.
       // 30000 is a good number; value higher than about 32768 will cause errors.
       final ViewFactory defaultFactory = (new StyledEditorKit()).getViewFactory();
       setEditorKit(new StyledEditorKit() {
          private static final long serialVersionUID = 1L;
          @Override public final ViewFactory getViewFactory() {
            return new ViewFactory() {
              public final View create(Element x) {
                 if (!AbstractDocument.SectionElementName.equals(x.getName())) return defaultFactory.create(x);
                 return new BoxView(x, View.Y_AXIS) {
                    @Override public final float getMinimumSpan(int axis) { return super.getPreferredSpan(axis); }
                    @Override public final void layout(int width,int height) { super.layout(30000, height); }
                 };
              }
            };
          }
       });
       doc = getStyledDocument();
       styleNormal = doc.addStyle("N", null);
       StyleConstants.setBold      (styleNormal, false);
       StyleConstants.setForeground(styleNormal, Color.BLACK);
       StyleConstants.setFontFamily(styleNormal, fontFamily);
       StyleConstants.setFontSize  (styleNormal, fontSize);
       StyleConstants.setBold      (styleSymbol         = doc.addStyle("S", styleNormal), true);
       StyleConstants.setForeground(styleKeyword        = doc.addStyle("K", styleSymbol), new Color(30, 30, 168));
       StyleConstants.setForeground(styleNumber         = doc.addStyle("9", styleSymbol), new Color(168,10,10));
       StyleConstants.setForeground(styleIden           = doc.addStyle("I", styleNormal), Color.BLACK);
       StyleConstants.setForeground(styleComment        = doc.addStyle("0", styleNormal), new Color(10, 148, 10));
       StyleConstants.setForeground(styleBlockComment   = doc.addStyle("1", styleNormal), new Color(10, 148, 10));
       StyleConstants.setForeground(styleJavadocComment = doc.addStyle("2", styleSymbol), new Color(10, 148, 10));
       mostRecentFont = new Font(fontFamily, Font.PLAIN, fontSize);
       setTabSize(tabSize);
       if (text.length()>0) setText(text);
       addKeyListener(new KeyListener() {
           public void keyPressed(KeyEvent e) { latest = e.getWhen(); }
           public void keyReleased(KeyEvent e) { latest = e.getWhen(); }
           public void keyTyped(KeyEvent e) { latest = e.getWhen(); }
       });
       getDocument().addDocumentListener(new DocumentListener() {
          public void changedUpdate(DocumentEvent e) { }
          public void insertUpdate(DocumentEvent e) { removeUpdate(e); }
          public void removeUpdate(DocumentEvent e) {
              if (e instanceof UndoableEdit) {
                 UndoableEditEvent event = new UndoableEditEvent(this, (UndoableEdit)e);
                 for(UndoableEditListener listener: undoListeners) listener.undoableEditHappened(event);
              }
              needReApply = true;
          }
       });
       if (enabled && timer==null) myEnableHighlighting();
    }

    /** True iff the text area now needs to reapply styles. */
    private boolean needReApply = true;

    /** {@inheritDoc} */
    @Override public void setText(String text) {
        super.setText("");
        if (text.length()==0) return;
        try { doc.insertString(0, text, styleNormal); setCaretPosition(0); myReapplyAll(); } catch(BadLocationException ex) { }
    }

    /** Caches the most recent font. */
    private Font mostRecentFont;

    /** Caches the most recent tab size. */
    private int tabSize;

    /** {@inheritDoc} */
    @Override public void setFont(Font font) {
        if (doc==null) return;
        if (this.mostRecentFont == font) return; else this.mostRecentFont = font;
        for(Style s: new Style[]{styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleIden, styleSymbol}) {
           StyleConstants.setFontFamily(s, font.getFamily());
           StyleConstants.setFontSize(s, font.getSize());
        }
        int oldTabSize = tabSize++;
        setTabSize(oldTabSize); // forces the recomputation of the tab positions based on the new font
        myReapplyAll();
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setTabSize(int) setTabSize} method. */
    public void setTabSize(int tab) {
        if (doc==null) return;
        if (tab<1) tab=1; else if (tab>100) tab=100;
        if (this.tabSize == tab) return; else this.tabSize = tab;
        int gap = (tab * getFontMetrics(mostRecentFont).charWidth('X') * 2) / 2;
        tabAttribute = new SimpleAttributeSet();
        final TabStop[] pos = new TabStop[100];
        for(int i=0; i<100; i++) { pos[i] = new TabStop(i*gap+gap); }
        final TabSet tabSet = new TabSet(pos);
        StyleConstants.setTabSet(tabAttribute, tabSet);
        doc.setParagraphAttributes(0, doc.getLength(), tabAttribute, false);
    }

    /** Apply appropriate styles to the entire text. */
    private void myReapplyAll() {
        if (doc==null || !enabled) return;
        String txt=getText();
        int comment=0, n=txt.length();
        doc.setParagraphAttributes(0, n, tabAttribute, true);
        for(int i=0; i<n; i++) {
            char c = txt.charAt(i);
            if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
               int d = txt.indexOf('\n', i);
               doc.setCharacterAttributes(i, d<0 ? (n-i) : (d-i), styleComment, true);
               if (d<0) return; else i=d;
            } else if
                ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               ||(comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
                doc.setCharacterAttributes(i, 3, styleJavadocComment, true); i=i+2; comment=2;
            } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
                doc.setCharacterAttributes(i, 2, styleBlockComment, true); i++; comment=1;
            } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
                doc.setCharacterAttributes(i, 2, comment==1 ? styleBlockComment : styleJavadocComment, true); i++; comment=0;
            } else if (comment>0) {
                doc.setCharacterAttributes(i, 1, comment==1 ? styleBlockComment : styleJavadocComment, true);
            } else if ((c>='0' && c<='9') || myIdenStart(c)) {
               int oldi=i; i++; while(i<n && myIden(txt.charAt(i))) i++;
               Style s = styleNormal;
               if (c>='0' && c<='9') s=styleNumber; else if (myIsKeyword(txt, oldi, i-oldi)) s=styleKeyword;
               doc.setCharacterAttributes(oldi, i-oldi, s, true);
               i--;
            } else {
               int oldi=i; i++; while(i<n && txt.charAt(i)!='-' && txt.charAt(i)!='/' && !myIden(txt.charAt(i))) i++;
               doc.setCharacterAttributes(oldi, i-oldi, styleSymbol, true);
               i--;
            }
        }
        getInputAttributes().removeAttribute(getInputAttributes());
        getInputAttributes().addAttributes(styleNormal);
        needReApply = false;
    }

    /*
    public static void main(String[] args) throws Exception {
      // a set of tests to verify our implementation of getLineCount(), getLineStartOffset(), and getLineOfOffset()
      for(String a: new String[]{"", "a", "ab", "abc"})
      for(String b: new String[]{"", "\n", "d", "\nd", "de", "\nde", "def", "\ndef"})
      for(String c: new String[]{"", "\n", "x", "\nx", "xy", "\nxy", "xyz", "\nxyz"})
      for(String d: new String[]{"", "\n", "x", "\nx", "xy", "\nxy", "xyz", "\nxyz"}) {
        JTextArea jt = new JTextArea(a+b+c+d);
        String text = jt.getText();
        int ans1 = jt.getLineCount(), ans2 = myHelper(text, 2, 0);
        if (ans1 != ans2) throw new RuntimeException("Diff2: ans1="+ans1+" ans2="+ans2);
        try { myHelper(text, 1, -1); throw new RuntimeException("Exception expected"); } catch(BadLocationException ex) { }
        try { myHelper(text, 1, ans1); throw new RuntimeException("Exception expected"); } catch(BadLocationException ex) { }
        for(int line=ans1-1; line>=0; line--) {
            ans1=jt.getLineStartOffset(line); ans2=myHelper(text, 1, line);
            if (ans1 != ans2) throw new RuntimeException("Diff1: line="+line+" ans1="+ans1+" ans2="+ans2);
        }
        for(int offset=0; offset<=text.length(); offset++) {
            ans1=jt.getLineOfOffset(offset); ans2=myHelper(text, 3, offset);
            if (ans1 != ans2) throw new RuntimeException("Diff2: offset="+offset+" ans1="+ans1+" ans2="+ans2);
        }
      }
      // this simply opens a new window and put the text area in the middle
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
           String text = ""; try { text=Util.readAll("/tmp/tests/test47.als"); } catch(IOException ex) { }
           JFrame jf = new JFrame("Demo");
           jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
           jf.setLayout(new BorderLayout());
           final OurTextArea area = new OurTextArea(text, "Monospaced", 18, 4);
           JScrollPane scroll = new JScrollPane(area, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
           jf.add(scroll, BorderLayout.CENTER);
           jf.pack();
           jf.setLocation(500,50);
           jf.setSize(850,450);
           jf.setVisible(true);
           area.requestFocusInWindow();
        }
      });
    }
    */
}
