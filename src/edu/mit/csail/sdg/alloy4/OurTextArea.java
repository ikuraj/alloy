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
import java.awt.Font;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * Graphical syntax-highlighting editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread
 */

public final class OurTextArea extends JTextPane {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

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

    /** The various style to use when displaying text in the text area. */
    private final Style styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleIden, styleSymbol;

    /** The styled document being displayed. */
    private final StyledDocument doc;

    /** Constructs a text area widget. */
    public OurTextArea(String text, String fontFamily, int fontSize) {
       super();
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
       StyleConstants.setBold(styleNormal, false);
       StyleConstants.setForeground(styleNormal, Color.BLACK);
       StyleConstants.setFontFamily(styleNormal, fontFamily);
       StyleConstants.setFontSize(styleNormal, fontSize);
       styleSymbol = doc.addStyle("S", styleNormal); StyleConstants.setBold(styleSymbol, true);
       styleIden = doc.addStyle("I", styleNormal); StyleConstants.setForeground(styleIden, Color.BLACK);
       styleNumber = doc.addStyle("9", styleNormal); StyleConstants.setForeground(styleNumber, Color.RED);
       styleKeyword = doc.addStyle("K", styleNormal); StyleConstants.setForeground(styleKeyword, new Color(30, 30, 168)); StyleConstants.setBold(styleKeyword, true);
       styleComment = doc.addStyle("0", styleNormal); StyleConstants.setForeground(styleComment, new Color(30, 168, 30));
       styleBlockComment = doc.addStyle("1", styleNormal); StyleConstants.setForeground(styleBlockComment, new Color(30, 168, 30));
       styleJavadocComment = doc.addStyle("2", styleNormal); StyleConstants.setForeground(styleJavadocComment, new Color(210, 30, 30));
       if (text.length()>0) setText(text);
       final Runnable rerun = new Runnable() {
           public void run() { myReapplyAll(); }
       };
       getDocument().addDocumentListener(new DocumentListener() {
          public void changedUpdate(DocumentEvent e) { }
          public void insertUpdate(DocumentEvent e) { SwingUtilities.invokeLater(rerun); }
          public void removeUpdate(DocumentEvent e) { SwingUtilities.invokeLater(rerun); }
       });
    }

    /** {@inheritDoc} */
    @Override public void setText(String text) {
        super.setText("");
        if (text.length()==0) return;
        try { doc.insertString(0, text, styleNormal); setCaretPosition(0); myReapplyAll(); } catch(BadLocationException ex) { }
    }

    /** {@inheritDoc} */
    @Override public void setFont(Font font) {
        super.setFont(font);
        if (styleNormal==null) return; // This can happen, since the parent's constructor may call setFont!
        for(Style s: new Style[]{styleNormal, styleNumber, styleKeyword, styleComment, styleBlockComment, styleJavadocComment, styleIden, styleSymbol}) {
           StyleConstants.setFontFamily(s, font.getFamily());
           StyleConstants.setFontSize(s, font.getSize());
        }
        myReapplyAll();
    }

    /** Apply the given style to the section from text[start] up to but excluding text[start+len] */
    private void myReapply(int start, int len, Style s) {
        Element elem = doc.getCharacterElement(start);
        //int end = elem.getEndOffset();
        //System.out.println(elem.getStartOffset()+".."+elem.getEndOffset()+" : "+elem.getAttributes()); System.out.flush();
        //System.out.println(elem.getClass()); System.out.flush();
        //System.out.println(elem.getAttributes().getClass()); System.out.flush();
        MutableAttributeSet atr = (MutableAttributeSet)(elem.getAttributes());
        boolean flag = atr.isEqual(s); flag=false;
        if (!flag) { /*System.out.print("X");*/ doc.setCharacterAttributes(start, len, s, true); }
        //else { System.out.print("."); }
    }

    /** Apply appropriate styles to the entire text. */
    private void myReapplyAll() {
        String txt=getText();
        int comment=0, n=txt.length();
        myReapply(0, n, styleNormal);
        for(int i=0; i<n; i++) {
            char c = txt.charAt(i);
            if (c==' ' || c=='\t') continue;
            if (comment==0 && (c=='/' || c=='-') && i<n-1 && txt.charAt(i+1)==c) {
               int d = txt.indexOf('\n', i);
               myReapply(i, d<0 ? (n-i) : (d-i), styleComment);
               if (d<0) return; else i=d;
            } else if
                ((comment==0 && c=='/' && i<n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*' && txt.charAt(i+3)!='/')
               ||(comment==0 && c=='/' && i==n-3 && txt.charAt(i+1)=='*' && txt.charAt(i+2)=='*')) {
                myReapply(i, 3, styleJavadocComment); i=i+2; comment=2;
            } else if (comment==0 && c=='/' && i<n-1 && txt.charAt(i+1)=='*') {
                myReapply(i, 2, styleBlockComment); i++; comment=1;
            } else if (comment>0 && c=='*' && i<n-1 && txt.charAt(i+1)=='/') {
                myReapply(i, 2, comment==1 ? styleBlockComment : styleJavadocComment); i++; comment=0;
            } else if (comment>0) {
                myReapply(i, 1, comment==1 ? styleBlockComment : styleJavadocComment);
            } else if ((c>='0' && c<='9') || myIdenStart(c)) {
               int oldi=i; i++; while(i<n && myIden(txt.charAt(i))) i++;
               Style s = styleNormal;
               if (c>='0' && c<='9') s=styleNumber; else if (myIsKeyword(txt, oldi, i-oldi)) s=styleKeyword;
               myReapply(oldi, i-oldi, s);
               i--;
            } else {
               int oldi=i; i++; while(i<n && txt.charAt(i)!='-' && txt.charAt(i)!='/' && txt.charAt(i)>=32 && !myIden(txt.charAt(i))) i++;
               myReapply(oldi, i-oldi, styleSymbol);
               i--;
            }
        }
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineOfOffset(int) getLineOfOffset} method. */
    public int getLineOfOffset(int offset) throws BadLocationException {
        String text=getText();
        int i=0, n=text.length(), y=0;
        if (offset<0 || offset>=n) throw new BadLocationException("", offset);
        while(i<n) {
            int j=text.indexOf('\n',i);
            if (j<0) return y;
            if (offset>=i && offset<=j) return y;
            i=j+1; y++;
        }
        return y;
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineStartOffset(int) getLineStartOffset} method. */
    public int getLineStartOffset(int line) throws BadLocationException {
        String text=getText();
        int i=0, n=text.length(), y=0;
        while(true) {
            if (y==line) return i;
            if (i>=n) i=(-1); else i=text.indexOf('\n',i);
            if (line<0 || i<0) throw new BadLocationException("", line);
            i++; y++;
        }
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineEndOffset(int) getLineEndOffset} method. */
    public int getLineEndOffset(int line) throws BadLocationException {
        String text=getText();
        int i=0, n=text.length(), y=0;
        while(true) {
            if (y==line) return i;
            if (i>=n) i=(-1); else i=text.indexOf('\n',i);
            if (line<0 || i<0) throw new BadLocationException("", line);
            i++; y++;
        }
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#getLineCount() getLineCount} method. */
    public int getLineCount() {
        String text=getText();
        int i=0, n=text.length();
        for(int line=0; ; line++) {
            i = text.indexOf('\n', i);
            if (i<0) return line+1;
            if (i==n-1) return line+2;
            i = i + 1;
        }
    }

    /** Implements JTextArea's {@link javax.swing.JTextArea#setTabSize(int) setTabSize} method. */
    public void setTabSize(int tab) { }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                try {
                    String text = Util.readAll("/zweb/zweb/w/p/public/def.als");
                    JFrame jf = new JFrame("Demo");
                    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                    jf.setLayout(new BorderLayout());
                    JTextPane area = new OurTextArea(text+"sig abc // def", "Monospaced", 10);
                    JScrollPane scroll = new JScrollPane(area, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    jf.add(scroll);
                    jf.pack();
                    jf.setLocation(50,50);
                    jf.setSize(1200,250);
                    jf.setVisible(true);
                    area.requestFocusInWindow();
                } catch(IOException ex) { }
            }
        });
    }


    // MISSING FEATURES:
    // 1) Undo listeners
    // 2) Edit too slow
    // 3) Highlighting
}
