package edu.mit.csail.sdg.alloy4;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Highlighter.HighlightPainter;
import javax.swing.undo.UndoManager;
import static java.awt.Color.BLACK;
import static java.awt.Color.WHITE;

/**
 * Graphical tabbed editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT thread (except "highlight()" method, which can be called by any thread)
 *
 * <p><b>Invariant</b>: list.get(i).filename must not contain duplicate entries.
 */

public final class OurTabbedEditor {

    /** Background color for the list of tabs. */
    private static final Color gray=new Color(.9f, .9f, .9f);

    /** Background color for an inactive tab. */
    private static final Color inactive=new Color(.8f, .8f, .8f);

    /** Border color for each tab. */
    private static final Color border=Color.LIGHT_GRAY;

    /** This defines notifyChange and notifyFocusGained events this tabbed editor may send to the parent. */
    public interface Parent {
        /** This method is called when a tab is added or removed, or if the text is modified, or if cursor in text area is moved. */
        public void notifyChange();
        /** This method is called when a tab in this tabbed editor gains the focus. */
        public void notifyFocusGained();
    }

    /** The font to use in the JTextArea */
    private Font font;

    /** The tabsize to use in the JTextArea */
    private int tabSize;

    /** The parent. */
    private final Parent parent;

    /** The parent's JFrame. */
    private final JFrame parentFrame;

    /** The entire area (Clickable tabs PLUS the text editor area). */
    private final JPanel frame;

    /** The list of clickable tabs. */
    private final JPanel content;

    /** The scroller that wraps around this.content */
    private final JScrollPane scroller;

    /** The list of tabs. */
    private final List<Tab> list = new ArrayList<Tab>();

    /** The currently selected tab from 0 to list.size()-1 (This value is 0 if there are no tabs) */
    private int me=0;

    /** This defines the data associated with each tab. */
    private static final class Tab {
        /** The Tab on top. */
        public JPanel tab;
        /** The JPanel containing the decoration around the tab. */
        public JLabel label;
        /** The text area. */
        public final JTextArea body;
        /** The ScrollPane containing the text area. */
        public final JScrollPane scrolledbody;
        /** The undo manager associated with this text area. */
        public final UndoManager undo=new UndoManager();
        /** The highlighter associated with this text area. */
        public final Highlighter highlighter=new DefaultHighlighter();
        /** The filename; always nonempty, canonical, absolute, and unique among all Tab objects in this editor. */
        public String filename;
        /** True if this is associated with an actual file; false if it is still an "untitled" tab. */
        public boolean isFile;
        /** True if the JTextArea has been modified since it was last loaded or saved. */
        public boolean modified=false;
        /** Constructs a new Tab */
        private Tab(JPanel tab, JLabel label, JTextArea body, String filename, boolean isFile) {
            this.tab=tab;
            this.label=label;
            this.body=body;
            scrolledbody=new JScrollPane(body, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrolledbody.setBorder(new EmptyBorder(0,0,0,0));
            undo.setLimit(100);
            body.setHighlighter(highlighter);
            this.filename=filename;
            this.isFile=isFile;
        }
    }

    /** The anonymous filename to give to the next unnamed text buffer. */
    private int nextNumber=1;

    /** Whether it is allowed to read/write files. */
    private boolean allowIO=true;
    /** Turn on the IO ability. */
    public void enableIO() { allowIO=true; }
    /** Turn off the IO ability. */
    public void disableIO() { allowIO=false; }

    /** The HighlightPainter to use to paint the highlights. */
    private final HighlightPainter highlightPainter=new Highlighter.HighlightPainter() {
        private final Color color = new Color(0.9f, 0.4f, 0.4f);
        public void paint(Graphics g, int start, int end, Shape shape, JTextComponent text) {
            Color oldcolor=g.getColor();
            g.setColor(color);
            try {
                Rectangle box = shape.getBounds();
                Rectangle a = text.getUI().modelToView(text,start);
                Rectangle b = text.getUI().modelToView(text,end);
                if (a.y == b.y) {
                    // same line; if start==end, then draw all the way to the right edge.
                    Rectangle r = a.union(b);
                    g.fillRect(r.x, r.y, (r.width<=1 ? (box.x+box.width-r.x) : r.width), r.height);
                } else {
                    // On the first line, draw from "start" and extends to the right-most edge
                    g.fillRect(a.x, a.y, box.x+box.width-a.x, a.height);
                    // If there is a line between first and third, then draw that
                    if (a.y+a.height != b.y) g.fillRect(box.x, a.y+a.height, box.width, b.y-(a.y+a.height));
                    // Draw the last line
                    g.fillRect(box.x, b.y, b.x-box.x, b.height);
                }
            } catch (BadLocationException e) { }
            g.setColor(oldcolor);
        }
    };

    /** Constructs a tabbed editor pane. */
    public OurTabbedEditor(final Parent parent, final JFrame parentFrame, final Font font, final int tabSize) {
        this.parent=parent;
        this.parentFrame=parentFrame;
        this.font=font;
        this.tabSize=tabSize;
        JPanel glue = OurUtil.makeHB(new Object[]{null});
        glue.setBorder(new OurBorder(null,null,border,null));
        content=OurUtil.makeHB(glue);
        if (!Util.onMac()) {content.setOpaque(true); content.setBackground(gray);}
        scroller=new JScrollPane(content, JScrollPane.VERTICAL_SCROLLBAR_NEVER, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroller.setFocusable(false);
        scroller.setBorder(new EmptyBorder(0,0,0,0));
        frame=new JPanel();
        frame.setBorder(new EmptyBorder(0,0,0,0));
        frame.setLayout(new BorderLayout());
        frame.add(scroller, BorderLayout.NORTH);
        frame.add(new JPanel(), BorderLayout.CENTER); // Create an "initial" content area beneath the list-of-tabs
        newTab();
        scroller.addComponentListener(new ComponentListener() {
            public void componentResized(ComponentEvent e) {setSelectedIndex(me);}
            public void componentMoved(ComponentEvent e) {setSelectedIndex(me);}
            public void componentShown(ComponentEvent e) {setSelectedIndex(me);}
            public void componentHidden(ComponentEvent e) {}
        });
    }

    /** Create a new anonymous file name. */
    private String newname() {
        again:
        while(true) {
            String filename=Util.canon("Untitled "+nextNumber+".als");
            nextNumber++;
            for(Tab t:list) if (t.filename.equals(filename)) continue again;
            return filename;
        }
    }

    /** Create a new empty tab. */
    public void newTab() { newTab(newname(), "", false); }

    /**
     * Create a new tab with the given filename and initial content.
     * <p> Note: if a text buffer with that filename already exists, we will switch to it and ignore "content"
     */
    public boolean newTab(String filename) {
        filename=Util.canon(filename);
        if (switchToFilename(filename)) return true;
        try {
            String content = allowIO ? Util.readAll(filename) : "";
            newTab(filename, content, true);
            return true;
        }
        catch(IOException e) {
            return false;
        }
    }

    /**
     * Create a new tab with the given filename and initial content.
     * <p> Note: if a text buffer with that filename already exists, we will switch to it and ignore "content"
     */
    private void newTab(String filename, String fileContent, boolean isFile) {
        // If exists, then switch to that tab directly
        if (switchToFilename(filename)) return;
        // Make the tab on top
        final JLabel x=new JLabel("");
        x.setFont(OurUtil.getVizFont().deriveFont(Font.BOLD));
        x.setOpaque(true);
        x.setBorder(new OurBorder(border, border, WHITE, border));
        x.setBackground(WHITE);
        x.setForeground(BLACK);
        x.addMouseListener(new MouseListener() {
            public void mousePressed(MouseEvent e) {
                for(int i=0;i<list.size();i++) if (list.get(i).label==x) {setSelectedIndex(i); break;}
            }
            public void mouseClicked(MouseEvent e) {}
            public void mouseReleased(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        JPanel h4=OurUtil.makeBox(4, 1); h4.setBorder(new OurBorder(null,null,border,null));
        JPanel h2=OurUtil.makeBox(3, 1); h2.setBorder(new OurBorder(null,null,border,null));
        JPanel xx;
        if (Util.onMac())
            xx=OurUtil.makeVL(null, 2, OurUtil.makeHB(h4, x, h2));
        else
            xx=OurUtil.makeVL(null, 2, OurUtil.makeHB(h4, x, h2, gray), gray);
        xx.setAlignmentX(0.0f);
        xx.setAlignmentY(1.0f);
        // Make the JTextArea
        final JTextArea text=new JTextArea(fileContent);
        text.setBackground(Color.WHITE);
        text.setBorder(new EmptyBorder(1,1,1,1));
        text.setLineWrap(false);
        text.setEditable(true);
        text.setTabSize(tabSize);
        text.setFont(font);
        if (!Util.onMac()) {
            text.getActionMap().put("my_copy", new AbstractAction("my_copy") {
                private static final long serialVersionUID = 1L;
                public void actionPerformed(ActionEvent e) { text.copy(); }
            });
            text.getActionMap().put("my_cut", new AbstractAction("my_cut") {
                private static final long serialVersionUID = 1L;
                public void actionPerformed(ActionEvent e) { text.cut(); }
            });
            text.getActionMap().put("my_paste", new AbstractAction("my_paste") {
                private static final long serialVersionUID = 1L;
                public void actionPerformed(ActionEvent e) { text.paste(); }
            });
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.CTRL_MASK), "my_copy");
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.SHIFT_MASK), "my_cut");
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.SHIFT_MASK), "my_paste");
        }
        text.getActionMap().put("my_next", new AbstractAction("my_next") {
            private static final long serialVersionUID = 1L;
            public void actionPerformed(ActionEvent e) {
                int j=(-1);
                for(int n=list.size(), i=0; i<n; i++) if (list.get(i).body==text) {j=i+1; if (j>=n) j=0; break;}
                if (j>=0) setSelectedIndex(j);
            }
        });
        text.getActionMap().put("my_prev", new AbstractAction("my_prev") {
            private static final long serialVersionUID = 1L;
            public void actionPerformed(ActionEvent e) {
                int j=(-1);
                for(int n=list.size(), i=0; i<n; i++) if (list.get(i).body==text) {j=i-1; if (j<0) j=n-1; break;}
                if (j>=0) setSelectedIndex(j);
            }
        });
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_MASK), "my_prev");
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_MASK), "my_next");
        // Add everything
        content.add(xx, list.size());
        final Tab tab=new Tab(xx, x, text, filename, isFile);
        list.add(tab);
        setTitle(tab.label, filename);
        // Add these listeners last, to make sure this object is fully initialized first
        text.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) {parent.notifyChange();}
        });
        text.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) {parent.notifyFocusGained();}
            public final void focusLost(FocusEvent e) {}
        });
        text.getDocument().addDocumentListener(new DocumentListener() {
            public final void changedUpdate(DocumentEvent e) {
                tab.highlighter.removeAllHighlights();
                setTitle(tab.label, tab.filename+" *");
                tab.modified=true;
                parent.notifyChange();
            }
            public final void removeUpdate(DocumentEvent e) { changedUpdate(e); }
            public final void insertUpdate(DocumentEvent e) { changedUpdate(e); }
        });
        text.getDocument().addUndoableEditListener(new UndoableEditListener() {
            public final void undoableEditHappened(UndoableEditEvent event) { tab.undo.addEdit(event.getEdit()); }
        });
        if (isFile)
          for(int i=list.size()-1; i>=0; i--)
            if (!list.get(i).isFile && list.get(i).body.getText().trim().length()==0)
              { list.get(i).modified=false; close(i); break; } // So that we take over the rightmost untitled tab
        // Must call this method to switch to the new tab; and it will call parent.notifyChange() which is important
        setSelectedIndex(list.size()-1);
    }

    /**
     * If allowIO==false, return an empty map; otherwise return a map from each tab's filename to the text content in that tab.
     * Note: the returned map is modifiable, and the caller is free to do whatever with it
     * (the changes do NOT affect this OurTabbedEditor object)
     */
    public Map<String,String> takeSnapshot() {
        Map<String,String> map = new LinkedHashMap<String,String>();
        if (allowIO) for(Tab t:list) map.put(t.filename, t.body.getText());
        return map;
    }

    /** Returns the list of filenames corresponding to each text buffer. */
    public List<String> getFilenames() {
        List<String> ans=new ArrayList<String>(list.size());
        for(Tab t:list) ans.add(t.filename);
        return ans;
    }

    /** Return the filename of the current text buffer. */
    public String getFilename() { if (me>=0 && me<list.size()) return list.get(me).filename; else return ""; }

    /** Changes the tabsize in every text buffer. */
    public void setTabSize(int tabSize) { this.tabSize=tabSize; for(Tab t:list) t.body.setTabSize(tabSize); }

    /** Changes the font in every text buffer. */
    public void setFont(Font font) { this.font=font; for(Tab t:list) t.body.setFont(font); }

    /** Removes all highlights from the current text buffer. */
    public void removeAllHighlights() { if (me>=0 && me<list.size()) list.get(me).highlighter.removeAllHighlights(); }

    /** Returne ths entire JPanel of this tabbed text editor. */
    public Component getUI() { return frame; }

    /** Returns the JTextArea of the current text buffer. */
    public JTextArea text() { return (me>=0 && me<list.size()) ? list.get(me).body : new JTextArea(); }

    /** True if the current text buffer has 1 or more "undo" that it can perform. */
    public boolean canUndo() { return (me>=0 && me<list.size()) ? list.get(me).undo.canUndo() : false; }

    /** True if the current text buffer has 1 or more "redo" that it can perform. */
    public boolean canRedo() { return (me>=0 && me<list.size()) ? list.get(me).undo.canRedo() : false; }

    /** True if the i-th text buffer has been modified since it was last loaded/saved */
    public boolean modified(int i) { return (i>=0 && i<list.size()) ? list.get(i).modified : false; }

    /** True if the current text buffer has been modified since it was last loaded/saved */
    public boolean modified() { return (me>=0 && me<list.size()) ? list.get(me).modified : false; }

    /** True if the i-th text buffer corresponds to an actual file. */
    public boolean isFile(int i) { return (i>=0 && i<list.size()) ? list.get(i).isFile : false; }

    /** True if the current text buffer corresponds to an actual file. */
    public boolean isFile() { return (me>=0 && me<list.size()) ? list.get(me).isFile : false; }

    /** Perform "undo" on the current text buffer. */
    public void undo() { if (me>=0 && me<list.size()) list.get(me).undo.undo(); }

    /** Perform "redo" on the current text buffer. */
    public void redo() { if (me>=0 && me<list.size()) list.get(me).undo.redo(); }

    /**
     * Close the i-th tab (then create a new empty tab if there were no tabs remaining)
     *
     * If the text editor content is not modified since the last save, return true; otherwise, ask the user.
     * <p> If the user says to save it, we will attempt to save it, then return true iff the save succeeded.
     * <p> If the user says to discard it, this method returns true.
     * <p> If the user says to cancel it, this method returns false.
     */
    public boolean close(int i) {
        String filename=list.get(i).filename;
        if (allowIO && list.get(i).modified) {
            Boolean ans=OurDialog.askSaveDiscardCancel(parentFrame, "The file \""+getShorterTitle(filename)+"\"");
            if (ans==null) return false;
            if (ans.booleanValue()) if (!save(false)) return false;
        }
        list.get(i).body.setText("");
        if (list.size()==1) {
            list.get(i).undo.discardAllEdits();
            list.get(i).highlighter.removeAllHighlights();
            list.get(i).modified=false;
            if (list.get(i).isFile) { list.get(i).isFile=false; list.get(i).filename=newname(); }
            setTitle(list.get(i).label, list.get(i).filename);
        } else {
            content.remove(i);
            list.remove(i);
            if (me>=list.size()) me=list.size()-1;
        }
        // Must call this to change the active tab and call parent.notifyChange() (which is important)
        setSelectedIndex(me);
        return true;
    }

    /** Close the current tab (then create a new empty tab if there were no tabs remaining) */
    public void close() { if (me>=0 && me<list.size()) close(me); }

    /** Close every tab, then create a new empty tab. */
    public boolean closeAll() {
        for(int i=list.size()-1; i>=0; i--) if (list.get(i).modified==false) if (close(i)==false) return false;
        for(int i=list.size()-1; i>=0; i--) if (close(i)==false) return false;
        return true;
    }

    /** Save the current tab to a file. */
    public boolean save(boolean alwaysPickNewName) {
        if (!allowIO) return false;
        if (me<0 || me>=list.size()) return false;
        String filename=list.get(me).filename;
        if (list.get(me).isFile==false || alwaysPickNewName) {
            String start = Pref.Dir.get();
            if (!(new File(start)).isDirectory()) start=System.getProperty("user.home");
            File file=OurDialog.askFile(parentFrame, false, start, ".als", ".als files");
            if (file==null) return false;
            filename=Util.canon(file.getPath());
            if (file.exists() && !OurDialog.askOverwrite(parentFrame,filename)) return false;
        }
        if (saveAs(filename)) { Pref.Dir.set((new File(filename)).getParent()); return true; }
        return false;
    }

    /** Save the current tab to a file. */
    public boolean saveAs(String filename) {
        if (!allowIO) return false;
        if (me<0 || me>=list.size()) return false;
        filename=Util.canon(filename);
        for(int j=0; j<list.size(); j++) {
            if (j!=me && list.get(j).filename.equals(filename)) {
                OurDialog.alert(parentFrame, "Error. The filename \""+filename+"\"\nis already open in one of the tab.", "Error");
                return false;
            }
        }
        try {
            Util.writeAll(filename, text().getText());
        } catch (IOException e) {
            OurDialog.alert(parentFrame, "Error writing to the file \""+filename+"\"", "Error");
            return false;
        }
        filename=Util.canon(filename);
        setTitle(list.get(me).label, filename);
        list.get(me).filename=filename;
        list.get(me).modified=false;
        list.get(me).isFile=true;
        parent.notifyChange();
        return true;
    }

    /**
     * Highlights the text editor, based on the location information in the Err object.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void highlight(final Err e) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() { public final void run() { highlight(e); }});
            return;
        }
        if (allowIO && e.pos!=null && e.pos.y>0 && e.pos.x>0) try {
            String f=Util.canon(e.pos.filename);
            if (!switchToFilename(f)) {
                String content;
                try {content=Util.readAll(f);} catch(IOException ex) {return;} // Error not fatal, since we just won't show the file
                newTab(f, content, true);
            }
            int c=text().getLineStartOffset(e.pos.y-1)+e.pos.x-1;
            int d=text().getLineStartOffset(e.pos.y2-1)+e.pos.x2-1;
            list.get(me).highlighter.removeAllHighlights();
            list.get(me).highlighter.addHighlight(c, d+1, highlightPainter);
            text().setSelectionStart(c);
            text().setSelectionEnd(c);
            text().requestFocusInWindow();
            parent.notifyChange();
        } catch(BadLocationException ex) { }
    }

    /** Returns the number of tabs; always 1 or above. */
    public int getTabCount() { return list.size(); }

    /** Returns a short title for a filename. */
    private String getShorterTitle(String x) {
        int j=x.lastIndexOf('/'); if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('\\'); if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('.'); if (j>=0) x=x.substring(0,j);
        return x;
    }

    /** Changes the label of a JLabel. */
    private void setTitle(JLabel label, String x) {
        boolean modified = x.endsWith(" *");
        if (modified) x=x.substring(0, x.length()-2);
        label.setToolTipText(x);
        x=getShorterTitle(x);
        if (x.length()>12) x=x.substring(0,12)+"...";
        label.setText("  "+x+(modified?" *  ":"  "));
    }

    /** Switch the currently selected tab; returns false if no tab corresponds to that filename. */
    public boolean switchToFilename(String filename) {
        for(int i=0; i<list.size(); i++) if (list.get(i).filename.equals(filename)) {setSelectedIndex(i); return true;}
        return false;
    }

    /** Returns the currently selected tab. */
    public int getSelectedIndex() { return me; }

    /** Switch the currently selected tab (Note: it always calls parent.notifyChange()) */
    public void setSelectedIndex(final int i) {
        if (i<0 || i>=list.size()) return;
        frame.revalidate();
        me=i;
        for(int j=0; j<list.size(); j++) {
            JLabel x=list.get(j).label;
            x.setBorder(new OurBorder(border,border, j!=i?border:WHITE,border));
            x.setBackground(j!=i ? inactive : WHITE);
        }
        frame.removeAll();
        if (list.size()>1) frame.add(scroller, BorderLayout.NORTH);
        frame.add(list.get(me).scrolledbody, BorderLayout.CENTER);
        frame.repaint();
        parent.notifyChange();
        list.get(i).body.requestFocusInWindow();
        content.scrollRectToVisible(new Rectangle(0,0,0,0)); // Forces recalculation
        Point p=list.get(me).tab.getLocation();
        Dimension r=list.get(me).tab.getSize();
        content.scrollRectToVisible(new Rectangle(p.x, 0, r.width, 1));
    }
}
