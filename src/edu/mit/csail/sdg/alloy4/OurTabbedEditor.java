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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;
import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import static javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED;
import static javax.swing.JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED;
import static javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.JScrollPane.VERTICAL_SCROLLBAR_NEVER;
import static java.awt.Color.BLACK;
import static java.awt.Color.WHITE;
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
import javax.swing.undo.UndoManager;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/**
 * Graphical tabbed editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread (except "highlight()" method,
 * which can be called by any thread)
 *
 * <p><b>Invariant</b>: list.get(i).filename must not contain duplicate entries.
 */

public final class OurTabbedEditor {

    /** This defines notifyChange and notifyFocusGained events this tabbed editor may send to the parent. */
    public interface Parent {
        /** This method is called when a tab is added/removed, we switch to another tab, text is modified, or text cursor moved. */
        public void notifyChange ();
        /** This method is called when a tab in this tabbed editor gains the focus. */
        public void notifyFocusGained ();
    }

    /** This defines the data associated with each tab. */
    private static final class Tab {
        /** The JLabel on top. */
        private final JLabel label;
        /** The JPanel containing the decoration around the JLabel. */
        private final JPanel panel;
        /** The text area. */
        private final OurTextArea text;
        /** The ScrollPane containing the text area. */
        private final JScrollPane scroll;
        /** The undo manager associated with this text area. */
        private final UndoManager undo = new UndoManager();
        /** The highlighter associated with this text area. */
        private final Highlighter highlighter;
        /** The filename; always nonempty, canonical, absolute, and unique among all Tab objects in this editor. */
        private String filename;
        /** True if this is associated with an actual file; false if it is still an "untitled" tab. */
        private boolean isFile;
        /** True if the OurTextArea has been modified since it was last loaded or saved. */
        private boolean modified=false;
        /** Constructs a new Tab */
        private Tab(JPanel panel, JLabel label, OurTextArea text, String filename, boolean isFile) {
            this.panel=panel;
            this.label=label;
            this.text=text;
            this.highlighter=new DefaultHighlighter();
            text.setHighlighter(this.highlighter);
            this.scroll=new JScrollPane(text, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
            this.scroll.setBorder(new EmptyBorder(0,0,0,0));
            this.undo.setLimit(100);
            this.filename=filename;
            this.isFile=isFile;
        }
    }

    /** Background color for the list of tabs. */
    private static final Color GRAY=new Color(.9f, .9f, .9f);

    /** Background color for an inactive tab. */
    private static final Color INACTIVE=new Color(.8f, .8f, .8f);

    /** Background color for a inactive and highlighted tab. */
    private static final Color INACTIVE_HIGHLIGHTED=new Color(.7f, .5f, .5f);

    /** Foreground color for a active and highlighted tab. */
    private static final Color ACTIVE_HIGHLIGHTED=new Color(.5f, .2f, .2f);

    /** Default border color for each tab. */
    private static final Color BORDER=Color.LIGHT_GRAY;

    /** The font to use in the OurTextArea */
    private Font font;

    /** The tabsize to use in the OurTextArea */
    private int tabSize;

    /** The parent. */
    private final Parent parent;

    /** The parent's JFrame. */
    private final JFrame parentFrame;

    /** The entire area (Clickable tabs PLUS the text editor area). */
    private final JPanel frame;

    /** The list of clickable tabs. */
    private final JPanel tabBar;

    /** The scroller that wraps around this.tabbar */
    private final JScrollPane tabBarScroller;

    /** The list of tabs. */
    private final List<Tab> tabs=new ArrayList<Tab>();

    /** The currently selected tab from 0 to list.size()-1 (This value is 0 if there are no tabs) */
    private int me=0;

    /** The anonymous filename to give to the next unnamed text buffer. */
    private int nextNumber=1;

    /** The HighlightPainter to use to paint the highlights. */
    private static final class OurTabbedHighlighter implements Highlighter.HighlightPainter {
        private final Color color;
        public OurTabbedHighlighter(Color color) { this.color = color; }
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
                    // If there are line(s) between the first line and the last line, then draw them
                    if (a.y+a.height != b.y) {
                        g.fillRect(box.x, a.y+a.height, box.width, b.y-(a.y+a.height));
                    }
                    // Draw the last line
                    g.fillRect(box.x, b.y, b.x-box.x, b.height);
                }
            } catch (BadLocationException e) {
                // Failure to highlight is not fatal
            }
            g.setColor(oldcolor);
        }
    }

    /** Adjusts the background and foreground of all labels. */
    private void adjustLabelColor() {
        int i=me;
        for(int j=0; j<tabs.size(); j++) {
            Tab t=tabs.get(j);
            JLabel label=t.label;
            boolean hl=(t.highlighter!=null && t.highlighter.getHighlights().length>0);
            label.setBorder(new OurBorder(BORDER, BORDER, j!=i?BORDER:WHITE, BORDER));
            label.setBackground(j!=i ? (hl ? INACTIVE_HIGHLIGHTED : INACTIVE) : WHITE);
            label.setForeground(hl ? (j!=i ? BLACK : ACTIVE_HIGHLIGHTED) : BLACK);
        }
    }

    /** Removes all highlights from every text buffer. */
    public void removeAllHighlights() {
        for(Tab t:tabs) if (t.highlighter!=null) t.highlighter.removeAllHighlights();
        adjustLabelColor();
    }

    /** Switch to the i-th tab (Note: if successful, it will then always call parent.notifyChange()) */
    public void setSelectedIndex(final int i) {
        if (i<0 || i>=tabs.size()) return;
        me=i;
        frame.revalidate();
        adjustLabelColor();
        frame.removeAll();
        if (tabs.size()>1) frame.add(tabBarScroller, BorderLayout.NORTH);
        frame.add(tabs.get(me).scroll, BorderLayout.CENTER);
        frame.repaint();
        parent.notifyChange();
        tabs.get(me).text.requestFocusInWindow();
        tabBar.scrollRectToVisible(new Rectangle(0,0,0,0)); // Forces recalculation
        Point p=tabs.get(me).panel.getLocation();
        Dimension r=tabs.get(me).panel.getSize();
        tabBar.scrollRectToVisible(new Rectangle(p.x, 0, r.width+200, 1));
    }

    /** Switch to the tab with the given filename then return true; returns false if no tab has that filename. */
    public boolean switchToFilename(String filename) {
        for(int i=0; i<tabs.size(); i++) {
            if (tabs.get(i).filename.equals(filename)) {
                if (i!=me) setSelectedIndex(i);
                return true;
            }
        }
        return false;
    }

    /** Returns a short title for a filename. */
    private static String getShorterTitle(String x) {
        int j=x.lastIndexOf('/');
        if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('\\');
        if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('.');
        if (j>=0) x=x.substring(0,j);
        return x;
    }

    /** Changes the label of a JLabel. */
    private static void setTitle(JLabel label, String x) {
        boolean modified = x.endsWith(" *");
        if (modified) { x=x.substring(0, x.length()-2); }
        label.setToolTipText(x);
        x=getShorterTitle(x);
        if (x.length()>14) { x=x.substring(0,14)+"..."; }
        label.setText("  "+x+(modified?" *  ":"  "));
    }

    /** Refresh the given tab; return true if no error occurred. */
    public boolean refresh(int i) {
        if (i<0 || i>=tabs.size()) return true;
        Tab t = tabs.get(i);
        if (!t.isFile) return true; // a totally "untitled" text buffer does not have a on-disk file to refresh from
        if (t.modified) {
            boolean ans=OurDialog.yesno(parentFrame,
                "You have unsaved changes to \""+getShorterTitle(t.filename)
                +"\"\nAre you sure you wish to discard your changes and reload it from disk?");
            if (!ans) return false;
        }
        String content;
        try {
            content = Util.readAll(t.filename);
        } catch(Throwable ex) {
            OurDialog.alert(parentFrame, "Error reading the file \""+t.filename+"\"", "Error");
            return false;
        }
        int caret = t.text.getCaretPosition();
        t.text.setText(content);
        try { t.text.setCaretPosition(caret); } catch(IllegalArgumentException ex) { t.text.setCaretPosition(0); }
        t.modified=false;
        setTitle(t.label, t.filename);
        parent.notifyChange();
        return true;
    }

    /** Save the current tab to a file. */
    public boolean saveAs(String filename) {
        if (me<0 || me>=tabs.size()) return false;
        filename=Util.canon(filename);
        for(int i=0; i<tabs.size(); i++) {
            if (i!=me && tabs.get(i).filename.equals(filename)) {
                OurDialog.alert(parentFrame, "Error. The filename \""+filename+"\"\nis already open in one of the tab.", "Error");
                return false;
            }
        }
        try {
            Util.writeAll(filename, tabs.get(me).text.getText());
        } catch (Throwable e) {
            OurDialog.alert(parentFrame, "Error writing to the file \""+filename+"\"", "Error");
            return false;
        }
        filename=Util.canon(filename); // We need this since after writing, the canonical form of the filename may have changed
        setTitle(tabs.get(me).label, filename);
        tabs.get(me).filename=filename;
        tabs.get(me).modified=false;
        tabs.get(me).isFile=true;
        parent.notifyChange();
        return true;
    }

    /**
     * Save the current tab content to the file system.
     * @param alwaysPickNewName - if true, it will always pop up a File Selection dialog box to ask for the filename
     * @return null if an error occurred (otherwise, return the filename)
     */
    public String save(final boolean alwaysPickNewName) {
        if (me<0 || me>=tabs.size()) { return null; }
        String filename = tabs.get(me).filename;
        if (tabs.get(me).isFile==false || filename.startsWith(Util.jarPrefix()) || alwaysPickNewName) {
            File file=OurDialog.askFile(parentFrame, false, null, ".als", ".als files");
            if (file==null) return null;
            filename=Util.canon(file.getPath());
            if (file.exists() && !OurDialog.askOverwrite(parentFrame,filename)) { return null; }
        }
        if (saveAs(filename)) {
            Util.setCurrentDirectory(new File(filename).getParentFile());
            return Util.canon(filename);
        }
        return null;
    }

    /** Create a new anonymous file name. */
    private String newname() {
        again:
        while(true) {
            String filename=Util.canon("Untitled "+nextNumber+".als");
            nextNumber++;
            for(Tab t:tabs) {
                if (t.filename.equals(filename)) { continue again; }
            }
            return filename;
        }
    }

    /**
     * Close the i-th tab and then create a new empty tab if there were no tabs remaining.
     *
     * If the text editor content is not modified since the last save, return true; otherwise, ask the user.
     * <p> If the user says to save it, we will attempt to save it, then return true iff the save succeeded.
     * <p> If the user says to discard it, this method returns true.
     * <p> If the user says to cancel it, this method returns false.
     */
    private boolean close(int i) {
        removeAllHighlights();
        String filename=tabs.get(i).filename;
        if (tabs.get(i).modified) {
            Boolean ans=OurDialog.askSaveDiscardCancel(parentFrame, "The file \""+getShorterTitle(filename)+"\"");
            if (ans==null || (ans.booleanValue() && save(false)==null)) { return false; }
        }
        tabs.get(i).text.setText("");
        if (tabs.size()==1) {
            tabs.get(i).undo.discardAllEdits();
            tabs.get(i).modified=false;
            if (tabs.get(i).isFile) { tabs.get(i).isFile=false; tabs.get(i).filename=newname(); }
            setTitle(tabs.get(i).label, tabs.get(i).filename);
        } else {
            tabBar.remove(i);
            tabs.remove(i);
            if (me>=tabs.size()) { me=tabs.size()-1; }
        }
        // Must call this to change the active tab and call parent.notifyChange() (which is important)
        setSelectedIndex(me);
        return true;
    }

    /** Close the current tab (then create a new empty tab if there were no tabs remaining) */
    public void close() {
        if (me>=0 && me<tabs.size()) close(me);
    }

    /** Close every tab, then create a new empty tab. */
    public boolean closeAll() {
        // first attempt to close all the unmodified files
        for(int i=tabs.size()-1; i>=0; i--) {
            if (tabs.get(i).modified==false && close(i)==false) return false;
        }
        // then close the modified files one-by-one until an error occurred or if the user refuses to save nor discard a file
        for(int i=tabs.size()-1; i>=0; i--) {
            if (close(i)==false) return false;
        }
        return true;
    }

    /**
     * Create a new tab with the given filename and initial content.
     * <p> Note: if a text buffer with that filename already exists, we will switch to it and ignore "fileContent" and "isFile"
     */
    private void newTab(String filename, String fileContent, boolean isFile) {
        // If exists, then switch to that tab directly
        if (switchToFilename(filename)) return;
        // Make the new tab
        final JLabel lb = OurUtil.label(OurUtil.getVizFont().deriveFont(Font.BOLD), "");
        lb.setOpaque(true);
        lb.setBorder(new OurBorder(BORDER, BORDER, WHITE, BORDER));
        lb.setBackground(WHITE);
        lb.setForeground(BLACK);
        lb.addMouseListener(new MouseAdapter() {
            @Override public final void mousePressed(MouseEvent e) {
                for(int i=0; i<tabs.size(); i++) {
                    if (tabs.get(i).label==lb) {
                        setSelectedIndex(i);
                        break;
                    }
                }
            }
        });
        JPanel h4=OurUtil.makeH(4); h4.setBorder(new OurBorder(null,null,BORDER,null));
        JPanel h2=OurUtil.makeH(3); h2.setBorder(new OurBorder(null,null,BORDER,null));
        JPanel pan;
        if (Util.onMac()) {
            pan=OurUtil.makeVL(null, 2, OurUtil.makeHB(h4, lb, h2));
        } else {
            pan=OurUtil.makeVL(null, 2, OurUtil.makeHB(h4, lb, h2, GRAY), GRAY);
        }
        pan.setAlignmentX(0.0f);
        pan.setAlignmentY(1.0f);
        // Make the OurTextArea
        final OurTextArea text = new OurTextArea(Util.convertLineBreak(fileContent), font.getFamily(), font.getSize());
        text.setBackground(Color.WHITE);
        text.setBorder(new EmptyBorder(1,1,1,1));
        text.setTabSize(tabSize);
        if (!Util.onMac()) {
            text.getActionMap().put("my_copy", new AbstractAction("my_copy") {
                private static final long serialVersionUID = 1L;
                public final void actionPerformed(ActionEvent e) { text.copy(); parent.notifyChange(); }
            });
            text.getActionMap().put("my_cut", new AbstractAction("my_cut") {
                private static final long serialVersionUID = 1L;
                public final void actionPerformed(ActionEvent e) { text.cut(); parent.notifyChange(); }
            });
            text.getActionMap().put("my_paste", new AbstractAction("my_paste") {
                private static final long serialVersionUID = 1L;
                public final void actionPerformed(ActionEvent e) { text.paste(); parent.notifyChange(); }
            });
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.CTRL_MASK), "my_copy");
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.SHIFT_MASK), "my_cut");
            text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.SHIFT_MASK), "my_paste");
        }
        text.getActionMap().put("my_next", new AbstractAction("my_next") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) {
                int j=(-1);
                for(int n=tabs.size(), i=0; i<n; i++) {
                    if (tabs.get(i).text==text) {
                        j=i+1;
                        if (j>=n) j=0;
                        break;
                    }
                }
                if (j>=0) setSelectedIndex(j);
            }
        });
        text.getActionMap().put("my_prev", new AbstractAction("my_prev") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) {
                int j=(-1);
                for(int n=tabs.size(), i=0; i<n; i++) {
                    if (tabs.get(i).text==text) {
                        j=i-1;
                        if (j<0) j=n-1;
                        break;
                    }
                }
                if (j>=0) setSelectedIndex(j);
            }
        });
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_MASK), "my_prev");
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_MASK), "my_next");
        // Add everything
        tabBar.add(pan, tabs.size());
        final Tab tab=new Tab(pan, lb, text, filename, isFile);
        tabs.add(tab);
        setTitle(tab.label, filename);
        // Add these listeners last, to make sure this object is fully initialized first
        text.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) { parent.notifyChange(); }
        });
        text.addFocusListener(new FocusAdapter() {
            public final void focusGained(FocusEvent e) { parent.notifyFocusGained(); }
        });
        text.getDocument().addDocumentListener(new DocumentListener() {
            public final void insertUpdate(DocumentEvent e) {
                removeAllHighlights();
                setTitle(tab.label, tab.filename+" *");
                tab.modified=true;
                parent.notifyChange();
            }
            public final void removeUpdate(DocumentEvent e) { insertUpdate(e); }
            public final void changedUpdate(DocumentEvent e) { } // font changes are irrelevant
        });
        text.getDocument().addUndoableEditListener(new UndoableEditListener() {
            public final void undoableEditHappened(UndoableEditEvent event) { tab.undo.addEdit(event.getEdit()); }
        });
        // If it's a file, we want to remove the rightmost untitled empty tab
        if (isFile) {
            for(int i=tabs.size()-1; i>=0; i--) {
                if (!tabs.get(i).isFile && tabs.get(i).text.getText().trim().length()==0) {
                    tabs.get(i).modified=false;
                    close(i);
                    break;
                }
            }
        }
        // Must call this method to switch to the new tab; and it will call parent.notifyChange() which is important
        setSelectedIndex(tabs.size()-1);
    }

    /** Create a new empty tab. */
    public void newTab() {
        newTab(newname(), "", false);
    }

    /**
     * Create a new tab with the given filename.
     * <p> If a text buffer with that filename already exists, we will just switch to it.
     * <p> Otherwise, we will create a new empty tab (and if allowIO==true, we'll populate the new tab with the content from disk).
     */
    public void newTab(String filename) throws IOException {
        filename=Util.canon(filename);
        if (switchToFilename(filename)) return;
        String content = Util.readAll(filename);
        newTab(filename, content, true);
    }

    /** Returns the number of tabs. */
    public int getTabCount() {
        return tabs.size();
    }

    /**
     * If allowIO==false, return a new empty map; otherwise return a newly constructed map from each tab's filename to the text content in that tab.
     * Note: the returned map is modifiable, and the caller is free to do whatever with it
     * (the changes do NOT affect this OurTabbedEditor object)
     */
    public Map<String,String> takeSnapshot() {
        Map<String,String> map = new LinkedHashMap<String,String>();
        for(Tab t:tabs) { map.put(t.filename, t.text.getText()); }
        return map;
    }

    /** Returns the list of filenames corresponding to each text buffer. */
    public ConstList<String> getFilenames() {
        TempList<String> ans=new TempList<String>(tabs.size());
        for(Tab t:tabs) { ans.add(t.filename); }
        return ans.makeConst();
    }

    /** Return the filename of the current text buffer. */
    public String getFilename() {
        if (me>=0 && me<tabs.size()) { return tabs.get(me).filename; }
        return "";
    }

    /** Changes the tabsize of every text buffer. */
    public void setTabSize(int tabSize) {
        this.tabSize=tabSize;
        for(Tab t:tabs) { t.text.setTabSize(tabSize); }
    }

    /** Changes the font of every text buffer. */
    public void setFont(Font font) {
        this.font=font;
        for(Tab t:tabs) { t.text.setFont(font); }
    }

    /** Returne ths entire JPanel of this tabbed text editor. */
    public Component getUI() {
        return frame;
    }

    /** Returns the OurTextArea of the current text buffer. */
    public OurTextArea text() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).text : new OurTextArea("", "Monospaced", 10);
    }

    /** True if the current text buffer has 1 or more "undo" that it can perform. */
    public boolean canUndo() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).undo.canUndo() : false;
    }

    /** True if the current text buffer has 1 or more "redo" that it can perform. */
    public boolean canRedo() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).undo.canRedo() : false;
    }

    /** True if the i-th text buffer has been modified since it was last loaded/saved */
    public boolean modified(int i) {
        return (i>=0 && i<tabs.size()) ? tabs.get(i).modified : false;
    }

    /** True if the current text buffer has been modified since it was last loaded/saved */
    public boolean modified() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).modified : false;
    }

    /** True if the i-th text buffer corresponds to an actual file. */
    public boolean isFile(int i) {
        return (i>=0 && i<tabs.size()) ? tabs.get(i).isFile : false;
    }

    /** True if the current text buffer corresponds to an actual file. */
    public boolean isFile() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).isFile : false;
    }

    /** Returns the currently selected tab. */
    public int getSelectedIndex() {
        return me;
    }

    /** Perform "undo" on the current text buffer. */
    public void undo() {
        if (me>=0 && me<tabs.size()) { tabs.get(me).undo.undo(); }
    }

    /** Perform "redo" on the current text buffer. */
    public void redo() {
        if (me>=0 && me<tabs.size()) { tabs.get(me).undo.redo(); }
    }

    /**
     * Highlights the text editor, based on the location information in the Pos object.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void highlight(final Pos p, final Color color, final boolean clearOldHighlightsFirst) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { highlight(p, color, clearOldHighlightsFirst); }
            });
            return;
        }
        if (clearOldHighlightsFirst) removeAllHighlights();
        if (p!=null && p.filename.length()>0 && p.y>0 && p.x>0) {
            try {
                String f=Util.canon(p.filename);
                if (!switchToFilename(f)) {
                    String content;
                    try {
                        content=Util.readAll(f);
                    } catch(IOException ex) {
                        // Highlight is not critical
                        if (1==1) { OurDialog.alert(parentFrame, "Error reading the file \""+f+"\"", "Error"); return; }
                        adjustLabelColor();
                        parent.notifyChange();
                        return;
                    }
                    newTab(f, content, true);
                }
                int c=text().getLineStartOffset(p.y-1)+p.x-1;
                int d=text().getLineStartOffset(p.y2-1)+p.x2-1;
                if (tabs.get(me).highlighter!=null) tabs.get(me).highlighter.addHighlight(c, d+1, new OurTabbedHighlighter(color));
                // Setting cursor to 0 first should ensure the textarea will scroll to the highlighted section
                text().setSelectionStart(0);
                text().setSelectionEnd(0);
                text().setSelectionStart(c);
                text().setSelectionEnd(c);
                text().requestFocusInWindow();
            } catch(BadLocationException ex) {
                // Failure to highlight is not fatal
            }
            adjustLabelColor();
            parent.notifyChange();
        }
    }

    /**
     * Highlights the text editor, based on the location information in the set of Pos objects.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void highlight(final Iterable<Pos> set, final Color color, final boolean clearOldHighlightsFirst) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { highlight(set, color, clearOldHighlightsFirst); }
            });
            return;
        }
        if (clearOldHighlightsFirst) removeAllHighlights();
        OurTextArea text=null;
        int c=0, d;
        again:
        for(Pos p:set) if (p!=null && p.filename.length()>0 && p.y>0 && p.x>0) {
            try {
                String f=Util.canon(p.filename);
                if (!switchToFilename(f)) {
                    String content;
                    try {
                        content=Util.readAll(f);
                    } catch(IOException ex) {
                        // Highlight is not critical
                        if (1==1) { OurDialog.alert(parentFrame, "Error reading the file \""+f+"\"", "Error"); break again; }
                        continue again;
                    }
                    newTab(f, content, true);
                }
                text = text();
                c = text.getLineStartOffset(p.y-1)+p.x-1;
                d = text.getLineStartOffset(p.y2-1)+p.x2-1;
                if (tabs.get(me).highlighter!=null) tabs.get(me).highlighter.addHighlight(c, d+1, new OurTabbedHighlighter(color));
            } catch(BadLocationException ex) {
                // Failure to highlight is not fatal
            }
        }
        if (text!=null) {
            // Setting cursor to 0 first should ensure the textarea will scroll to the highlighted section
            text.setSelectionStart(0);
            text.setSelectionEnd(0);
            text.setSelectionStart(c);
            text.setSelectionEnd(c);
            text.requestFocusInWindow();
        }
        adjustLabelColor();
        parent.notifyChange();
    }

    /**
     * Highlights the text editor, based on the location information in the Err object.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void highlight(final Err e) { highlight(e.pos, new Color(0.9f, 0.4f, 0.4f), true); }

    /** Constructs a tabbed editor pane. */
    public OurTabbedEditor(final Parent parent, final JFrame parentFrame, final Font font, final int tabSize) {
        this.parent=parent;
        this.parentFrame=parentFrame;
        this.font=font;
        this.tabSize=tabSize;
        JPanel glue = OurUtil.makeHB(new Object[]{null});
        glue.setBorder(new OurBorder(null,null,BORDER,null));
        tabBar=OurUtil.makeHB(glue);
        if (!Util.onMac()) {
            tabBar.setOpaque(true);
            tabBar.setBackground(GRAY);
        }
        tabBarScroller = new JScrollPane(tabBar, VERTICAL_SCROLLBAR_NEVER, HORIZONTAL_SCROLLBAR_NEVER);
        tabBarScroller.setFocusable(false);
        tabBarScroller.setBorder(new EmptyBorder(0,0,0,0));
        frame=new JPanel();
        frame.setBorder(new EmptyBorder(0,0,0,0));
        frame.setLayout(new BorderLayout());
        frame.add(tabBarScroller, BorderLayout.NORTH);
        frame.add(new JPanel(), BorderLayout.CENTER); // Create an "initial" content area beneath the list-of-tabs
        newTab();
        tabBarScroller.addComponentListener(new ComponentListener() {
            public final void componentResized(ComponentEvent e) { setSelectedIndex(me); }
            public final void componentMoved(ComponentEvent e) { setSelectedIndex(me); }
            public final void componentShown(ComponentEvent e) { setSelectedIndex(me); }
            public final void componentHidden(ComponentEvent e) { }
        });
    }
}
