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
import static javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.JScrollPane.VERTICAL_SCROLLBAR_NEVER;
import static java.awt.Color.BLACK;
import static java.awt.Color.WHITE;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/**
 * Graphical tabbed editor.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread (except "highlight()" method,
 * which can be called by any thread)
 *
 * <p><b>Invariant</b>: list.get(i).filename must not contain duplicate entries.
 */

public final class OurTabbedEditor extends JPanel {

    //=====================================================================//

    /** Graphical highlighter for a JTextComponent. */
    private static final class OurTabbedHighlighter implements Highlighter.HighlightPainter {

        /** The color to draw the highlights. */
        private final Color color;

        /** Constructs a highlighter. */
        public OurTabbedHighlighter(Color color) { this.color = color; }

        /** This method actually highlights. */
        public void paint(Graphics g, int start, int end, Shape shape, JTextComponent text) {
            Color oldcolor = g.getColor();
            g.setColor(color);
            try {
                Rectangle box = shape.getBounds();
                Rectangle a = text.getUI().modelToView(text, start);
                Rectangle b = text.getUI().modelToView(text, end);
                if (a.y == b.y) {
                    // same line; if start==end, then draw all the way to the right edge.
                    Rectangle r = a.union(b);
                    g.fillRect(r.x, r.y, (r.width<=1 ? (box.x+box.width-r.x) : r.width), r.height);
                } else {
                    // On the first line, draw from "start" and extends to the right-most edge
                    g.fillRect(a.x, a.y, box.x+box.width-a.x, a.height);
                    // If there are line(s) between the first line and the last line, then draw them
                    if (a.y+a.height != b.y) g.fillRect(box.x, a.y+a.height, box.width, b.y-(a.y+a.height));
                    // Draw the last line
                    g.fillRect(box.x, b.y, b.x-box.x, b.height);
                }
            } catch (BadLocationException e) {
                // Failure to highlight is not fatal
            }
            g.setColor(oldcolor);
        }
    }

    //=====================================================================//

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

        /** The highlighter associated with this text area. */
        private final Highlighter highlighter = new DefaultHighlighter();

        /** The filename; always nonempty, canonical, absolute, and unique among all Tab objects in this editor. */
        private String filename;

        /** True if this is associated with an actual file; false if it is still an "untitled" tab. */
        private boolean isFile;

        /** True if the JTextArea has been modified since it was last loaded or saved. */
        private boolean modified = false;

        /** Constructs a new Tab */
        private Tab(JPanel panel, JLabel label, OurTextArea text, String filename, boolean isFile) {
            this.panel = panel;
            this.label = label;
            this.text = text;
            this.text.setHighlighter(highlighter);
            this.scroll = OurUtil.scrollpane(text);
            this.filename = filename;
            this.isFile = isFile;
        }
    }

    //=====================================================================//

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** Background color for the list of tabs. */
    private static final Color GRAY = new Color(0.9f, 0.9f, 0.9f);

    /** Background color for an inactive tab. */
    private static final Color INACTIVE = new Color(0.8f, 0.8f, 0.8f);

    /** Background color for a inactive and highlighted tab. */
    private static final Color INACTIVE_HIGHLIGHTED = new Color(0.7f, 0.5f, 0.5f);

    /** Foreground color for a active and highlighted tab. */
    private static final Color ACTIVE_HIGHLIGHTED = new Color(0.5f, 0.2f, 0.2f);

    /** Default border color for each tab. */
    private static final Color BORDER = Color.LIGHT_GRAY;

    /** The font family to use in the JTextArea */
    private String fontFamily;

    /** The font size to use in the JTextArea */
    private int fontSize;

    /** The tabsize to use in the JTextArea */
    private int tabSize;

    /** Whether syntax highlighting is current enabled or not. */
    private boolean syntaxHighlighting;

    /** This runnable is notified when a tab is added/removed, we switch to another tab, text is modified, or text cursor moved. */
    private final Runnable notifyChange;

    /** This runnable is notified when a tab in this tabbed editor gains the focus. */
    private final Runnable notifyFocused;

    /** The parent's JFrame. */
    private final JFrame parentFrame;

    /** The list of clickable tabs. */
    private final JPanel tabBar;

    /** The scrollPane that wraps around this.tabbar */
    private final JScrollPane tabBarScroller;

    /** The list of tabs. */
    private final List<Tab> tabs = new ArrayList<Tab>();

    /** The currently selected tab from 0 to list.size()-1 (This value must be 0 if there are no tabs) */
    private int me = 0;

    /** The anonymous filename to give to the next unnamed text buffer. */
    private int nextNumber = 1;

    //=====================================================================//

    /** Adjusts the background and foreground of all labels. */
    private void do_adjustLabelColor() {
        for(int i=0; i<tabs.size(); i++) {
            Tab tab = tabs.get(i);
            JLabel label = tab.label;
            boolean hl = (tab.highlighter.getHighlights().length > 0);
            label.setBorder(new OurBorder(BORDER, BORDER, (i!=me ? BORDER : WHITE), BORDER));
            label.setBackground(i!=me ? (hl ? INACTIVE_HIGHLIGHTED : INACTIVE) : WHITE);
            label.setForeground(hl ? (i!=me ? BLACK : ACTIVE_HIGHLIGHTED) : BLACK);
        }
    }

    /** Removes all highlights from every text buffer. */
    public void do_removeAllHighlights() {
        for(Tab tab:tabs) tab.highlighter.removeAllHighlights();
        do_adjustLabelColor();
    }

    /** Switch to the i-th tab (Note: if successful, it will then always call notifyChange.run()) */
    private void do_setSelectedIndex(final int i) {
        if (i<0 || i>=tabs.size()) return;
        me=i;
        revalidate();
        do_adjustLabelColor();
        removeAll();
        if (tabs.size()>1) add(tabBarScroller, BorderLayout.NORTH);
        add(tabs.get(me).scroll, BorderLayout.CENTER);
        repaint();
        notifyChange.run();
        tabs.get(me).text.requestFocusInWindow();
        tabBar.scrollRectToVisible(new Rectangle(0,0,0,0)); // Forces recalculation
        Point p = tabs.get(me).panel.getLocation();
        Dimension r = tabs.get(me).panel.getSize();
        tabBar.scrollRectToVisible(new Rectangle(p.x, 0, r.width+200, 1));
    }

    /** Switch to the tab with the given filename then return true; returns false if no tab has that filename. */
    public boolean do_switchToFilename(String filename) {
        for(int i=0; i<tabs.size(); i++) {
            if (tabs.get(i).filename.equals(filename)) {
                if (i!=me) do_setSelectedIndex(i);
                return true;
            }
        }
        return false;
    }

    /** Returns a short title for a filename. */
    private static String do_getShorterTitle(String x) {
        int j=x.lastIndexOf('/');
        if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('\\');
        if (j>=0) x=x.substring(j+1);
        j=x.lastIndexOf('.');
        if (j>=0) x=x.substring(0,j);
        return x;
    }

    /** Changes the label of a JLabel. */
    private static void do_setTitle(JLabel label, String filename, boolean modified) {
        label.setToolTipText(filename);
        filename = do_getShorterTitle(filename);
        if (filename.length()>14) { filename=filename.substring(0,14)+"..."; }
        label.setText("  " + filename + (modified?" *  ":"  "));
    }

    /** Refresh the given tab; return true if no error occurred. */
    public boolean do_refresh(int i) {
        if (i<0 || i>=tabs.size()) return true;
        Tab t = tabs.get(i);
        if (!t.isFile) return true; // "untitled" text buffer does not have a on-disk file to refresh from
        if (t.modified) {
            boolean ans = OurDialog.yesno(parentFrame,
                "You have unsaved changes to \"" + do_getShorterTitle(t.filename)
                + "\"\nAre you sure you wish to discard your changes and reload it from disk?",
                "Discard your changes", "Cancel this operation");
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
        t.text.do_clearUndo();
        t.modified = false;
        do_setTitle(t.label, t.filename, false);
        notifyChange.run();
        return true;
    }

    /**
     * Save the current tab content to the file system
     * @return null if an error occurred (otherwise, return the filename)
     */
    private String do_saveAs(String filename) {
        if (me<0 || me>=tabs.size()) return null;
        filename = Util.canon(filename);
        for(int i=0; i<tabs.size(); i++) {
            if (i!=me && tabs.get(i).filename.equals(filename)) {
                OurDialog.alert(parentFrame, "Error. The filename \""+filename+"\"\nis already open in one of the tab.", "Error");
                return null;
            }
        }
        try {
            Util.writeAll(filename, tabs.get(me).text.getText());
        } catch (Throwable e) {
            OurDialog.alert(parentFrame, "Error writing to the file \""+filename+"\"", "Error");
            return null;
        }
        filename = Util.canon(filename); // We need this since after writing, the canonical form of the filename may have changed
        do_setTitle(tabs.get(me).label, filename, false);
        tabs.get(me).filename = filename;
        tabs.get(me).modified = false;
        tabs.get(me).isFile = true;
        notifyChange.run();
        return filename;
    }

    /**
     * Save the current tab content to the file system.
     * @param alwaysPickNewName - if true, it will always pop up a File Selection dialog box to ask for the filename
     * @return null if an error occurred (otherwise, return the filename)
     */
    public String do_save(boolean alwaysPickNewName) {
        if (me<0 || me>=tabs.size()) return null;
        String filename = tabs.get(me).filename;
        if (alwaysPickNewName || tabs.get(me).isFile==false || filename.startsWith(Util.jarPrefix())) {
            File file = OurDialog.askFile(parentFrame, false, null, ".als", ".als files");
            if (file==null) return null;
            filename = Util.canon(file.getPath());
            if (file.exists() && !OurDialog.askOverwrite(parentFrame, filename)) return null;
        }
        filename = do_saveAs(filename);
        if (filename!=null) Util.setCurrentDirectory(new File(filename).getParentFile());
        return filename;
    }

    /** Create a new anonymous file name. */
    private String do_newname() {
        again:
        while(true) {
            String filename = Util.canon("Untitled "+nextNumber+".als");
            nextNumber++;
            for(Tab t:tabs) if (t.filename.equals(filename)) continue again;
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
    private boolean do_close(int i) {
        do_removeAllHighlights();
        String filename = tabs.get(i).filename;
        if (tabs.get(i).modified) {
            Boolean ans = OurDialog.askSaveDiscardCancel(parentFrame, "The file \"" + do_getShorterTitle(filename) + "\"");
            if (ans==null || (ans && do_save(false)==null)) return false;
            // check to make sure the i-th buffer still exists; if there is no longer such a buffer, then we're done
            if (i<0 || i>=tabs.size()) return true;
        }
        tabs.get(i).text.setText("");
        if (tabs.size()==1) {
            tabs.get(i).text.do_clearUndo();
            tabs.get(i).modified = false;
            if (tabs.get(i).isFile) { tabs.get(i).isFile=false; tabs.get(i).filename=do_newname(); }
            do_setTitle(tabs.get(i).label, tabs.get(i).filename, false);
        } else {
            tabBar.remove(i);
            tabs.remove(i);
            if (me>=tabs.size()) me=tabs.size()-1;
        }
        // Must call this to change the active tab and call notifyChange.run() (which is important)
        do_setSelectedIndex(me);
        return true;
    }

    /** Close the current tab (then create a new empty tab if there were no tabs remaining) */
    public void do_close() {
        if (me>=0 && me<tabs.size()) do_close(me);
    }

    /** Close every tab then create a new empty tab; returns true iff success. */
    public boolean do_closeAll() {
        // first attempt to close all the unmodified files
        for(int i=tabs.size()-1; i>=0; i--) if (tabs.get(i).modified==false) do_close(i);
        // then close the modified files one-by-one until an error occurred or if the user refuses to save nor discard a file
        for(int i=tabs.size()-1; i>=0; i--) if (do_close(i)==false) return false;
        return true;
    }

    /** Returns the number of tabs. */
    public int do_getTabCount() {
        return tabs.size();
    }

    /**
     * If allowIO==false, return a new empty map; otherwise return a newly constructed map from each tab's filename to the text content in that tab.
     * Note: the returned map is modifiable, and the caller is free to do whatever with it
     * (the changes do NOT affect this OurTabbedEditor object)
     */
    public Map<String,String> do_takeSnapshot() {
        Map<String,String> map = new LinkedHashMap<String,String>();
        for(Tab t:tabs) { map.put(t.filename, t.text.getText()); }
        return map;
    }

    /** Returns the list of filenames corresponding to each text buffer. */
    public ConstList<String> do_getFilenames() {
        TempList<String> ans=new TempList<String>(tabs.size());
        for(Tab t:tabs) { ans.add(t.filename); }
        return ans.makeConst();
    }

    /** Return the filename of the current text buffer. */
    public String do_getFilename() {
        if (me>=0 && me<tabs.size()) { return tabs.get(me).filename; }
        return "";
    }

    /** Changes the tabsize of every text buffer. */
    public void do_setTabSize(int tabSize) {
        this.tabSize=tabSize;
        for(Tab t:tabs) { t.text.do_setTabSize(tabSize); }
    }

    /** Changes the font of every text buffer. */
    public void do_setFont(String fontFamily, int fontSize) {
        this.fontFamily = fontFamily;
        this.fontSize = fontSize;
        for(Tab t:tabs) { t.text.do_setFont(fontFamily, fontSize); }
    }

    /** Enables or disables syntax highlighting. */
    public void do_syntaxHighlighting(boolean flag) {
        syntaxHighlighting = flag;
        for(Tab t:tabs) { t.text.do_syntaxHighlighting(flag); }
    }

    /** Returns the JTextArea of the current text buffer. */
    public OurTextArea do_text() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).text : new OurTextArea(true, "", "Monospaced", 10, 4);
    }

    /** True if the i-th text buffer has been modified since it was last loaded/saved */
    public boolean do_modified(int i) {
        return (i>=0 && i<tabs.size()) ? tabs.get(i).modified : false;
    }

    /** True if the current text buffer has been modified since it was last loaded/saved */
    public boolean do_modified() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).modified : false;
    }

    /** True if the current text buffer corresponds to an actual file. */
    public boolean do_isFile() {
        return (me>=0 && me<tabs.size()) ? tabs.get(me).isFile : false;
    }

    /** Switches to the previous tab. */
    public void do_prev() {
        int i = me-1;
        if (i<0) i=tabs.size()-1;
        if (i!=me) do_setSelectedIndex(i);
    }

    /** Switches to the next tab. */
    public void do_next() {
        int i = me+1;
        if (i>=tabs.size()) i=0;
        if (i!=me) do_setSelectedIndex(i);
    }

    /**
     * Create a new tab with the given filename and initial content.
     * <p> Note: if a text buffer with that filename already exists, we will switch to it and ignore "fileContent" and "isFile"
     */
    private void do_newTab(String filename, String fileContent, boolean isFile) {
        // If exists, then switch to that tab directly
        if (do_switchToFilename(filename)) return;
        // Make the new tab
        final JLabel lb = OurUtil.label("", OurUtil.getVizFont().deriveFont(Font.BOLD), BLACK, WHITE, new OurBorder(BORDER, BORDER, WHITE, BORDER));
        lb.addMouseListener(new MouseAdapter() {
            @Override public final void mousePressed(MouseEvent e) {
                for(int i=0; i<tabs.size(); i++) if (tabs.get(i).label==lb) { do_setSelectedIndex(i); break; }
            }
        });
        JPanel h1  = OurUtil.makeH(4); h1.setBorder(new OurBorder(null, null, BORDER, null));
        JPanel h2  = OurUtil.makeH(3); h2.setBorder(new OurBorder(null, null, BORDER, null));
        JPanel pan = Util.onMac() ? OurUtil.makeVL(null, 2, OurUtil.makeHB(h1, lb, h2)) : OurUtil.makeVL(null, 2, OurUtil.makeHB(h1, lb, h2, GRAY), GRAY);
        pan.setAlignmentX(0.0f);
        pan.setAlignmentY(1.0f);
        // Make the JTextArea
        final OurTextArea text = new OurTextArea(syntaxHighlighting, Util.convertLineBreak(fileContent), fontFamily, fontSize, tabSize);
        text.getActionMap().put("alloy_next", new AbstractAction("alloy_next") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) { do_next(); }
        });
        text.getActionMap().put("alloy_prev", new AbstractAction("alloy_prev") {
            private static final long serialVersionUID = 1L;
            public final void actionPerformed(ActionEvent e) { do_prev(); }
        });
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP,   InputEvent.CTRL_MASK), "alloy_prev");
        text.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, InputEvent.CTRL_MASK), "alloy_next");
        // Add everything
        tabBar.add(pan, tabs.size());
        final Tab tab = new Tab(pan, lb, text, filename, isFile);
        tabs.add(tab);
        do_setTitle(tab.label, filename, false);
        // Add these listeners last, to make sure this object is fully initialized first
        text.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) { notifyChange.run(); }
        });
        text.addFocusListener(new FocusAdapter() {
            public final void focusGained(FocusEvent e) { notifyFocused.run(); }
        });
        text.getDocument().addDocumentListener(new DocumentListener() {
            public final void insertUpdate(DocumentEvent e) {
                do_removeAllHighlights();
                do_setTitle(tab.label, tab.filename, true);
                tab.modified=true;
                notifyChange.run();
            }
            public final void removeUpdate(DocumentEvent e) { insertUpdate(e); }
            public final void changedUpdate(DocumentEvent e) { } // font changes are irrelevant
        });
        // If it's a file, we want to remove the rightmost untitled empty tab
        if (isFile) for(int i=tabs.size()-1; i>=0; i--) if (!tabs.get(i).isFile && tabs.get(i).text.getText().length()==0) {
            tabs.get(i).modified=false;
            do_close(i);
            break;
        }
        // Must call this method to switch to the new tab; and it will call notifyChange.run() which is important
        do_setSelectedIndex(tabs.size()-1);
    }

    /** Create a new empty tab. */
    public void do_newTab() {
        do_newTab(do_newname(), "", false);
    }

    /**
     * Create a new tab with the given filename.
     * <p> If a text buffer with that filename already exists, we will just switch to it.
     * <p> Otherwise, we will read the given filename and create a new tab for it.
     * @return false iff an error occurred
     */
    public boolean do_newTab(String filename) {
        filename = Util.canon(filename);
        if (do_switchToFilename(filename)) return true;
        try {
           String content = Util.readAll(filename);
           do_newTab(filename, content, true);
           return true;
        } catch(IOException ex) {
           OurDialog.alert(parentFrame, "Error reading the file \""+filename+"\"", "Error");
           return false;
        }
    }

    /**
     * Highlights the text editor, based on the location information in the set of Pos objects.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void do_highlight(final Iterable<Pos> set, final Color color, final boolean clearOldHighlightsFirst) {
        if (!SwingUtilities.isEventDispatchThread()) {
            OurUtil.invokeAndWait(new Runnable() {
                public final void run() { do_highlight(set, color, clearOldHighlightsFirst); }
            });
            return;
        }
        if (clearOldHighlightsFirst) do_removeAllHighlights();
        OurTabbedHighlighter ht = new OurTabbedHighlighter(color);
        OurTextArea text = null;
        int c=0, d;
        for(Pos p:set) if (p!=null && p.filename.length()>0 && p.y>0 && p.x>0) {
            try {
                if (!do_newTab(p.filename)) break;
                text = do_text();
                c = text.do_getLineStartOffset(p.y-1)+p.x-1;
                d = text.do_getLineStartOffset(p.y2-1)+p.x2-1;
                tabs.get(me).highlighter.addHighlight(c, d+1, ht);
            } catch(BadLocationException ex) {
                // Failure to highlight is not fatal
            }
        }
        if (text!=null) {
            // Setting cursor to 0 first should ensure the textarea will scroll to the highlighted section
            text.setSelectionStart(0); text.setSelectionEnd(0);
            text.setSelectionStart(c); text.setSelectionEnd(c);
        }
        do_text().requestFocusInWindow();
        do_adjustLabelColor();
        notifyChange.run();
    }

    /**
     * Highlights the text editor, based on the location information in the Pos object.
     * <p> Note: this method can be called by any thread (not just the AWT event thread)
     */
    public void do_highlight(final Pos pos) { do_highlight(Util.asList(pos), new Color(0.9f, 0.4f, 0.4f), true); }

    /** Constructs a tabbed editor pane. */
    public OurTabbedEditor(final Runnable notifyChanged, final Runnable notifyFocused, final JFrame parentFrame, final String fontFamily, final int fontSize, final int tabSize) {
        this.notifyChange = notifyChanged;
        this.notifyFocused = notifyFocused;
        this.parentFrame = parentFrame;
        this.fontFamily = fontFamily;
        this.fontSize = fontSize;
        this.tabSize = tabSize;
        JPanel glue = OurUtil.makeHB(new Object[]{null});
        glue.setBorder(new OurBorder(null, null, BORDER, null));
        tabBar = OurUtil.makeHB(glue);
        if (!Util.onMac()) { tabBar.setOpaque(true); tabBar.setBackground(GRAY); }
        tabBarScroller = new JScrollPane(tabBar, VERTICAL_SCROLLBAR_NEVER, HORIZONTAL_SCROLLBAR_NEVER);
        tabBarScroller.setFocusable(false);
        tabBarScroller.setBorder(null);
        setBorder(null);
        setLayout(new BorderLayout());
        do_newTab();
        tabBarScroller.addComponentListener(new ComponentListener() {
            public final void componentResized(ComponentEvent e) { do_setSelectedIndex(me); }
            public final void componentMoved(ComponentEvent e) { do_setSelectedIndex(me); }
            public final void componentShown(ComponentEvent e) { do_setSelectedIndex(me); }
            public final void componentHidden(ComponentEvent e) { }
        });
    }
}
