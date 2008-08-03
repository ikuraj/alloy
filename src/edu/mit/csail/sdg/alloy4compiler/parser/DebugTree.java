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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import edu.mit.csail.sdg.alloy4.OurTabbedEditor;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Browsable;

/**
 * This class represents a graphical parse tree displayer.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DebugTree extends JTree {

    private static final long serialVersionUID = 1L;
    private final boolean onWindows;
    private final Font font;
    private final TreeCellRenderer renderer;

    public static JFrame show (final Browsable root, final OurTabbedEditor editor) {
        final DebugTree tree = new DebugTree(root, 12);
        tree.setBorder(new EmptyBorder(3,3,3,3));
        final JScrollPane scr = new JScrollPane(tree, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scr.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) { tree.requestFocusInWindow(); }
            public void focusLost(FocusEvent e) { }
        });
        final JFrame x = new JFrame("Parse Tree");
        x.setLayout(new BorderLayout());
        x.add(scr, BorderLayout.CENTER);
        x.pack();
        x.setSize(500, 500);
        x.setLocationRelativeTo(null);
        x.setVisible(true);
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public final void valueChanged(TreeSelectionEvent e) {
                TreePath path = tree.getSelectionPath();
                if (path==null) return;
                Object x = path.getLastPathComponent();
                if (!(x instanceof Browsable)) return;
                Browsable b = ((Browsable)x);
                if (editor!=null) editor.do_highlight(b.pos());
            }
        });
        return x;
    }

    /** This class represents the TreeModel for a graphical parse tree displayer. */
    private static final class DebugTreeModel implements TreeModel {
        private final Browsable instance;
        public DebugTreeModel(Browsable instance) { this.instance=instance; }
        private List<?> getChildren(Object parent) {
            ArrayList<Browsable> ans = new ArrayList<Browsable>();
            if (parent instanceof Browsable) for(Browsable x: ((Browsable)parent).getSubnodes()) ans.add(x);
            return ans;
        }
        public Object getChild(Object parent, int index) {
            List<?> ans = getChildren(parent);
            return (index>=0 && index<ans.size()) ? ans.get(index) : null;
        }
        public int getIndexOfChild(Object parent, Object child) {
            List<?> ans = getChildren(parent);
            for(int i=0; i<ans.size(); i++) if (ans.get(i).equals(child)) return i;
            return -1;
        }
        public Object getRoot() {return instance;}
        public int getChildCount(Object parent) { return getChildren(parent).size(); }
        public boolean isLeaf(Object node) { return getChildren(node).isEmpty(); }
        public void valueForPathChanged(TreePath path, Object newValue) {}
        public void addTreeModelListener(TreeModelListener l) {}
        public void removeTreeModelListener(TreeModelListener l) {}
    }

    public DebugTree(Browsable root, int fontSize) {
        this.font = OurUtil.getVizFont().deriveFont((float)fontSize);
        this.onWindows = Util.onWindows();
        this.renderer = new DefaultTreeCellRenderer();
        JLabel r = (JLabel)renderer;
        r.setFont(font);
        r.invalidate();
        r.validate();
        setRowHeight(r.getPreferredSize().height);
        setModel(new DebugTreeModel(root));
        setCellRenderer(renderer);
        setFont(font);
        setBorder(new EmptyBorder(2, 2, 2, 2));
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        putClientProperty("JTree.lineStyle", "Angled");
        setRootVisible(true);
        setBackground(Color.WHITE);
        setOpaque(true);
    }

    @Override public String convertValueToText(Object val,boolean selected,boolean expanded,boolean leaf,int row,boolean focus) {
        String c = ">";
        String x = (val instanceof Browsable) ? ((Browsable)val).getDescription() : String.valueOf(val);
        if (onWindows) c = selected ? " style=\"color:#ffffff;\">" : " style=\"color:#000000;\">";
        return "<html> <span" + c + x + "</span></html>";
    }
}
