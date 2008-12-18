/* Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.util.IdentityHashMap;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import edu.mit.csail.sdg.alloy4.OurUtil;

/** Graphical tree.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public abstract class OurTree extends JTree {

   /** The list of events that the OurTree object may fire. */
   public enum Event { SELECT };

   /** The current list of listeners. */
   public final Listeners listeners = new Listeners();

   /** Custom TreeModel that calls the do_root() and do_ask() methods to lazily construct the tree on a as-needed basis. */
   private final class OurTreeModel implements TreeModel {
      /** Caches the result of calling do_ask() in order to make sure we present the same Object reference for these methods. */
      private final IdentityHashMap<Object,List<?>> map = new IdentityHashMap<Object,List<?>>();
      /** Constructs this tree model. */
      public OurTreeModel() { }
      /** {@inheritDoc} */
      public Object getChild(Object parent, int index) {
         List<?> ans = map.get(parent);
         if (ans==null) { ans=do_ask(parent); map.put(parent, ans); }
         return (index>=0 && index<ans.size()) ? ans.get(index) : null;
      }
      /** {@inheritDoc} */
      public int getIndexOfChild(Object parent, Object child) {
         List<?> ans = map.get(parent);
         if (ans==null) { ans=do_ask(parent); map.put(parent, ans); }
         for(int i=0; i<ans.size(); i++) if (ans.get(i).equals(child)) return i;
         return -1;
      }
      /** {@inheritDoc} */
      public Object getRoot() { return do_root(); }
      /** {@inheritDoc} */
      public int getChildCount(Object node) { getChild(node, 0); return map.get(node).size(); }
      /** {@inheritDoc} */
      public boolean isLeaf(Object node) { getChild(node, 0); return map.get(node).isEmpty(); }
      /** {@inheritDoc} */
      public void valueForPathChanged(TreePath path, Object newValue) { }
      /** {@inheritDoc} */
      public void addTreeModelListener(TreeModelListener l) { }
      /** {@inheritDoc} */
      public void removeTreeModelListener(TreeModelListener l) { }
   }

   /** Custom TreeCellRenderer to print the tree nodes better. (The idea of using JLabel is inspired by DefaultTreeCellRenderer) */
   private static final class OurTreeRenderer extends JLabel implements TreeCellRenderer {
      /** This suppresses javac's warning about missing serialVersionUID. */
      private static final long serialVersionUID = 0;
      /** Whether the current object is selected or not. */
      private boolean isSelected;
      /** Whether the current object is focused or not. */
      private boolean isFocused;
      /** Constructs this renderer. */
      public OurTreeRenderer() {
         super("Anything"); // This ensures that the height is calculated properly
         setFont(OurUtil.getVizFont().deriveFont((float)80));
         setVerticalAlignment(JLabel.BOTTOM);
         setBorder(new EmptyBorder(0, 3, 0, 3));
      }
      /** This method is called by Swing to return an object to be drawn. */
      public Component getTreeCellRendererComponent
      (JTree tree, Object value, boolean isSelected, boolean expanded, boolean isLeaf, int row, boolean isFocused) {
         if (value instanceof DefaultMutableTreeNode) value = ((DefaultMutableTreeNode)value).getUserObject();
         String string = tree.convertValueToText(value, isSelected, expanded, isLeaf, row, isFocused);
         this.isFocused = isFocused;
         this.isSelected = isSelected;
         this.setText(string);
         this.setForeground(UIManager.getColor(isSelected ? "Tree.selectionForeground" : "Tree.textForeground"));
         this.setSize(getPreferredSize());
         return this;
      }
      /** This method is called by Swing to draw this object. */
      @Override public void paint(Graphics g) {
         int w=getWidth(), h=getHeight();
         Color background = isSelected ? UIManager.getColor("Tree.selectionBackground") : Color.WHITE;
         Color border = isFocused ? UIManager.getColor("Tree.selectionBorderColor") : null;
         if (background!=null) { g.setColor(background); g.fillRect(0,0,w,h); }
         if (border!=null && isSelected) { g.setColor(border); g.drawRect(0,0,w-1,h-1); }
         super.paint(g);
      }
   }

   /** This suppresses javac's warning about missing serialVersionUID. */
   private static final long serialVersionUID = 0;

   /** Subclass should implement this method to return the root of the tree. */
   public abstract Object do_root();

   /** Subclass should implement this method to return the list of children nodes given a particular node. */
   public abstract List<?> do_ask(Object parent);

   /** Subclass should call this when all fields are initialized; we won't call do_root() and do_ask() until subclass calls this. */
   protected final void do_start() { setModel(new OurTreeModel()); }

   /** This method is called by Swing to figure out what text should be displayed for each node in the tree. */
   @Override public abstract String convertValueToText(Object v, boolean select, boolean expand, boolean leaf, int i, boolean focus);

   /** Construct a Tree object with the given font size. */
   public OurTree(int fontSize) {
      Font font = OurUtil.getVizFont().deriveFont((float)fontSize);
      OurTreeRenderer renderer = new OurTreeRenderer();
      renderer.setFont(font);
      renderer.invalidate();
      renderer.validate();
      setRowHeight(renderer.getPreferredSize().height);
      setCellRenderer(renderer);
      setFont(font);
      setBorder(new EmptyBorder(2, 2, 2, 2));
      getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
      putClientProperty("JTree.lineStyle", "Angled");
      setRootVisible(true);
      setBackground(Color.WHITE);
      setOpaque(true);
      addTreeSelectionListener(new TreeSelectionListener() {
         public void valueChanged(TreeSelectionEvent e) {
            TreePath path = OurTree.this.getSelectionPath();
            if (path!=null) OurTree.this.listeners.fire(OurTree.this, OurTree.Event.SELECT, path.getLastPathComponent());
         }
      });
   }
}
