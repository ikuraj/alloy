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
import java.awt.Font;
import java.awt.Graphics;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeModelListener;
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

   /** Custom TreeModel that calls the alloyAskRoot() and alloyAsk() method to lazily construct the tree on a as-needed basis. */
   private final class OurTreeModel implements TreeModel {
      /** {@inheritDoc} */
      public Object getChild(Object parent, int index) {
         List<?> ans = alloyAsk(parent);
         return (index>=0 && index<ans.size()) ? ans.get(index) : null;
      }
      /** {@inheritDoc} */
      public int getIndexOfChild(Object parent, Object child) {
         List<?> ans = alloyAsk(parent);
         for(int i=0; i<ans.size(); i++) if (ans.get(i).equals(child)) return i;
         return -1;
      }
      /** {@inheritDoc} */
      public Object getRoot() { return alloyAskRoot(); }
      /** {@inheritDoc} */
      public int getChildCount(Object node) { return alloyAsk(node).size(); }
      /** {@inheritDoc} */
      public boolean isLeaf(Object node) { return alloyAsk(node).isEmpty(); }
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
      /** Constructs the Renderer. */
      public OurTreeRenderer() {
         super("Anything"); // This ensures that the height is calculated properly
         setFont(OurUtil.getVizFont().deriveFont((float)80));
         setVerticalAlignment(JLabel.BOTTOM);
         setBorder(new EmptyBorder(0, 3, 0, 3));
      }
      /** Returns an object to be drawn. */
      public Component getTreeCellRendererComponent(JTree tree, Object value, boolean isSelected, boolean expanded, boolean isLeaf, int row, boolean isFocused) {
         if (value instanceof DefaultMutableTreeNode) value = ((DefaultMutableTreeNode)value).getUserObject();
         String string = tree.convertValueToText(value, isSelected, expanded, isLeaf, row, isFocused);
         this.isFocused = isFocused;
         this.isSelected = isSelected;
         this.setText(string);
         this.setForeground(UIManager.getColor(isSelected ? "Tree.selectionForeground" : "Tree.textForeground"));
         this.setSize(getPreferredSize());
         return this;
      }
      /** We override the paint() method. */
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
   public abstract Object alloyAskRoot();

   /** Subclass should implement this method to return the list of children nodes given a particular node.
    * <p> Note: in order for the tree model to operate correctly, you must make sure a.equals(b) implies alloyAsk(a).equals(alloyAsk(b))
    */
   public abstract List<?> alloyAsk(Object parent);

   /** Subclass should call this method when it has initialized its fields properly; we will not call alloyAskRoot() and alloyAsk() until subclass calls this method. */
   protected final void alloyStart() { setModel(new OurTreeModel()); }

   /** {@inheritDoc} */
   @Override public abstract String convertValueToText(Object val, boolean selected, boolean expanded, boolean leaf, int row, boolean focus);

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
   }
}
