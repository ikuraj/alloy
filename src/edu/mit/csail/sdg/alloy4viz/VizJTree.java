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

package edu.mit.csail.sdg.alloy4viz;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.util.ArrayList;
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
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Util;
import static edu.mit.csail.sdg.alloy4.Util.encode;

/** GUI tree that displays an instance as a tree.
*
* <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
*/

public final class VizJTree extends JTree {

   /** This provides a TreeModel view of an AlloyInstance. */
   private final class VizJTreeModel implements TreeModel {

      private List<Pair<AlloyAtom,AlloyRelation>> getFields(AlloyAtom atom) {
         List<Pair<AlloyAtom,AlloyRelation>> ans = new ArrayList<Pair<AlloyAtom,AlloyRelation>>();
         for(AlloyRelation rel:instance.model.getRelations())
            for(AlloyTuple tuple:instance.relation2tuples(rel))
               if (tuple.getStart().equals(atom)) { ans.add(new Pair<AlloyAtom,AlloyRelation>(atom,rel)); break; }
         return ans;
      }

      private List<Object> getTuplesOrAtoms(AlloyRelation rel, AlloyAtom atom) {
         List<Object> ans=new ArrayList<Object>();
         for(AlloyTuple tuple: instance.relation2tuples(rel))
            if (tuple.getStart().equals(atom)) {if (tuple.getArity()==2) ans.add(tuple.getEnd()); else ans.add(tuple);}
         return ans;
      }

      private List<?> getChildren(Object parent) {
         if (parent instanceof AlloyInstance) return typesAndSets;
         if (parent instanceof AlloyType) return instance.type2atoms((AlloyType)parent);
         if (parent instanceof AlloySet) return instance.set2atoms((AlloySet)parent);
         if (parent instanceof AlloyAtom) return getFields((AlloyAtom)parent);
         if (parent instanceof Pair) {
            Pair<?,?> p=(Pair<?,?>)parent;
            return getTuplesOrAtoms((AlloyRelation)p.b, (AlloyAtom)p.a);
         }
         if (parent instanceof AlloyTuple) {
            List<AlloyAtom> x = new ArrayList<AlloyAtom>(((AlloyTuple)parent).getAtoms());
            x.remove(0);
            for(int i=x.size()-1; i>0; i--) {
               for(int j=i-1; j>=0; j--) {
                  if (x.get(i).equals(x.get(j))) { x.remove(i); break; }
               }
            }
            return x;
         }
         return blank;
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

   /** Custom TreeCellRenderer to print the tree nodes better. (The idea of using JLabel is inspired by DefaultTreeCellRenderer) */
   private static final class VizJTreeRenderer extends JLabel implements TreeCellRenderer {

      /** This suppresses javac's warning about missing serialVersionUID. */
      private static final long serialVersionUID = 1L;

      /** Whether the current object is selected or not. */
      private boolean isSelected;

      /** Whether the current object is focused or not. */
      private boolean isFocused;

      /** Constructs the Renderer. */
      public VizJTreeRenderer() {
         super("Anything"); // This ensures that the height is calculated properly
         setFont(OurUtil.getVizFont().deriveFont((float)80));
         setVerticalAlignment(JLabel.BOTTOM);
         setBorder(new EmptyBorder(0, 3, 0, 3));
      }

      /** Returns an object to be drawn. */
      public Component getTreeCellRendererComponent(JTree tree, Object value, boolean isSelected, boolean expanded, boolean isLeaf, int row, boolean isFocused) {
         if (value instanceof DefaultMutableTreeNode) value=((DefaultMutableTreeNode)value).getUserObject();
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

   private static List<Object> blank = ConstList.make();

   private static final long serialVersionUID = 1L;

   private final String title;

   private final VizState theme;

   private final boolean onWindows;

   private final AlloyInstance instance;

   private final ConstList<AlloyElement> typesAndSets;

   public VizJTree(AlloyInstance instance, String title, VizState theme, int fontSize) {
      this.instance = instance;
      this.title = title;
      this.theme = theme;
      this.onWindows = Util.onWindows();
      this.typesAndSets = new TempList<AlloyElement>(instance.model.getTypes()).addAll(instance.model.getSets()).makeConst();
      Font font = OurUtil.getVizFont().deriveFont((float)fontSize);
      VizJTreeRenderer renderer = new VizJTreeRenderer();
      renderer.setFont(font);
      renderer.invalidate();
      renderer.validate();
      setRowHeight(renderer.getPreferredSize().height);
      setModel(new VizJTreeModel());
      setCellRenderer(renderer);
      setFont(font);
      setBorder(new EmptyBorder(2, 2, 2, 2));
      getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
      putClientProperty("JTree.lineStyle", "Angled");
      setRootVisible(true);
      setBackground(Color.WHITE);
      setOpaque(true);
   }

   @Override public String convertValueToText(Object val, boolean selected, boolean expanded, boolean leaf, int row, boolean focus) {
      String c = ">";
      if (onWindows) c = selected ? " style=\"color:#ffffff;\">" : " style=\"color:#000000;\">";
      if (val instanceof AlloyInstance) return "<html> <b"+c+encode(title)+"</b></html>";
      if (val instanceof AlloyType) return "<html> <b"+c+"sig</b> <span"+c+encode(((AlloyType)val).getName())+"</span></html>";
      if (val instanceof AlloySet) return "<html> <b"+c+"set</b> <span"+c+encode(((AlloySet)val).getName())+"</span></html>";
      if (val instanceof AlloyAtom) return "<html> <span"+c+encode(((AlloyAtom)val).getVizName(theme, true))+"</span></html>";
      if (val instanceof Pair) return "<html> <b"+c+"field</b> <span"+c+encode(((AlloyRelation)(((Pair<?,?>)val).b)).getName())+"</span></html>";
      if (val instanceof AlloyTuple) {
         StringBuilder sb=new StringBuilder("<html> <span"+c);
         List<AlloyAtom> atoms=((AlloyTuple)val).getAtoms();
         for(int i=1; i<atoms.size(); i++) {
            if (i>1) sb.append(" -> ");
            sb.append(encode(atoms.get(i).getVizName(theme, true)));
         }
         sb.append("</span></html>");
         return sb.toString();
      }
      return "";
   }
}
