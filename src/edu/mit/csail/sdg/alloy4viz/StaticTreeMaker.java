/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.EmptyBorder;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * This utility class takes an AlloyInstance and generates a JTree showing
 * the types, sets, and relation tuples from it.
 *
 * <p><b>Thread Safety:</b>  Can be called only by the AWT event thread.
 */

public final class StaticTreeMaker {

    /** This provides a TreeModel view of an AlloyInstance. */
    private static final class StaticTreeModel implements TreeModel {
        private final AlloyInstance instance;
        private final List<AlloyElement> typesAndSets;
        public StaticTreeModel(AlloyInstance instance) {
            this.instance=instance;
            typesAndSets=new ArrayList<AlloyElement>(instance.model.getTypes());
            typesAndSets.addAll(instance.model.getSets());
        }
        private List<Pair<AlloyAtom,AlloyRelation>> getFields(AlloyAtom atom) {
            List<Pair<AlloyAtom,AlloyRelation>> ans=new ArrayList<Pair<AlloyAtom,AlloyRelation>>();
            for(AlloyRelation rel:instance.model.getRelations())
                for(AlloyTuple tuple:instance.relation2tuples(rel))
                    if (tuple.getStart().equals(atom)) { ans.add(new Pair<AlloyAtom,AlloyRelation>(atom,rel)); break; }
            return ans;
        }
        private List<Object> getTuplesOrAtoms(AlloyRelation rel, AlloyAtom atom) {
            List<Object> ans=new ArrayList<Object>();
            for(AlloyTuple tuple:instance.relation2tuples(rel))
                if (tuple.getStart().equals(atom)) {if (tuple.getArity()==2) ans.add(tuple.getEnd()); else ans.add(tuple);}
            return ans;
        }
        private List<Object> blank = Collections.unmodifiableList(new ArrayList<Object>());
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

    /** This implementation overrides the convertValueToText() method to pretty-print the values. */
    private static final class StaticJTree extends JTree {
        private static final long serialVersionUID = 1L;
        private final String title;
        private final VizState theme;
        private final boolean onWindows;
        public StaticJTree(AlloyInstance instance, String title, VizState theme) {
            this.title=title;
            this.theme=theme;
            this.onWindows=Util.onWindows();
            setModel(new StaticTreeModel(instance));
        }
        @Override public String convertValueToText(Object val,boolean selected,boolean expanded,boolean leaf,int row,boolean focus) {
            String c = ">";
            if (onWindows) c = selected ? " style=\"color:#ffffff;\">" : " style=\"color:#000000;\">";
            if (val instanceof AlloyInstance) return "<html> <b"+c+encode(title)+"</b></html>";
            if (val instanceof AlloyType) return "<html> <b"+c+"sig</b> <span"+c+encode(((AlloyType)val).getName())+"</span></html>";
            if (val instanceof AlloySet) return "<html> <b"+c+"set</b> <span"+c+encode(((AlloySet)val).getName())+"</span></html>";
            if (val instanceof AlloyAtom) return "<html> <span"+c+encode(((AlloyAtom)val).getVizName(theme,true))+"</span></html>";
            if (val instanceof Pair) return "<html> <b"+c+"field</b> <span"+c+encode(((AlloyRelation)(((Pair<?,?>)val).b)).getName())+"</span></html>";
            if (val instanceof AlloyTuple) {
                StringBuilder sb=new StringBuilder("<html> <span"+c);
                List<AlloyAtom> atoms=((AlloyTuple)val).getAtoms();
                for(int i=1; i<atoms.size(); i++) {
                    if (i>1) sb.append(" -> ");
                    sb.append(encode(atoms.get(i).getVizName(theme,true)));
                }
                sb.append("</span></html>");
                return sb.toString();
            }
            return "";
        }

    }

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private StaticTreeMaker() { }

    /** Encodes a String for HTML. */
    private static String encode(String x) {
        StringBuilder sb=new StringBuilder();
        for(int i=0; i<x.length(); i++) {
            char c=x.charAt(i);
            if (c=='&') sb.append("&amp;");
            else if (c=='<') sb.append("&lt;");
            else if (c=='>') sb.append("&gt;");
            else sb.append(c);
        }
        return sb.toString();
    }

    /**
     * Generate a scrollpane containing a JTree (showing the sigs, sets, and relations in the given instance)
     * @param instance - the instance to display
     * @param theme - the theme for displaying the atom labels (can be null if we don't want to use a theme)
     */
    public static JScrollPane makeTree(AlloyInstance instance, String title, VizState theme) {
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
        final JTree tree = new StaticJTree(instance,title,theme);
        tree.setBorder(new EmptyBorder(2,2,2,2));
        tree.setFont(OurUtil.getVizFont());
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.setRootVisible(true);
        tree.setBackground(Color.WHITE);
        tree.setOpaque(true);
        tree.setCellRenderer(renderer);
        final JScrollPane scroll=OurUtil.scrollpane(tree);
        scroll.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { tree.requestFocusInWindow(); }
            public final void focusLost(FocusEvent e) { }
        });
        scroll.setBorder(new OurBorder(true,false,true,false));
        scroll.setBackground(Color.WHITE);
        return scroll;
    }
}
