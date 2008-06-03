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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SpinnerNumberModel;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurCombobox;
import edu.mit.csail.sdg.alloy4.OurCheckbox;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * GUI panel for making customization changes.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizCustomizationPanel extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** This is the VizState object that this customization panel will customize. */
    private final VizState vizState;

    /** This is the background color for the upper-half of the customization panel. */
    private static final Color wcolor = new Color(0.9f, 0.9f, 0.9f);

    /** This is the upper-half of the customization panel. */
    private final JPanel zoomPane;

    /** This is the lower-half of the customization panel. */
    private JScrollPane widgetsScrollPane = null;

    /** The JSplitPane separating this customization panel with the main graph panel. */
    private final JSplitPane divider;

    /**
     * If it's an instance of AlloyElement, that means it's the latest selected type/set/relation.
     * If it's an Integer 1, 2, or 3, that means the latest selected panel is
     * the General Graph Settings, Default Type+Set, or Default Relation panels respectively.
     * All else, that means the zoom panel is empty.
     */
    private Object lastElement = null;

    //=============================================================================================================//

    /**
     * Constructs a customization panel.
     * @param divider - the JSplitPane separating the left-customization-half with the right-graph-half
     * @param vizState - the VizState object that will be customized by this customization panel
     */
    public VizCustomizationPanel(JSplitPane divider, VizState vizState) {
        this.divider = divider;
        this.vizState = vizState;
        setBorder(null);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        zoomPane = new JPanel();
        zoomPane.setBorder(new OurBorder(false,false,true,false));
        zoomPane.setLayout(new BoxLayout(zoomPane, BoxLayout.Y_AXIS));
        zoomPane.setAlignmentX(0f);
        zoomPane.setBackground(wcolor);
        remakeAll();
    }

    //=============================================================================================================//

    /**
     * This method selects a particular treenode and shows the details of a particular Type/Set/Relation.
     *
     * <p>
     * If x is an instance of AlloyElement, that means it's the latest selected type/set/relation.
     * If x is an Integer 1, 2, or 3, that means the latest selected panel is
     * the General Graph Settings, Default Type+Set, or Default Relation panels respectively.
     */
    private void zoom(Object x) {
        lastElement = x;
        zoomPane.removeAll();
        if (x instanceof AlloyNodeElement) makeNodeOptionsPanel(zoomPane, (AlloyNodeElement)x);
        else if (x instanceof AlloyRelation) makeEdgeOptionsPanel(zoomPane, (AlloyRelation)x);
        else if (Integer.valueOf(1).equals(x)) createGeneralWidget(zoomPane);
        else if (Integer.valueOf(2).equals(x)) createDefaultNodeWidget(zoomPane);
        else if (Integer.valueOf(3).equals(x)) createDefaultEdgeWidget(zoomPane);
        else {
            // The following 2 lines make sure the panel doesn't get too small on Mac
            zoomPane.add(OurUtil.makeH(wcolor, new JLabel(" "), (Object)null));
            zoomPane.add(OurUtil.makeBox(250, 200, wcolor, (Object)null));
        }
        Dimension dim = zoomPane.getPreferredSize();
        if (divider!=null && divider.getDividerLocation()<dim.width) divider.setDividerLocation(dim.width);
        if (divider!=null && divider.getDividerLocation()>dim.width) dim.width=divider.getDividerLocation();
        dim.height = 150;
        zoomPane.setPreferredSize(dim);
        dim.width = 450;
        zoomPane.setMinimumSize(dim);
        zoomPane.repaint();
        validate();
    }

    //=============================================================================================================//

    /**
     * Custom TreeCellRenderer to print the tree nodes better.
     * The idea of using JLabel is inspired by the DefaultTreeCellRenderer implementation.
     */
    private final class OurRenderer extends JLabel implements TreeCellRenderer {
        /** This suppresses javac's warning about missing serialVersionUID. */
        private static final long serialVersionUID = 1L;
        /** This stores the height of one line of text. */
        private int height;
        /** If preferredHeight>0, then preferredWidth is the desired width for the current object being drawn. */
        private int preferredWidth=0;
        /** If preferredHeight>0, then preferredHeight is the desired height for the current object being drawn. */
        private int preferredHeight=0;
        /** Whether the current object is selected or not. */
        private boolean isSelected;
        /** Whether the current object is focused or not. */
        private boolean isFocused;
        /** Constructs the Renderer. */
        public OurRenderer() {
            setVerticalAlignment(JLabel.BOTTOM);
            setBorder(new EmptyBorder(0, 3, 0, 3));
            setFont(OurUtil.getVizFont());
            setText("ABC"); // So that we can derive the height
            height=super.getPreferredSize().height;
        }
        /** Returns an object to be drawn. */
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                boolean isSelected, boolean expanded, boolean isLeaf, int row, boolean isFocused) {
            if (value instanceof DefaultMutableTreeNode) value=((DefaultMutableTreeNode)value).getUserObject();
            String string="";
            if (value instanceof AlloySet) {
                AlloySet x=(AlloySet)value;
                string="<html><b>set</b> "+x.getName()+"</html>";
            } else if (value instanceof AlloyRelation) {
                AlloyRelation x=(AlloyRelation)value;
                string=x.toString();
            } else if (value instanceof AlloyType) {
                AlloyType x=(AlloyType)value;
                if (!vizState.getCurrentModel().hasType(x))
                    string="<html><b>sig</b> "+typename(x)+" <font color=\"#808080\">(projected)</font></html>";
                else string="<html><b>sig</b> "+typename(x)+"</html>";
            }
            else if (Integer.valueOf(1).equals(value)) string="<html><b>general graph settings</b></html>";
            else if (Integer.valueOf(2).equals(value)) string="<html><b>types and sets</b></html>";
            else if (Integer.valueOf(3).equals(value)) string="<html><b>relations</b></html>";
            else string="";
            this.isFocused = isFocused;
            this.isSelected = isSelected;
            this.setText(string);
            this.setForeground(UIManager.getColor(isSelected ? "Tree.selectionForeground" : "Tree.textForeground"));
            // By default, we don't want to specify a width or height. Only specify if we need to...
            preferredHeight=0;
            if (Integer.valueOf(2).equals(value) || Integer.valueOf(3).equals(value)) {
                Dimension d=super.getPreferredSize();
                preferredWidth=d.width+3;
                preferredHeight=(d.height*2);
            }
            return this;
        }
        /** We override the getPreferredSize() method to return a custom size for "sig" and for "univ". */
        @Override public Dimension getPreferredSize() {
            if (preferredHeight!=0) return new Dimension(preferredWidth, preferredHeight);
            Dimension d=super.getPreferredSize();
            return new Dimension(d.width+3, d.height);
        }
        /** We override the paint() method to avoid drawing the box around the "extra space" above sig and univ. */
        @Override public void paint(Graphics g) {
            int w=getWidth(), h=getHeight(), y=h-height;
            Color background = isSelected ? UIManager.getColor("Tree.selectionBackground") : Color.WHITE;
            Color border = isFocused ? UIManager.getColor("Tree.selectionBorderColor") : null;
            if (background!=null) { g.setColor(background); g.fillRect(0,y,w,h-y); }
            if (border!=null && isSelected) { g.setColor(border); g.drawRect(0,y,w-1,h-1-y); }
            super.paint(g);
        }
    }

    //=============================================================================================================//

    /** Generate nodes for that type, all its subtypes and subsets. */
    private TreePath remakeForType(final boolean hidePrivate, final boolean hideMeta, TreePath path, AlloyType type) {
        TreePath last=null; // If nonnull, that means we've found which entry we should highlight
        AlloyModel old=vizState.getOriginalModel(), now=vizState.getCurrentModel();
        DefaultMutableTreeNode rad=null, rad2;
        if (!now.hasType(type) && !vizState.canProject(type)) return null;
        DefaultMutableTreeNode tailnode=(DefaultMutableTreeNode)(path.getLastPathComponent());
        tailnode.add(rad=new DefaultMutableTreeNode(type));
        path=path.pathByAddingChild(rad);
        if (type.equals(lastElement)) last=path;
        // Generate the nodes for all AlloySet(s) whose types == this type
        for (AlloySet s:now.getSets())
          if (!(hidePrivate && s.isPrivate) && !(hideMeta && s.isMeta) && s.getType().equals(type)) {
            rad.add(rad2=new DefaultMutableTreeNode(s));
            if (s.equals(lastElement)) last=path.pathByAddingChild(rad2);
          }
        // Generate the nodes for all AlloyType that inherit from this
        if (!type.isLeaf)
          for(AlloyType t:old.getDirectSubTypes(type))
            if (!(hidePrivate && t.isPrivate) && !(hideMeta && t.isMeta)) {
              TreePath possibleLast=remakeForType(hidePrivate, hideMeta, path, t);
              if (possibleLast!=null) last=possibleLast;
            }
        return last;
    }

    /** Regenerate all the customization widgets based on the latest settings. */
    public void remakeAll() {
        TreePath last=null; // If nonnull, that means we've found which entry we should highlight

        // Generate a new JPanel to house all the widgets
        JPanel elementsPanel = new JPanel();
        elementsPanel.setBorder(null);
        elementsPanel.setLayout(new BoxLayout(elementsPanel, BoxLayout.Y_AXIS));
        final boolean hidePrivate = vizState.hidePrivate();
        final boolean hideMeta = vizState.hideMeta();

        // Generate the nodes of the tree
        DefaultMutableTreeNode top = new DefaultMutableTreeNode("Theme Customizations");
        // Global Settings
        DefaultMutableTreeNode radA = new DefaultMutableTreeNode(1);
        top.add(radA);
        if (Integer.valueOf(1).equals(lastElement)) last=new TreePath(new Object[]{top,radA});
        // Types and Sets
        DefaultMutableTreeNode radTS = new DefaultMutableTreeNode(2);
        top.add(radTS);
        if (Integer.valueOf(2).equals(lastElement)) last=new TreePath(new Object[]{top,radTS});
        TreePath possibleLast=remakeForType(hidePrivate, hideMeta, new TreePath(new Object[]{top,radTS}), AlloyType.UNIV);
        if (possibleLast!=null) last=possibleLast;
        // Relations
        DefaultMutableTreeNode radR = new DefaultMutableTreeNode(3);
        top.add(radR);
        if (Integer.valueOf(3).equals(lastElement)) last=new TreePath(new Object[]{top,radR});
        for (AlloyRelation rel:vizState.getCurrentModel().getRelations())
          if (!(hidePrivate && rel.isPrivate) && !(hideMeta && rel.isMeta)) {
            DefaultMutableTreeNode rad;
            radR.add(rad=new DefaultMutableTreeNode(rel));
            if (rel.equals(lastElement)) last=new TreePath(new Object[]{top,radR,rad});
          }

        // Now, generate the tree
        final JTree tree = OurUtil.make(new JTree(top), Color.BLACK, Color.WHITE, new EmptyBorder(8,8,2,2));
        tree.setRootVisible(false);
        tree.setRowHeight(0); // To allow variable row height on Mac OS X
        tree.setCellRenderer(new OurRenderer());
        tree.setShowsRootHandles(false);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public final void valueChanged(TreeSelectionEvent e) {
                TreePath path = tree.getSelectionPath();
                if (path==null) {zoom(null);return;}
                Object x = path.getLastPathComponent();
                if (!(x instanceof DefaultMutableTreeNode)) {zoom(null);return;}
                zoom(((DefaultMutableTreeNode)x).getUserObject());
            }
        });

        // Remove the old widgets on display, and show these new widgets
        widgetsScrollPane = OurUtil.scrollpane(tree, Color.BLACK, Color.WHITE, Util.onMac() ? new OurBorder(false,false,false,true) : null);
        widgetsScrollPane.setAlignmentX(0f);
        widgetsScrollPane.getVerticalScrollBar().setUnitIncrement(50);

        // Pre-expand the entire tree
        for (int i=0; i<tree.getRowCount(); i++) tree.expandRow(i);

        // If we found the tree node corresponding to the previously selected item, then select it now.
        if (last!=null && lastElement!=null) {
            zoom(lastElement);
        } else {
            last=new TreePath(new Object[]{top,radA});
            zoom(1);
        }
        tree.scrollPathToVisible(last);
        tree.setSelectionPath(last);
        this.removeAll();
        this.add(zoomPane);
        this.add(widgetsScrollPane);
        this.validate();
    }

    //=============================================================================================================//

    /** Generates the node settings widgets for the given type or set, and add them to "parent". */
    private void makeNodeOptionsPanel(final JPanel answer, final AlloyNodeElement elt) {
        final boolean enabled = !(elt instanceof AlloyType) || (vizState.getCurrentModel().hasType((AlloyType)elt));
        if (elt instanceof AlloyType)
            answer.add(makelabel(" "+typename((AlloyType)elt)));
        else
            answer.add(makelabel(" "+elt.toString()));
        final JTextField labelText = OurUtil.textfield(vizState.label.get(elt), 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addKeyListener(new KeyListener() {
            public final void keyTyped(KeyEvent e) { }
            public final void keyPressed(KeyEvent e) { }
            public final void keyReleased(KeyEvent e) { vizState.label.put(elt, labelText.getText()); }
        });
        labelText.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { vizState.label.put(elt, labelText.getText()); }
        });
        labelText.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { }
            public final void focusLost(FocusEvent e) { vizState.label.put(elt, labelText.getText()); }
        });
        //
        final AlloyModel model=vizState.getCurrentModel();
        final AlloyNodeElement elt2;
        if (elt instanceof AlloyType) elt2=model.getSuperType((AlloyType)elt); else if (elt instanceof AlloySet) elt2=((AlloySet)elt).getType(); else elt2=null;
        JComboBox color = new OurCombobox(true, DotColor.values(DotColor.MAGIC), 100, 35, vizState.nodeColor.get(elt)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { if (value==null) return "Inherit"; else return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { if (value==null) value=vizState.nodeColor.resolve(elt2); return value==null ? null : ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.nodeColor.put(elt, (DotColor)value); }
        };
        JComboBox shape = new OurCombobox(true, DotShape.values(), 125, 35, vizState.shape.get(elt)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { if (value==null) return "Inherit"; else return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { if (value==null) value=vizState.shape.resolve(elt2); return value==null ? null : ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.shape.put(elt, (DotShape)value); }
        };
        JComboBox style = new OurCombobox(true, DotStyle.values(), 95, 35, vizState.nodeStyle.get(elt)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { if (value==null) return "Inherit"; else return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { if (value==null) value=vizState.nodeStyle.resolve(elt2); return value==null ? null : ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.nodeStyle.put(elt, (DotStyle)value); }
        };
        //
        answer.add(OurUtil.makeH(10, labelText, wcolor, color, style, shape, 2, null));
        if (elt instanceof AlloyType) {
            JPanel vis = vizState.nodeVisible    .pick(elt, "Show",                   "Display members as nodes");
            JPanel con = vizState.hideUnconnected.pick(elt, "Hide unconnected nodes", "Hide nodes without arcs");
            JPanel num = vizState.number         .pick(elt, "Number nodes",           "Attach atom number to node label as suffix");
            JPanel proj = null;
            if (vizState.canProject((AlloyType)elt))
                proj = new OurCheckbox("Project over this sig", "Click here to " + (enabled?"":"un") + "project over this signature", enabled ? OurCheckbox.ALL_OFF : OurCheckbox.ALL_ON) {
                    private static final long serialVersionUID = 1L;
                    public Icon do_action() {
                        if (enabled) projectAlloyType((AlloyType)elt); else deprojectAlloyType((AlloyType)elt);
                        lastElement=elt;
                        return enabled ? ALL_ON : ALL_OFF;
                    }
                };
            labelText.setEnabled(enabled && !vizState.useOriginalName());
            color.setEnabled(enabled);
            shape.setEnabled(enabled);
            style.setEnabled(enabled);
            vis.setEnabled(enabled);
            con.setEnabled(enabled);
            num.setEnabled(enabled && !vizState.useOriginalName());
            JPanel a=OurUtil.makeVR(wcolor,vis,num),b;
            if (proj!=null) b=OurUtil.makeVR(wcolor, con, proj); else b=OurUtil.makeVR(wcolor, con);
            answer.add(OurUtil.makeHT(wcolor, 15,a,15,b,2,null));
        } else {
            JPanel vis  = vizState.nodeVisible    .pick(elt, "Show",                        "Include members of set as nodes");
            JPanel attr = vizState.showAsAttr     .pick(elt, "Show in relation attributes", "Show set membership in relation attributes");
            JPanel lab  = vizState.showAsLabel    .pick(elt, "Show as labels",              "Show membership in set by labeling nodes");
            JPanel con  = vizState.hideUnconnected.pick(elt, "Hide unconnected nodes",      "Hide nodes without arcs");
            JPanel a=OurUtil.makeVR(wcolor,vis,lab), b=OurUtil.makeVR(wcolor, con, attr);
            answer.add(OurUtil.makeHT(wcolor, 15,a,15,b,2,null));
        }
    }

    //=============================================================================================================//

    /** Generates the edge settings widgets for the given relation, and add them to "parent". */
    private void makeEdgeOptionsPanel(final JPanel parent, final AlloyRelation rel) {
        final JTextField labelText = OurUtil.textfield(vizState.label.get(rel), 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addKeyListener(new KeyListener() {
           public void keyTyped(KeyEvent e)    { }
           public void keyPressed(KeyEvent e)  { }
           public void keyReleased(KeyEvent e) { vizState.label.put(rel, labelText.getText()); }
        });
        labelText.addActionListener(new ActionListener() {
           public final void actionPerformed(ActionEvent e) { vizState.label.put(rel, labelText.getText()); }
        });
        labelText.addFocusListener(new FocusListener() {
           public final void focusGained(FocusEvent e) { }
           public final void focusLost(FocusEvent e)   { vizState.label.put(rel, labelText.getText()); }
        });
        final JLabel weightLabel = OurUtil.label("Weight:");
        final JSpinner weightSpinner = new JSpinner(new SpinnerNumberModel(vizState.weight.get(rel), 0, 999, 1));
        weightSpinner.setMaximumSize(weightSpinner.getPreferredSize());
        weightSpinner.setToolTipText("A higher weight will cause the edge to be shorter and straighter.");
        weightSpinner.addKeyListener(new KeyListener() {
            public void keyTyped(KeyEvent e)    { }
            public void keyPressed(KeyEvent e)  { }
            public void keyReleased(KeyEvent e) { vizState.weight.put(rel, (Integer) (weightSpinner.getValue())); }
        });
        weightSpinner.addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e)  { vizState.weight.put(rel, (Integer) (weightSpinner.getValue())); }
            public void mousePressed(MouseEvent e)  { vizState.weight.put(rel, (Integer) (weightSpinner.getValue())); }
            public void mouseReleased(MouseEvent e) { vizState.weight.put(rel, (Integer) (weightSpinner.getValue())); }
            public void mouseEntered(MouseEvent e)  { }
            public void mouseExited(MouseEvent e)   { }
        });
        weightSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) { vizState.weight.put(rel, (Integer) (weightSpinner.getValue())); }
        });
        JPanel weightPanel = OurUtil.makeH(weightLabel, 5, weightSpinner);
        weightPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        weightPanel.setAlignmentY(0.5f);
        weightPanel.setToolTipText("A higher weight will cause the edge to be shorter and straighter.");
        OurCombobox color = new OurCombobox(true, DotColor.values(DotColor.WHITE), 110, 35, vizState.edgeColor.get(rel)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return value==null ? "Inherit" : ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { if (value==null) value=vizState.edgeColor.get(null); return value==null ? null : ((DotAttribute)value).getIcon(vizState.getEdgePalette()); }
            @Override public void   do_changed(Object value) { vizState.edgeColor.put(rel, (DotColor)value); }
        };
        OurCombobox style = new OurCombobox(true, DotStyle.values(), 105, 35, vizState.edgeStyle.get(rel)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return value==null ? "Inherit" : ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { if (value==null) value=vizState.edgeStyle.get(null); return value==null ? null : ((DotAttribute)value).getIcon(vizState.getEdgePalette()); }
            @Override public void   do_changed(Object value) { vizState.edgeStyle.put(rel, (DotStyle)value); }
        };
        JPanel visible    = vizState.edgeVisible.pick(rel, "Show as arcs",      "Show relation as arcs");
        JPanel attr       = vizState.attribute  .pick(rel, "Show as attribute", "Additionally display this relation as an attribute on the nodes' labels");
        JPanel back       = vizState.layoutBack .pick(rel, "Layout backwards",  "Layout graph as if arcs were reversed");
        JPanel merge      = vizState.mergeArrows.pick(rel, "Merge arrows",      "Merge opposing arrows between the same nodes as one bidirectional arrow");
        JPanel constraint = vizState.constraint .pick(rel, "Influence layout",  "Whether this edge influences the graph layout");
        JPanel panel1 = OurUtil.makeVR(wcolor, visible, attr, constraint);
        JPanel panel2 = OurUtil.makeVR(wcolor, back, merge);
        parent.add(makelabel(rel.toString()));
        parent.add(OurUtil.makeH(10, labelText, wcolor, 5, color, 5, style, 3, weightPanel, 2, null));
        parent.add(OurUtil.makeHT(wcolor, 10, panel1, 15, panel2, 2, null));
    }

    //=============================================================================================================//

    /** Generates the "general graph settings" widgets, and add them to "parent". */
    private void createGeneralWidget(JPanel parent) {
        final List<Object> fontSizes = Util.asList((Object)9,10,11,12,14,16,18,20,22,24,26,28,32,36,40,44,48,54,60,66,72);
        JLabel nLabel = OurUtil.label("Node Color Palette:");
        JLabel eLabel = OurUtil.label("Edge Color Palette:");
        JLabel aLabel = OurUtil.label("Use original atom names:");
        JLabel pLabel = OurUtil.label("Hide private sigs/relations:");
        JLabel mLabel = OurUtil.label("Hide meta sigs/relations:");
        JLabel fLabel = OurUtil.label("Font Size:");
        JComboBox fontSize = new OurCombobox(false, fontSizes, 60, 32, vizState.getFontSize()) {
            private static final long serialVersionUID = 1L;
            @Override public void do_changed(Object value) { if (fontSizes.contains(value)) vizState.setFontSize((Integer)value); }
        };
        JComboBox nodepal = new OurCombobox(false, DotPalette.values(), 100, 32, vizState.getNodePalette()) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public void   do_changed(Object value) { vizState.setNodePalette((DotPalette)value); }
        };
        JComboBox edgepal = new OurCombobox(false, DotPalette.values(), 100, 32, vizState.getEdgePalette()) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public void   do_changed(Object value) { vizState.setEdgePalette((DotPalette)value); }
        };
        JPanel name = new OurCheckbox("", "Whether the visualizer should use the original atom names as-is.", vizState.useOriginalName() ? OurCheckbox.ON : OurCheckbox.OFF) {
            private static final long serialVersionUID = 1L;
            public Icon do_action() { boolean x = vizState.useOriginalName();  vizState.useOriginalName(!x); return (!x ? ON : OFF); }
        };
        JPanel priv = new OurCheckbox("", "Whether the visualizer should hide private sigs, sets, and relations by default.", vizState.hidePrivate() ? OurCheckbox.ON : OurCheckbox.OFF) {
            private static final long serialVersionUID = 1L;
            public Icon do_action() { boolean x = vizState.hidePrivate();  vizState.hidePrivate(!x); remakeAll(); return (!x ? ON : OFF); }
        };
        JPanel meta = new OurCheckbox("", "Whether the visualizer should hide meta sigs, sets, and relations by default.", vizState.hideMeta() ? OurCheckbox.ON : OurCheckbox.OFF) {
            private static final long serialVersionUID = 1L;
            public Icon do_action() { boolean x = vizState.hideMeta();  vizState.hideMeta(!x); remakeAll(); return (!x ? ON : OFF); }
        };
        parent.add(makelabel(" General Graph Settings:"));
        parent.add(OurUtil.makeBox(6,6,wcolor));
        parent.add(OurUtil.makeH(wcolor, 25, nLabel, 5, nodepal, 8, aLabel, 5, name, 2, null));
        parent.add(OurUtil.makeH(wcolor, 25, eLabel, 5, edgepal, 8, fLabel, 5, fontSize, 2, null));
        parent.add(OurUtil.makeH(wcolor, 25, pLabel, 5, priv,    2, null));
        parent.add(OurUtil.makeH(wcolor, 25, mLabel, 5, meta,    2, null));
    }

    //=============================================================================================================//

    /** Generates the "default type and set settings" widgets, and add them to "parent". */
    private void createDefaultNodeWidget(JPanel parent) {
        JComboBox color = new OurCombobox(false, DotColor.values(DotColor.MAGIC), 110, 35, vizState.nodeColor.get(null)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { return ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.nodeColor.put(null, (DotColor)value); }
        };
        JComboBox shape = new OurCombobox(false, DotShape.values(), 135, 35, vizState.shape.get(null)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { return ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.shape.put(null, (DotShape)value); }
        };
        JComboBox style = new OurCombobox(false, DotStyle.values(), 110, 35, vizState.nodeStyle.get(null)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { return ((DotAttribute)value).getIcon(vizState.getNodePalette()); }
            @Override public void   do_changed(Object value) { vizState.nodeStyle.put(null, (DotStyle)value); }
        };
        JPanel vis  = vizState.nodeVisible    .pick("Show",                        "Show members of type as nodes");
        JPanel hide = vizState.hideUnconnected.pick("Hide unconnected nodes",      "Hide nodes without arcs");
        JPanel num  = vizState.number         .pick("Number nodes",                "Attach atom number to node label as suffix");
        JPanel label= vizState.showAsLabel    .pick("Show as labels",              "Show members as labels");
        JPanel attr = vizState.showAsAttr     .pick("Show in relation attributes", "Show set membership of endpoints when relation attributes are enabled");
        parent.add(makelabel(" Default Type and Set Settings:"));
        parent.add(OurUtil.makeH(wcolor, 10, color, 7, style, 7, shape, 2, null));
        JPanel a=OurUtil.makeVL(wcolor, vis, num, label), b=OurUtil.makeVL(wcolor, hide, attr);
        parent.add(OurUtil.makeHT(wcolor, 10, a, 10, b, 2, null));
    }

    //=============================================================================================================//

    /** Generates the "default relation settings" widgets, and add them to "parent". */
    private void createDefaultEdgeWidget(JPanel parent) {
        JComboBox colorComboE = new OurCombobox(false, DotColor.values(DotColor.WHITE), 110, 35, vizState.edgeColor.get(null)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { return ((DotAttribute)value).getIcon(vizState.getEdgePalette()); }
            @Override public void   do_changed(Object value) { vizState.edgeColor.put(null, (DotColor)value); }
        };
        JComboBox outlineComboE = new OurCombobox(false, DotStyle.values(), 110, 35, vizState.edgeStyle.get(null)) {
            private static final long serialVersionUID = 1L;
            @Override public String do_getText(Object value) { return ((DotAttribute)value).getDisplayedText(); }
            @Override public Icon   do_getIcon(Object value) { return ((DotAttribute)value).getIcon(vizState.getEdgePalette()); }
            @Override public void   do_changed(Object value) { vizState.edgeStyle.put(null, (DotStyle)value); }
        };
        JPanel dispCBE       = vizState.edgeVisible.pick("Show as arcs",       "Show relations as arcs");
        JPanel mergeCBE      = vizState.mergeArrows.pick("Merge arrows",       "Merge opposing arrows of the same relation");
        JPanel constraintCBE = vizState.constraint .pick("Influence layout",   "Whether this edge influences the graph layout");
        JPanel attrCBE       = vizState.attribute  .pick("Show as attributes", "Show relations as attributes on nodes");
        JPanel laybackCBE    = vizState.layoutBack .pick("Layout backwards",   "Layout graph as if arcs were reversed");
        parent.add(makelabel(" Default Relation Settings:"));
        parent.add(OurUtil.makeH(wcolor, 10, colorComboE, 8, outlineComboE, 2, null));
        JPanel a=OurUtil.makeVL(wcolor, dispCBE, attrCBE, constraintCBE, 10), b=OurUtil.makeVL(wcolor, laybackCBE, mergeCBE);
        parent.add(OurUtil.makeHT(wcolor, 10, a, 10, b, 2, null));
    }

    //=============================================================================================================//

    /** Convenient helper method that returns a description of an AlloyType (and what it extends). */
    private String typename(AlloyType type) {
        if (type.equals(AlloyType.UNIV)) return "univ";
        AlloyType sup=vizState.getOriginalModel().getSuperType(type);
        if (sup!=null && !sup.equals(AlloyType.UNIV)) return type.getName()+" extends "+sup.getName();
        return type.getName();
    }

    /** Generates a black JLabel for the given String. */
    private JLabel makelabel(String label) { return OurUtil.label(label, OurUtil.getVizFont().deriveFont(Font.BOLD)); }

    /** Project over the given type if we are allowed to. */
    private void projectAlloyType(AlloyType type) { vizState.project(type); remakeAll(); }

    /** Unproject over the given type if it is currently projected. */
    private void deprojectAlloyType(AlloyType type) { vizState.deproject(type); remakeAll(); }
}
