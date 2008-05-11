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
import edu.mit.csail.sdg.alloy4.OurBinaryCheckbox;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurCombobox;
import edu.mit.csail.sdg.alloy4.OurTristateCheckbox;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.OurCombobox.ComboGetterSetter;
import edu.mit.csail.sdg.alloy4.OurBinaryCheckbox.BinaryGetterSetter;
import edu.mit.csail.sdg.alloy4.OurTristateCheckbox.GetterSetter;

/**
 * GUI panel for making customization changes.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizCustomizationPanel extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** These are the possible fields that can be modified from VizState. */
    private enum Field {
        COLOR, STYLE, VISIBLE, LABEL, SAMERANK,
        NUMBER, SHOWINATTR, SHOWLABEL, SHAPE, HIDEUNCONNECTED,
        CONSTRAINT, WEIGHT, ATTRIBUTE, MERGEARROWS, LAYOUTBACK;
    };

    /** This is the VizState object that this customization panel will customize. */
    private final VizState vizState;

    /** This is the background color for the upper-half of the customization panel. */
    private static final Color wcolor = new Color(0.9f, 0.9f, 0.9f);

    /** This is the upper-half of the customization panel. */
    private final JPanel zoomPane;

    /** This is the lower-half of the customization panel. */
    private JScrollPane widgetsScrollPane=null;

    /** The JSplitPane separating this customization panel with the main graph panel. */
    private final JSplitPane divider;

    /**
     * If it's an instance of AlloyElement, that means it's the latest selected type/set/relation.
     * If it's an Integer 1, 2, or 3, that means the latest selected panel is
     * the General Graph Settings, Default Type+Set, or Default Relation panels respectively.
     * All else, that means the zoom panel is empty.
     */
    private Object lastElement=null;

    //=============================================================================================================//

    /**
     * Constructs a customization panel.
     * @param divider - the JSplitPane separating the left-customization-half with the right-graph-half
     * @param vizState - the VizState object that will be customized by this customization panel
     */
    public VizCustomizationPanel(JSplitPane divider, VizState vizState) {
        this.divider=divider;
        this.vizState=vizState;
        setBorder(null);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        zoomPane=new JPanel();
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
        lastElement=x;
        zoomPane.removeAll();
        if (x instanceof AlloyNodeElement) makeNodeOptionsPanel(zoomPane, (AlloyNodeElement)x);
        else if (x instanceof AlloyRelation) makeEdgeOptionsPanel(zoomPane, (AlloyRelation)x);
        else if (Integer.valueOf(1).equals(x)) createGeneralWidget(zoomPane);
        else if (Integer.valueOf(2).equals(x)) createDefaultNodeWidget(zoomPane);
        else if (Integer.valueOf(3).equals(x)) createDefaultEdgeWidget(zoomPane);
        else {
            // The following 2 lines make sure the panel doesn't get too small on Mac
            zoomPane.add(OurUtil.makeH(wcolor,new JLabel(" "),(Object)null));
            zoomPane.add(OurUtil.makeBox(250,200,wcolor,(Object)null));
        }
        Dimension dim=zoomPane.getPreferredSize();
        if (divider!=null && divider.getDividerLocation()<dim.width) divider.setDividerLocation(dim.width);
        if (divider!=null && divider.getDividerLocation()>dim.width) dim.width=divider.getDividerLocation();
        dim.height=150;
        zoomPane.setPreferredSize(dim);
        dim.width=450;
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
    private TreePath remakeForType(final boolean hidePrivate, TreePath path, AlloyType type) {
        TreePath last=null; // If nonnull, that means we've found which entry we should highlight
        AlloyModel old=vizState.getOriginalModel(), now=vizState.getCurrentModel();
        DefaultMutableTreeNode rad=null, rad2;
        if (!now.hasType(type) && !vizState.canProject(type)) return null;
        DefaultMutableTreeNode tailnode=(DefaultMutableTreeNode)(path.getLastPathComponent());
        tailnode.add(rad=new DefaultMutableTreeNode(type));
        path=path.pathByAddingChild(rad);
        if (type.equals(lastElement)) last=path;
        // Generate the nodes for all AlloySet(s) whose types == this type
        for (AlloySet s:now.getSets()) if (!(hidePrivate && s.isPrivate) && s.getType().equals(type)) {
            rad.add(rad2=new DefaultMutableTreeNode(s));
            if (s.equals(lastElement)) last=path.pathByAddingChild(rad2);
        }
        // Generate the nodes for all AlloyType that inherit from this
        for(AlloyType t:old.getDirectSubTypes(type)) if (!(hidePrivate && t.isPrivate)) {
            TreePath possibleLast=remakeForType(hidePrivate, path, t);
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
        TreePath possibleLast=remakeForType(hidePrivate, new TreePath(new Object[]{top,radTS}), AlloyType.UNIV);
        if (possibleLast!=null) last=possibleLast;
        // Relations
        DefaultMutableTreeNode radR = new DefaultMutableTreeNode(3);
        top.add(radR);
        if (Integer.valueOf(3).equals(lastElement)) last=new TreePath(new Object[]{top,radR});
        for (AlloyRelation rel:vizState.getCurrentModel().getRelations()) if (!(hidePrivate && rel.isPrivate)) {
            DefaultMutableTreeNode rad;
            radR.add(rad=new DefaultMutableTreeNode(rel));
            if (rel.equals(lastElement)) last=new TreePath(new Object[]{top,radR,rad});
        }

        // Now, generate the tree
        final JTree tree = new JTree(top);
        tree.setOpaque(true);
        tree.setRootVisible(false);
        tree.setRowHeight(0); // To allow variable row height on Mac OS X
        tree.setCellRenderer(new OurRenderer());
        tree.setShowsRootHandles(false);
        tree.setBorder(new EmptyBorder(8,8,2,2));
        tree.setFont(OurUtil.getVizFont());
        tree.setBackground(Color.WHITE);
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
        widgetsScrollPane = new JScrollPane(tree);
        widgetsScrollPane.setBackground(Color.WHITE);
        widgetsScrollPane.setAlignmentX(0f);
        widgetsScrollPane.getVerticalScrollBar().setUnitIncrement(50);
        if (Util.onMac()) widgetsScrollPane.setBorder(new OurBorder(false,false,false,true));
        else widgetsScrollPane.setBorder(new EmptyBorder(0,0,0,0));

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
        ComboGetterSetter cgs=new ComboGetterSetter() {
            public Icon getIcon(Object key, Object value) {
                if (value==null) {
                    AlloyModel model=vizState.getCurrentModel();
                    AlloyNodeElement elt2=null;
                    if (elt instanceof AlloyType) elt2=model.getSuperType((AlloyType)elt);
                    if (elt instanceof AlloySet) elt2=((AlloySet)elt).getType();
                    if (key==Field.COLOR) value=vizState.nodeColor(elt2,model);
                    if (key==Field.STYLE) value=vizState.nodeStyle(elt2,model);
                    if (key==Field.SHAPE) value=vizState.shape(elt2,model);
                }
                if (value==null) return null; else return ((DotAttribute)value).getIcon(vizState.getNodePalette());
            }
            public String getText(Object key, Object value) {
                if (value==null) return "Inherit"; else return ((DotAttribute)value).getDisplayedText() ;
            }
            public Object getValue(Object key) {
                if (key==Field.COLOR) return vizState.nodeColor(elt);
                if (key==Field.STYLE) return vizState.nodeStyle(elt);
                /*if (key==Field.SHAPE)*/ return vizState.shape(elt);
            }
            public void setValue(Object key, Object value) {
                if (key==Field.COLOR) vizState.nodeColor(elt, (DotColor)value);
                if (key==Field.STYLE) vizState.nodeStyle(elt, (DotStyle)value);
                if (key==Field.SHAPE) vizState.shape(elt, (DotShape)value);
            }
        };
        GetterSetter gs=new GetterSetter() {
            public Boolean get(Object key) {
                if (key==Field.VISIBLE) return vizState.nodeVisible(elt);
                if (key==Field.SAMERANK) return vizState.nodeSameRank(elt);
                if (key==Field.HIDEUNCONNECTED) return vizState.hideUnconnected(elt);
                if (key==Field.NUMBER) return vizState.number((AlloyType)elt);
                if (key==Field.SHOWLABEL) return vizState.showAsLabel((AlloySet)elt);
                /*if (key==Field.SHOWINATTR)*/ return vizState.showAsAttr((AlloySet)elt);
            }
            public boolean getInherited(Object key) {
                AlloyModel model=vizState.getCurrentModel();
                if (key==Field.VISIBLE) return vizState.nodeVisible(elt,model);
                if (key==Field.SAMERANK) return vizState.nodeSameRank(elt,model);
                if (key==Field.HIDEUNCONNECTED) return vizState.hideUnconnected(elt,model);
                if (key==Field.NUMBER) return vizState.number((AlloyType)elt,model);
                if (key==Field.SHOWLABEL) return vizState.showAsLabel((AlloySet)elt,model);
                /*if (key==Field.SHOWINATTR)*/ return vizState.showAsAttr((AlloySet)elt,model);
            }
            public void set(Object key, Boolean value) {
                if (key==Field.VISIBLE) vizState.nodeVisible(elt, value);
                if (key==Field.SAMERANK) vizState.nodeSameRank(elt, value);
                if (key==Field.HIDEUNCONNECTED) vizState.hideUnconnected(elt, value);
                if (key==Field.NUMBER) vizState.number((AlloyType)elt, value);
                if (key==Field.SHOWLABEL) vizState.showAsLabel((AlloySet)elt, value);
                if (key==Field.SHOWINATTR) vizState.showAsAttr((AlloySet)elt, value);
            }
        };
        GetterSetter ps=new GetterSetter() {
            public Boolean get(Object key) { return !enabled; }
            public boolean getInherited(Object key) { return true; }
            public void set(Object key, Boolean value) {
                if (enabled) { lastElement=elt; projectAlloyType((AlloyType)elt); }
                else { lastElement=elt; deprojectAlloyType((AlloyType)elt); }
            }
        };
        if (elt instanceof AlloyType)
            answer.add(makelabel(" "+typename((AlloyType)elt)));
        else
            answer.add(makelabel(" "+elt.toString()));
        final JTextField labelText = OurUtil.textfield(vizState.label(elt), 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addKeyListener(new KeyListener() {
            public final void keyTyped(KeyEvent e) { }
            public final void keyPressed(KeyEvent e) { }
            public final void keyReleased(KeyEvent e) { vizState.label(elt, labelText.getText()); }
        });
        labelText.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { vizState.label(elt, labelText.getText()); }
        });
        labelText.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { }
            public final void focusLost(FocusEvent e) { vizState.label(elt, labelText.getText()); }
        });
        JComboBox color = new OurCombobox(cgs, true, DotColor.values(DotColor.MAGIC), 100, 35, Field.COLOR);
        JComboBox shape = new OurCombobox(cgs, true, DotShape.values(),               125, 35, Field.SHAPE);
        JComboBox style = new OurCombobox(cgs, true, DotStyle.values(),                95, 35, Field.STYLE);
        answer.add(OurUtil.makeH(10, labelText, wcolor, color, style, shape, 2, null));
        if (elt instanceof AlloyType) {
            JPanel vis =new OurTristateCheckbox(gs, Field.VISIBLE,         "Show",                   "Display members as nodes");
            //JPanel rank=new OurTristateCheckbox(gs, Field.SAMERANK,        "Align members",          "Aligns nodes of this type");
            JPanel con =new OurTristateCheckbox(gs, Field.HIDEUNCONNECTED, "Hide unconnected nodes", "Hide nodes without arcs");
            JPanel num =new OurTristateCheckbox(gs, Field.NUMBER,          "Number nodes",           "Attach atom number to node label as suffix");
            JPanel proj;
            if (!vizState.canProject((AlloyType)elt)) proj=null;
            else if (enabled) proj=new OurTristateCheckbox(ps, null, "Project over this sig", "Click here to project over this signature");
            else proj=new OurTristateCheckbox(ps, null, "Project over this sig", "Click here to unproject over this signature");
            labelText.setEnabled(enabled && !vizState.useOriginalName());
            color.setEnabled(enabled);
            shape.setEnabled(enabled);
            style.setEnabled(enabled);
            vis.setEnabled(enabled);
            //rank.setEnabled(enabled);
            con.setEnabled(enabled);
            num.setEnabled(enabled && !vizState.useOriginalName());
            JPanel a=OurUtil.makeVR(wcolor,vis,num),b;
            if (proj!=null) b=OurUtil.makeVR(wcolor,/*rank,*/con,proj); else b=OurUtil.makeVR(wcolor,/*rank,*/con);
            answer.add(OurUtil.makeHT(wcolor, 15,a,15,b,2,null));
        } else {
            JPanel vis =new OurTristateCheckbox(gs, Field.VISIBLE,         "Show",                        "Include members of set as nodes");
            //JPanel rank=new OurTristateCheckbox(gs, Field.SAMERANK,        "Align members",               "Aligns members of this set");
            JPanel attr=new OurTristateCheckbox(gs, Field.SHOWINATTR,      "Show in relation attributes", "Show set membership in relation attributes");
            JPanel lab =new OurTristateCheckbox(gs, Field.SHOWLABEL,       "Show as labels",              "Show membership in set by labeling nodes");
            JPanel con =new OurTristateCheckbox(gs, Field.HIDEUNCONNECTED, "Hide unconnected nodes",      "Hide nodes without arcs");
            JPanel a=OurUtil.makeVR(wcolor,vis,lab), b=OurUtil.makeVR(wcolor,/*rank,*/con,attr);
            answer.add(OurUtil.makeHT(wcolor, 15,a,15,b,2,null));
        }
    }

    //=============================================================================================================//

    /** Generates the edge settings widgets for the given relation, and add them to "parent". */
    private void makeEdgeOptionsPanel(final JPanel parent, final AlloyRelation rel) {
        ComboGetterSetter cgs=new ComboGetterSetter() {
            public Icon getIcon(Object key, Object value) {
                if (value==null && key==Field.COLOR) value=vizState.edgeColor(null);
                if (value==null && key==Field.STYLE) value=vizState.edgeStyle(null);
                if (value==null) return null; else return ((DotAttribute)value).getIcon(vizState.getEdgePalette());
            }
            public String getText(Object key, Object value) {
                if (value==null) return "Inherit"; else return ((DotAttribute)value).getDisplayedText() ;
            }
            public Object getValue(Object key) {
                if (key==Field.COLOR) return vizState.edgeColor(rel);
                /*if (key==Field.STYLE)*/ return vizState.edgeStyle(rel);
            }
            public void setValue(Object key, Object value) {
                if (key==Field.COLOR) vizState.edgeColor(rel, (DotColor)value);
                if (key==Field.STYLE) vizState.edgeStyle(rel, (DotStyle)value);
            }
        };
        GetterSetter gs=new GetterSetter() {
            public Boolean get(Object key) {
                if (key==Field.VISIBLE) return vizState.edgeVisible(rel);
                if (key==Field.SAMERANK) return vizState.edgeSameRank(rel);
                if (key==Field.MERGEARROWS) return vizState.mergeArrows(rel);
                if (key==Field.ATTRIBUTE) return vizState.attribute(rel);
                if (key==Field.CONSTRAINT) return vizState.constraint(rel);
                /*if (key==Field.LAYOUTBACK)*/ return vizState.layoutBack(rel);
            }
            public boolean getInherited(Object key) {
                boolean val=false;
                if (key==Field.VISIBLE) val=vizState.edgeVisible(rel, vizState.getCurrentModel());
                if (key==Field.SAMERANK) val=vizState.edgeSameRank(rel, vizState.getCurrentModel());
                if (key==Field.MERGEARROWS) val=vizState.mergeArrows(rel, vizState.getCurrentModel());
                if (key==Field.ATTRIBUTE) val=vizState.attribute(rel, vizState.getCurrentModel());
                if (key==Field.CONSTRAINT) val=vizState.constraint(rel, vizState.getCurrentModel());
                if (key==Field.LAYOUTBACK) val=vizState.layoutBack(rel, vizState.getCurrentModel());
                return val;
            }
            public void set(Object key, Boolean value) {
                if (key==Field.VISIBLE) vizState.edgeVisible(rel, value);
                if (key==Field.SAMERANK) vizState.edgeSameRank(rel, value);
                if (key==Field.MERGEARROWS) vizState.mergeArrows(rel, value);
                if (key==Field.ATTRIBUTE) vizState.attribute(rel, value);
                if (key==Field.CONSTRAINT) vizState.constraint(rel, value);
                if (key==Field.LAYOUTBACK) vizState.layoutBack(rel, value);
            }
        };
        final JTextField labelText = OurUtil.textfield(vizState.label(rel), 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addKeyListener(new KeyListener() {
            public void keyTyped(KeyEvent e) { }
            public void keyPressed(KeyEvent e) { }
            public void keyReleased(KeyEvent e) { vizState.label(rel, labelText.getText()); }
        });
        labelText.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { vizState.label(rel, labelText.getText()); }
        });
        labelText.addFocusListener(new FocusListener() {
            public final void focusGained(FocusEvent e) { }
            public final void focusLost(FocusEvent e) { vizState.label(rel, labelText.getText()); }
        });
        final JLabel weightLabel = OurUtil.label(OurUtil.getVizFont(), "Weight:");
        final JSpinner weightSpinner = new JSpinner(new SpinnerNumberModel(vizState.weight(rel), 0, 999, 1));
        weightSpinner.setMaximumSize(weightSpinner.getPreferredSize());
        weightSpinner.setToolTipText("A higher weight will cause the edge to be shorter and straighter.");
        weightSpinner.addKeyListener(new KeyListener() {
            public void keyTyped(KeyEvent e) { }
            public void keyPressed(KeyEvent e) { }
            public void keyReleased(KeyEvent e) { vizState.weight(rel, (Integer) (weightSpinner.getValue())); }
        });
        weightSpinner.addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e) { vizState.weight(rel, (Integer) (weightSpinner.getValue())); }
            public void mousePressed(MouseEvent e) { vizState.weight(rel, (Integer) (weightSpinner.getValue())); }
            public void mouseReleased(MouseEvent e) { vizState.weight(rel, (Integer) (weightSpinner.getValue())); }
            public void mouseEntered(MouseEvent e) { }
            public void mouseExited(MouseEvent e) { }
        });
        weightSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) { vizState.weight(rel, (Integer) (weightSpinner.getValue())); }
        });
        JPanel weightPanel = OurUtil.makeH(weightLabel, 5, weightSpinner);
        weightPanel.setBorder(new EmptyBorder(5,5,5,5));
        weightPanel.setAlignmentY(0.5f);
        weightPanel.setToolTipText("A higher weight will cause the edge to be shorter and straighter.");
        OurCombobox color = new OurCombobox(cgs, true, DotColor.values(DotColor.WHITE), 110, 35, Field.COLOR);
        OurCombobox style = new OurCombobox(cgs, true, DotStyle.values(),               105, 35, Field.STYLE);
        JPanel visible=new OurTristateCheckbox(gs, Field.VISIBLE,     "Show as arcs",      "Show relation as arcs");
        JPanel attr=new OurTristateCheckbox(gs,    Field.ATTRIBUTE,   "Show as attribute", "Additionally display this relation as an attribute on the nodes' labels");
        //JPanel rank=new OurTristateCheckbox(gs,    Field.SAMERANK,    "Align endpoints",   "Align nodes connected by this relation's arcs");
        JPanel back=new OurTristateCheckbox(gs,    Field.LAYOUTBACK,  "Layout backwards",  "Layout graph as if arcs were reversed");
        JPanel merge=new OurTristateCheckbox(gs,   Field.MERGEARROWS, "Merge arrows",      "Merge opposing arrows between the same nodes as one bidirectional arrow");
        JPanel constraint=new OurTristateCheckbox(gs,Field.CONSTRAINT, "Influence layout",      "Whether this edge influences the graph layout");
        JPanel panel1=OurUtil.makeVR(wcolor, visible, attr, constraint);
        JPanel panel2=OurUtil.makeVR(wcolor, /*rank,*/ back, merge);
        parent.add(makelabel(rel.toString()));
        parent.add(OurUtil.makeH(10, labelText, wcolor, 5, color, 5, style, 3, weightPanel, 2, null));
        parent.add(OurUtil.makeHT(wcolor, 10, panel1, 15, panel2, 2, null));
    }

    //=============================================================================================================//

    /** Generates the "general graph settings" widgets, and add them to "parent". */
    private void createGeneralWidget(JPanel parent) {
        final List<Object> fontSizes = Util.asList((Object)9,10,11,12,14,16,18,20,22,24,26,28,32,36,40,44,48,54,60,66,72);
        final ComboGetterSetter cgs=new ComboGetterSetter() {
            public Icon getIcon(Object key, Object value) { return null; }
            public String getText(Object key, Object value) {
                if ("0".equals(key)) return value.toString(); else return ((DotAttribute)value).getDisplayedText();
            }
            public Object getValue(Object key) {
                if ("1".equals(key)) return vizState.getOrientation();
                if ("2".equals(key)) return vizState.getNodePalette();
                if ("3".equals(key)) return vizState.getEdgePalette();
                return vizState.getFontSize();
            }
            public void setValue(Object key, Object value) {
                if ("1".equals(key)) vizState.setOrientation((DotOrientation)value);
                if ("2".equals(key)) vizState.setNodePalette((DotPalette)value);
                if ("3".equals(key)) vizState.setEdgePalette((DotPalette)value);
                if (value instanceof Integer && fontSizes.contains(value)) vizState.setFontSize((Integer)value);
            }
        };
        final BinaryGetterSetter bgs = new BinaryGetterSetter() {
            public boolean get(Object key) { return vizState.useOriginalName(); }
            public void set(Object key, boolean value) { vizState.useOriginalName(value); }
        };
        final BinaryGetterSetter pgs = new BinaryGetterSetter() {
            public boolean get(Object key) { return vizState.hidePrivate(); }
            public void set(Object key, boolean value) { vizState.hidePrivate(value); remakeAll(); }
        };
        JLabel nLabel = OurUtil.label(OurUtil.getVizFont(), "Node Color Palette:");
        JLabel eLabel = OurUtil.label(OurUtil.getVizFont(), "Edge Color Palette:");
        JLabel aLabel = OurUtil.label(OurUtil.getVizFont(), "Use original atom names:");
        JLabel pLabel = OurUtil.label(OurUtil.getVizFont(), "Hide private sigs/relations:");
        JLabel fLabel = OurUtil.label(OurUtil.getVizFont(), "Font Size:");
        JComboBox fontSize= new OurCombobox(cgs, false, fontSizes,                60, 32, "0");
        JComboBox nodepal = new OurCombobox(cgs, false, DotPalette.values(),     100, 32, "2");
        JComboBox edgepal = new OurCombobox(cgs, false, DotPalette.values(),     100, 32, "3");
        JPanel name = new OurBinaryCheckbox(bgs, null, "", "Whether the visualizer should use the original atom names as-is.");
        JPanel priv = new OurBinaryCheckbox(pgs, null, "", "Whether the visualizer should hide private sigs, sets, and relations by default.");
        parent.add(makelabel(" General Graph Settings:"));
        parent.add(OurUtil.makeBox(6,6,wcolor));
        parent.add(OurUtil.makeH(wcolor, 25, nLabel, 5, nodepal, 8, aLabel, 5, name, 2, null));
        parent.add(OurUtil.makeH(wcolor, 25, eLabel, 5, edgepal, 8, fLabel, 5, fontSize, 2, null));
        parent.add(OurUtil.makeH(wcolor, 25, pLabel, 5, priv,  2, null));
    }

    //=============================================================================================================//

    /** Generates the "default type and set settings" widgets, and add them to "parent". */
    private void createDefaultNodeWidget(JPanel parent) {
        ComboGetterSetter cgs=new ComboGetterSetter() {
            public Icon getIcon(Object key, Object value) {
                return ((DotAttribute)value).getIcon(vizState.getNodePalette());
            }
            public String getText(Object key, Object value) {
                return ((DotAttribute)value).getDisplayedText();
            }
            public Object getValue(Object key) {
                if (key==Field.COLOR) return vizState.nodeColor(null);
                if (key==Field.SHAPE) return vizState.shape(null);
                /*if (key==Field.STYLE)*/ return vizState.nodeStyle(null);
            }
            public void setValue(Object key, Object value) {
                if (key==Field.COLOR) vizState.nodeColor(null, (DotColor)value);
                if (key==Field.SHAPE) vizState.shape(null, (DotShape)value);
                if (key==Field.STYLE) vizState.nodeStyle(null, (DotStyle)value);
            }
        };
        BinaryGetterSetter gs = new BinaryGetterSetter() {
            public boolean get(Object key) {
                if (key==Field.VISIBLE) return vizState.nodeVisible(null);
                if (key==Field.SAMERANK) return vizState.nodeSameRank(null);
                if (key==Field.HIDEUNCONNECTED) return vizState.hideUnconnected(null);
                if (key==Field.NUMBER) return vizState.number(null);
                if (key==Field.SHOWLABEL) return vizState.showAsLabel(null);
                /*if (key==Field.SHOWINATTR)*/ return vizState.showAsAttr(null);
            }
            public void set(Object key, boolean value) {
                if (key==Field.VISIBLE) vizState.nodeVisible(null, value);
                if (key==Field.SAMERANK) vizState.nodeSameRank(null, value);
                if (key==Field.HIDEUNCONNECTED) vizState.hideUnconnected(null, value);
                if (key==Field.NUMBER) vizState.number(null, value);
                if (key==Field.SHOWLABEL) vizState.showAsLabel(null, value);
                if (key==Field.SHOWINATTR) vizState.showAsAttr(null, value);
            }
        };
        JComboBox color = new OurCombobox(cgs, false, DotColor.values(DotColor.MAGIC), 110, 35, Field.COLOR);
        JComboBox shape = new OurCombobox(cgs, false, DotShape.values(),               135, 35, Field.SHAPE);
        JComboBox style = new OurCombobox(cgs, false, DotStyle.values(),               110, 35, Field.STYLE);
        JPanel vis  = new OurBinaryCheckbox(gs, Field.VISIBLE,         "Show",                   "Show members of type as nodes");
        //JPanel rank = new OurBinaryCheckbox(gs, Field.SAMERANK,        "Align members",          "Align nodes of the same type");
        JPanel hide = new OurBinaryCheckbox(gs, Field.HIDEUNCONNECTED, "Hide unconnected nodes", "Hide nodes without arcs");
        JPanel num  = new OurBinaryCheckbox(gs, Field.NUMBER,          "Number nodes",           "Attach atom number to node label as suffix");
        JPanel label= new OurBinaryCheckbox(gs, Field.SHOWLABEL,  "Show as labels", "Show members as labels");
        JPanel attr = new OurBinaryCheckbox(gs, Field.SHOWINATTR, "Show in relation attributes", "Show set membership of endpoints when relation attributes are enabled");
        parent.add(makelabel(" Default Type and Set Settings:"));
        parent.add(OurUtil.makeH(wcolor, 10, color, 7, style, 7, shape, 2, null));
        JPanel a=OurUtil.makeVL(wcolor,vis,num,label), b=OurUtil.makeVL(wcolor,/*rank,*/hide,attr);
        parent.add(OurUtil.makeHT(wcolor, 10, a, 10, b, 2, null));
    }

    //=============================================================================================================//

    /** Generates the "default relation settings" widgets, and add them to "parent". */
    private void createDefaultEdgeWidget(JPanel parent) {
        ComboGetterSetter cgs=new ComboGetterSetter() {
            public Icon getIcon(Object key, Object value) {
                return ((DotAttribute)value).getIcon(vizState.getEdgePalette());
            }
            public String getText(Object key, Object value) {
                return ((DotAttribute)value).getDisplayedText();
            }
            public Object getValue(Object key) {
                if (key==Field.COLOR) return vizState.edgeColor(null);
                /* if (key==Field.STYLE) */ return vizState.edgeStyle(null);
            }
            public void setValue(Object key, Object value) {
                if (key==Field.COLOR) vizState.edgeColor(null, (DotColor)value);
                if (key==Field.STYLE) vizState.edgeStyle(null, (DotStyle)value);
            }
        };
        BinaryGetterSetter gs = new BinaryGetterSetter() {
            public boolean get(Object key) {
                if (key==Field.VISIBLE) return vizState.edgeVisible(null);
                if (key==Field.SAMERANK) return vizState.edgeSameRank(null);
                if (key==Field.MERGEARROWS) return vizState.mergeArrows(null);
                if (key==Field.ATTRIBUTE) return vizState.attribute(null);
                if (key==Field.CONSTRAINT) return vizState.constraint(null);
                /*if (key==Field.LAYOUTBACK)*/ return vizState.layoutBack(null);
            }
            public void set(Object key, boolean value) {
                if (key==Field.VISIBLE) vizState.edgeVisible(null, value);
                if (key==Field.SAMERANK) vizState.edgeSameRank(null, value);
                if (key==Field.MERGEARROWS) vizState.mergeArrows(null, value);
                if (key==Field.ATTRIBUTE) vizState.attribute(null, value);
                if (key==Field.CONSTRAINT) vizState.constraint(null, value);
                if (key==Field.LAYOUTBACK) vizState.layoutBack(null, value);
            }
        };
        JComboBox colorComboE   = new OurCombobox(cgs, false, DotColor.values(DotColor.WHITE), 110, 35, Field.COLOR);
        JComboBox outlineComboE = new OurCombobox(cgs, false, DotStyle.values(),               110, 35, Field.STYLE);
        JPanel dispCBE       = new OurBinaryCheckbox(gs, Field.VISIBLE,     "Show as arcs", "Show relations as arcs");
        //JPanel rankCBE     = new OurBinaryCheckbox(gs, Field.SAMERANK,    "Align endpoints", "Align nodes connected by the same relationships' arcs");
        JPanel mergeCBE      = new OurBinaryCheckbox(gs, Field.MERGEARROWS, "Merge arrows", "Merge opposing arrows of the same relation");
        JPanel constraintCBE = new OurBinaryCheckbox(gs, Field.CONSTRAINT,  "Influence layout", "Whether this edge influences the graph layout");
        JPanel attrCBE       = new OurBinaryCheckbox(gs, Field.ATTRIBUTE,   "Show as attributes", "Show relations as attributes on nodes");
        JPanel laybackCBE    = new OurBinaryCheckbox(gs, Field.LAYOUTBACK,  "Layout backwards", "Layout graph as if arcs were reversed");
        parent.add(makelabel(" Default Relation Settings:"));
        parent.add(OurUtil.makeH(wcolor, 10, colorComboE, 8, outlineComboE, 2, null));
        JPanel a=OurUtil.makeVL(wcolor,dispCBE,attrCBE,constraintCBE,10), b=OurUtil.makeVL(wcolor,/*rankCBE,*/laybackCBE,mergeCBE);
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
    private JLabel makelabel(String label) { return OurUtil.label(OurUtil.getVizFont().deriveFont(Font.BOLD), label); }

    /** Project over the given type if we are allowed to. */
    private void projectAlloyType(AlloyType type) { vizState.project(type); remakeAll(); }

    /** Unproject over the given type if it is currently projected. */
    private void deprojectAlloyType(AlloyType type) { vizState.deproject(type); remakeAll(); }
}
