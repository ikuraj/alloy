/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

import kodviz.util.Params;

import kodviz.alloyviz.EdgeViz;
import kodviz.alloyviz.NodeViz;
import kodviz.alloyviz.VizState;
import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotOrientation;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotPaletteManager;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;


/**
 * GeneralLayoutPanel is the panel in which the General Layout options
 * (i.e. font size, projection, orientation) will appear.
 */
class GeneralLayoutPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);
    private Dimension MAXIMUM_SIZE = new Dimension(Short.MAX_VALUE, 35);

    private VizState _vizState;

    private JComboBox orientCombo, colorComboN, shapeComboN, outlineComboN;
    private JCheckBox dispCBN, rankCBN, hideUnconnCBN, showLblCBN, showAttrCBN, numberCBN;

    private JComboBox colorComboE, outlineComboE, nodePalCB, edgePalCB;
    //private JSpinner weightSpinnerE;
    private JCheckBox dispCBE, rankCBE, attrCBE, mergeCBE, laybackCBE;

    private CustomizationPanel _parent;

    private boolean _loadingState;

    private DotPaletteManager dpm;
    private Vector nodeColors;
    private Vector edgeColors;

    GeneralLayoutPanel(VizState vizState_, CustomizationPanel parent_) {
        super();
        _parent = parent_;
        _loadingState = false;
        _vizState = vizState_;
        dpm = new DotPaletteManager();
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        addWidgets();
        updateAll();
    }

    /*
     * Add all of the customization widgets to the GeneralLayoutPanel.
     */
    private void addWidgets() {

        JPanel fontPaletteWidget;
        JPanel orientationWidget;
        JPanel defaultSettingsPanel = new JPanel();
        JPanel typeWidget, setWidget, edgeWidget;
        //JPanel savedCustomizationsWidget;

        fontPaletteWidget = createFontPaletteWidget();
        fontPaletteWidget.setAlignmentX(LEFT_ALIGNMENT);
        orientationWidget = createOrientationWidget();
        orientationWidget.setAlignmentX(LEFT_ALIGNMENT);
        typeWidget = createDefaultTypeWidget();
        setWidget = createDefaultSetWidget();
        edgeWidget = createDefaultEdgeWidget();
        //savedCustomizationsWidget = createSavedCustomizationsWidget();

        add(fontPaletteWidget);
        add(orientationWidget);
        defaultSettingsPanel.setLayout(new BoxLayout(defaultSettingsPanel, BoxLayout.Y_AXIS));
        typeWidget.setAlignmentX(LEFT_ALIGNMENT);
        setWidget.setAlignmentX(LEFT_ALIGNMENT);
        edgeWidget.setAlignmentX(LEFT_ALIGNMENT);
        defaultSettingsPanel.add(new HorizontalRule());
        defaultSettingsPanel.add(new JLabel("Default Settings"));
        defaultSettingsPanel.add(typeWidget);
        defaultSettingsPanel.add(setWidget);
        defaultSettingsPanel.add(edgeWidget);
        defaultSettingsPanel.setAlignmentX(LEFT_ALIGNMENT);
        defaultSettingsPanel.setMaximumSize(
            new Dimension(
                defaultSettingsPanel.getMaximumSize().width,
                defaultSettingsPanel.getPreferredSize().height));

        //defaultSettingsPanel.setBorder(BorderFactory.createCompoundBorder(WIDGET_BORDER,
        //								  BorderFactory.createTitledBorder("Default Settings")));
        defaultSettingsPanel.setBorder(WIDGET_BORDER);
        add(defaultSettingsPanel);
        //add(savedCustomizationsWidget);
    }

    /*
     * Creates a widget to modify font size and also to change palettes
     */
    @SuppressWarnings("unchecked")
    private JPanel createFontPaletteWidget() {

        // initialize to the current global prefs, loading set to true because this is during initialization
        // (the calls below use _loadingState instead because they're inside the listeners)
        Font textFont = Params.glob.getFontParam("GUI", "textfont");        
        _vizState.setFontName(textFont.getFamily(), true);
        _vizState.setFontSize(textFont.getSize(), true);

        JLabel labelNode = new JLabel("Node Color:");
        labelNode.setBorder(WIDGET_BORDER);

        nodePalCB = new JComboBox(new Vector(DotPalette.getValidValues()));        
        nodePalCB.setMaximumSize(new Dimension(100, 35));
        nodePalCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                DotPalette selectedPalette = (DotPalette)nodePalCB.getSelectedItem();
                // need to update colors in Node
                DotColor newColor =
                    dpm.findBestNodeMatch(
                        _vizState.getNodePalette(),
                        selectedPalette,
                        (DotColor)colorComboN.getSelectedItem());
                nodeColors.clear();
                List colorList = dpm.getNodeColors(selectedPalette);
                nodeColors.addAll(colorList);
                colorComboN.setSelectedItem(newColor);
                _vizState.setNodePalette(selectedPalette, _loadingState);
                // need to reflect changes onto modulepanel's combo boxes
                _parent.updatePalettes();
            }
        });

        JLabel labelEdge = new JLabel("Edge Color:");
        labelEdge.setBorder(WIDGET_BORDER);

        edgePalCB = new JComboBox(new Vector(DotPalette.getValidValues()));
        edgePalCB.setMaximumSize(new Dimension(100, 35));
        edgePalCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                DotPalette selectedPalette = (DotPalette)edgePalCB.getSelectedItem();
                DotColor newColor =
                    dpm.findBestEdgeMatch(
                        _vizState.getEdgePalette(),
                        selectedPalette,
                        (DotColor)colorComboE.getSelectedItem());
                // update colors in Edge
                edgeColors.clear();
                List colorList = dpm.getEdgeColors(selectedPalette);
                edgeColors.addAll(colorList);
                colorComboE.setSelectedItem(newColor);

                _vizState.setEdgePalette(selectedPalette, _loadingState);

                _parent.updatePalettes();
            }
        });

        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

        panel.add(labelNode);
        panel.add(nodePalCB);
        panel.add(labelEdge);
        panel.add(edgePalCB);

        panel.setBorder(WIDGET_BORDER);
        panel.setMaximumSize(MAXIMUM_SIZE);
        return panel;
    }

    /*
     * Creates a widget to modify the graph orientation.
     */
    @SuppressWarnings("unchecked")
    private JPanel createOrientationWidget() {

        Vector orientations = new Vector();

        JLabel label = new JLabel("Orientation:");
        label.setBorder(WIDGET_BORDER);

        for (Iterator i = DotOrientation.getValidValues().iterator(); i.hasNext();) {
            orientations.add(i.next());
        }

        // Need to get current orientation from customization.
        orientCombo = new JComboBox(orientations);
        orientCombo.setMaximumSize(new Dimension(125, 35));
        orientCombo.setBorder(WIDGET_BORDER);
        orientCombo.setRenderer(new DotPropertyListCellRenderer());
        orientCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _vizState.setOrientation(
                    (DotOrientation)orientCombo.getSelectedItem(),
                    _loadingState);
            }
        });

        /*JLabel viewLabel = new JLabel("Views: ");
        viewLabel.setBorder(WIDGET_BORDER);
        views = new Vector(_vizState.getAllViews());
        //System.out.println("VIEWS: "+views.toString());
        viewCombo = new JComboBox(views);
        viewCombo.setPreferredSize(new Dimension(100,35));
        viewCombo.setBorder(WIDGET_BORDER);
        viewCombo.setRenderer(new ViewListCellRenderer());
        viewCombo.addActionListener(new ActionListener() {
        	public void actionPerformed(ActionEvent e) {
        	    _vizState.setView((View)viewCombo.getSelectedItem());		    
        	    // any changes in palette will be worked out before vals are loaded in
        	    
        	    //_parent.updatePalettes();
        	    _parent.updateGUISelections();
        	}
            });*/

        JPanel panel = new JPanel();

        /*panel.setLayout(new BorderLayout());
          panel.add(label, BorderLayout.WEST);
          panel.add(combo, BorderLayout.EAST);
        */
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(label);
        panel.add(orientCombo);
        //panel.add(viewLabel);
        //panel.add(viewCombo);
        panel.setMaximumSize(MAXIMUM_SIZE);
        return panel;
    }

    /*
     * Creates a widget to select among previously saved customizations.
     */
    /*private JPanel createSavedCustomizationsWidget() {

        JLabel label = new JLabel("Views:");
        label.setBorder(WIDGET_BORDER);

        // We need to actually get the saved customizations here.
        Object[] choices = new Object[] { "Choice 1", "Choice 2", "Choice 3" };

        JComboBox combo = new JComboBox(choices);
        combo.setMaximumSize(new Dimension(100, 35));
        combo.setBorder(WIDGET_BORDER);

        JPanel panel = new JPanel();

        //panel.setLayout(new BorderLayout());
        //panel.add(label, BorderLayout.WEST);
        //panel.add(combo, BorderLayout.EAST);
        
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(label);
        panel.add(combo);
        panel.setMaximumSize(MAXIMUM_SIZE);
        return panel;
    }
    */
    
    @SuppressWarnings("unchecked")
    private JPanel createDefaultTypeWidget() {
        String TITLE_LBL = "Types:";
        String DISP_LBL = "Show";
        String DISP_TT = "Show members of type as nodes";
        String RANK_LBL = "Align by type";
        String RANK_TT = "Align nodes of the same type";
        String HIDE_UNCONN_LBL = "Hide unconnected nodes";
        String HIDE_UNCONN_TT = "Hide nodes without arcs";
        String NUMBER_LBL = "Number nodes";
        String NUMBER_TT = "Attach atom number to node label as suffix";

        /*
        String SHOWLBL_LBL = "Display set labels";
        String SHOWLBL_TT = "Display all set labels in their member nodes";
        */

        JPanel panel = new JPanel();
        panel.setBorder(WIDGET_BORDER);

        JPanel upperPanel = new JPanel();
        upperPanel.setLayout(new BoxLayout(upperPanel, BoxLayout.X_AXIS));
        upperPanel.setAlignmentX(LEFT_ALIGNMENT);

        dispCBN = new JCheckBox(DISP_LBL);
        dispCBN.setToolTipText(DISP_TT);
        dispCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        rankCBN = new JCheckBox(RANK_LBL);
        rankCBN.setToolTipText(RANK_TT);
        rankCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        hideUnconnCBN = new JCheckBox(HIDE_UNCONN_LBL);
        hideUnconnCBN.setToolTipText(HIDE_UNCONN_TT);
        hideUnconnCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        numberCBN = new JCheckBox(NUMBER_LBL);
        numberCBN.setToolTipText(NUMBER_TT);
        numberCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        /*
        showLblCBN = new JCheckBox(SHOWLBL_LBL);
        showLblCBN.setToolTipText(SHOWLBL_TT);
        showLblCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });
        */

        upperPanel.add(Box.createRigidArea(new Dimension(20, 0)));
        //upperPanel.add(Box.createHorizontalGlue());
        upperPanel.add(dispCBN);
        upperPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        upperPanel.add(rankCBN);
        upperPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        upperPanel.add(hideUnconnCBN);
        upperPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        upperPanel.add(numberCBN);
        /*
        upperPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        upperPanel.add(showLblCBN);*/
        //upperPanel.add(Box.createHorizontalGlue());

        JPanel lowerPanel = new JPanel();

        lowerPanel.setLayout(new BoxLayout(lowerPanel, BoxLayout.X_AXIS));
        lowerPanel.setAlignmentX(LEFT_ALIGNMENT);

        // int rendererElt = ModulePanel.TYPE;
        // arbituarily chosen--there'd be no nulls anyway.

        // NODE COLOR
        nodeColors = new Vector();
        List currentColors = dpm.getNodeColors(_vizState.getNodePalette());
        nodeColors.addAll(currentColors);
        colorComboN = new JComboBox(nodeColors);
        colorComboN.setRenderer(new DotPropertyListCellRenderer());
        //colorComboN.setPreferredSize(new Dimension(colorComboN.getPreferredSize().width, 32));
        colorComboN.setMaximumSize(new Dimension(100, 35));
        colorComboN.setBorder(WIDGET_BORDER);
        colorComboN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        // SHAPE
        Vector shapeList = new Vector();
        shapeList.addAll(DotShape.getValidValues());
        shapeComboN = new JComboBox(shapeList);
        shapeComboN.setRenderer(new DotPropertyListCellRenderer());
        //shapeComboN.setPreferredSize(new Dimension(shapeComboN.getPreferredSize().width, 32));
        shapeComboN.setMaximumSize(new Dimension(155, 35));
        shapeComboN.setBorder(WIDGET_BORDER);
        shapeComboN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        // OUTLINE
        Vector outlineList = new Vector();
        outlineList.addAll(DotStyle.getValidValues());
        outlineComboN = new JComboBox(outlineList);
        outlineComboN.setRenderer(new DotPropertyListCellRenderer());
        outlineComboN.setPreferredSize(new Dimension(95, 35));
        outlineComboN.setMaximumSize(new Dimension(95, 35));
        outlineComboN.setBorder(WIDGET_BORDER);
        outlineComboN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        lowerPanel.add(colorComboN);
        lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        lowerPanel.add(shapeComboN);
        lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        lowerPanel.add(outlineComboN);
        panel.setLayout(new BorderLayout());

        panel.add(new JLabel(TITLE_LBL), BorderLayout.WEST);
        panel.add(upperPanel, BorderLayout.CENTER);
        panel.add(lowerPanel, BorderLayout.SOUTH);
        panel.setMaximumSize(new Dimension(550, 75));
        return panel;

    }

    private JPanel createDefaultSetWidget() {
        String TITLE_LBL = "Sets:";
        String SHOWLBL_LBL = "Show as labels";
        String SHOWLBL_TT = "Show members as labels";
        String SHOW_ATTR_LBL = "Show in attributes";
        String SHOW_ATTR_TT =
            "Show set membership of endpoints when relation attributes are enabled";

        JPanel upperPanel = new JPanel();
        upperPanel.setLayout(new BoxLayout(upperPanel, BoxLayout.X_AXIS));
        upperPanel.setAlignmentX(LEFT_ALIGNMENT);

        JPanel panel = new JPanel();
        panel.setBorder(WIDGET_BORDER);

        showLblCBN = new JCheckBox(SHOWLBL_LBL);
        showLblCBN.setToolTipText(SHOWLBL_TT);
        showLblCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        showAttrCBN = new JCheckBox(SHOW_ATTR_LBL);
        showAttrCBN.setToolTipText(SHOW_ATTR_TT);
        showAttrCBN.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateNode();
            }
        });

        upperPanel.add(Box.createRigidArea(new Dimension(30, 0)));
        upperPanel.add(showLblCBN);
        upperPanel.add(Box.createRigidArea(new Dimension(10, 0)));
        upperPanel.add(showAttrCBN);

        panel.setLayout(new BorderLayout());
        panel.add(new JLabel(TITLE_LBL), BorderLayout.WEST);
        panel.add(upperPanel, BorderLayout.SOUTH);
        panel.setMaximumSize(new Dimension(300, 55));

        return panel;
    }

    @SuppressWarnings("unchecked")
    private JPanel createDefaultEdgeWidget() {
        String TITLE_LBL = "Relations:";
        String DISP_LBL = "Show as arcs";
        String DISP_TT = "Show relations as arcs";
        String RANK_LBL = "Align endpoints";
        String RANK_TT = "Align nodes connected by the same relationships' arcs";
        String ATTR_LBL = "Show as attributes";
        String ATTR_TT = "Show relations as attributes on nodes";
        String MERGE_LBL = "Merge arrows";
        String MERGE_TT = "Merge opposing arrows of the same relation";
        String LAYBACK_LBL = "Layout backwards";
        String LAYBACK_TT = "Layout graph as if arcs were reversed";

        JPanel panel = new JPanel();
        panel.setBorder(WIDGET_BORDER);

        JPanel upperPanel = new JPanel();
        upperPanel.setLayout(new BoxLayout(upperPanel, BoxLayout.X_AXIS));
        upperPanel.setAlignmentX(LEFT_ALIGNMENT);

        dispCBE = new JCheckBox(DISP_LBL);
        dispCBE.setToolTipText(DISP_TT);
        dispCBE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        rankCBE = new JCheckBox(RANK_LBL);
        rankCBE.setToolTipText(RANK_TT);
        rankCBE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        attrCBE = new JCheckBox(ATTR_LBL);
        attrCBE.setToolTipText(ATTR_TT);
        attrCBE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        mergeCBE = new JCheckBox(MERGE_LBL);
        mergeCBE.setToolTipText(MERGE_TT);
        mergeCBE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        laybackCBE = new JCheckBox(LAYBACK_LBL);
        laybackCBE.setToolTipText(LAYBACK_TT);
        laybackCBE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        upperPanel.add(Box.createRigidArea(new Dimension(20, 0)));
        upperPanel.add(dispCBE);
        upperPanel.add(Box.createRigidArea(new Dimension(10, 0)));
        upperPanel.add(attrCBE);
        upperPanel.add(Box.createRigidArea(new Dimension(10, 0)));
        upperPanel.add(rankCBE);
        upperPanel.add(Box.createRigidArea(new Dimension(10, 0)));
        upperPanel.add(mergeCBE);

        JPanel lowerPanel = new JPanel();

        lowerPanel.setLayout(new BoxLayout(lowerPanel, BoxLayout.X_AXIS));
        lowerPanel.setAlignmentX(LEFT_ALIGNMENT);

        // int rendererElt = ModulePanel.TYPE;
        // arbituarily chosen--there'd be no nulls anyway.

        // EDGE COLOR
        edgeColors = new Vector();
        List colorList = dpm.getEdgeColors(_vizState.getEdgePalette());
        edgeColors.addAll(colorList);
        colorComboE = new JComboBox(edgeColors);
        colorComboE.setRenderer(new DotPropertyListCellRenderer());
        colorComboE.setMaximumSize(new Dimension(100, 35));
        colorComboE.setBorder(WIDGET_BORDER);
        colorComboE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        // OUTLINE
        Vector outlineList = new Vector();
        outlineList.addAll(DotStyle.getValidValues());
        outlineComboE = new JComboBox(outlineList);
        outlineComboE.setRenderer(new DotPropertyListCellRenderer());
        outlineComboE.setMaximumSize(new Dimension(95, 35));
        outlineComboE.setBorder(WIDGET_BORDER);
        outlineComboE.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateEdge();
            }
        });

        /*
        weightSpinnerE = new JSpinner(new SpinnerNumberModel(0, 0, 999, 1));
        weightSpinnerE.setMaximumSize(new Dimension(70, 30));
        JPanel weightPanel = putInWestEastPanel("Weight:", weightSpinnerE);
        weightPanel.setBorder(WIDGET_BORDER);
        weightPanel.setToolTipText(
            "A higher weight will cause the edge to be shorter and straighter.");
        weightSpinnerE.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                updateEdge();
            }
        });
        */

        lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        lowerPanel.add(colorComboE);
        lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        lowerPanel.add(outlineComboE);
        //lowerPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        //lowerPanel.add(weightPanel);
        lowerPanel.add(Box.createRigidArea(new Dimension(10, 0)));
        lowerPanel.add(laybackCBE);

        panel.setLayout(new BorderLayout());

        panel.add(new JLabel(TITLE_LBL), BorderLayout.WEST);
        panel.add(upperPanel, BorderLayout.CENTER);
        panel.add(lowerPanel, BorderLayout.SOUTH);
        panel.setMaximumSize(new Dimension(580, 75));
        return panel;
    }

    // copy and pasted from one of the option panels
    /*private JPanel putInWestEastPanel(String compLabel, JComponent comp) {
        JPanel panel = new JPanel();

        JLabel label = new JLabel(compLabel);

        //comp.setMaximumSize(OPTIONS_MAXSIZE);
        //label.setMaximumSize(OPTIONS_MAXSIZE);
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(label);
        panel.add(Box.createRigidArea(new Dimension(10, 0)));
        panel.add(comp);

        return panel;
    }*/

    private void updateNode() {
        // same fix as in ModulePanel's updateNode.  prevents overwriting in initialization.
        if (!_loadingState) {
            _vizState.setUniversalNodeViz(
                new NodeViz(
                    new Boolean(dispCBN.isSelected()),
                    "",
                    (DotColor)colorComboN.getSelectedItem(),
                    (DotShape)shapeComboN.getSelectedItem(),
                    (DotStyle)outlineComboN.getSelectedItem(),
                    new Boolean(rankCBN.isSelected()),
                    new Boolean(showLblCBN.isSelected()),
                    null,
                    new Boolean(showAttrCBN.isSelected()),
                    new Boolean(hideUnconnCBN.isSelected()),
                    new Boolean(numberCBN.isSelected())));
            _vizState.recomputeInheritedSettings();
        }
    }

    private void updateEdge() {
        if (!_loadingState) {
            _vizState
                .setUniversalEdgeViz(new EdgeViz(
                    new Boolean(dispCBE.isSelected()),
                    "",
                    (DotColor)colorComboE.getSelectedItem(),
                    (DotStyle)outlineComboE.getSelectedItem(),
                    0, //((Integer)weightSpinnerE.getValue()).intValue(),
                    new Boolean(attrCBE.isSelected()),
                    new Boolean(rankCBE.isSelected()),
                    new Boolean(mergeCBE.isSelected()),
                    null,
            // for now
            new Boolean(laybackCBE.isSelected())));
            _vizState.recomputeInheritedSettings();
        }
    }

    void updateNodeSelections(NodeViz viz) {
        dispCBN.setSelected(viz.isVisible().booleanValue());
        colorComboN.setSelectedItem(viz.getColor());
        shapeComboN.setSelectedItem(viz.getShape());
        outlineComboN.setSelectedItem(viz.getStyle());
        rankCBN.setSelected(viz.isSameRank().booleanValue());
        showLblCBN.setSelected(viz.showLabel().booleanValue());
        showAttrCBN.setSelected(viz.showInAttr().booleanValue());
        hideUnconnCBN.setSelected(viz.hideUnconnected().booleanValue());
        numberCBN.setSelected(viz.numberAtoms().booleanValue());
    }

    void updateEdgeSelections(EdgeViz viz) {
        dispCBE.setSelected(viz.isVisible().booleanValue());
        colorComboE.setSelectedItem(viz.getColor());
        outlineComboE.setSelectedItem(viz.getStyle());
        //weightSpinnerE.setValue(new Integer(viz.getWeight()));
        attrCBE.setSelected(viz.isAttribute().booleanValue());
        rankCBE.setSelected(viz.isSameRank().booleanValue());
        mergeCBE.setSelected(viz.mergeArrows().booleanValue());
        laybackCBE.setSelected(viz.layoutBack().booleanValue());
    }

    void updateAll() {
        // fixes same bug as in ModulePanel's updateAllGUI...foolish to not remember this earlier
        _loadingState = true;
        nodePalCB.setSelectedItem(_vizState.getNodePalette());
        edgePalCB.setSelectedItem(_vizState.getEdgePalette());
        updateNodeSelections(_vizState.getUniversalNodeViz());
        updateEdgeSelections(_vizState.getUniversalEdgeViz());
        orientCombo.setSelectedItem(_vizState.getOrientation());        
        _loadingState = false;
    }
}

/*class ViewListCellRenderer extends JLabel implements ListCellRenderer {
    
    public ViewListCellRenderer() {
	super();
	setOpaque(true);
    }
    
    public Component getListCellRendererComponent(JList list, Object value, int index,
						  boolean isSelected, boolean cellHasFocus) {
	//System.out.println(value);
	setText(((View)value).getName());
	setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
	
	if (isSelected) {
	    setBackground(list.getSelectionBackground());
	    setForeground(list.getSelectionForeground());
	}
	else {
	    setBackground(list.getBackground());
	    setForeground(list.getForeground());
	}
	return this;
    }
}*/
