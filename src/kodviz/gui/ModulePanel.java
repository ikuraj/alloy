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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.Border;

import kodviz.util.Dbg;

import kodviz.alloyviz.AlloyModule;
import kodviz.alloyviz.AlloyNodeElement;
import kodviz.alloyviz.AlloyRelation;
import kodviz.alloyviz.AlloySet;
import kodviz.alloyviz.AlloyType;
import kodviz.alloyviz.CustVar;
import kodviz.alloyviz.EdgeViz;
import kodviz.alloyviz.NodeViz;
import kodviz.alloyviz.VizResolver;
import kodviz.alloyviz.VizState;
import kodviz.dotviz.DotPalette;


/**
 * ModulePanel generates from a Model the panel containing visualization
 * options, etc. 
 */

@SuppressWarnings("deprecation")
class ModulePanel extends JPanel {

	private static final long serialVersionUID = 1L;

	static final int TYPE = 1;
    static final int SET = 2;
    static final int RELATION = 3;
    
    static final String NAME_CONFLICT = "Conflicting variable name";    

    private static final String SELECT_CB_TEXT = "Show unselected";
    private static final String OPTIONS_CB_TEXT = "Show basic options";
    private static final String ADVOPTIONS_CB_TEXT = "Show advanced options";
    private static final String PROJ_BTN_TEXT1 = "Projection...";
    private static final String PROJ_BTN_TEXT2 = "Back to options...";
    private static final String PROJ_BTN_TTTEXT =
        "Switch between node/edge appearance mode and type projection selection mode";
    private static final String DEFINEVAR_BTN_TEXT = "Define var";

    private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);
    // the main problem is the height...

    //private Model _model;
    private SwitchablePanel _nodePanel, _edgePanel;
    private JPanel _projectionPanel;
    private JComponent _elementsPanel;
    private VizState _vizState;

    private AlloyModule _originalModule, _currentModule;

    private boolean inProjectionMode, _loadingState, _specialState;

    private JCheckBox selectCB, basOptionsCB, advOptionsCB;
    private JButton projectBtn, defineVarBtn;

    /**
     * Constructor takes a vizState (should be the one that created this panel...) and a
     * model to draw widgets with
     */
    ModulePanel(VizState vizState_, AlloyModule origModule_, AlloyModule curModule_) {
        super();
        _originalModule = origModule_;
        _currentModule = curModule_;
        _vizState = vizState_;

        _loadingState = false;
        _specialState = false;

        _nodePanel = new SwitchablePanel("Nodes");
        _edgePanel = new SwitchablePanel("Edges");
        _elementsPanel = createElementsPanel(_currentModule);
        JPanel selectOptionsWidget = createSelectOptionsWidget();

        _elementsPanel.setBorder(null);
        this.setBorder(null);

        inProjectionMode = false;

        setLayout(new BorderLayout());
        add(selectOptionsWidget, BorderLayout.NORTH);
        add(_elementsPanel, BorderLayout.CENTER);

        _projectionPanel = new JPanel();
        _projectionPanel.setLayout(new BorderLayout());

        // ugly bug--you aren't necessarily unprojected even in the first run
        // because of palette sharing
        List tempProj = new ArrayList();
        List tempUnproj = new ArrayList();
        _vizState.classifyTypesInModule(_originalModule, tempProj, tempUnproj);
        //System.out.println("tempproj "+tempProj+"\ntempunproj "+tempUnproj);

        _projectionPanel.add(new ProjectionListPanel(this, tempProj, tempUnproj));

        resizeComboBoxes(_elementsPanel, selectCB.getFont());

        updateAllGUI();

        // NOW ASSUMES THAT THE VIEW IS COMPLETE (i.e. all relevant nodes/edges have a view present)
    }

    private JComponent createElementsPanel(AlloyModule module_) {

        JPanel eltPanel = new JPanel();
        JPanel nodesWidget;
        JPanel edgesWidget;

        eltPanel.setLayout(new BoxLayout(eltPanel, BoxLayout.Y_AXIS));

        nodesWidget = createNodesWidget(module_);
        nodesWidget.setAlignmentX(LEFT_ALIGNMENT);
        edgesWidget = createEdgesWidget(module_);
        edgesWidget.setAlignmentX(LEFT_ALIGNMENT);

        eltPanel.add(nodesWidget);
        eltPanel.add(edgesWidget);

        /*JPanel dummy =*/  new JPanel();
        JScrollPane scrollPane = new JScrollPane(eltPanel);

        // SETS SCROLLBAR SPEED
        scrollPane.getVerticalScrollBar().setUnitIncrement(50);

        //, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        //dummy.add(scrollPane);

        return scrollPane;
        //return eltPanel;
    }

    private SwitchablePanel createNodesWidget(AlloyModule module_) {

        _nodePanel.add(new HorizontalRule());
        _nodePanel.add(new JLabel("Types"));

        int index = 0; // this *should* be true
        for (Iterator iter = module_.getTypes().iterator(); iter.hasNext();) {
            AlloyType temp = (AlloyType)iter.next();
            String name = removeModuleName(temp.getName());
            JCheckBox cb = new JCheckBox(name);
            cb.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    Object cb = e.getItemSelectable();
                    int index = _nodePanel.getCheckBoxes().indexOf(cb);
                    updateNode(index);
                }
            });
            cb.setAlignmentX(LEFT_ALIGNMENT);
            JPanel nop = new NodeOptionsPanel(name, this, index, true, _vizState.getNodePalette());
            nop.setAlignmentX(LEFT_ALIGNMENT);

            _nodePanel.addEntry(
                cb,
                new NodeHeaderPanel(this, index, true, temp.isCustom()),
                nop,
                temp);
            index++;
        }

        _nodePanel.add(new HorizontalRule());
        _nodePanel.add(new JLabel("Sets"));

        for (Iterator iter = module_.getSets().iterator(); iter.hasNext();) {
            AlloySet temp = (AlloySet)iter.next();
            StringBuffer name = new StringBuffer(removeModuleName(temp.getName()));
            name.append(": ");
            name.append(temp.getType().getName());
            JCheckBox cb = new JCheckBox(name.toString());
            cb.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    Object cb = e.getItemSelectable();
                    int index = _nodePanel.getCheckBoxes().indexOf(cb);
                    updateNode(index);
                }
            });
            cb.setAlignmentX(LEFT_ALIGNMENT);
            JPanel nop =
                new NodeOptionsPanel(
                    temp.getName(),
                    this,
                    index,
                    false,
                    _vizState.getNodePalette());
            nop.setAlignmentX(LEFT_ALIGNMENT);

            _nodePanel.addEntry(
                cb,
                new NodeHeaderPanel(this, index, false, temp.isCustom()),
                nop,
                temp);
            index++;
        }

        _nodePanel.add(Box.createVerticalStrut(10));

        return _nodePanel;

    }

    /**
     * removes the module name from the front of qualified type or 
     * set name.  If name is unqualified, returns itself.
     */
    private String removeModuleName(String typeOrSetName) {
        return typeOrSetName.replaceFirst(_currentModule.getName() + "/", "");
    }

    private SwitchablePanel createEdgesWidget(AlloyModule module_) {

        int index = 0;
        if (module_.getRelations().isEmpty()) {
            _edgePanel.setVisible(false);
        } else {
            _edgePanel.setVisible(true);
        }

        _edgePanel.add(new HorizontalRule());
        _edgePanel.add(new JLabel("Relations"));

        for (Iterator iter = module_.getRelations().iterator(); iter.hasNext();) {
            AlloyRelation temp = (AlloyRelation)iter.next();
            StringBuffer name = new StringBuffer(temp.getName());
            name.append(": ");
            Iterator typeIter = temp.getTypes().iterator();
            if (typeIter.hasNext()) {
                //System.out.println(name);
                AlloyType blah = (AlloyType)typeIter.next();
                //System.out.println(blah);
                name.append(blah.getName());
            }
            while (typeIter.hasNext()) {
                name.append("->");
                name.append(((AlloyType)typeIter.next()).getName());
            }
            JCheckBox cb = new JCheckBox(name.toString());
            cb.setAlignmentX(LEFT_ALIGNMENT);
            cb.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    Object cb = e.getItemSelectable();
                    int index = _edgePanel.getCheckBoxes().indexOf(cb);
                    updateEdge(index);
                }
            });
            DotPalette tempPal = _vizState.getEdgePalette();
            JPanel eop = new EdgeOptionsPanel(temp.getName(), this, index, tempPal);
            eop.setAlignmentX(LEFT_ALIGNMENT);

            _edgePanel.addEntry(cb, new EdgeHeaderPanel(this, index, temp.isCustom()), eop, temp);
            index++;
        }
        //_edgePanel.setBorder(BorderFactory.createCompoundBorder(WIDGET_BORDER,
        //						   BorderFactory.createTitledBorder("Edges")));
        return _edgePanel;
    }

    private JPanel createSelectOptionsWidget() {

        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

        selectCB = new JCheckBox(SELECT_CB_TEXT);
        basOptionsCB = new JCheckBox(OPTIONS_CB_TEXT);
        advOptionsCB = new JCheckBox(ADVOPTIONS_CB_TEXT);

        selectCB.setBorder(WIDGET_BORDER);
        basOptionsCB.setBorder(WIDGET_BORDER);
        advOptionsCB.setBorder(WIDGET_BORDER);

        selectCB.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.DESELECTED) {
                    _nodePanel.hideUnselected();
                    _edgePanel.hideUnselected();
                } else {
                    _nodePanel.showUnselected();
                    _edgePanel.showUnselected();
                }
            }
        });
        basOptionsCB.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.DESELECTED) {
                    _nodePanel.hideBasicOptions();
                    _edgePanel.hideBasicOptions();
                } else {
                    _nodePanel.showBasicOptions();
                    _edgePanel.showBasicOptions();
                }
            }
        });
        advOptionsCB.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.DESELECTED) {
                    _nodePanel.hideAdvancedOptions();
                    _edgePanel.hideAdvancedOptions();
                } else {
                    _nodePanel.showAdvancedOptions();
                    _edgePanel.showAdvancedOptions();
                }
            }
        });

        selectCB.setSelected(true);
        basOptionsCB.setSelected(true);
        advOptionsCB.setSelected(true);

        //
        // SWITCHING between projection and appearance modes
        //

        projectBtn = new JButton(PROJ_BTN_TEXT1);
        projectBtn.setToolTipText(PROJ_BTN_TTTEXT);
        projectBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (inProjectionMode) {
                    // switch back to appearance mode
                    projectBtn.setText(PROJ_BTN_TEXT1);
                    _projectionPanel.setVisible(false);
                    remove(_projectionPanel);
                    _elementsPanel.setVisible(true);
                    add(_elementsPanel, BorderLayout.CENTER);
                    validate();
                    //setLeftComponent(_elementsPanel);
                    //selectCB.setEnabled(true);
                    //optionsCB.setEnabled(true);
                    selectCB.setVisible(true);
                    basOptionsCB.setVisible(true);
                    advOptionsCB.setVisible(true);
                    defineVarBtn.setVisible(true);
                    inProjectionMode = false;
                } else {
                    // switch to projection mode
                    projectBtn.setText(PROJ_BTN_TEXT2);
                    _elementsPanel.setVisible(false);
                    remove(_elementsPanel);
                    _projectionPanel.setVisible(true);
                    add(_projectionPanel, BorderLayout.CENTER);
                    validate();
                    //setLeftComponent(_projectionPanel);
                    //selectCB.setEnabled(false);
                    //optionsCB.setEnabled(false);
                    selectCB.setVisible(false);
                    basOptionsCB.setVisible(false);
                    advOptionsCB.setVisible(false);
                    defineVarBtn.setVisible(false);
                    inProjectionMode = true;
                }
            }
        });

        defineVarBtn = new JButton(DEFINEVAR_BTN_TEXT);
        defineVarBtn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                DefVarDialog dvd = new DefVarDialog(ModulePanel.this);
                dvd.show();
            }
        });

        /*panel.add(selectCB);
        panel.add(Box.createRigidArea(new Dimension(10, 20))); jbaek disabled  TODO*/
        panel.add(basOptionsCB);
        panel.add(Box.createRigidArea(new Dimension(10, 20)));
        panel.add(advOptionsCB);
        panel.add(Box.createRigidArea(new Dimension(10, 20)));
        panel.add(projectBtn);
        /*panel.add(Box.createRigidArea(new Dimension(10, 20)));
        panel.add(defineVarBtn); jbaek disabled to remove vars TODO*/

        return panel;

    }

    /**
     * Returns null if variable was added successfully.  Otherwise, returns error message
     * (either evaluation error or variable name conflict).
     */
    @SuppressWarnings("deprecation")
    String addCustVar(String varName, String expr) {
    	
        AlloyModule mod = _vizState.getUnprojectedModel().getModuleByName(_currentModule.getName());
        AlloyType temp = new AlloyType(varName);
        if (mod.getTypes().contains(temp)) {
            return NAME_CONFLICT + ": "+ temp.getName();
        }
        for (Iterator i = mod.getSets().iterator(); i.hasNext();) {
            AlloySet set = (AlloySet)i.next();
            if (set.getName().equals(varName)) {
                return NAME_CONFLICT + ": "+ temp.getName();
            }
        }

        for (Iterator i = mod.getRelations().iterator(); i.hasNext();) {
            AlloyRelation rel = (AlloyRelation)i.next();
            if (rel.getName().equals(varName)) {
                return NAME_CONFLICT + ": "+ temp.getName();
            }
        }

        CustVar cv = new CustVar(varName, expr, mod.getName());
        String ret = _vizState.addCustVar(cv);
        if (ret!=null) {		
            return ret;
        }
        
        projectAndRedraw();
        updateAllGUI();
        filterCheckBoxes();

        return null;
    }

    /**
     * removes cust var from existence
     * @param index position on corresponding SwitchablePanel
     */
    void removeNodeCustVar(int index) {
        AlloyNodeElement elt = (AlloyNodeElement)_nodePanel.getElements().get(index);
        // this line is here to get rid of that panel so that its textfield
        // doesn't fire an updateNode from "losing focus"
        _nodePanel.remove((JPanel)_nodePanel.getVizOptions().get(index));
        _vizState.removeCustVar(elt);
        projectAndRedraw();
        updateAllGUI();
        filterCheckBoxes();
    }
    
    /**
     * called from NodeHeaderPanel after edit button is clicked
     * @param index
     */
    void editNodeCustVar(int index) {
        AlloyNodeElement elt = (AlloyNodeElement)_nodePanel.getElements().get(index);
        CustVar oldCV = _vizState.getCustForElt(_vizState.getOrigEltForCustomizedElt(elt));
        
        Dbg.chk(oldCV);
                        
        DefVarDialog dvd = new DefVarDialog(ModulePanel.this, true, oldCV, false, index);
        dvd.show();       
    }
    
    /**
     * called from EdgeHeaderPanel after edit button is clicked
     * @param index
     */
    void editEdgeCustVar(int index) {
        AlloyRelation elt = (AlloyRelation)_edgePanel.getElements().get(index);
        CustVar oldCV = _vizState.getCustForElt(_vizState.getOrigEltForCustomizedElt(elt));
        
        Dbg.chk(oldCV);
                        
        DefVarDialog dvd = new DefVarDialog(ModulePanel.this, true, oldCV, true, index);
        dvd.show();       
    }
    
    /**
     * called from DefVarDialog
     */
    String editCustVar(CustVar oldCV, String newExpr, boolean isEditedRelation, int editedEltIndex) {
        CustVar newCV = new CustVar(oldCV.getName(), newExpr, oldCV.getModuleName());
        String ret = _vizState.validateCustVar(newCV);
        
        if (ret!=null) {		
            return ret;
        } 
        
        // if it validated okay, then remove the old var

        if (isEditedRelation) {
            removeEdgeCustVar(editedEltIndex);
        } else {
            removeNodeCustVar(editedEltIndex);
        }

        // now add the new var

        ret = _vizState.addCustVar(newCV);
        
        // if it validated above, it should NOT be null here!
        Dbg.chk(ret==null);
        
        projectAndRedraw();
        updateAllGUI();
        filterCheckBoxes();

        return null;
    
    }

    void removeEdgeCustVar(int index) {
        AlloyRelation elt = (AlloyRelation)_edgePanel.getElements().get(index);
        _edgePanel.remove((JPanel)_edgePanel.getVizOptions().get(index));
        _vizState.removeCustVar(elt);
        projectAndRedraw();
        updateAllGUI();
        filterCheckBoxes();
    }

    void projectOn(AlloyType type) {
        _vizState.projectOn(type);
        projectAndRedraw();
        updateNodeEdgeOnly();
        filterCheckBoxes();
    }

    void deproject(AlloyType type) {
        _vizState.deproject(type);
        projectAndRedraw();
        updateNodeEdgeOnly();
		filterCheckBoxes();
    }

    void projectAndRedraw() {
        _vizState.applyProjection();
        _currentModule = _vizState.getCurrentModel().getModuleByName(_originalModule.getName());
        regenerateAppearanceWidgets();
    }

    void updateNode(int index) {
        if (!_loadingState) {
            AlloyNodeElement elt = (AlloyNodeElement)_nodePanel.getElements().get(index);
            NodeOptionsPanel panel = (NodeOptionsPanel)_nodePanel.getVizOptions().get(index);
            NodeHeaderPanel header = (NodeHeaderPanel)_nodePanel.getHeaders().get(index);

            _vizState
                .updateNodeView(
                    elt,
                    new NodeViz(
                        header.getVisibility(),
                        panel.getLabel(),
                        panel.getColor(),
                        panel.getShape(),
                        panel.getOutline(),
                        panel.getRanking(),
                        panel.getShowLabel(),
                        new Boolean(
                            ((JCheckBox)_nodePanel.getCheckBoxes().get(index)).isSelected()),
                        panel.getShowInAttr(),
                        panel.getHideUnconn(),
                        panel.getNumberAtoms()),
            // for now only!
            !_specialState);
            _vizState.recomputeInheritedSettings();
        }

        //this.validate();
        this.revalidate();
        this.repaint();

    }

    void updateEdge(int index) {
        if (!_loadingState) {
            AlloyRelation rel = (AlloyRelation)_edgePanel.getElements().get(index);
            EdgeOptionsPanel panel = (EdgeOptionsPanel)_edgePanel.getVizOptions().get(index);
            EdgeHeaderPanel header = (EdgeHeaderPanel)_edgePanel.getHeaders().get(index);

            _vizState.updateEdgeView(
                rel,
                new EdgeViz(
                    header.getVisibility(),
                    panel.getLabel(),
                    panel.getColor(),
                    panel.getOutline(),
                    panel.getWeight(),
                    panel.getAttribute(),
                    panel.getRanking(),
                    panel.getMerge(),
                    new Boolean(((JCheckBox)_edgePanel.getCheckBoxes().get(index)).isSelected()),
                    panel.getLayoutBack()),
                !_specialState);
            _vizState.recomputeInheritedSettings();
        }
        //this.validate();
        this.revalidate();
        this.repaint();
    }

    void updateAll() {
        // update Nodes
        for (int i = 0; i < _nodePanel.getElements().size(); i++) {
            updateNode(i);
        }

        // update Edges
        for (int i = 0; i < _edgePanel.getElements().size(); i++) {
            updateEdge(i);
        }

        // the projection panel is not updated here because it changes the vizState in real time.	

    }

    /**
     * returns a DotProperty or Boolean.
     * 
     * @param index index of node in module panel
     * @param field use values in viz.alloyviz.VizResolver
     */
    Object getInheritedNodeProperty(int index, String field) {
        AlloyNodeElement elt = (AlloyNodeElement)_nodePanel.getElements().get(index);
        NodeViz viz = _vizState.getInheritedNodeViz(elt);
        if (viz == null) {
            // in projection, return some non-null value.  this should only happen
            // when the viz is making internal changes in the moments during projection.
            viz = _vizState.getUniversalNodeViz();
        }

        return VizResolver.lookupNodeSetting(viz, field);

    }

    Object getInheritedEdgeProperty(int index, String field) {
        AlloyRelation rel = (AlloyRelation)_edgePanel.getElements().get(index);
        EdgeViz viz = _vizState.getInheritedEdgeViz(rel);
        if (viz == null) {
            viz = _vizState.getUniversalEdgeViz();
        }
        return VizResolver.lookupEdgeSetting(viz, field);
    }

    /**
     * these 2 methods are used when a view is loaded and the changes need to be
     * reflected on the selected options in the gui.
     * not passing in a View to minimize dependencies to the AlloyViz package
     *
     * if the elt_ passed in does not exist does nothing
     */
    void updateNodeSelections(AlloyNodeElement elt_) {
        NodeViz viz_ = _vizState.getNodeViz(elt_);
        // THIS WAS WRONG BEFORE!  New elements that are not in a saved view are indeed
        // in nodePanel, so the original check of index on _nodePanel.getElements() is
        // bogus
        int index = _nodePanel.getElements().indexOf(elt_);
        NodeOptionsPanel panel = (NodeOptionsPanel)_nodePanel.getVizOptions().get(index);

        if (viz_ != null) {
            NodeHeaderPanel header = (NodeHeaderPanel)_nodePanel.getHeaders().get(index);
            header.setVisibility(viz_.isVisible());
            panel.setLabel(viz_.getLabel());
            panel.setColor(viz_.getColor());
            panel.setShape(viz_.getShape());
            panel.setOutline(viz_.getStyle());
            panel.setRanking(viz_.isSameRank());
            panel.setShowLabel(viz_.showLabel());
            panel.setShowInAttr(viz_.showInAttr());
            panel.setHideUnconn(viz_.hideUnconnected());
            panel.setNumberAtoms(viz_.numberAtoms());
            ((JCheckBox)_nodePanel.getCheckBoxes().get(index)).setSelected(viz_.isSelected());
        } else {
            // see comment for edge for loadingState details
            _loadingState = false;
            updateNode(index);
            _loadingState = true;
        }
        panel.changePalette(_vizState.getNodePalette());
    }

    void updateEdgeSelections(AlloyRelation rel_) {
        EdgeViz viz_ = _vizState.getEdgeViz(rel_);
        // same comments as for nodes above
        int index = _edgePanel.getElements().indexOf(rel_);
        EdgeOptionsPanel panel = (EdgeOptionsPanel)_edgePanel.getVizOptions().get(index);
        if (viz_ != null) {
            EdgeHeaderPanel header = (EdgeHeaderPanel)_edgePanel.getHeaders().get(index);
            header.setVisibility(viz_.isVisible());
            panel.setLabel(viz_.getLabel());
            panel.setColor(viz_.getColor());
            panel.setOutline(viz_.getStyle());
            panel.setRanking(viz_.isSameRank());
            panel.setWeight(viz_.getWeight());
            panel.setAttribute(viz_.isAttribute());
            panel.setMerge(viz_.mergeArrows());
            panel.setLayoutBack(viz_.layoutBack());
            ((JCheckBox)_edgePanel.getCheckBoxes().get(index)).setSelected(viz_.isSelected());
        } else {
            // loadingState stuff looks sketchy, but it's needed so that updateEdge will function.
            _loadingState = false;
            updateEdge(index);
            _loadingState = true;
            //viz_ = _vizState.getEdgeViz(rel_);
            //System.out.println(viz_);	    
        }
        panel.changePalette(_vizState.getEdgePalette());
    }

    void updateProjectionPanel() {
        _projectionPanel.removeAll();
        List proj = new ArrayList();
        List unProj = new ArrayList();
        _vizState.classifyTypesInModule(_originalModule, proj, unProj);
        _projectionPanel.add(new ProjectionListPanel(this, unProj, proj));
        _projectionPanel.validate();
    }

    /**
     * Master method for reflecting the view stored on _vizState onto the GUI
     *
     * Setting up to true forces a back update on the view.  This is usually not
     * necessary and is currently just used to set the changed colors on the view.     
     */
    void updateAllGUI() {
        updateAllGUI(false);
    }

    void updateAllGUI(boolean up) {
        /* This variable came from the realization of the DIRTIEST bug ever.  Because of the
           way the callbacks work, as I populate the widgets (e.g. the color combo box) with
           the loaded values, I actually invoke the action listener of those widgets every
           time, and these listeners are hooked up to updateEdge, which is why all the later
           elements get screwed over.
         */
        _loadingState = true;
        for (Iterator iter = _currentModule.getTypes().iterator(); iter.hasNext();) {
            updateNodeSelections((AlloyNodeElement)iter.next());
        }
        for (Iterator iter = _currentModule.getSets().iterator(); iter.hasNext();) {
            updateNodeSelections((AlloyNodeElement)iter.next());
        }
        for (Iterator iter = _currentModule.getRelations().iterator(); iter.hasNext();) {
            updateEdgeSelections((AlloyRelation)iter.next());
        }
        updateProjectionPanel();
        _loadingState = false;
        if (up) {
            _specialState = true;
            updateAll();
            _specialState = false;
        }
    }

    /**
     * alternative lesser version used internally
     */
    private void updateNodeEdgeOnly() {
        _loadingState = true;
        for (Iterator iter = _currentModule.getTypes().iterator(); iter.hasNext();) {
            updateNodeSelections((AlloyNodeElement)iter.next());
        }
        for (Iterator iter = _currentModule.getSets().iterator(); iter.hasNext();) {
            updateNodeSelections((AlloyNodeElement)iter.next());
        }
        for (Iterator iter = _currentModule.getRelations().iterator(); iter.hasNext();) {
            updateEdgeSelections((AlloyRelation)iter.next());
        }
        _loadingState = false;
    }

    void regenerateAppearanceWidgets() {
        Component oldPanel = _elementsPanel;
        _nodePanel = new SwitchablePanel("Nodes");
        _edgePanel = new SwitchablePanel("Edges");

        _elementsPanel = createElementsPanel(_currentModule);
        filterCheckBoxes();
        //if (_onMac())
        //resizeComboBoxes(_elementsPanel, new Font("LucidaGrande", Font.PLAIN, 11));
        resizeComboBoxes(_elementsPanel, selectCB.getFont());
        if (!inProjectionMode) {
            remove(oldPanel);
            add(_elementsPanel, BorderLayout.CENTER);
            validate();
            //setLeftComponent(_elementsPanel);
        }

        
        //advOptionsCB.doClick();
        //advOptionsCB.doClick();
        //_elementsPanel.validate();
        //updateAll();
    }

    // filters viz options/unselected checkboxes according to the current
    // option.  
    void filterCheckBoxes() {


        _nodePanel.doSelections(
            selectCB.isSelected(),
            basOptionsCB.isSelected(),
            advOptionsCB.isSelected());
        _edgePanel.doSelections(
            selectCB.isSelected(),
            basOptionsCB.isSelected(),
            advOptionsCB.isSelected());
            
    }

    private void resizeComboBoxes(Component comp, Font font) {
        comp.setFont(font);
        try {
            JComboBox combo = (JComboBox)comp;
            Dimension dim = combo.getMinimumSize();
            dim.height = 32;
            combo.setMinimumSize(dim);
            dim.width = combo.getPreferredSize().width;
            combo.setPreferredSize(dim);

        } catch (ClassCastException e) {
            try {
                Container cont = (Container)comp;
                Component[] components = cont.getComponents();
                for (int i = 0; i < cont.getComponentCount(); i++) {
                    resizeComboBoxes(components[i], font);
                }
            } catch (ClassCastException e2) {
            }
        }
    }

    /*private static boolean _onMac() {
        return System.getProperty("mrj.version") != null
            && UIManager.getSystemLookAndFeelClassName().equals(
                UIManager.getLookAndFeel().getClass().getName());
    }*/
}