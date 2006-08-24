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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import kodviz.alloyviz.AlloyModule;
import kodviz.alloyviz.VizState;


/**
 * CustomizationPanel sits in the left part of the VizFrame and contains widgets
 * for customizing the visualization.
 */
class CustomizationPanel extends JPanel {

	private static final long serialVersionUID = 1L;

    private VizState _vizState;
    private Map _modsToModPanels;
    //private ViewPanel viewPanel; --- no longer needed in Alloy4 - jbaek TODO
    private GeneralLayoutPanel genPanel;
    private boolean _initializing;
    private VizPanel _parent;

    @SuppressWarnings("unused")
    private final JFileChooser fc = new JFileChooser();

    CustomizationPanel(VizState vizState_, VizPanel parent_) {
        super();
        _initializing = true;
        _vizState = vizState_;
        _parent = parent_;
        setLayout(new BorderLayout());
        _modsToModPanels = new HashMap();
        add(makeTabbedPane(), BorderLayout.CENTER);
        //add(makeButtonsPanel(), BorderLayout.SOUTH);
        _initializing = false;
    }

    @SuppressWarnings("unchecked")
    private JTabbedPane makeTabbedPane() {

        JTabbedPane tab = new JTabbedPane();
        // viewPanel = new ViewPanel(_vizState, this); --- jbaek TODO; deprecated
        // tab.addTab("Views", viewPanel); ---- jbaek TODO; deprecated
        
        // IMPORTANT--NOTE CALL TO VIEWPANEL COMES BEFORE MODULEPANEL
        // THIS ISN'T TRIVIAL (though it won't change anything anymore) BECAUSE
        // THE AUTOMATIC DISPLAY() CALL IN THE BEGINNING OCCURS IN VIEWPANEL'S
        // CONSTRUCTOR, SO MAKE ALL CHANGES TO THE MODEL (proj, etc.) BEFORE
        // THE MODULEPANELS.
        genPanel = new GeneralLayoutPanel(_vizState, this);
        tab.addTab("General", new JScrollPane(genPanel));

        for (Iterator modIter = _vizState.getOriginalModel().getModules().iterator();
            modIter.hasNext();
            ) {
            AlloyModule origMod = (AlloyModule)modIter.next();
            AlloyModule curMod = _vizState.getCurrentModel().getModuleByName(origMod.getName());
            ModulePanel temp = new ModulePanel(_vizState, origMod, curMod);
            _modsToModPanels.put(origMod.getName(), temp);
            tab.addTab(origMod.getName(), temp);
        }
        
        // sets default tab to general tab
        tab.setSelectedIndex(1);        
        
        return tab;
    }

    /*
    private JPanel makeButtonsPanel() {
    
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    
    JButton loadBtn = new JButton(LOAD_LBL);
    JButton saveBtn = new JButton(SAVE_LBL);
    JButton newViewBtn = new JButton(NEWVIEW_LBL);
    
    loadBtn.setToolTipText(LOAD_TT);
    saveBtn.setToolTipText(SAVE_TT);
    newViewBtn.setToolTipText(NEWVIEW_TT);	
    
    loadBtn.setMaximumSize(new Dimension(100, 30));
    loadBtn.setPreferredSize(new Dimension(100, 30));
    saveBtn.setMaximumSize(new Dimension(100, 30));
    saveBtn.setPreferredSize(new Dimension(100, 30));
    newViewBtn.setMaximumSize(new Dimension(100, 30));
    newViewBtn.setPreferredSize(new Dimension(100, 30));
    
    loadBtn.addActionListener(new ActionListener() {
    	public void actionPerformed(ActionEvent e) {
    	    int returnVal = fc.showOpenDialog(null);
    	    
    	    if (returnVal == JFileChooser.APPROVE_OPTION) {
    		File file = fc.getSelectedFile();
    		_vizState.loadPalette(file);
    		_palLocation = file;
    		updateGUISelections();
    	    } else {
    		// do nothing
    	    }
    	}
        });
    
    saveBtn.addActionListener(new ActionListener() {
    	public void actionPerformed(ActionEvent e) {
    	    if (_palLocation==null) {
    		int returnVal = fc.showSaveDialog(null);
    		if (returnVal == JFileChooser.APPROVE_OPTION) {
    		    File file = fc.getSelectedFile();
    		    if (file.exists()) {
    			int opt = JOptionPane.showConfirmDialog(null,
    								"File exists.  Overwrite data?",
    								"Warning",						      
    								JOptionPane.YES_NO_OPTION,
    								JOptionPane.WARNING_MESSAGE);
    			if (opt!=JOptionPane.NO_OPTION) {				
    			    _vizState.savePalette(file);
    			}
    		    }
    		    else {
    			_vizState.savePalette(file);			    
    		    }
    		}
    	    }
    	    else {
    		// save to the previous location
    		_vizState.savePalette(_palLocation);
    	    }
    	}
        });				  
    
    newViewBtn.addActionListener(new ActionListener() {
    	public void actionPerformed(ActionEvent e) {
    	    String name="";
    	    while (name!=null && name.equals("")) {
    		name = JOptionPane.showInputDialog(null,					    
    						   "Enter name of new view",
    						   "Adding view",
    						   JOptionPane.PLAIN_MESSAGE);
    	    }
    	    if (name!=null) {
    		_vizState.addView(name);
    		updateGUISelections();
    	    }
    	}
        });
    
    panel.add(Box.createHorizontalGlue());
    panel.add(newViewBtn);	
    panel.add(Box.createRigidArea(new Dimension(30,30)));
    panel.add(loadBtn);	
    panel.add(Box.createRigidArea(new Dimension(30,30)));
    panel.add(saveBtn);
    panel.add(Box.createHorizontalGlue());
    panel.setBorder(WIDGET_BORDER);
    return panel;
    
    JPanel retPanel = new JPanel();
    retPanel.setLayout(new BorderLayout());
    retPanel.add(panel);
    return retPanel;
    }*/

    void updatePalettes() {
        if (!_initializing) {
            for (Iterator modIter = _vizState.getCurrentModel().getModules().iterator();
                modIter.hasNext();
                ) {
                AlloyModule mod = (AlloyModule)modIter.next();
                ModulePanel modPanel = (ModulePanel)_modsToModPanels.get(mod.getName());
                modPanel.updateAllGUI(true);
            }
        }
    }

    void updateGUISelections() {
        if (!_initializing) {
            for (Iterator modIter = _vizState.getCurrentModel().getModules().iterator();
                modIter.hasNext();
                ) {
                AlloyModule mod = (AlloyModule)modIter.next();
                ModulePanel modPanel = (ModulePanel)_modsToModPanels.get(mod.getName());
                modPanel.projectAndRedraw(); // in case loaded view has projection info
                modPanel.updateAllGUI();
                modPanel.filterCheckBoxes(); // note ORDER (comes AFTER updateAllGUI)
            }            
            //viewPanel.updateAll();  // automatically updated by listener
            genPanel.updateAll();
        }
    }

    void redrawGraph() {        
        _parent.display();
        //System.out.println("redrawGraph\n");
    }
}
