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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ext.nanoxml.XMLParseException;

import kodviz.alloyviz.IncompatiblePaletteException;
import kodviz.alloyviz.View;
import kodviz.alloyviz.ViewPalette;
import kodviz.alloyviz.VizState;
import kodviz.alloyviz.VizStateEvent;
import kodviz.alloyviz.VizStateListener;
import kodviz.util.ExtensionFileFilter;
import kodviz.util.FileChooser;
import kodviz.util.Params;
import kodviz.xml.PaletteXMLParseException;


/**
 * GeneralLayoutPanel is the panel in which the General Layout options
 * (i.e. font size, projection, orientation) will appear.
 */
@SuppressWarnings("unchecked")
class ViewPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private static final String PALETTE_EXTENSION = "pal";

    private VizState _vizState;
    private CustomizationPanel _parent;
    private boolean _loadingState;

    // maps model names to the palette file location
    private static Map _modelsToPalLocs = new HashMap();
    // maps pal locations to pal objects
    private static Map _pathsToPals = new HashMap();

    private Vector views;
    private JList viewList;
    private DefaultListModel listModel;

    private JButton deleteButton;

    private final ExtensionFileFilter extFileFilter;
    private final FileChooser chooser;

    private boolean _chooserOnly;

    /**
     * If we ever learn how to set this field properly then the Mac can have nice 
     * dialog boxes for opening and saving palettes.
     */
    final private JFrame _frame = null;  

    ViewPanel(VizState vizState_, CustomizationPanel parent_) {
	    this(vizState_, parent_, false);
    }

    ViewPanel(VizState vizState_, CustomizationPanel parent_, boolean chooserOnly_) {
        super();
        _parent = parent_;
        _loadingState = false;
        _vizState = vizState_;
	_chooserOnly = chooserOnly_;
        //_modelsToPalLocs.put(_vizState.getModelName(), null);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        addWidgets();
        updateAll();

        if (_vizState.getModelFile() != null) {
            chooser = new FileChooser(_vizState.getModelFile().getParentFile().getAbsolutePath());
        } else {
            chooser = new FileChooser(Params.glob.getParam("MAIN", "modeldir"));
	}
        extFileFilter = new ExtensionFileFilter(PALETTE_EXTENSION);
        chooser.setFileFilter(extFileFilter);

	vizState_.addVizStateListener(new VizStateListener() {
		public void viewsChanged(VizStateEvent e) {
		    updateAll();
		}
	    });
    }

    /*
     * Add all of the customization widgets to the GeneralLayoutPanel.
     */
    private void addWidgets() {
        _loadingState = true;
        add(createViewWidget());
        add(createPaletteButtons());
        _loadingState = false;
    }

    /*
     * Creates a widget to modify the graph orientation.
     */
    private JPanel createViewWidget() {
        JPanel viewPanel = new JPanel();
        viewPanel.setLayout(new BoxLayout(viewPanel, BoxLayout.X_AXIS));

        listModel = new DefaultListModel();
        viewList = new JList(listModel);
        views = new Vector(_vizState.getAllViews());
        Iterator iter = views.iterator();

        while (iter.hasNext()) {
            listModel.addElement(iter.next());
        }
        viewList.setCellRenderer(new ViewListCellRenderer());
        viewList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        viewList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                if (_loadingState || e.getValueIsAdjusting())
                    return;

                _vizState.setView(getView());
                _parent.updateGUISelections();
		if (_chooserOnly)
		    _parent.redrawGraph();
                //System.out.println("ViewPanel: 147\n");
            }
        });
        viewList.setPreferredSize(new Dimension(125, 150));

        viewList.setSelectedValue(_vizState.getPaletteCurrentView(), true);

        JScrollPane scroll = new JScrollPane(viewList);
        scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scroll.setAlignmentX(LEFT_ALIGNMENT);
        viewPanel.add(scroll);
	if (!_chooserOnly) {
	    viewPanel.add(createViewButtons());
	}

        return viewPanel;
    }

    private JPanel createViewButtons() {
        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        JButton newButton = new JButton("New View");
        newButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "";
                while (name != null && name.equals("")) {
                    name =
                        JOptionPane.showInputDialog(
                            null,
                            "Enter name of new view",
                            "Adding view",
                            JOptionPane.PLAIN_MESSAGE);
                }
                if (name != null) {
                    _vizState.addView(name);
                    _parent.updateGUISelections();
                    //_parent.redrawGraph();
                    //System.out.println("ViewPanel: 186\n"); 
                }
            }
        });
        panel.add(newButton);

        deleteButton = new JButton("Delete View");
        deleteButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // need at least one view in palette, otherwise bad stuff may happen
                if (!(listModel.getSize() == 1)) {
                    // int index =
                    listModel.indexOf(getView());
 
                    _vizState.removeView(getView());

                    //System.out.println("Deleting view: " + getView().getName());

                    //_parent.updateGUISelections();   
                    //_parent.redrawGraph();
                    //System.out.println("ViewPanel:214\n");
                }
            }
        });
        panel.add(deleteButton);
        // should be disabled the first time
        deleteButton.setEnabled(false);

        JButton renameButton = new JButton("Rename View");
        renameButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // make a list of all the view names except the current view's name
                Iterator views = _vizState.getAllViews().iterator();
                List viewNames = new ArrayList();
                while (views.hasNext()) {
                    View curView = (View)views.next();
                    if (curView != getView())
                        viewNames.add(curView.getName());
                }

                String name = "";
                while (name != null && name.equals("")) {
                    name =
                        JOptionPane.showInputDialog(
                            null,
                            "Enter new view name",
                            "Renaming view",
                            JOptionPane.PLAIN_MESSAGE);
                    if (viewNames.contains(name))
                        name = "";
                }
                if (name != null) {
                    getView().setName(name);
                    _parent.updateGUISelections();
                }
            }
        });
        panel.add(renameButton);

        JButton revertButton = new JButton("Revert View");
        revertButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // NEEDS TO BE IMPLEMENTED
                _vizState.resetView();
                //System.out.println("Reverting view: " + getView().getName());
                _parent.updateGUISelections();
                //_parent.redrawGraph();
                //System.out.println("ViewPanel:264\n");
            }
        });
        panel.add(revertButton);

        return panel;
    }

    private JPanel createPaletteButtons() {
        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

        JButton loadButton = new JButton("Load Palette");
        loadButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {

                if (_vizState.paletteChanged()) {
                    int opt =
                        JOptionPane.showConfirmDialog(
                            null,
                            "Palette has not been saved. Do you want to save the current palette?",
                            "Warning",
                            JOptionPane.YES_NO_CANCEL_OPTION,
                            JOptionPane.WARNING_MESSAGE);
                    if (opt == JOptionPane.CANCEL_OPTION) {
                        // do nothing 
                    } else {
                        if (opt == JOptionPane.YES_OPTION) {
                            savePalette(false);
                        } else if (opt == JOptionPane.NO_OPTION) {
                            int returnVal = chooser.showOpenDialog(_frame);

                            if (returnVal == FileChooser.APPROVE_OPTION) {
                                File file = chooser.getSelectedFile();
                                ViewPalette pal = (ViewPalette)_pathsToPals.get(file);
                                pal = null; // inserted by jbaek TODO
                                if (pal != null) {
                                    int opt2 =
                                        JOptionPane.showConfirmDialog(
                                            null,
                                            "Palette is already used by another instance and will be shared.\nClick 'Cancel' to abort.",
                                            "Loading",
                                            JOptionPane.OK_CANCEL_OPTION,
                                            JOptionPane.INFORMATION_MESSAGE);
                                    if (opt2 == JOptionPane.CANCEL_OPTION) {
                                        return;
                                    }
                                    _vizState.setPalette(pal);
                                } else {
                                    try {
                                        _vizState.loadPalette(file);
                                    } catch (IncompatiblePaletteException ipe) {
                                        JOptionPane.showMessageDialog(
                                            null,
                                            "File uses an incompatible palette version.",
                                            "Version error",
                                            JOptionPane.ERROR_MESSAGE);
                                        return;
                                    } catch (XMLParseException xe) { // jbaek added
                                    	JOptionPane.showMessageDialog(
                                    			null,
                                    			"Invalid XML Format.",
                                    			"Parse Error",
                                    			JOptionPane.ERROR_MESSAGE);
                                    } catch (PaletteXMLParseException xe) { // jbaek added
                                    	JOptionPane.showMessageDialog(
                                    			null,
                                    			"Error parsing the palette XML.",
                                    			"Parse Error",
                                    			JOptionPane.ERROR_MESSAGE);
                                    } catch (Exception er) {
                                        JOptionPane.showMessageDialog(
                                            null,
                                            "Error loading palette."+er.toString(),
                                            "Error",
                                            JOptionPane.ERROR_MESSAGE);
                                        return;
                                    }
                                    _pathsToPals.put(file, _vizState.getPalette());
                                }

                                VizPanel.associateWithPalette(
                                    _vizState.getModelName(),
                                    _vizState.getPalette());

                                viewList.setSelectedValue(_vizState.getPaletteCurrentView(), true);

                                _modelsToPalLocs.put(_vizState.getModelName(), file);
                                _parent.updateGUISelections();
                            }
                        }
                    }
                } else /*!_vizState.paletteChanged()*/ {
                    int returnVal = chooser.showOpenDialog(_frame);
                    if (returnVal == FileChooser.APPROVE_OPTION) {
                        File file = chooser.getSelectedFile();
                        ViewPalette pal = (ViewPalette)_pathsToPals.get(file);
                        pal = null; // added by jbaek TODO
                        if (pal != null) {
                        	int opt2 =
                                JOptionPane.showConfirmDialog(
                                    null,
                                    "Palette is already used by another instance and will be shared.\nClick 'Cancel' to abort.",
                                    "Loading",
                                    JOptionPane.OK_CANCEL_OPTION,
                                    JOptionPane.INFORMATION_MESSAGE);
                            if (opt2 == JOptionPane.CANCEL_OPTION) {
                                return;
                            }
                            _vizState.setPalette(pal);
                        } else {
                            try {
                                _vizState.loadPalette(file);
                            } catch (IncompatiblePaletteException ipe) {
                                JOptionPane.showMessageDialog(
                                    null,
                                    "File uses an incompatible palette version.",
                                    "Version error",
                                    JOptionPane.ERROR_MESSAGE);
                                return;
                            } catch (Exception er) {
                                JOptionPane.showMessageDialog(
                                    null,
                                    "Error loading palette."+er.toString(),
                                    "Error",
                                    JOptionPane.ERROR_MESSAGE);
                                er.printStackTrace(); // jbaek
                                return;
                            }
                            _pathsToPals.put(file, _vizState.getPalette());
                        }

                        //TODO update settings at this point
                        //System.out.println("Settings should be updated here");
                        //instanceViewerPanel.updateSettings();

                        VizPanel.associateWithPalette(
                            _vizState.getModelName(),
                            _vizState.getPalette());

                        viewList.setSelectedValue(_vizState.getPaletteCurrentView(), true);

                        _modelsToPalLocs.put(_vizState.getModelName(), file);
                        _parent.updateGUISelections();
                    }
                }
            }
        });

        panel.add(loadButton);

	if (!_chooserOnly) {
	    JButton saveButton = new JButton("Save Palette");
	    saveButton.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			savePalette(false);
		    }
		});
	    panel.add(saveButton);
	    
	    JButton saveAsButton = new JButton("Save Palette As");
	    saveAsButton.addActionListener(new ActionListener() {
		    public void actionPerformed(ActionEvent e) {
			savePalette(true);
            }
		});
	    panel.add(saveAsButton);
	}

        return panel;
    }

    private View getView() {
        return (View)listModel.get(viewList.getSelectedIndex());
    }

    private void savePalette(boolean saveAs) {
        if (saveAs || _modelsToPalLocs.get(_vizState.getModelName()) == null) {
            int returnVal = chooser.showSaveDialog(_frame);
            if (returnVal == FileChooser.APPROVE_OPTION) {
                File file = extFileFilter.addExtensionIfNone(chooser.getSelectedFile());
                if (file.exists()) {
                    int opt =
                        JOptionPane.showConfirmDialog(
                            null,
                            "File exists.  Overwrite data?",
                            "Warning",
                            JOptionPane.YES_NO_OPTION,
                            JOptionPane.WARNING_MESSAGE);
                    if (opt != JOptionPane.NO_OPTION) {
                        _vizState.savePalette(file);
                        if (saveAs) {
                            // make a new copy of palette as the one for this location
                            ViewPalette newPal = _vizState.getPalette().copy();
                            _vizState.setPalette(newPal);
                            _pathsToPals.put(file, newPal);
                        } else if (_pathsToPals.get(file) == null) {
                            _pathsToPals.put(file, _vizState.getPalette());
                        }
                        _modelsToPalLocs.put(_vizState.getModelName(), file);
                    }
                } else {
                    // _vizState.savePalette needs to set paletteChanged to false
                    _vizState.savePalette(file);
                    if (saveAs) {
                        // make a new copy of palette as the one for this location
                        ViewPalette newPal = _vizState.getPalette().copy();
                        _vizState.setPalette(newPal);
                        _pathsToPals.put(file, newPal);
                    } else if (_pathsToPals.get(file) == null) {
                        _pathsToPals.put(file, _vizState.getPalette());
                    }
                    _modelsToPalLocs.put(_vizState.getModelName(), file);
                }
            }
        } else {
            // save to the previous location			
            _vizState.savePalette((File)_modelsToPalLocs.get(_vizState.getModelName()));
        }
    }

    void updateAll() {
        // fixes same bug as in ModulePanel's updateAllGUI...foolish for not remember this earlier
        _loadingState = true;
        views.clear();
        views.addAll(_vizState.getAllViews());

        listModel.clear();
        Iterator iter = views.iterator();
        while (iter.hasNext()) {
            listModel.addElement(iter.next());
        }
        viewList.setSelectedIndex(listModel.indexOf(_vizState.getView()));
	if (!_chooserOnly) {
	    if (listModel.getSize() == 1) {
		deleteButton.setEnabled(false);
	    } else {
		deleteButton.setEnabled(true);
	    }
	}
        //_parent.redrawGraph();
        _loadingState = false;
        //System.out.println("redrawGraph called\n");
    }
}

class ViewListCellRenderer extends JLabel implements ListCellRenderer {

	private static final long serialVersionUID = 1L;

	public ViewListCellRenderer() {
        super();
        setOpaque(true);
    }

    public Component getListCellRendererComponent(
        JList list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus) {
        setText(((View)value).getName());
        setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));

        if (isSelected) {
            setBackground(list.getSelectionBackground());
            setForeground(list.getSelectionForeground());
        } else {
            setBackground(list.getBackground());
            setForeground(list.getForeground());
        }

        return this;
    }
}
