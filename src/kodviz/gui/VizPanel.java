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
import java.awt.GridLayout;
import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.UIManager;

import kodviz.alloyviz.Cartoonist;
import kodviz.alloyviz.Model;
import kodviz.alloyviz.View;
import kodviz.alloyviz.ViewPalette;
import kodviz.alloyviz.VizInstance;
import kodviz.alloyviz.VizState;
import kodviz.graph.Cartoon;

/**
 * This class is *the* class you call to start up the whole visualizer tool.
 */

@SuppressWarnings("unchecked")
public class VizPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private VizInstance _instance;
    private VizState _vizState;
    private Cartoonist cartoonist;
    private String _filePath, _modelName;

    //private final JSplitPane splitPane;
    private JPanel editorPanel, graphPanel;
    //private JScrollPane graphScrollPanel;
    //private final JPanel buttonPanel;
    private CustomizationPanel custPanel;

    //private final ImageIcon closedIcon;
    //private final ImageIcon openIcon;
    //private final JButton toggleButton;

    //private final JButton applyButton;

    private Set vizPanelListeners;

    // to keep track of the palette loaded for a particular model
    private static Map _namesToPalettes = new HashMap();
    // to keep track of the object associated with each palette filename

    // to use all functionality, you really shouldn't omit the ae argument
    // simply construct a new AlloyEvaluator with the SolutionData as its
    // argument and it's good to go, but a barebones version is still
    // here for compatibility

    public VizPanel(VizInstance instance_) {
        this(instance_, null);
    }

    public VizPanel(VizInstance instance_, String filePath_) {
        this(instance_, filePath_, false);
    }

    public VizPanel(VizInstance instance_, String filePath_, boolean isMeta_) {
        super();

        vizPanelListeners = new HashSet();

        _instance = instance_;
        Model _model = _instance.getModel();
        
        // new file name stuff
        _filePath = filePath_;
        File tempF = _filePath == null ? null : new File(_filePath);
        //String temp = tempF.getName();
        //_fileName = temp.substring(0,temp.lastIndexOf('.')); // removes last extension
        //model_.setName(_fileName);
        if (isMeta_) {
        	// jbaek added --- cannot be meta!
            throw new RuntimeException();
        } else {
            _modelName = _model.getName();
            ViewPalette pal = (ViewPalette)_namesToPalettes.get(_modelName);
            if (pal == null) {
                _vizState = new VizState(_model, _instance);
                _namesToPalettes.put(_modelName, _vizState.getPalette());
            } else {
                _vizState = new VizState(_model, _instance, pal);
            }
        }
        _vizState.setModelFile(tempF);

        cartoonist = new Cartoonist();
        //splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        editorPanel = new JPanel();
        graphPanel = new JPanel();
        graphPanel.setLayout(new GridLayout());
        //graphScrollPanel = new JScrollPane(graphPanel);
        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.setLayout(new GridLayout());
        rightPanel.add(graphPanel, BorderLayout.CENTER);

        editorPanel.setLayout(new BorderLayout());
        custPanel = new CustomizationPanel(_vizState, this);
        editorPanel.add(custPanel, BorderLayout.CENTER);

        //buttonPanel = new JPanel(new FlowLayout());
        //rightPanel.add(buttonPanel, BorderLayout.SOUTH);

        //closedIcon = new ImageIcon(loadImage("images/Play16.gif"));
        //openIcon = new ImageIcon(loadImage("images/Down16.gif"));
        //toggleButton = new JButton();
        //toggleButton.setBorderPainted(false);
        /*
            toggleButton.setAction(new AbstractAction("Open Settings") {
                private boolean visible = false;
                public void actionPerformed(ActionEvent e) {
                    visible = !visible;
                    if (visible) {
                        toggleButton.setText("Close Settings");
                        //splitPane.setResizeWeight(0.0);
                        applyButton.setVisible(true);
                        //editorPanel.add(buttonPanel, BorderLayout.NORTH);
                        //splitPane.setLeftComponent(editorPanel);
                        //((BasicSplitPaneUI)splitPane.getUI()).getDivider().setVisible(true);
                        //splitPane.setDividerLocation(0.75);
                fireCustomizerOpened();
                    }
                    else {
                        toggleButton.setText("Open Settings");
                        //splitPane.setDividerLocation(0.9);
                        applyButton.setVisible(false);
                        //splitPane.setLeftComponent(null);
                        //((BasicSplitPaneUI)splitPane.getUI()).getDivider().setVisible(false);
                        //splitPane.setResizeWeight(1.0);
                fireCustomizerClosed();
                    }
                }
            });
            buttonPanel.add(toggleButton);
            applyButton = new JButton("Apply");
            applyButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    display();
                    //splitPane.setDividerLocation(splitPane.getDividerLocation());
                    //System.out.println("Apply button\n");
                }
            });
        buttonPanel.add(applyButton);
            //buttonPanel.setMaximumSize(buttonPanel.getPreferredSize());

            editorPanel.add(buttonPanel, BorderLayout.NORTH);
        */

        //splitPane.setRightComponent(rightPanel);
        //splitPane.setLeftComponent(null);
        //splitPane.setResizeWeight(0.0);
        //((BasicSplitPaneUI)splitPane.getUI()).getDivider().setVisible(false);
        //applyButton.setVisible(false);
        //splitPane.setDividerLocation(0.25);

        if (_onMac()) {
            custPanel.setBorder(null);
            graphPanel.setBorder(null);
            rightPanel.setBorder(null);
            editorPanel.setBorder(null);
            //graphScrollPanel.setBorder(null);
            //splitPane.setBorder(null);

            //graphScrollPanel.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            //graphScrollPanel.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

            Font font = new Font("LucidaGrande", Font.PLAIN, 11);
            resizeComboBoxes(editorPanel, font);
            //toggleButton.setFont(font);
            //applyButton.setFont(font);
        }

        this.setLayout(new GridLayout());
        //this.add(buttonPanel, BorderLayout.NORTH);
        //this.add(splitPane, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.CENTER);

        display();
        //System.out.println("VizPanel Constructor\n");
    }

    /**
     * associates a model name with a specific palette
     */
    static void associateWithPalette(String name, ViewPalette pal) {
        _namesToPalettes.put(name, pal);
    }

    void display() {
        Cartoon c = cartoonist.produceCartoon(_instance, _vizState.getView());
        graphPanel.removeAll();
        //System.out.println(_vizState.getView());
        graphPanel.add(new GraphPanel(_instance, _vizState.getView().getModelView().getProjectionFrame(), c));
        validate();
        repaint();
    }

    public void updateEditor() {
        // no longer checks whether or not the palette is correct first
        // just set it regardless because we need the extra cust var
        // processing in setPalette/setView
        ViewPalette correctPal = (ViewPalette)_namesToPalettes.get(_modelName);
        _vizState.setPalette(correctPal);

        custPanel.updateGUISelections();
        display();

    }

    public JPanel getCurrentGraph() {
        Cartoon c = cartoonist.produceCartoon(_instance, _vizState.getView());
        return new GraphPanel(_instance, _vizState.getView().getModelView().getProjectionFrame(), c);
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

    /* jbaek -- deprecated (no longer using viewpanel)
    public JPanel makeViewChooser() {
        return new ViewPanel(_vizState, custPanel, true);
    }*/

    public JPanel getSettingsPanel() {
        return editorPanel;
    }

    /*private static Image loadImage(String pathName) {
        Image image = ResourceManager.getImage(pathName);

        if (image == null) {
            //System.out.println("Couldn't load image " + pathName);
            return new BufferedImage(5, 5, BufferedImage.TYPE_INT_RGB);
        } else {
            return image;
        }
    }*/

    private static boolean _onMac() {
        return System.getProperty("mrj.version") != null
            && UIManager.getSystemLookAndFeelClassName().equals(UIManager.getLookAndFeel().getClass().getName());
    }

    public void addVizPanelListener(VizPanelListener l) {
        vizPanelListeners.add(l);
    }

    public void removeVizPanelListener(VizPanelListener l) {
        vizPanelListeners.remove(l);
    }

    /*private void fireCustomizerOpened() {
        Iterator it = vizPanelListeners.iterator();
        while (it.hasNext()) {
            VizPanelListener l = (VizPanelListener)it.next();
            l.customizerOpened(new VizPanelEvent(VizPanel.this));
        }
    }*/

    /*private void fireCustomizerClosed() {
        Iterator it = vizPanelListeners.iterator();
        while (it.hasNext()) {
            VizPanelListener l = (VizPanelListener)it.next();
            l.customizerClosed(new VizPanelEvent(VizPanel.this));
        }
    }*/
    
    public View getCurrentView() { // added by jbaek
    	return _vizState.getView().copy();
    }
    
    public boolean isPaletteChanged() { // added by jbaek
    	return _vizState.paletteChanged();
    }
    public void setPaletteAsUnchanged() { // added by jbaek
    	_vizState.setPaletteAsUnchanged(); 
    }
    
    protected void loadPaletteXML(File f_) throws Exception {
    	_vizState.loadPaletteXML(f_);
    	custPanel.updateGUISelections();
    	display();
    }
    protected void savePaletteXML(File f_) throws Exception {
    	_vizState.savePaletteXML(f_);
    }
}
