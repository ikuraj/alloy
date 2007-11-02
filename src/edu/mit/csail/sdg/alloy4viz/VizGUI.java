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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.prefs.Preferences;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import edu.mit.csail.sdg.alloy4.Computer;
import edu.mit.csail.sdg.alloy4.MultiRunner;
import edu.mit.csail.sdg.alloy4.OurBinaryCheckbox;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurConsole;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;
import edu.mit.csail.sdg.alloy4.Util.IntPref;
import edu.mit.csail.sdg.alloy4.Util.StringPref;
import edu.mit.csail.sdg.alloy4graph.VizViewer;

/**
 * GUI main window for the visualizer.
 *
 * <p><b>Thread Safety:</b>  Can be called only by the AWT event thread.
 */

public final class VizGUI implements MultiRunnable, ComponentListener {

    /** Simple test driver method. */
    public static void main(final String[] args) {
        // Make sure args.length==1
        if (args.length!=1) {
            System.out.println("Syntax error: please specify the XML file name to load.");
            System.exit(1);
        }
        // Enable better look-and-feel
        if (Util.onMac() || Util.onWindows()) {
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Alloy Analyzer "+Version.version());
            System.setProperty("com.apple.mrj.application.growbox.intrudes","true");
            System.setProperty("com.apple.mrj.application.live-resize","true");
            System.setProperty("com.apple.macos.useScreenMenuBar","true");
            System.setProperty("apple.laf.useScreenMenuBar","true");
            try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()); }
            catch (Throwable e) { }
        }
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() { main(args); }
            });
            return;
        }
        JFrame f = new JFrame("Test");
        VizGUI gui = new VizGUI(false, args[0], null, null, null);
        f.getContentPane().setLayout(new BorderLayout());
        f.getContentPane().add(gui.getPanel(), BorderLayout.CENTER);
        f.pack();
        f.setSize(700, 500);
        f.setLocation(0, 0);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setVisible(true);
    }

    /** The background color for the toolbar. */
    private static final Color background = new Color(0.9f, 0.9f, 0.9f);

    /** The icon for a "checked" menu item. */
    private static final Icon iconYes=OurUtil.loadIcon("images/menu1.gif");

    /** The icon for an "unchecked" menu item. */
    private static final Icon iconNo=OurUtil.loadIcon("images/menu0.gif");

    /** Whether the JVM should shutdown after the last file is closed. */
    private final boolean standalone;

    /** The current display mode. */
    private VisualizerMode currentMode = VisualizerMode.get();

    /** The JFrame for the main GUI window; or null if we intend to display the graph inside a user-given JPanel instead. */
    private final JFrame frame;

    /** The toolbar. */
    private final JToolBar toolbar;

    /** The projection popup menu. */
    private final JPopupMenu projectionPopup;

    private final JButton projectionButton, openSettingsButton, closeSettingsButton, loadSettingsButton, saveSettingsButton, saveAsSettingsButton, resetSettingsButton;
    private final JButton updateSettingsButton, openEvaluatorButton, closeEvaluatorButton, enumerateButton;
    private final JButton magicLayout;
    private final JButton magicColour;

    /** The buttons for switching the display modes. */
    private final JButton vizButton, xmlButton, treeButton, dotButton, kodSrcButton, kodInstButton, plugin0Button;

    /** This list must contain all the display mode buttons (that is, vizButton, xmlButton...) */
    private final List<JButton> solutionButtons = new ArrayList<JButton>();

    /** The "theme" menu. */
    private final JMenu thememenu;

    /** The "window" menu. */
    private final JMenu windowmenu;

    /** The "show next" menu item. */
    private final JMenuItem enumerateMenu;

    /** 0: theme and evaluator are both invisible; 1: theme is visible; 2: evaluator is visible. */
    private int settingsOpen=0;

    /** The current instance and visualization settings; null if none is loaded. */
    private VizState myState=null;

    /** The customization panel to the left; null if it is not yet loaded. */
    private VizCustomizationPanel myCustomPanel=null;

    /** The evaluator panel to the left; null if it is not yet loaded. */
    private OurConsole myEvaluatorPanel=null;

    /** The graphical panel to the right; null if it is not yet loaded. */
    private VizGraphPanel myGraphPanel=null;

    /** The splitpane between the customization panel and the graph panel. */
    private final JSplitPane splitpane;

    /** Returns the JSplitPane containing the customization/evaluator panel in the left and the graph on the right. */
    public JSplitPane getPanel() { return splitpane; }

    /** The last known divider position between the customization panel and the graph panel. */
    private int lastDividerPosition=0;

    /**
     * If nonnull, you can pass in an expression to be evaluated.
     * If it throws an exception, that means an error has occurred.
     */
    private final Computer evaluator;

    /**
     * If nonnull, you can call "enumerator.run(-1, XMLFilename)" to tell it to find the next solution.
     */
    private final MultiRunnable enumerator;

    //==============================================================================================//

    /** The current theme file; "" if there is no theme file loaded. */
    private String thmFileName="";

    /** Returns the current THM filename; "" if no theme file is currently loaded. */
    public String getThemeFilename() { return thmFileName; }

    //==============================================================================================//

    /** The current XML file; "" if there is no XML file loaded. */
    private String xmlFileName="";

    /** Returns the current XML filename; "" if no file is currently loaded. */
    public String getXMLfilename() { return xmlFileName; }

    //==============================================================================================//

    /** The list of XML files loaded in this session so far. */
    private final List<String> xmlLoaded=new ArrayList<String>();

    /** Return the list of XML files loaded in this session so far. */
    public List<String> getInstances() { return Collections.unmodifiableList(xmlLoaded); }

    //==============================================================================================//

    /** This maps each XML filename to a descriptive title. */
    private Map<String,String> xml2title = new LinkedHashMap<String,String>();

    /** Returns a short descriptive title associated with an XML file. */
    public String getInstanceTitle(String xmlFileName) {
        xmlFileName=Util.canon(xmlFileName);
        String answer=xml2title.get(xmlFileName);
        return (answer==null) ? "(unknown)" : answer;
    }

    //==============================================================================================//

    /** Add a vertical divider to the toolbar. */
    private void addDivider() {
        JPanel divider=OurUtil.makeBox(1,40,Color.LIGHT_GRAY);
        divider.setAlignmentY(0.5f);
        if (!Util.onMac()) toolbar.add(OurUtil.makeH(5,background)); else toolbar.add(OurUtil.makeH(5));
        toolbar.add(divider);
        if (!Util.onMac()) toolbar.add(OurUtil.makeH(5,background)); else toolbar.add(OurUtil.makeH(5));
    }

    //======== The Preferences ======================================================================================//
    //======== Note: you must make sure each preference has a unique key ============================================//

    /** This enum defines the set of possible visualizer modes. */
    private enum VisualizerMode {
        /** Visualize using graphviz's dot. */  Viz("graphviz"),
        /** See the DOT content. */             DOT("dot"),
        /** See the XML file content. */        XML("xml"),
        /** See the instance as a tree. */      Tree("tree"),
        /** See the raw input to Kodkod. */     KInput("kodkodJava"),
        /** See the raw output from Kodkod. */  KOutput("kodkodInstance"),
        /** Execute external plugin #0. */      Plugin0("plugin0"),
        /** Execute external plugin #1. */      Plugin1("plugin1"),
        /** Execute external plugin #2. */      Plugin2("plugin2"),
        /** Execute external plugin #3. */      Plugin3("plugin3"),
        /** Execute external plugin #4. */      Plugin4("plugin4"),
        /** Execute external plugin #5. */      Plugin5("plugin5"),
        /** Execute external plugin #6. */      Plugin6("plugin6"),
        /** Execute external plugin #7. */      Plugin7("plugin7"),
        /** Execute external plugin #8. */      Plugin8("plugin8"),
        /** Execute external plugin #9. */      Plugin9("plugin9");
        /** This is a unique String for this value; it should be kept consistent in future versions. */
        private final String id;
        /** Constructs a new VisualizerMode value with the given id. */
        private VisualizerMode(String id) { this.id=id; }
        /** Given an id, return the enum value corresponding to it (if there's no match, then return Viz). */
        private static VisualizerMode parse(String id) {
            for(VisualizerMode vm:values()) if (vm.id.equals(id)) return vm;
            return Viz;
        }
        /** Saves this value into the Java preference object. */
        public void set() { Preferences.userNodeForPackage(Util.class).put("VisualizerMode",id); }
        /** Reads the current value of the Java preference object (if it's not set, then return Viz). */
        public static VisualizerMode get() { return parse(Preferences.userNodeForPackage(Util.class).get("VisualizerMode","")); }
    };

    /** The latest X corrdinate of the Alloy Visualizer window. */
    private static final IntPref VizX = new IntPref("VizX",0,-1,65535);

    /** The latest Y corrdinate of the Alloy Visualizer window. */
    private static final IntPref VizY = new IntPref("VizY",0,-1,65535);

    /** The latest width of the Alloy Visualizer window. */
    private static final IntPref VizWidth = new IntPref("VizWidth",0,-1,65535);

    /** The latest height of the Alloy Visualizer window. */
    private static final IntPref VizHeight = new IntPref("VizHeight",0,-1,65535);

    /** The first file in Alloy Visualizer's "open recent theme" list. */
    private static final StringPref Theme0 = new StringPref("Theme0");

    /** The second file in Alloy Visualizer's "open recent theme" list. */
    private static final StringPref Theme1 = new StringPref("Theme1");

    /** The third file in Alloy Visualizer's "open recent theme" list. */
    private static final StringPref Theme2 = new StringPref("Theme2");

    /** The fourth file in Alloy Visualizer's "open recent theme" list. */
    private static final StringPref Theme3 = new StringPref("Theme3");

    //==============================================================================================//

    /**
     * Creates a new visualization GUI window; this method can only be called by the AWT event thread.
     * @param standalone - whether the JVM should shutdown after the last file is closed
     * @param xmlFileName - the filename of the incoming XML file; "" if there's no file to open
     * @param windowmenu - if standalone==false and windowmenu!=null, then this will be added as a menu on the menubar
     *
     * <p> Note: if standalone==false and xmlFileName.length()==0, then we will initially hide the window.
     */
    public VizGUI(boolean standalone, String xmlFileName, JMenu windowmenu) {
        this(standalone, xmlFileName, windowmenu, null, null);
    }

    /**
     * Creates a new visualization GUI window; this method can only be called by the AWT event thread.
     * @param standalone - whether the JVM should shutdown after the last file is closed
     * @param xmlFileName - the filename of the incoming XML file; "" if there's no file to open
     * @param windowmenu - if standalone==false and windowmenu!=null, then this will be added as a menu on the menubar
     * @param enumerator - if it's not null, it provides solution enumeration ability
     * @param evaluator - if it's not null, it provides solution evaluation ability
     *
     * <p> Note: if standalone==false and xmlFileName.length()==0, then we will initially hide the window.
     */
    public VizGUI(boolean standalone, String xmlFileName, JMenu windowmenu, MultiRunnable enumerator, Computer evaluator) {
        this(standalone, xmlFileName, windowmenu, enumerator, evaluator, true);
    }

    /**
     * Creates a new visualization GUI window; this method can only be called by the AWT event thread.
     * @param standalone - whether the JVM should shutdown after the last file is closed
     * @param xmlFileName - the filename of the incoming XML file; "" if there's no file to open
     * @param windowmenu - if standalone==false and windowmenu!=null, then this will be added as a menu on the menubar
     * @param enumerator - if it's not null, it provides solution enumeration ability
     * @param evaluator - if it's not null, it provides solution evaluation ability
     * @param makeWindow - if false, then we will only construct the JSplitPane, without making the window
     *
     * <p> Note: if standalone==false and xmlFileName.length()==0 and makeWindow==true, then we will initially hide the window.
     */
    public VizGUI(boolean standalone, String xmlFileName, JMenu windowmenu, MultiRunnable enumerator, Computer evaluator, boolean makeWindow) {

        this.enumerator=enumerator;
        this.standalone=standalone;
        this.evaluator=evaluator;
        this.frame = makeWindow ? new JFrame("Alloy Visualizer") : null;

        // Figure out the desired x, y, width, and height
        int screenWidth=OurUtil.getScreenWidth(), screenHeight=OurUtil.getScreenHeight();
        int width=VizWidth.get();
        if (width<0) width=screenWidth-150; else if (width<100) width=100;
        if (width>screenWidth) width=screenWidth;
        int height=VizHeight.get();
        if (height<0) height=screenHeight-150; else if (height<100) height=100;
        if (height>screenHeight) height=screenHeight;
        int x=VizX.get(); if (x<0 || x>screenWidth-10) x=0;
        int y=VizY.get(); if (y<0 || y>screenHeight-10) y=0;

        // Create the menubar
        JMenuBar mb = OurUtil.makeMenuBar();
        JMenu fileMenu = OurUtil.makeMenu(mb, "File", KeyEvent.VK_F, null, 0);
        OurUtil.makeMenuItem(fileMenu, "Open...", true, KeyEvent.VK_O, KeyEvent.VK_O, this, EV_LOAD_INSTANCE);
        OurUtil.makeMenuItem(fileMenu, "Close", true, KeyEvent.VK_W, KeyEvent.VK_W, this, EV_CLOSE);
        if (!standalone)
            OurUtil.makeMenuItem(fileMenu, "Close All", true, KeyEvent.VK_A, -1, this, EV_CLOSE_ALL);
        else
            OurUtil.makeMenuItem(fileMenu, "Quit", true, KeyEvent.VK_Q, KeyEvent.VK_Q, this, EV_CLOSE_ALL);
        JMenu instanceMenu = OurUtil.makeMenu(mb, "Instance", KeyEvent.VK_I, null, 0);
        enumerateMenu = OurUtil.makeMenuItem(instanceMenu, "Show Next Solution", true, KeyEvent.VK_N, KeyEvent.VK_N, this, EV_BAR_NEXT);
        thememenu = OurUtil.makeMenu(mb, "Theme", KeyEvent.VK_T, this, EV_THEME);
        if (standalone || windowmenu==null)
            this.windowmenu=(windowmenu=OurUtil.makeMenu(mb, "Window", KeyEvent.VK_W, this, EV_WINDOW));
        else
            this.windowmenu=windowmenu;
        mb.add(windowmenu);
        thememenu.setEnabled(false);
        windowmenu.setEnabled(false);
        if (frame!=null) frame.setJMenuBar(mb);

        // Create the toolbar
        projectionPopup = new JPopupMenu();
        projectionButton = new JButton("Projection: none");
        projectionButton.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                repopulateProjectionPopup();
                if (projectionPopup.getComponentCount()>0) projectionPopup.show(projectionButton, 10, 10);
            }
        });
        repopulateProjectionPopup();
        toolbar = new JToolBar();
        toolbar.setVisible(false);
        toolbar.setFloatable(false);
        toolbar.setBorder(new EmptyBorder(0,0,0,0));
        if (!Util.onMac()) toolbar.setBackground(background);
        vizButton=makeSolutionButton("Viz",
                "Show Visualization", "images/24_graph.gif", EV_BAR_VIZ);
        dotButton=makeSolutionButton("Dot",
                "Show the Dot File for the Graph", "images/24_plaintext.gif", EV_BAR_DOT);
        xmlButton=makeSolutionButton("XML",
                "Show XML", "images/24_plaintext.gif", EV_BAR_XML);
        treeButton=makeSolutionButton("Tree",
                "Show Tree", "images/24_texttree.gif", EV_BAR_TREE);
        kodSrcButton=makeSolutionButton("KK In",
                "Show KodKod Input", "images/24_plaintext.gif", EV_BAR_KODKOD_IN);
        kodInstButton=makeSolutionButton("KK Out",
                "Show KodKod Instance", "images/24_plaintext.gif", EV_BAR_KODKOD_OUT);
        plugin0Button=makeSolutionButton("Plugin",
                "Run external plugin", "images/24_graph.gif", EV_BAR_PLUGIN0);
        if (frame!=null) addDivider();
        toolbar.add(closeSettingsButton=OurUtil.button("Close",
                "Close the theme customization panel", "images/24_settings_close2.gif", this, EV_BAR_CLOSE_THEME));
        toolbar.add(updateSettingsButton=OurUtil.button("Apply",
                "Apply the changes to the current theme", "images/24_settings_apply2.gif", this, EV_BAR_APPLY));
        toolbar.add(openSettingsButton=OurUtil.button("Theme",
                "Open the theme customization panel", "images/24_settings.gif", this, EV_BAR_OPEN_THEME));
        toolbar.add(magicLayout=OurUtil.button("Magic Layout",
                "Automatic theme customization (will reset current theme)", "images/24_settings_apply2.gif", this, EV_BAR_MAGIC_LAYOUT));
        toolbar.add(magicColour=OurUtil.button("Magic Colour",
                "Automatic theme colour and shape customization", "images/24_settings_apply2.gif", this, EV_BAR_MAGIC_COLOR));
        toolbar.add(openEvaluatorButton=OurUtil.button("Evaluator",
                "Open the evaluator", "images/24_settings.gif", this, EV_BAR_OPEN_EVAL));
        toolbar.add(closeEvaluatorButton=OurUtil.button("Close Evaluator",
                "Close the evaluator", "images/24_settings_close2.gif", this, EV_BAR_CLOSE_EVAL));
        toolbar.add(enumerateButton=OurUtil.button("Next",
                "Show the next solution", "images/24_history.gif", this, EV_BAR_NEXT));
        toolbar.add(projectionButton);
        toolbar.add(loadSettingsButton=OurUtil.button("Load",
                "Load the theme customization from a theme file", "images/24_open.gif", this, EV_LOAD_THEME));
        toolbar.add(saveSettingsButton=OurUtil.button("Save",
                "Save the current theme customization", "images/24_save.gif", this, EV_SAVE_THEME));
        toolbar.add(saveAsSettingsButton=OurUtil.button("Save As",
                "Save the current theme customization as a new theme file", "images/24_save.gif", this, EV_SAME_THEME_AS));
        toolbar.add(resetSettingsButton=OurUtil.button("Reset",
                "Reset the theme customization", "images/24_settings_close2.gif", this, EV_RESET_THEME));
        settingsOpen=0;

        // Create the horizontal split pane
        splitpane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitpane.setOneTouchExpandable(false);
        splitpane.setResizeWeight(0.);
        splitpane.setContinuousLayout(true);
        splitpane.setBorder(null);
        ((BasicSplitPaneUI)(splitpane.getUI())).getDivider().setBorder(new OurBorder(false,true,false,false));

        // Display the window, then proceed to load the input file
        if (frame!=null) {
           frame.pack();
           frame.setSize(width,height);
           frame.setLocation(x,y);
           frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
           frame.addWindowListener(new MultiRunner(this, EV_CLOSE));
           frame.addComponentListener(this);
        }
        if (xmlFileName.length()>0) run(EVS_LOAD_INSTANCE, xmlFileName);
    }

    /** Invoked when the Visualizationwindow is resized. */
    public void componentResized(ComponentEvent e) {
        if (frame!=null) { VizWidth.set(frame.getWidth()); VizHeight.set(frame.getHeight()); componentMoved(e); }
    }

    /** Invoked when the Visualizationwindow is moved. */
    public void componentMoved(ComponentEvent e) {
        if (frame!=null) { Point p=frame.getLocation(); VizX.set(p.x); VizY.set(p.y); }
    }

    /** Invoked when the Visualizationwindow is shown. */
    public void componentShown(ComponentEvent e) {}

    /** Invoked when the Visualizationwindow is hidden. */
    public void componentHidden(ComponentEvent e) {}

    /** Helper method that repopulates the Porjection popup menu. */
    private void repopulateProjectionPopup() {
        int num=0;
        String label="Projection: none";
        if (myState==null) {
            projectionButton.setEnabled(false);
            return;
        }
        projectionButton.setEnabled(true);
        projectionPopup.removeAll();
        final Set<AlloyType> projected = myState.getProjectedTypes();
        for(final AlloyType t: myState.getOriginalModel().getTypes()) if (myState.canProject(t)) {
            final boolean on = projected.contains(t);
            final JMenuItem m = OurUtil.makeMenuItem(t.getName(), on ? OurBinaryCheckbox.on : OurBinaryCheckbox.off);
            m.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    if (on) myState.deproject(t); else myState.project(t);
                    updateDisplay();
                }
            });
            projectionPopup.add(m);
            if (on) { num++; if (num==1) label="Projected over "+t.getName(); }
        }
        projectionButton.setText(num>1 ? ("Projected over "+num+" sigs") : label);
    }

    /** Helper method that refreshes the right-side visualization panel with the latest settings. */
    private void updateDisplay() {
        if (myState==null) return;
        // First, update the toolbar
        currentMode.set();
        for(JButton button:solutionButtons) button.setEnabled(settingsOpen!=1);
        switch (currentMode) {
            case Tree: treeButton.setEnabled(false); break;
            case KInput: kodSrcButton.setEnabled(false); break;
            case KOutput: kodInstButton.setEnabled(false); break;
            case XML: xmlButton.setEnabled(false); break;
            case DOT: dotButton.setEnabled(false); break;
            case Plugin0: plugin0Button.setEnabled(false); break;
            default: vizButton.setEnabled(false);
        }
        final boolean isMeta = myState.getOriginalInstance().isMetamodel;
        vizButton.setVisible(frame!=null);
        treeButton.setVisible(frame!=null);
        dotButton.setVisible(frame!=null);
        xmlButton.setVisible(frame!=null);
        kodSrcButton.setVisible(myState.getOriginalInstance().kodkod_input.length()>0 && frame!=null);
        kodInstButton.setVisible(myState.getOriginalInstance().kodkod_output.length()>0 && frame!=null);
        magicLayout.setVisible(!isMeta && (settingsOpen==0 || settingsOpen==1) && currentMode==VisualizerMode.Viz);
        magicColour.setVisible(false); // hidden for now
        projectionButton.setVisible((settingsOpen==0 || settingsOpen==1) && currentMode==VisualizerMode.Viz);
        openSettingsButton.setVisible(               settingsOpen==0 && currentMode==VisualizerMode.Viz);
        loadSettingsButton.setVisible(frame==null && settingsOpen==1 && currentMode==VisualizerMode.Viz);
        saveSettingsButton.setVisible(frame==null && settingsOpen==1 && currentMode==VisualizerMode.Viz);
        saveAsSettingsButton.setVisible(frame==null && settingsOpen==1 && currentMode==VisualizerMode.Viz);
        resetSettingsButton.setVisible(frame==null && settingsOpen==1 && currentMode==VisualizerMode.Viz);
        closeSettingsButton.setVisible(settingsOpen==1 && currentMode==VisualizerMode.Viz);
        updateSettingsButton.setVisible(settingsOpen==1 && currentMode==VisualizerMode.Viz);
        openEvaluatorButton.setVisible(!isMeta && settingsOpen==0 && evaluator!=null);
        closeEvaluatorButton.setVisible(!isMeta && settingsOpen==2 && evaluator!=null);
        enumerateMenu.setEnabled(!isMeta && settingsOpen==0 && enumerator!=null);
        enumerateButton.setVisible(!isMeta && settingsOpen==0 && enumerator!=null);
        toolbar.setVisible(true);
        // Now, generate the graph or tree or textarea that we want to display on the right
        JComponent content;
        if (frame!=null) frame.setTitle(makeVizTitle());
        switch (currentMode) {
           case Plugin0:
           case Tree:
               if (currentMode!=VisualizerMode.Tree) runPlugin(myState);
               content=StaticTreeMaker.makeTree(myState.getOriginalInstance(), makeVizTitle(), myState); break;
           case XML: content=getTextComponent(xmlFileName); break;
           case KInput: content=makeTextArea(myState.getOriginalInstance().kodkod_input); break;
           case KOutput: content=makeTextArea(myState.getOriginalInstance().kodkod_output); break;
           default:
                if (myGraphPanel==null) myGraphPanel=new VizGraphPanel(myState, currentMode == VisualizerMode.DOT);
                else {myGraphPanel.seeDot(currentMode==VisualizerMode.DOT); myGraphPanel.remakeAll();}
                content=myGraphPanel;
        }
        // Now, display them!
        final Box instanceTopBox = Box.createHorizontalBox();
        instanceTopBox.add(toolbar);
        final JPanel instanceArea = new JPanel(new BorderLayout());
        instanceArea.add(instanceTopBox, BorderLayout.NORTH);
        instanceArea.add(content, BorderLayout.CENTER);
        instanceArea.setVisible(true);
        if (!Util.onMac()) { instanceTopBox.setBackground(background); instanceArea.setBackground(background); }
        if (1==1 || settingsOpen>0) { // for now, let's always have the JSplitPane... until we're sure this is what we want
            JComponent left;
            if (settingsOpen==1) {
                if (myCustomPanel==null) myCustomPanel=new VizCustomizationPanel(splitpane,myState); else
                   myCustomPanel.remakeAll();
                left=myCustomPanel;
            } else if (settingsOpen>1) {
                if (myEvaluatorPanel==null)
                    myEvaluatorPanel=new OurConsole(evaluator,
                       "The ", true, "Alloy Evaluator ", false,
                       "allows you to type\nin Alloy expressions and see their values.\nFor example, ", true,
                       "univ", false, " shows the list of all atoms.\n");
                evaluator.setSourceFile(xmlFileName);
                left = myEvaluatorPanel;
                left.setBorder(new OurBorder(false,false,false,false));
            } else {
                left=null;
            }
            if (frame!=null && frame.getContentPane()==splitpane) lastDividerPosition=splitpane.getDividerLocation();
            splitpane.setRightComponent(instanceArea);
            splitpane.setLeftComponent(left);
            if (left!=null) {
               Dimension dim=left.getPreferredSize();
               if (lastDividerPosition<50 && frame!=null) lastDividerPosition=frame.getWidth()/2;
               if (lastDividerPosition<dim.width) lastDividerPosition=dim.width;
               if (settingsOpen==2 && lastDividerPosition>400) lastDividerPosition=400;
               splitpane.setDividerLocation(lastDividerPosition);
            }
            if (frame!=null) frame.setContentPane(splitpane);
        } else {
            if (frame!=null) frame.setContentPane(instanceArea);
        }
        if (settingsOpen!=2) {
            content.requestFocusInWindow();
        } else {
            myEvaluatorPanel.requestFocusInWindow();
        }
        repopulateProjectionPopup();
        if (frame!=null) frame.validate(); else splitpane.validate();
    }

    /** Helper method that creates a button and add it to both the "SolutionButtons" list, as well as the toolbar. */
    private JButton makeSolutionButton(String label, String toolTip, String image, int mode) {
        JButton button = OurUtil.button(label, toolTip, image, this, mode);
        solutionButtons.add(button);
        toolbar.add(button);
        return button;
    }

    /** Helper method that returns a concise description of the instance currently being displayed. */
    private String makeVizTitle() {
        String filename = (myState!=null ? myState.getOriginalInstance().filename : "");
        String commandname = (myState!=null ? myState.getOriginalInstance().commandname : "");
        int i=filename.lastIndexOf('/');
        if (i>=0) filename=filename.substring(i+1);
        i=filename.lastIndexOf('\\');
        if (i>=0) filename=filename.substring(i+1);
        int n=filename.length();
        if (n>4 && filename.substring(n-4).equalsIgnoreCase(".als")) filename=filename.substring(0,n-4);
        if (filename.length()>0) return "("+filename+") "+commandname; else return commandname;
    }

    /** Helper method that inserts "filename" into the "recently opened THEME file list". */
    private void addThemeHistory(String filename) {
        String name0=Theme0.get(), name1=Theme1.get(), name2=Theme2.get();
        if (name0.equals(filename)) return; else {Theme0.set(filename); Theme1.set(name0);}
        if (name1.equals(filename)) return; else Theme2.set(name1);
        if (name2.equals(filename)) return; else Theme3.set(name2);
    }

    /** Helper method that reads a file and then return a JTextArea containing it. */
    private JComponent getTextComponent(String filename) {
        String text="";
        try {
            text="<!-- "+filename+" -->\n"+Util.readAll(filename);
        } catch(IOException ex) {
            text="# Error reading from "+filename;
        }
        return makeTextArea(text);
    }

    /** Helper method that returns a JScrollPane containing the given string in a JTextArea. */
    private JScrollPane makeTextArea(final String text) {
        final JTextArea t = OurUtil.textarea(text,10,10);
        t.setBackground(Color.WHITE);
        t.setEditable(false);
        t.setLineWrap(true);
        t.setWrapStyleWord(true);
        JScrollPane ans=new JScrollPane(t,
            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setBorder(new OurBorder(true,false,true,false));
        return ans;
    }

    /** Returns the VizViewer that contains the graph; can be null if the graph hasn't been loaded yet. */
    public VizViewer getViewer() {
        if (null == myGraphPanel) return null;
        return myGraphPanel.alloyGetViewer();
    }

    //========================================= EVENTS ============================================================================================
    // The return values of events are undefined, except these five: { ev_saveTheme evs_saveTheme evs_saveThemeTS ev_saveThemeAs ev_saveThemeAsTS }
    //=============================================================================================================================================

    /** This event asks the user for a new XML file to load. */
    public static final int EV_LOAD_INSTANCE = 101;

    /** This event loads a new XML instance file if it's not the current file. */
    public static final int EVS_LOAD_INSTANCE = 102;

    /** This event loads a new XML instance file (reloading it from disk even if the filename equals the current filename) */
    public static final int EVS_LOAD_INSTANCE_FORCEFULLY = 103;

    /**
     * This event closes the current XML instance; if there are previously loaded files, we will load one of them;
     * otherwise, this window will set itself as invisible (if not in standalone mode),
     * or it will terminate the entire application (if in standalone mode).
     */
    public static final int EV_CLOSE = 104;

    /**
     * This event closes every XML file.
     * if in standalone mode, the JVM will then shutdown; otherwise it will just set the window invisible.
     */
    public static final int EV_CLOSE_ALL = 105;

    /** This event refreshes the "theme" menu. */
    public static final int EV_THEME = 201;

    /** This events asks the user for a new theme file to load. */
    public static final int EV_LOAD_THEME = 202;

    /** This events asks the user for a new theme file to load. */
    public static final int EV_LOAD_SAMPLE_THEME = 203;

    /** This events loads a specific theme file. */
    public static final int EVS_LOAD_THEME = 204;

    /** This events clears the history of loaded themes. */
    public static final int EV_CLEAR_THEME_HISTORY = 205;

    /** This event saves the current theme; returns true if it succeeded. */
    public static final int EV_SAVE_THEME = 206;

    /** This event saves a specific current theme; returns true if it succeeded. */
    public static final int EVS_SAVE_THEME = 207;

    /** This event saves the current theme to a new ".thm" file; returns true if it succeeded. */
    public static final int EV_SAME_THEME_AS = 208;

    /** This event resets the current theme. */
    public static final int EV_RESET_THEME = 209;

    /** This event saves a specific current theme; returns true if it succeeded. */
    public static final int EVS_SAVE_THEME_AS_TS = 210;

    /** This event saves the current theme to a new ".thm" file; returns true if it succeeded. */
    public static final int EV_SAVE_THEME_AS_TS = 211;

    /** This event refreshes the window menu. */
    public static final int EV_WINDOW = 301;

    /** This event shows the window and bring it to the front (if not already). */
    public static final int EV_SHOW = 302;

    /** This event minimizes the window. */
    public static final int EV_MINIMIZE = 303;

    /** This event alternatingly maximizes or restores the window. */
    public static final int EV_MAXIMIZE = 304;

    /** This event brings up an alert message. */
    public static final int EVS_ALERT = 305;

    private static final int EV_BAR_OPEN_THEME = 1001;
    private static final int EV_BAR_CLOSE_THEME = 1002;
    private static final int EV_BAR_APPLY = 1003;
    private static final int EV_BAR_OPEN_EVAL = 1004;
    private static final int EV_BAR_CLOSE_EVAL = 1005;
    private static final int EV_BAR_NEXT = 1006;
    private static final int EV_BAR_VIZ = 2001;
    private static final int EV_BAR_XML = 2002;
    private static final int EV_BAR_TREE = 2003;
    private static final int EV_BAR_KODKOD_IN = 2004;
    private static final int EV_BAR_KODKOD_OUT = 2005;
    private static final int EV_BAR_DOT = 2006;
    private static final int EV_BAR_PLUGIN0 = 2007;
    private static final int EV_BAR_MAGIC_LAYOUT=2008;
    private static final int EV_BAR_MAGIC_COLOR=2009;

    /** Performs the function given by "key" on the argument "arg"; returns true if it succeeds. */
    public boolean run(final int key, final int arg) {
        return true;
    }

    /** Performs the function given by "key" on the argument "arg"; returns true if it succeeds. */
    public boolean run(final int key, final String arg) {
        if (key==EVS_ALERT) {
            OurDialog.alert(frame, arg, "Alloy 4");
        }
        if (key==EVS_LOAD_INSTANCE || key==EVS_LOAD_INSTANCE_FORCEFULLY) {
            String xmlFileName=Util.canon(arg);
            File f=new File(xmlFileName);
            if (key==EVS_LOAD_INSTANCE_FORCEFULLY || !xmlFileName.equals(this.xmlFileName)) {
                AlloyInstance myInstance;
                try {
                    if (!f.canRead()) throw new Exception("");
                    myInstance = StaticInstanceReader.parseInstance(f);
                } catch (Throwable e) {
                    xmlLoaded.remove(arg);
                    xmlLoaded.remove(xmlFileName);
                    JOptionPane.showMessageDialog(null, "File does not exist or is not a valid Alloy instance: "
                       +e.getMessage()+"\n\nFile: "+xmlFileName,
                       "Error", JOptionPane.ERROR_MESSAGE);
                    if (xmlLoaded.size()>0) {run(EVS_LOAD_INSTANCE, xmlLoaded.get(xmlLoaded.size()-1)); return true;}
                    run(EV_CLOSE_ALL);
                    return true;
                }
                if (runPlugin(null)) plugin0Button.setVisible(true);
                else {
                    plugin0Button.setVisible(false);
                    if (currentMode==VisualizerMode.Plugin0) currentMode=VisualizerMode.Tree;
                }
                if (myInstance.kodkod_input.length()>0) kodSrcButton.setVisible(true);
                else {
                    kodSrcButton.setVisible(false);
                    if (currentMode==VisualizerMode.KInput) currentMode=VisualizerMode.Tree;
                }
                if (myInstance.kodkod_output.length()>0) kodInstButton.setVisible(true);
                else {
                    kodInstButton.setVisible(false);
                    if (currentMode==VisualizerMode.KOutput) currentMode=VisualizerMode.Tree;
                }
                if (myState==null) myState=new VizState(myInstance); else myState.loadInstance(myInstance);
                repopulateProjectionPopup();
                xml2title.put(xmlFileName, makeVizTitle());
                this.xmlFileName = xmlFileName;
            }
            if (!xmlLoaded.contains(xmlFileName)) xmlLoaded.add(xmlFileName);
            toolbar.setEnabled(true);
            settingsOpen=0;
            thememenu.setEnabled(true);
            windowmenu.setEnabled(true);
            if (frame!=null) {
               frame.setVisible(true);
               frame.setTitle("Alloy Visualizer "+Version.version()+" loading... Please wait...");
               if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)!=JFrame.MAXIMIZED_BOTH)
                   frame.setExtendedState(JFrame.NORMAL);
               frame.requestFocus();
               frame.toFront();
            }
            updateDisplay();
        }
        if (key==EVS_LOAD_THEME) {
            if (myState==null) return false; // Can only load if there is a VizState loaded
            String filename=Util.canon(arg);
            try {
                myState.loadPaletteXML(filename);
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(null,"Exception: "+ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            repopulateProjectionPopup();
            if (myCustomPanel!=null) myCustomPanel.remakeAll();
            if (myGraphPanel!=null) myGraphPanel.remakeAll();
            addThemeHistory(filename);
            thmFileName=filename;
            updateDisplay();
        }
        if (key==EVS_SAVE_THEME || key==EVS_SAVE_THEME_AS_TS) {
            if (myState==null) return false; // Can only save if there is a VizState loaded
            String filename=Util.canon(arg);
            try {
                if (key==EVS_SAVE_THEME) myState.savePaletteXML(filename); else myState.savePaletteTS(filename);
                filename=Util.canon(filename); // Since the canon name may have changed
                addThemeHistory(filename);
            } catch (Throwable er) {
                JOptionPane.showMessageDialog(null, "Error saving the theme: "+er.toString(), "Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            thmFileName = filename;
        }
        return true;
    }

    /** Performs the function given by "key"; returns true if it succeeds. */
    public boolean run(final int key) {

        if (key==EV_LOAD_INSTANCE) {
            File file=OurDialog.askFile(frame, true, null, ".xml", ".xml instance files");
            if (file==null) return false;
            Util.setCurrentDirectory(file.getParentFile());
            run(EVS_LOAD_INSTANCE_FORCEFULLY, file.getPath());
        }

        if (key==EV_CLOSE) {
            xmlLoaded.remove(xmlFileName);
            if (xmlLoaded.size()>0) return run(EVS_LOAD_INSTANCE, xmlLoaded.get(xmlLoaded.size()-1));
            if (standalone) System.exit(0); else if (frame!=null) frame.setVisible(false);
        }

        if (key==EV_CLOSE_ALL) {
            xmlLoaded.clear();
            xmlFileName="";
            if (standalone) System.exit(0); else if (frame!=null) frame.setVisible(false);
        }

        if (key==EV_THEME) {
            String defaultTheme=System.getProperty("alloy.theme0");
            thememenu.removeAll();
            OurUtil.makeMenuItem(thememenu, "Load Theme...",                  true, KeyEvent.VK_L, -1, this, EV_LOAD_THEME);
            if (defaultTheme!=null && defaultTheme.length()>0 && (new File(defaultTheme)).isDirectory())
               OurUtil.makeMenuItem(thememenu, "Load Sample Theme...",        true, KeyEvent.VK_B, -1, this, EV_LOAD_SAMPLE_THEME);
            OurUtil.makeMenuItem(thememenu, "Save Theme",                     true, KeyEvent.VK_S, -1, this, EV_SAVE_THEME);
            OurUtil.makeMenuItem(thememenu, "Save Theme As...",               true, KeyEvent.VK_A, -1, this, EV_SAME_THEME_AS);
            OurUtil.makeMenuItem(thememenu, "Reset Theme",                    true, KeyEvent.VK_R, -1, this, EV_RESET_THEME);
        }

        if (key==EV_LOAD_THEME || key==EV_LOAD_SAMPLE_THEME) {
            String defaultTheme=System.getProperty("alloy.theme0");
            if (defaultTheme==null) defaultTheme="";
            if (myState==null) return false; // Can only load if there is a VizState loaded
            if (myState.changedSinceLastSave()) {
                Boolean opt = OurDialog.askSaveDiscardCancel(frame, "The current theme");
                if (opt==null) return false;
                if (opt.booleanValue() && !run(EV_SAVE_THEME)) return false;
            }
            File file=OurDialog.askFile(frame, true, (key==EV_LOAD_THEME ? null : defaultTheme), ".thm", ".thm theme files");
            if (file==null) return false;
            if (key==EV_LOAD_THEME) Util.setCurrentDirectory(file.getParentFile());
            return run(EVS_LOAD_THEME, file.getPath());
        }

        if (key==EV_CLEAR_THEME_HISTORY) {
            Theme0.set(""); Theme1.set(""); Theme2.set(""); Theme3.set("");
        }

        if (key==EV_SAVE_THEME) {
            if (thmFileName.length()==0) return run(EV_SAME_THEME_AS); else return run(EVS_SAVE_THEME, thmFileName);
        }

        if (key==EV_SAME_THEME_AS) {
            File file=OurDialog.askFile(frame, false, null, ".thm", ".thm theme files");
            if (file==null) return false;
            if (file.exists()) if (!OurDialog.askOverwrite(frame, Util.canon(file.getPath()))) return false;
            Util.setCurrentDirectory(file.getParentFile());
            return run(EVS_SAVE_THEME, file.getPath());
        }

        if (key==EV_SAVE_THEME_AS_TS) {
            File file=OurDialog.askFile(frame, false, null, ".tab", ".tab tab-delimited theme files");
            if (file==null) return false;
            if (file.exists()) if (!OurDialog.askOverwrite(frame, Util.canon(file.getPath()))) return false;
            Util.setCurrentDirectory(file.getParentFile());
            return run(EVS_SAVE_THEME_AS_TS, file.getPath());
        }

        if (key==EV_RESET_THEME || key==EV_BAR_MAGIC_LAYOUT) {
            if (myState==null) return false;
            if (key==EV_RESET_THEME) {
                if (!OurDialog.yesno(frame, "Are you sure you wish to clear all your customizations?", "Yes, clear them", "No, keep them")) return false;
            } else {
                if (!OurDialog.yesno(frame, "This will clear your original customizations. Are you sure?", "Yes, clear them", "No, keep them")) return false;
            }
            myState.resetTheme();
            if (key==EV_BAR_MAGIC_LAYOUT) {
                try { MagicLayout.magic(myState);  MagicColour.magic(myState); } catch(Throwable ex) { }
            }
            repopulateProjectionPopup();
            if (myCustomPanel!=null) myCustomPanel.remakeAll();
            if (myGraphPanel!=null) myGraphPanel.remakeAll();
            if (key==EV_RESET_THEME) thmFileName="";
            updateDisplay();
        }

        if (key==EV_BAR_MAGIC_COLOR) {
            try { MagicColour.magic(myState); } catch(Throwable ex) { }
            // same as above for ev_magicLayout
            repopulateProjectionPopup();
            if (myCustomPanel!=null) myCustomPanel.remakeAll();
            if (myGraphPanel!=null) myGraphPanel.remakeAll();
            updateDisplay();
        }

        if (key==EV_WINDOW) {
            windowmenu.removeAll();
            for(final String f:getInstances()) {
                JMenuItem it = OurUtil.makeMenuItem("Instance: "+getInstanceTitle(f), null);
                it.setIcon(f.equals(getXMLfilename())?iconYes:iconNo);
                it.addActionListener(new MultiRunner(this, EVS_LOAD_INSTANCE, f));
                windowmenu.add(it);
            }
        }

        if (key==EV_MINIMIZE && frame!=null) {
            frame.setExtendedState(JFrame.ICONIFIED);
        }

        if (key==EV_SHOW && frame!=null) {
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)!=JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            else
                frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
            frame.setVisible(true);
            frame.requestFocus();
            frame.toFront();
        }

        if (key==EV_MAXIMIZE && frame!=null) {
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)==JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            else
                frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        }

        if (key==EV_BAR_NEXT && settingsOpen==0) {
            if (xmlFileName.length()==0) {
                OurDialog.alert(frame,"Cannot display the next solution since "
                +"no instance is currently loaded.", "Error");
            } else if (enumerator==null) {
                OurDialog.alert(frame,"Cannot display the next solution since "
                +"the analysis engine is not loaded with the visualizer.", "Error");
            } else {
                try {
                  enumerator.run(-1, xmlFileName);
                } catch(Throwable ex) {
                  OurDialog.alert(frame, ex.getMessage(), "Error");
                }
            }
        }

        if (key==EV_BAR_OPEN_THEME) { settingsOpen=1; updateDisplay(); }
        if (key==EV_BAR_CLOSE_THEME) { settingsOpen=0; updateDisplay(); }
        if (key==EV_BAR_APPLY) { updateDisplay(); }
        if (key==EV_BAR_OPEN_EVAL) { settingsOpen=2; updateDisplay(); }
        if (key==EV_BAR_CLOSE_EVAL) { settingsOpen=0; updateDisplay(); }
        if (key==EV_BAR_VIZ) { currentMode=VisualizerMode.Viz; updateDisplay(); }
        if (key==EV_BAR_XML) { currentMode=VisualizerMode.XML; updateDisplay(); }
        if (key==EV_BAR_TREE) { currentMode=VisualizerMode.Tree; updateDisplay(); }
        if (key==EV_BAR_DOT) { currentMode=VisualizerMode.DOT; updateDisplay(); }
        if (key==EV_BAR_KODKOD_IN) { currentMode=VisualizerMode.KInput; updateDisplay(); }
        if (key==EV_BAR_KODKOD_OUT) { currentMode=VisualizerMode.KOutput; updateDisplay(); }
        if (key==EV_BAR_PLUGIN0) { currentMode=VisualizerMode.Plugin0; updateDisplay(); }
        return true;
    }

    private boolean runPlugin(VizState s) {
        try {
            String cname = System.getProperty("alloy.viz.plugin0");
            if (cname==null || cname.length()==0) return false;
            Class<?> c = Class.forName(cname);
            Method m = c.getMethod("plugin", new Class[]{VizState.class});
            if (s==null) return true;
            m.invoke(null, new Object[]{s});
            return true;
        } catch(Throwable ex) {
            return false;
        }
    }
}
