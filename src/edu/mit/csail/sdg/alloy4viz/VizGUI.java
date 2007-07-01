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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
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
import javax.swing.text.JTextComponent;
import edu.mit.csail.sdg.alloy4.Computer;
import edu.mit.csail.sdg.alloy4.MultiRunner;
import edu.mit.csail.sdg.alloy4.OurBinaryCheckbox;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;
import edu.mit.csail.sdg.alloy4.Util.IntPref;
import edu.mit.csail.sdg.alloy4.Util.StringPref;

/**
 * GUI main window for the visualizer.
 *
 * <p><b>Thread Safety:</b>  Can be called only by the AWT event thread.
 */

public final class VizGUI implements MultiRunnable, ComponentListener {

    /** Simple test driver method. */
    public static void main(String[] args) {
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
        Factory f=new Factory(true);
        for(String a:args) {f.open(a);}
    }

    /**
     * Since the entire VizGUI class can be called only by the AWT event thread, it is inconvenient to use;
     * thus, we created a wrapper class "Factory" for the most common operations: open, close, and closeAll.
     *
     * <p><b>Thread Safety:</b> Safe.
     */
    public static final class Factory implements Runnable {
        private final boolean standalone;
        private VizGUI window=null;
        /** Make a new factory; standalone indicates whether the JVM should shutdown when the last window closes. */
        public Factory(final boolean standalone) {
            this.standalone=standalone;
            SwingUtilities.invokeLater(this);
        }
        /** Start up the GUI, if it's not started up already. */
        public void run() {
            if (SwingUtilities.isEventDispatchThread()) {
                if (window==null) window=new VizGUI(standalone,"",null,null,null);
                window.run(ev_show);
            }
            else SwingUtilities.invokeLater(Factory.this);
        }
        /**
         * Load the given XML file.
         * NOTE: The method may return BEFORE the instance is loaded...
         */
        public void open(final String xmlFileName) {
            if (SwingUtilities.isEventDispatchThread()) window.run(evs_loadInstance, xmlFileName);
            else SwingUtilities.invokeLater(new MultiRunner(window, evs_loadInstance, xmlFileName));
        }
        /**
         * If the visualizer window is currently showing an instance, then close that instance.
         * Shuts down the JVM if standalone==true and there are no more instances to show.
         * NOTE: The method may return BEFORE the instances are closed...
         */
        public void close() {
            if (SwingUtilities.isEventDispatchThread()) window.run(ev_close);
            else SwingUtilities.invokeLater(new MultiRunner(window, ev_close));
        }
        /**
         * If standalone==true, shutdown the JVM; otherwise, close every instance then hide the visualizer window.
         * NOTE: The method may return BEFORE the instances are closed...
         */
        public void closeAll() {
            if (standalone) System.exit(0);
            if (SwingUtilities.isEventDispatchThread()) window.run(ev_closeAll);
            else SwingUtilities.invokeLater(new MultiRunner(window, ev_closeAll));
        }
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

    /** The JFrame for the main GUI window. */
    private final JFrame frame;

    /** The toolbar. */
    private final JToolBar toolbar;

    /** The projection popup menu. */
    private final JPopupMenu projectionPopup;

    private final JButton projectionButton, openSettingsButton, closeSettingsButton;
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
    private JTextComponent myEvaluatorPanel=null;

    /** The graphical panel to the right; null if it is not yet loaded. */
    private VizGraphPanel myGraphPanel=null;

    /** The splitpane between the customization panel and the graph panel. */
    private final JSplitPane splitpane;

    /** The last known divider position between the customization panel and the graph panel. */
    private int lastDividerPosition=0;

    /** The current theme file; "" if there is no theme file loaded. */
    private String thmFileName="";

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

    /**
     * The default directory for AlloyAnalyzer and AlloyVisualizer's "open"/"save" command.
     * NOTE: we intentionally use the same key "Dir" as the other Alloy4 components to make sure they use the same current directory.
     */
    private static final StringPref Dir = new StringPref("Dir");

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
    public VizGUI(boolean standalone, String xmlFileName, JMenu windowmenu) { this(standalone,xmlFileName,windowmenu,null,null); }

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

        this.enumerator=enumerator;
        this.standalone=standalone;
        this.evaluator=evaluator;
        this.frame=new JFrame("Alloy Visualizer");

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
        OurUtil.makeMenuItem(fileMenu, "Open...", true, KeyEvent.VK_O, KeyEvent.VK_O, this, ev_loadInstance);
        OurUtil.makeMenuItem(fileMenu, "Close", true, KeyEvent.VK_W, KeyEvent.VK_W, this, ev_close);
        if (!standalone)
            OurUtil.makeMenuItem(fileMenu, "Close All", true, KeyEvent.VK_A, -1, this, ev_closeAll);
        else
            OurUtil.makeMenuItem(fileMenu, "Quit", true, KeyEvent.VK_Q, KeyEvent.VK_Q, this, ev_closeAll);
        JMenu instanceMenu = OurUtil.makeMenu(mb, "Instance", KeyEvent.VK_I, null, 0);
        enumerateMenu = OurUtil.makeMenuItem(instanceMenu, "Show Next Solution", true, KeyEvent.VK_N, KeyEvent.VK_N, this, ev_toolbarEnumerate);
        thememenu = OurUtil.makeMenu(mb, "Theme", KeyEvent.VK_T, this, ev_theme);
        if (standalone || windowmenu==null)
            this.windowmenu=(windowmenu=OurUtil.makeMenu(mb, "Window", KeyEvent.VK_W, this, ev_window));
        else
            this.windowmenu=windowmenu;
        mb.add(windowmenu);
        thememenu.setEnabled(false);
        windowmenu.setEnabled(false);
        frame.setJMenuBar(mb);

        // Create the toolbar
        toolbar = new JToolBar();
        toolbar.setVisible(false);
        toolbar.setFloatable(false);
        toolbar.setBorder(new EmptyBorder(0,0,0,0));
        if (!Util.onMac()) toolbar.setBackground(background);
        vizButton=makeSolutionButton("Viz",
                "Show Visualization", "images/24_graph.gif", ev_toolbarViz);
        dotButton=makeSolutionButton("Dot",
                "Show the Dot File for the Graph", "images/24_plaintext.gif", ev_toolbarDot);
        xmlButton=makeSolutionButton("XML",
                "Show XML", "images/24_plaintext.gif", ev_toolbarXML);
        treeButton=makeSolutionButton("Tree",
                "Show Tree", "images/24_texttree.gif", ev_toolbarTree);
        kodSrcButton=makeSolutionButton("KK In",
                "Show KodKod Input", "images/24_plaintext.gif", ev_toolbarKodkodIn);
        kodInstButton=makeSolutionButton("KK Out",
                "Show KodKod Instance", "images/24_plaintext.gif", ev_toolbarKodkodOut);
        plugin0Button=makeSolutionButton("Plugin",
                "Run external plugin", "images/24_graph.gif", ev_toolbarPlugin0);
        addDivider();
        toolbar.add(openSettingsButton=OurUtil.button("Theme",
                "Open the theme customization panel", "images/24_settings.gif", this, ev_toolbarOpenTheme));
        toolbar.add(closeSettingsButton=OurUtil.button("Close Theme",
                "Close the theme customization panel", "images/24_settings_close2.gif", this, ev_toolbarCloseTheme));
        toolbar.add(updateSettingsButton=OurUtil.button("Apply",
                "Apply the changes to the current theme", "images/24_settings_apply2.gif", this, ev_toolbarApplyTheme));
        toolbar.add(magicLayout=OurUtil.button("Magic Layout",
                "Automatic theme customization (will reset current theme)", "images/24_settings_apply2.gif", this, ev_magicLayout));
        toolbar.add(magicColour=OurUtil.button("Magic Colour",
                "Automatic theme colour and shape customization", "images/24_settings_apply2.gif", this, ev_magicColour));
        toolbar.add(openEvaluatorButton=OurUtil.button("Evaluator",
                "Open the evaluator", "images/24_settings.gif", this, ev_toolbarOpenEvaluator));
        toolbar.add(closeEvaluatorButton=OurUtil.button("Close Evaluator",
                "Close the evaluator", "images/24_settings_close2.gif", this, ev_toolbarCloseEvaluator));
        toolbar.add(enumerateButton=OurUtil.button("Next",
                "Show the next solution", "images/24_history.gif", this, ev_toolbarEnumerate));
        projectionPopup = new JPopupMenu();
        projectionButton = new JButton("Projection: none");
        projectionButton.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                repopulateProjectionPopup();
                projectionPopup.show(projectionButton, 10, 10);
            }
        });
        repopulateProjectionPopup();
        toolbar.add(projectionButton);
        settingsOpen=0;

        // Create the horizontal split pane
        splitpane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitpane.setResizeWeight(0.);
        splitpane.setContinuousLayout(true);
        splitpane.setBorder(null);

        // Display the window, then proceed to load the input file
        frame.pack();
        frame.setSize(width,height);
        frame.setLocation(x,y);
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new MultiRunner(this, ev_close));
        frame.addComponentListener(this);
        if (xmlFileName.length()>0) run(evs_loadInstance, xmlFileName);
    }

    /** Invoked when the Visualizationwindow is resized. */
    public void componentResized(ComponentEvent e) {
        VizWidth.set(frame.getWidth()); VizHeight.set(frame.getHeight()); componentMoved(e);
    }

    /** Invoked when the Visualizationwindow is moved. */
    public void componentMoved(ComponentEvent e) {
        Point p=frame.getLocation(); VizX.set(p.x); VizY.set(p.y);
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
        final boolean isMeta = (myState!=null && myState.getOriginalInstance().isMetamodel);
        magicLayout.setVisible(!isMeta && settingsOpen==0 && currentMode==VisualizerMode.Viz);
        magicColour.setVisible(magicLayout.isVisible());
        projectionButton.setVisible(settingsOpen==0 && currentMode==VisualizerMode.Viz);
        openSettingsButton.setVisible(settingsOpen==0 && currentMode==VisualizerMode.Viz);
        closeSettingsButton.setVisible(settingsOpen==1);
        updateSettingsButton.setVisible(settingsOpen==1);
        openEvaluatorButton.setVisible(settingsOpen==0 && evaluator!=null);
        closeEvaluatorButton.setVisible(settingsOpen==2 && evaluator!=null);
        enumerateMenu.setEnabled(!isMeta && settingsOpen==0 && enumerator!=null);
        enumerateButton.setVisible(!isMeta && settingsOpen==0 && enumerator!=null);
        toolbar.setVisible(true);
        // Now, generate the graph or tree or textarea that we want to display on the right
        JComponent content;
        frame.setTitle(makeVizTitle());
        switch (currentMode) {
           case XML: content=getTextComponent(xmlFileName); break;
           case Plugin0: runPlugin(myState); // Intentional Fall Thru
           case Tree: content=StaticTreeMaker.makeTree(myState.getOriginalInstance(), makeVizTitle(), myState); break;
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
        if (settingsOpen>0) {
            JComponent left;
            if (settingsOpen==1) {
                if (myCustomPanel==null) myCustomPanel=new VizCustomizationPanel(splitpane,myState); else
                   myCustomPanel.remakeAll();
                left=myCustomPanel;
            } else {
                if (myEvaluatorPanel==null) {
                    myEvaluatorPanel=OurDialog.showConsole(null,
                    "The <b>Alloy Evaluator</b> allows you to type<br>"+
                    "in Alloy expressions and see their values.<br>", evaluator);
                }
                evaluator.setSourceFile(xmlFileName);
                left = new JScrollPane(myEvaluatorPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                myEvaluatorPanel.setCaretPosition(0);
                myEvaluatorPanel.setCaretPosition(myEvaluatorPanel.getDocument().getLength());
            }
            if (frame.getContentPane()==splitpane) lastDividerPosition=splitpane.getDividerLocation();
            splitpane.setRightComponent(instanceArea);
            splitpane.setLeftComponent(left);
            Dimension dim=left.getPreferredSize();
            if (lastDividerPosition<50) lastDividerPosition=frame.getWidth()/2;
            if (lastDividerPosition<dim.width) lastDividerPosition=dim.width;
            if (settingsOpen==2 && lastDividerPosition>400) lastDividerPosition=400;
            splitpane.setDividerLocation(lastDividerPosition);
            frame.setContentPane(splitpane);
        } else {
            frame.setContentPane(instanceArea);
        }
        if (settingsOpen!=2) {
            content.requestFocusInWindow();
        } else {
            myEvaluatorPanel.requestFocusInWindow();
            myEvaluatorPanel.setCaretPosition(myEvaluatorPanel.getDocument().getLength());
        }
        repopulateProjectionPopup();
        frame.validate();
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

    //========================================= EVENTS ============================================================================================
    // The return values of events are undefined, except these five: { ev_saveTheme evs_saveTheme evs_saveThemeTS ev_saveThemeAs ev_saveThemeAsTS }
    //=============================================================================================================================================

    /** This event asks the user for a new XML file to load. */
    private static final int ev_loadInstance = 101;

    /** This event loads a new XML instance file if it's not the current file. */
    public static final int evs_loadInstance = 102;

    /** This event loads a new XML instance file (reloading it from disk even if the filename equals the current filename) */
    public static final int evs_loadInstanceForcefully = 103;

    /**
     * This event closes the current XML instance; if there are previously loaded files, we will load one of them;
     * otherwise, this window will set itself as invisible (if not in standalone mode),
     * or it will terminate the entire application (if in standalone mode).
     */
    private static final int ev_close = 104;

    /**
     * This event closes every XML file.
     * if in standalone mode, the JVM will then shutdown; otherwise it will just set the window invisible.
     */
    private static final int ev_closeAll = 105;

    /** This event refreshes the "theme" menu. */
    private static final int ev_theme = 201;

    /** This events asks the user for a new theme file to load. */
    private static final int ev_loadTheme = 202;

    /** This events asks the user for a new theme file to load. */
    private static final int ev_loadProvidedTheme = 203;

    /** This events loads a specific theme file. */
    private static final int evs_loadTheme = 204;

    /** This events clears the history of loaded themes. */
    private static final int ev_clearThemeHistory = 205;

    /** This event saves the current theme; returns true if it succeeded. */
    private static final int ev_saveTheme = 206;

    /** This event saves a specific current theme; returns true if it succeeded. */
    private static final int evs_saveTheme = 207;

    /** This event saves the current theme to a new ".thm" file; returns true if it succeeded. */
    private static final int ev_saveThemeAs = 208;

    /** This event resets the current theme. */
    private static final int ev_resetTheme = 209;

    /** This event saves a specific current theme; returns true if it succeeded. */
    private static final int evs_saveThemeTS = 210;

    /** This event saves the current theme to a new ".thm" file; returns true if it succeeded. */
    private static final int ev_saveThemeAsTS = 211;

    /** This event refreshes the window menu. */
    private static final int ev_window = 301;

    /** This event shows the window and bring it to the front (if not already). */
    public static final int ev_show = 302;

    /** This event minimizes the window. */
    public static final int ev_minimize = 303;

    /** This event alternatingly maximizes or restores the window. */
    public static final int ev_maximize = 304;

    /** This event brings up an alert message. */
    public static final int evs_alert = 305;

    private static final int ev_toolbarOpenTheme = 1001;
    private static final int ev_toolbarCloseTheme = 1002;
    private static final int ev_toolbarApplyTheme = 1003;
    private static final int ev_toolbarOpenEvaluator = 1004;
    private static final int ev_toolbarCloseEvaluator = 1005;
    private static final int ev_toolbarEnumerate = 1006;

    private static final int ev_toolbarViz = 2001;
    private static final int ev_toolbarXML = 2002;
    private static final int ev_toolbarTree = 2003;
    private static final int ev_toolbarKodkodIn = 2004;
    private static final int ev_toolbarKodkodOut = 2005;
    private static final int ev_toolbarDot = 2006;
    private static final int ev_toolbarPlugin0 = 2007;
    private static final int ev_magicLayout=2008;
    private static final int ev_magicColour=2009;

    /** Performs the function given by "key" on the argument "arg"; returns true if it succeeds. */
    public boolean run(final int key, final int arg) {
        return true;
    }

    /** Performs the function given by "key" on the argument "arg"; returns true if it succeeds. */
    public boolean run(final int key, final String arg) {
        if (key==evs_alert) {
            OurDialog.alert(frame, arg, "Alloy 4");
        }
        if (key==evs_loadInstance || key==evs_loadInstanceForcefully) {
            String xmlFileName=Util.canon(arg);
            File f=new File(xmlFileName);
            if (key==evs_loadInstanceForcefully || !xmlFileName.equals(this.xmlFileName)) {
                AlloyInstance myInstance;
                try {
                    myInstance = StaticInstanceReader.parseInstance(f);
                } catch (Throwable e) {
                    xmlLoaded.remove(arg);
                    xmlLoaded.remove(xmlFileName);
                    JOptionPane.showMessageDialog(null, "File is not of valid Alloy XML format: "
                       +e.getMessage()+"\n\nFile: "+xmlFileName,
                       "Error", JOptionPane.ERROR_MESSAGE);
                    if (xmlLoaded.size()>0) {run(evs_loadInstance, xmlLoaded.get(xmlLoaded.size()-1)); return true;}
                    run(ev_closeAll);
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
            frame.setVisible(true);
            frame.setTitle("Alloy Visualizer "+Version.version()+" loading... Please wait...");
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)!=JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            frame.requestFocus();
            frame.toFront();
            updateDisplay();
        }
        if (key==evs_loadTheme) {
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
        if (key==evs_saveTheme || key==evs_saveThemeTS) {
            if (myState==null) return false; // Can only save if there is a VizState loaded
            String filename=Util.canon(arg);
            try {
                if (key==evs_saveTheme) myState.savePaletteXML(filename); else myState.savePaletteTS(filename);
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

        if (key==ev_loadInstance) {
            File file=OurDialog.askFile(frame, true, Dir.get(), ".xml", ".xml instance files");
            if (file==null) return false;
            Dir.set(file.getParent());
            run(evs_loadInstanceForcefully, file.getPath());
        }

        if (key==ev_close) {
            xmlLoaded.remove(xmlFileName);
            if (xmlLoaded.size()>0) return run(evs_loadInstance, xmlLoaded.get(xmlLoaded.size()-1));
            if (standalone) System.exit(0); else frame.setVisible(false);
        }

        if (key==ev_closeAll) {
            xmlLoaded.clear();
            xmlFileName="";
            if (standalone) System.exit(0); else frame.setVisible(false);
        }

        if (key==ev_theme) {
            String defaultTheme=System.getProperty("alloy.theme0");
            thememenu.removeAll();
            OurUtil.makeMenuItem(thememenu, "Load Theme...",                  true, KeyEvent.VK_L, -1, this, ev_loadTheme);
            if (defaultTheme!=null && defaultTheme.length()>0 && (new File(defaultTheme)).isDirectory())
               OurUtil.makeMenuItem(thememenu, "Load Sample Theme...",        true, KeyEvent.VK_B, -1, this, ev_loadProvidedTheme);
            OurUtil.makeMenuItem(thememenu, "Save Theme",                     true, KeyEvent.VK_S, -1, this, ev_saveTheme);
            OurUtil.makeMenuItem(thememenu, "Save Theme As...",               true, KeyEvent.VK_A, -1, this, ev_saveThemeAs);
            OurUtil.makeMenuItem(thememenu, "Reset Theme",                    true, KeyEvent.VK_R, -1, this, ev_resetTheme);
        }

        if (key==ev_loadTheme || key==ev_loadProvidedTheme) {
            String defaultTheme=System.getProperty("alloy.theme0");
            if (defaultTheme==null) defaultTheme="";
            if (myState==null) return false; // Can only load if there is a VizState loaded
            if (myState.changedSinceLastSave()) {
                Boolean opt = OurDialog.askSaveDiscardCancel(frame, "The current theme");
                if (opt==null) return false;
                if (opt.booleanValue() && !run(ev_saveTheme)) return false;
            }
            File file=OurDialog.askFile(frame, true, (key==ev_loadTheme ? Dir.get() : defaultTheme), ".thm", ".thm theme files");
            if (file==null) return false;
            if (key==ev_loadTheme) Dir.set(file.getParent());
            return run(evs_loadTheme, file.getPath());
        }

        if (key==ev_clearThemeHistory) {
            Theme0.set(""); Theme1.set(""); Theme2.set(""); Theme3.set("");
        }

        if (key==ev_saveTheme) {
            if (thmFileName.length()==0) return run(ev_saveThemeAs); else return run(evs_saveTheme, thmFileName);
        }

        if (key==ev_saveThemeAs) {
            File file=OurDialog.askFile(frame, false, Dir.get(), ".thm", ".thm theme files");
            if (file==null) return false;
            if (file.exists()) if (!OurDialog.askOverwrite(frame, Util.canon(file.getPath()))) return false;
            Dir.set(file.getParent());
            return run(evs_saveTheme, file.getPath());
        }

        if (key==ev_saveThemeAsTS) {
            File file=OurDialog.askFile(frame, false, Dir.get(), ".tab", ".tab tab-delimited theme files");
            if (file==null) return false;
            if (file.exists()) if (!OurDialog.askOverwrite(frame, Util.canon(file.getPath()))) return false;
            Dir.set(file.getParent());
            return run(evs_saveThemeTS, file.getPath());
        }

        if (key==ev_resetTheme || key==ev_magicLayout) {
            if (myState==null) return false;
            if (key==ev_resetTheme) {
                if (!OurDialog.yesno(frame, "Are you sure you wish to clear all your customizations?", "Yes, clear them", "No, keep them")) return false;
            }
            myState.resetTheme();
            if (key==ev_magicLayout) {
                try { MagicLayout.magic(myState);  MagicColour.magic(myState); } catch(Throwable ex) { }
            }
            repopulateProjectionPopup();
            if (myCustomPanel!=null) myCustomPanel.remakeAll();
            if (myGraphPanel!=null) myGraphPanel.remakeAll();
            if (key==ev_resetTheme) thmFileName="";
            updateDisplay();
        }

        if (key==ev_magicColour) {
            try { MagicColour.magic(myState); } catch(Throwable ex) { }
            // same as above for ev_magicLayout
            repopulateProjectionPopup();
            if (myCustomPanel!=null) myCustomPanel.remakeAll();
            if (myGraphPanel!=null) myGraphPanel.remakeAll();
            updateDisplay();
        }
        
        if (key==ev_window) {
            windowmenu.removeAll();
            for(final String f:getInstances()) {
                JMenuItem it = OurUtil.makeMenuItem("Instance: "+getInstanceTitle(f), null);
                it.setIcon(f.equals(getXMLfilename())?iconYes:iconNo);
                it.addActionListener(new MultiRunner(this, evs_loadInstance, f));
                windowmenu.add(it);
            }
        }

        if (key==ev_minimize) {
            frame.setExtendedState(JFrame.ICONIFIED);
        }

        if (key==ev_show) {
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)!=JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            else
                frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
            frame.setVisible(true);
            frame.requestFocus();
            frame.toFront();
        }

        if (key==ev_maximize) {
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)==JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            else
                frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        }

        if (key==ev_toolbarEnumerate && settingsOpen==0) {
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

        if (key==ev_toolbarOpenTheme) { settingsOpen=1; updateDisplay(); }
        if (key==ev_toolbarCloseTheme) { settingsOpen=0; updateDisplay(); }
        if (key==ev_toolbarApplyTheme) { updateDisplay(); }
        if (key==ev_toolbarOpenEvaluator) { settingsOpen=2; updateDisplay(); }
        if (key==ev_toolbarCloseEvaluator) { settingsOpen=0; updateDisplay(); }
        if (key==ev_toolbarViz) { currentMode=VisualizerMode.Viz; updateDisplay(); }
        if (key==ev_toolbarXML) { currentMode=VisualizerMode.XML; updateDisplay(); }
        if (key==ev_toolbarTree) { currentMode=VisualizerMode.Tree; updateDisplay(); }
        if (key==ev_toolbarDot) { currentMode=VisualizerMode.DOT; updateDisplay(); }
        if (key==ev_toolbarKodkodIn) { currentMode=VisualizerMode.KInput; updateDisplay(); }
        if (key==ev_toolbarKodkodOut) { currentMode=VisualizerMode.KOutput; updateDisplay(); }
        if (key==ev_toolbarPlugin0) { currentMode=VisualizerMode.Plugin0; updateDisplay(); }
        return true;
    }

    private boolean runPlugin(VizState s) {
        try {
            String cname = System.getProperty("alloy.viz.plugin0");
            if (cname==null || cname.length()==0) return false;
            Class c = Class.forName(cname);
            Method m = c.getMethod("plugin", new Class[]{VizState.class});
            if (s==null) return true;
            m.invoke(null, new Object[]{s});
            return true;
        } catch(Throwable ex) {
            return false;
        }
    }
}
