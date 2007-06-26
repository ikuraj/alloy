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

package edu.mit.csail.sdg.alloy4whole;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;
import java.util.prefs.Preferences;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.BadLocationException;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4compiler.parser.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options.SatSolver;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4.Computer;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.MailBug;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.MacUtil;
import edu.mit.csail.sdg.alloy4.MultiRunner;
import edu.mit.csail.sdg.alloy4.OurBorder;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.OurTabbedEditor;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Subprocess;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.Version;
import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;
import edu.mit.csail.sdg.alloy4.Util.BooleanPref;
import edu.mit.csail.sdg.alloy4.Util.IntPref;
import edu.mit.csail.sdg.alloy4.Util.StringPref;
import edu.mit.csail.sdg.alloy4viz.VizGUI;

/**
 * Simple graphical interface for accessing various features of the analyzer.
 *
 * <p> Except noted below, methods in this class can only be called by the AWT event thread.
 *
 * <p> The methods that might get called from other threads are:
 * <br> (1) the run() method in SatRunner is launched from a fresh thread
 * <br> (2) the run() method in the instance watcher (in constructor) is launched from a fresh thread
 */

public final class SimpleGUI implements MultiRunnable, ComponentListener, OurTabbedEditor.Parent {

    /** The latest welcome screen; each time we update the welcome screen, we increment this number. */
    private static final int welcomeLevel = 1;

    // Verify that the graphics environment is set up
    static {
        try {
            GraphicsEnvironment.getLocalGraphicsEnvironment();
        } catch(Throwable ex) {
            System.err.println("Unable to start the graphical environment.");
            System.err.println("If you're on Mac OS X:");
            System.err.println("   Please make sure you are running as the current local user.");
            System.err.println("If you're on Linux or FreeBSD:");
            System.err.println("   Please make sure your X Windows is configured.");
            System.err.println("   You can verify this by typing \"xhost\"; it should not give an error message.");
            System.err.flush();
            System.exit(1);
        }
    }

    //======== The Preferences ======================================================================================//
    //======== Note: you must make sure each preference has a unique key ============================================//

    /** True if Alloy Analyzer should let warning be nonfatal. */
    private static final BooleanPref WarningNonfatal = new BooleanPref("WarningNonfatal");

    /** True if Alloy Analyzer should use an external text editor rather than the builtin editor. */
    private static final BooleanPref ExternalEditor = new BooleanPref("ExternalEditor");

    /** True if Alloy Analyzer should automatically visualize the latest instance. */
    private static final BooleanPref AutoVisualize = new BooleanPref("AutoVisualize");

    /** True if Alloy Analyzer should record the raw Kodkod input and output. */
    private static final BooleanPref RecordKodkod = new BooleanPref("RecordKodkod");

    /** The latest welcome screen that the user has seen. */
    private static final IntPref Welcome = new IntPref("Welcome",0,0,1000);

    /** The latest X corrdinate of the Alloy Analyzer's main window. */
    private static final IntPref AnalyzerX = new IntPref("AnalyzerX",0,-1,65535);

    /** The latest Y corrdinate of the Alloy Analyzer's main window. */
    private static final IntPref AnalyzerY = new IntPref("AnalyzerY",0,-1,65535);

    /** The latest width of the Alloy Analyzer's main window. */
    private static final IntPref AnalyzerWidth = new IntPref("AnalyzerWidth",0,-1,65535);

    /** The latest height of the Alloy Analyzer's main window. */
    private static final IntPref AnalyzerHeight = new IntPref("AnalyzerHeight",0,-1,65535);

    /** The latest font size of the Alloy Analyzer. */
    private static final IntPref FontSize = new IntPref("FontSize",9,12,24);

    /** The latest font name of the Alloy Analyzer. */
    private static final StringPref FontName = new StringPref("FontName","Lucida Grande");

    /** The latest tab distance of the Alloy Analyzer. */
    private static final IntPref TabSize = new IntPref("TabSize",1,2,16);

    /** The skolem depth. */
    private static final IntPref SkolemDepth = new IntPref("SkolemDepth",0,0,2);

    /** The amount of memory (in M) to allocate for Kodkod and the SAT solvers. */
    private static final IntPref SubMemory = new IntPref("SubMemory",16,768,65535);

    /**
     * The default directory for AlloyAnalyzer and AlloyVisualizer's "open"/"save" command.
     * NOTE: we intentionally use the same key "Dir" as the other Alloy4 components to make sure they use the same current directory.
     */
    private static final StringPref Dir = new StringPref("Dir");

    /** The first file in Alloy Analyzer's "open recent" list. */
    private static final StringPref Model0 = new StringPref("Model0");

    /** The second file in Alloy Analyzer's "open recent" list. */
    private static final StringPref Model1 = new StringPref("Model1");

    /** The third file in Alloy Analyzer's "open recent" list. */
    private static final StringPref Model2 = new StringPref("Model2");

    /** The fourth file in Alloy Analyzer's "open recent" list. */
    private static final StringPref Model3 = new StringPref("Model3");

    /** This enum defines the set of possible message verbosity levels. */
    private enum Verbosity {
        /** Level 0. */  DEFAULT("0", "low"),
        /** Level 1. */  VERBOSE("1", "medium"),
        /** Level 2. */  DEBUG("2", "high"),
        /** Level 3. */  FULLDEBUG("3", "debug only");
        /** Returns true if it is greater than or equal to "other". */
        public boolean geq(Verbosity other) { return ordinal() >= other.ordinal(); }
        /** This is a unique String for this value; it should be kept consistent in future versions. */
        private final String id;
        /** This is the label that the toString() method will return. */
        private final String label;
        /** Constructs a new Verbosity value with the given id and label. */
        private Verbosity(String id, String label) { this.id=id; this.label=label; }
        /** Given an id, return the enum value corresponding to it (if there's no match, then return DEFAULT). */
        private static Verbosity parse(String id) {
            for(Verbosity vb:values()) if (vb.id.equals(id)) return vb;
            return DEFAULT;
        }
        /** Returns the human-readable label for this enum value. */
        @Override public final String toString() { return label; }
        /** Saves this value into the Java preference object. */
        private void set() { Preferences.userNodeForPackage(Util.class).put("Verbosity",id); }
        /** Reads the current value of the Java preference object (if it's not set, then return DEFAULT). */
        private static Verbosity get() { return parse(Preferences.userNodeForPackage(Util.class).get("Verbosity","")); }
    };

    //===================================================================================================//

    /** The uncaught exception handler. */
    private final MailBug exitReporter;

    /** The JFrame for the main window. */
    private final JFrame frame;

    /** The JFrame for the visualizer window. */
    private final VizGUI viz;

    /** The "File", "Edit", "Run", "Option", "Window", and "Help" menus. */
    private final JMenu filemenu, editmenu, runmenu, optmenu, windowmenu, windowmenu2, helpmenu;

    /** The toolbar. */
    private final JToolBar toolbar;

    /** The various toolbar buttons. */
    private final JButton newbutton, openbutton, loadbutton, savebutton, runbutton, stopbutton, showbutton;

    /** The Splitpane. */
    private final JSplitPane splitpane;

    /** The JLabel that displays the current line/column position, etc. */
    private final JLabel status;

    /** Whether the editor has the focus, or the log window has the focus. */
    private boolean lastFocusIsOnEditor = true;

    /** Sets the flag "lastFocusIsOnEditor" to be true. */
    public void notifyFocusGained() { lastFocusIsOnEditor=true; }

    /** The text editor. */
    private final OurTabbedEditor text;

    /** The "message panel" on the right. */
    private final SwingLogPanel log;

    /** The scrollpane containing the "message panel". */
    private final JScrollPane logpane;

    /** The last "find" that the user issued. */
    private String lastFind = "";

    /** The last find is case-sensitive or not. */
    private boolean lastFindCaseSensitive = true;

    /** The last find is forward or not. */
    private boolean lastFindForward = true;

    /** The icon for a "checked" menu item. */
    private static final Icon iconYes = OurUtil.loadIcon("images/menu1.gif");

    /** The icon for an "unchecked" menu item. */
    private static final Icon iconNo = OurUtil.loadIcon("images/menu0.gif");

    /** The system-specific file separator (forward-slash on UNIX, back-slash on Windows, etc.) */
    private static final String fs = System.getProperty("file.separator");

    /** The darker background color (for the MessageLog window and the Toolbar and the Status Bar, etc.) */
    private static final Color background = new Color(0.9f, 0.9f, 0.9f);

    /** Whether we should force ".als" when opening a file. */
    private boolean openAlsOnly = true;

    /** If true, that means we are currently generating a metamodel or running a SAT solve. */
    private boolean subrunning = false;

    /** If subrunning==true: 0 means SAT solving; 1 means metamodel; 2 means enumeration. */
    private int subrunningTask = 0;

    /** If nonnull and alive, that means it's the subprocess for running SAT solvers. */
    private Process subprocess = null;

    /** The amount of memory (in MB) currently allocated for this.subprocess */
    private int subMemoryNow = 0;

    /** The amount of memory (in MB) to allocate for this.subprocess */
    private int subMemory = SubMemory.get();

    /** The list of commands (this field will be cleared to null when the text buffer is edited) */
    private List<Command> commands = null;

    /** The latest executed command. */
    private int latestCommand = 0;

    /** The current choices of SAT solver. */
    private final List<SatSolver> satChoices;

    /** Whether the system is using external editor or not. */
    private boolean mode_externalEditor = ExternalEditor.get();

    /** Returns true if the system is using external editor or not. */
    boolean isUsingExternalEditor() { return mode_externalEditor; }

    /** Whether the system should automatically visualize the latest instance. */
    private boolean mode_autoVisualize = AutoVisualize.get();

    /** Whether warnings are nonfatal. */
    private boolean mode_warningNonFatal = WarningNonfatal.get();

    /** The most recent Alloy version (as queried from alloy.mit.edu); -1 if alloy.mit.edu has not replied yet. */
    private int latestAlloyVersion=(-1);

    /** The most recent Alloy version name (as queried from alloy.mit.edu); "unknown" if alloy.mit.edu has not replied yet. */
    private String latestAlloyVersionName="unknown";

    /** If it's not "", then it is the XML filename for the latest satisfying instance or the latest metamodel. */
    private String latestInstance = "";

    /** If it's not "", then it is the latest instance or metamodel during the most recent click of "Execute". */
    private String latestAutoInstance = "";

    //====== helper methods =================================================//

    /** Inserts "filename" into the "recently opened file list". */
    private void addHistory(String filename) {
        String name0=Model0.get(), name1=Model1.get(), name2=Model2.get();
        if (name0.equals(filename)) return; else {Model0.set(filename); Model1.set(name0);}
        if (name1.equals(filename)) return; else Model2.set(name1);
        if (name2.equals(filename)) return; else Model3.set(name2);
    }

    /** Updates the status bar at the bottom of the screen. */
    public void notifyChange() {
        commands=null;
        if (text==null) return; // If this was called prior to the "text" being fully initialized
        if (Util.onMac()) frame.getRootPane().putClientProperty("windowModified", Boolean.valueOf(text.modified()));
        if (text.isFile()) frame.setTitle(text.getFilename()); else frame.setTitle("Alloy Analyzer "+Version.version());
        if (mode_externalEditor) {
            toolbar.setBorder(new OurBorder(false,false,true,false));
            if (text.isFile())
                status.setText("<html><b>&nbsp; Current file:&nbsp; "+text.getFilename()+"</b></html>");
            else
                status.setText("<html><b>&nbsp; Current file:&nbsp; None</b></html>");
            return;
        }
        toolbar.setBorder(new OurBorder(false, false, text.getTabCount()<=1, false));
        try {
            int c=text.text().getCaretPosition();
            int y=text.text().getLineOfOffset(c)+1;
            int x=c-text.text().getLineStartOffset(y-1)+1;
            status.setText("<html>&nbsp; Line "+y+", Column "+x
                    +(text.modified()?" <b style=\"color:#B43333;\">[modified]</b></html>":"</html>"));
        } catch(BadLocationException ex) {
            status.setText("<html>&nbsp; Line ?, Column ?"
                    +(text.modified()?" <b style=\"color:#B43333;\">[modified]</b></html>":"</html>"));
        }
    }

    /** Make the frame visible, non-iconized, and focused. */
    private void bringup(JFrame frame) {
        frame.setVisible(true);
        if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)!=JFrame.MAXIMIZED_BOTH)
            frame.setExtendedState(JFrame.NORMAL);
        else
            frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        frame.requestFocus();
        frame.toFront();
    }

    /** Helper method that returns a hopefully very short name for a file name. */
    public static String slightlyShorterFilename(String name) {
        if (name.toLowerCase(Locale.US).endsWith(".als")) {
            int i=name.lastIndexOf('/');
            if (i>=0) name=name.substring(i+1);
            i=name.lastIndexOf('\\');
            if (i>=0) name=name.substring(i+1);
            return name.substring(0, name.length()-4);
        } else if (name.toLowerCase(Locale.US).endsWith(".xml")) {
            int i=name.lastIndexOf('/');
            if (i>0) i=name.lastIndexOf('/', i-1);
            if (i>=0) name=name.substring(i+1);
            i=name.lastIndexOf('\\');
            if (i>0) i=name.lastIndexOf('\\', i-1);
            if (i>=0) name=name.substring(i+1);
            return name.substring(0, name.length()-4);
        }
        return name;
    }

    /** Query the server to get the version number of the latest version of the Alloy Analyzer. */
    private static String checkForUpdate() {
        final String NEW_LINE = System.getProperty("line.separator");
        final String URL = "http://alloy.mit.edu/alloy4/download/alloy4.txt";
        BufferedReader in = null;
        try {
            URL url = new URL(URL);
            URLConnection connection = url.openConnection();
            connection.connect();
            in = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
            StringBuilder result = new StringBuilder();
            for (String inputLine = in.readLine(); inputLine != null; inputLine = in.readLine()) {
                result.append(inputLine);
                result.append(NEW_LINE);
            }
            return result.toString();
        } catch (Throwable ex) {
            return "";
        } finally {
            Util.close(in);
        }
    }

    private void copyFromJAR() {
        // Compute the appropriate platform
        String os = System.getProperty("os.name").toLowerCase(Locale.US).replace(' ','-');
        if (os.startsWith("mac-")) os="mac"; else if (os.startsWith("windows-")) os="windows";
        String arch = System.getProperty("os.arch").toLowerCase(Locale.US).replace(' ','-');
        if (arch.equals("powerpc")) arch="ppc-"+os; else arch=arch.replaceAll("\\Ai[3456]86\\z","x86")+"-"+os;
        // Find out the appropriate Alloy directory
        final String platformBinary=Helper.alloyHome()+fs+"binary";
        // Write a few test files
        try {
            (new File(platformBinary)).mkdirs();
            Util.writeAll(platformBinary+fs+"tmp.dot", "digraph \"test\" { \"A\" -> \"B\" }\n\n");
            Util.writeAll(platformBinary+fs+"tmp.cnf", "p cnf 3 1\n1 0\n");
        } catch(Err er) {
            // The error will be caught later by the "dot" test and "minisat" test
        }
        // Copy the platform-dependent binaries
        Util.copy(true, false, platformBinary,
           arch+"/libminisat.so", arch+"/libminisat.jnilib",
           arch+"/libminisatprover.so", arch+"/libminisatprover.jnilib",
           arch+"/libzchaff.so", arch+"/libzchaff.jnilib",
           arch+"/minisat1", arch+"/berkmin", arch+"/dotbin");
        Util.copy(false, false, platformBinary,
           arch+"/minisat.dll", arch+"/minisatprover.dll", arch+"/zchaff.dll",
           arch+"/minisat1.exe", arch+"/berkmin.exe",
           arch+"/dotbin.exe", arch+"/jpeg.dll", arch+"/libexpat.dll", arch+"/libexpatw.dll",
           arch+"/zlib1.dll", arch+"/z.dll", arch+"/freetype6.dll", arch+"/png.dll");
        // Copy the model files
        Util.copy(false, true, Helper.alloyHome(),
           "models/examples/algorithms/dijkstra.als",
           "models/examples/algorithms/djikstra.thm",
           "models/examples/algorithms/messaging.als",
           "models/examples/algorithms/messaging.thm",
           "models/examples/algorithms/opt_spantree.als",
           "models/examples/algorithms/opt_spantree.thm",
           "models/examples/algorithms/peterson.als",
           "models/examples/algorithms/ringlead.als",
           "models/examples/algorithms/ringlead.thm",
           "models/examples/algorithms/s_ringlead.als",
           "models/examples/algorithms/stable_mutex_ring.als",
           "models/examples/algorithms/stable_mutex_ring.thm",
           "models/examples/algorithms/stable_orient_ring.als",
           "models/examples/algorithms/stable_orient_ring.thm",
           "models/examples/algorithms/stable_ringlead.als",
           "models/examples/algorithms/stable_ringlead.thm",
           "models/examples/case_studies/INSLabel.als",
           "models/examples/case_studies/chord.als",
           "models/examples/case_studies/chord2.als",
           "models/examples/case_studies/chordbugmodel.als",
           "models/examples/case_studies/com.als",
           "models/examples/case_studies/firewire.als",
           "models/examples/case_studies/firewire.thm",
           "models/examples/case_studies/ins.als",
           "models/examples/case_studies/iolus.als",
           "models/examples/case_studies/sync.als",
           "models/examples/case_studies/syncimpl.als",
           "models/examples/puzzles/farmer.als",
           "models/examples/puzzles/farmer.thm",
           "models/examples/puzzles/handshake.als",
           "models/examples/puzzles/handshake.thm",
           "models/examples/puzzles/hanoi.als",
           "models/examples/puzzles/hanoi.thm",
           "models/examples/systems/file_system.als",
           "models/examples/systems/file_system.thm",
           "models/examples/systems/javatypes_soundness.als",
           "models/examples/systems/lists.als",
           "models/examples/systems/lists.thm",
           "models/examples/systems/marksweepgc.als",
           "models/examples/systems/views.als",
           "models/examples/toys/birthday.als",
           "models/examples/toys/birthday.thm",
           "models/examples/toys/ceilingsAndFloors.als",
           "models/examples/toys/ceilingsAndFloors.thm",
           "models/examples/toys/genealogy.als",
           "models/examples/toys/genealogy.thm",
           "models/examples/toys/grandpa.als",
           "models/examples/toys/grandpa.thm",
           "models/examples/toys/javatypes.als",
           "models/examples/toys/life.als",
           "models/examples/toys/numbering.als",
           "models/examples/toys/railway.als",
           "models/examples/toys/trivial.als",
           "models/examples/tutorial/farmer.als",
           "models/util/boolean.als", "models/util/graph.als", "models/util/integer.als",
           "models/util/natural.als", "models/util/ordering.als", "models/util/relation.als",
           "models/util/seqrel.als", "models/util/sequniv.als", "models/util/sequence.als",
           "models/util/ternary.als",
           "models/book/chapter2/addressBook1a.als", "models/book/chapter2/addressBook1b.als", "models/book/chapter2/addressBook1c.als",
           "models/book/chapter2/addressBook1d.als", "models/book/chapter2/addressBook1e.als", "models/book/chapter2/addressBook1f.als",
           "models/book/chapter2/addressBook1g.als", "models/book/chapter2/addressBook1h.als", "models/book/chapter2/addressBook2a.als",
           "models/book/chapter2/addressBook2b.als", "models/book/chapter2/addressBook2c.als", "models/book/chapter2/addressBook2d.als",
           "models/book/chapter2/addressBook2e.als", "models/book/chapter2/addressBook3a.als", "models/book/chapter2/addressBook3b.als",
           "models/book/chapter2/addressBook3c.als", "models/book/chapter2/addressBook3d.als", "models/book/chapter2/theme.thm",
           "models/book/chapter4/filesystem.als", "models/book/chapter4/grandpa1.als", "models/book/chapter4/grandpa2.als",
           "models/book/chapter4/grandpa3.als", "models/book/chapter4/lights.als",
           "models/book/chapter5/addressBook.als", "models/book/chapter5/lists.als",
           "models/book/chapter5/sets1.als", "models/book/chapter5/sets2.als",
           "models/book/chapter6/hotel.thm",
           "models/book/chapter6/hotel1.als", "models/book/chapter6/hotel2.als",
           "models/book/chapter6/hotel3.als", "models/book/chapter6/hotel4.als", "models/book/chapter6/mediaAssets.als",
           "models/book/chapter6/memory/abstractMemory.als", "models/book/chapter6/memory/cacheMemory.als",
           "models/book/chapter6/memory/checkCache.als", "models/book/chapter6/memory/checkFixedSize.als",
           "models/book/chapter6/memory/fixedSizeMemory.als", "models/book/chapter6/memory/fixedSizeMemory_H.als",
           "models/book/chapter6/ringElection.thm", "models/book/chapter6/ringElection1.als", "models/book/chapter6/ringElection2.als",
           "models/book/appendixA/addressBook1.als", "models/book/appendixA/addressBook2.als", "models/book/appendixA/barbers.als",
           "models/book/appendixA/closure.als", "models/book/appendixA/distribution.als", "models/book/appendixA/phones.als",
           "models/book/appendixA/prison.als", "models/book/appendixA/properties.als", "models/book/appendixA/ring.als",
           "models/book/appendixA/spanning.als", "models/book/appendixA/tree.als", "models/book/appendixA/tube.als",
           "models/book/appendixA/undirected.als",
           "models/book/appendixE/hotel.thm",
           "models/book/appendixE/p300-hotel.als", "models/book/appendixE/p303-hotel.als", "models/book/appendixE/p306-hotel.als"
           );
        // Record the locations
        System.setProperty("alloy.dotbin0", platformBinary+fs+"dotbin");
        System.setProperty("alloy.theme0", Helper.alloyHome()+fs+"models");
        System.setProperty("alloy.home", Helper.alloyHome());
    }

    /** Called when this window is resized. */
    public void componentResized(ComponentEvent e) {
        AnalyzerWidth.set(frame.getWidth());
        AnalyzerHeight.set(frame.getHeight());
        componentMoved(e);
    }

    /** Called when this window is moved. */
    public void componentMoved(ComponentEvent e) {
        Point p=frame.getLocation();
        AnalyzerX.set(p.x);
        AnalyzerY.set(p.y);
    }

    /** Called when this window is shown. */
    public void componentShown(ComponentEvent e) {}

    /** Called when this window is hidden. */
    public void componentHidden(ComponentEvent e) {}

    //====== Events =======================================================================//
    // The return values of events are undefined, except these two: { ev_save, ev_saveAs } //
    //=====================================================================================//

    /** This event clears the temporary files and then reinitialize the temporary directory. */
    private static final int ev_clearTemp = 101;

    /** This event displays a message that "updated version of Alloy is available" */
    private static final int evs_update = 102;

    /** This event refreshes the "file" menu. */
    private static final int ev_refreshFile = 103;

    /** This event occurs when the user clicks "file->new". */
    private static final int ev_new = 104;

    /** This event occurs when the user clicks "file->open". */
    private static final int ev_open = 105;

    /** This event occurs when the user clicks "file->OpenBuiltinModels". */
    private static final int ev_builtin = 106;

    /** This event occurs when the user clicks "file->save". */
    private static final int ev_save = 107;

    /** This event occurs when the user clicks "file->SaveAs". */
    private static final int ev_saveAs = 108;

    /** This event occurs when the user clicks "file->openRecent->clear" */
    private static final int ev_clearRecent = 109;

    /** This event occurs when the user clicks "file->close". */
    private static final int ev_close = 110;

    /** This event occurs when the user attempts to close the window via the OS. */
    private static final int ev_dispose = 111;

    /** This event occurs when the user clicks "file->quit". */
    private static final int ev_quit = 112;

    /** This event opens a particular file. */
    private static final int evs_open = 113;

    /** This event refreshes the "edit" menu. */
    private static final int ev_refreshEdit = 201;

    /** This event occurs when the user clicks "edit->undo". */
    private static final int ev_undo = 202;

    /** This event occurs when the user clicks "edit->redo". */
    private static final int ev_redo = 203;

    /** This event occurs when the user clicks "edit->copy". */
    private static final int ev_copy = 204;

    /** This event occurs when the user clicks "edit->cut". */
    private static final int ev_cut = 205;

    /** This event occurs when the user clicks "edit->paste". */
    private static final int ev_paste = 206;

    /** This event occurs when the user clicks "edit->find". */
    private static final int ev_find = 207;

    /** This event occurs when the user clicks "edit->FindNext". */
    private static final int ev_findNext = 208;

    /** This event occurs when the user clicks "edit->GoTo". */
    private static final int ev_goTo = 209;

    /** This event occurs when the user clicks "edit->GoToPrevFile". */
    private static final int ev_goToPrevFile = 210;

    /** This event occurs when the user clicks "edit->GoToNextFile". */
    private static final int ev_goToNextFile = 211;

    /** This event occurs when the LOG PANEL gains focus. */
    static final int ev_logFocused = 212;

    /** This event refreshes the "run" menu. */
    private static final int ev_refreshRun = 301;

    /** This event starts the execution of a "run" or "check" or "metamodel generation". */
    private static final int evi_execute = 302;

    /** This event stops the current run or check. */
    private static final int ev_stop = 303;

    /** This event executes the latest command. */
    private static final int ev_runLatest = 304;

    /** This events occurs when the solver has finished all its solvings. */
    static final int ev_done = 305;

    /** This events occurs when the solver has failed. */
    static final int ev_fail = 306;

    /** This event displays the latest instance. */
    private static final int ev_showLatest = 307;

    /** This event displays the meta model. */
    private static final int ev_showMetaModel = 308;

    /** This event displays a particular instance. */
    static final int evs_visualize = 309;

    /** This event changes the latest instance. */
    static final int evs_setLatest = 310;

    /** This event happens when the user tries to load the evaluator from the main GUI. */
    private static final int ev_loadEvaluator = 311;

    /** This event refreshes the "Options" menu. */
    private static final int ev_refreshOption = 401;

    /** This event refreshes the main window's "window" menu. */
    private static final int ev_refreshWindow = 501;

    /** This event refreshes the visualizer window's "window" menu. */
    private static final int ev_refreshWindow2 = 502;

    /** This event bring this window to the foreground. */
    private static final int ev_show = 503;

    /** This event minimizes the window. */
    private static final int ev_minimize = 504;

    /** This event alternatingly maximizes or restores the window. */
    private static final int ev_maximize = 505;

    /** This event displays the help html. */
    private static final int ev_help = 601;

    /** This event displays the about box. */
    private static final int ev_about = 602;

    /** This event displays the license box. */
    private static final int ev_license = 603;

    //====== Event Handler ========================================================================================//

    /**
     * The event handler for events without arguments.
     * <p> Eg. ev_close, ev_quit...
     */
    @SuppressWarnings("deprecation")
    public boolean run(final int key) {

        if (key==ev_clearTemp) {
            Helper.clearTemporarySpace();
            copyFromJAR();
            log.logBold("Temporary directory has been cleared.\n\n");
            log.logDivider();
            log.flush();
        }

        if (key==ev_loadEvaluator) {
            log.logRed("Note: the evaluator is now in the visualizer.\n"
            +"Just click the \"Evaluator\" toolbar button\n"
            +"when an instance is shown in the visualizer.\n");
            log.flush();
        }

        if (key==ev_open || key==ev_builtin) {
            String start = (key==ev_open) ? Dir.get() : (Helper.alloyHome()+File.separatorChar+"models");
            if (!(new File(start)).isDirectory()) start=System.getProperty("user.home");
            start=Util.canon(start);
            File file=OurDialog.askFile(frame, true, start, openAlsOnly?".als":"", ".als files");
            if (file==null) return false;
            if (!file.getPath().toLowerCase(Locale.US).endsWith(".als")) openAlsOnly=false;
            if (key==ev_open) Dir.set(file.getParent());
            return run(evs_open, file.getPath());
        }

        if ((key==ev_save || key==ev_saveAs) && !mode_externalEditor) {
            text.setDefaultDirectory(Dir.get());
            String ans=text.save(key==ev_saveAs);
            Dir.set(text.defaultDirectory());
            if (ans==null) return false;
            notifyChange();
            addHistory(ans);
            log.clearError();
            return true;
        }

        if (key==ev_close) {
            text.setDefaultDirectory(Dir.get());
            text.close();
            Dir.set(text.defaultDirectory());
        }

        if (key==ev_quit || key==ev_dispose) {
            text.setDefaultDirectory(Dir.get());
            boolean r = text.closeAll();
            Dir.set(text.defaultDirectory());
            if (r) System.exit(0);
        }

        if (key==ev_refreshEdit) {
            boolean canUndo = !mode_externalEditor && text.canUndo();
            boolean canRedo = !mode_externalEditor && text.canRedo();
            editmenu.removeAll();
            OurUtil.makeMenuItem(editmenu, "Undo", canUndo, KeyEvent.VK_Z, KeyEvent.VK_Z, this, ev_undo);
            if (Util.onMac())
                OurUtil.makeMenuItemWithShift(editmenu, "Redo", KeyEvent.VK_Z, new MultiRunner(this,ev_redo)).setEnabled(canRedo);
            else
                OurUtil.makeMenuItem(editmenu, "Redo", canRedo, KeyEvent.VK_Y, KeyEvent.VK_Y, this, ev_redo);
            editmenu.addSeparator();
            OurUtil.makeMenuItem(editmenu,"Cut"           , !mode_externalEditor, KeyEvent.VK_X,        KeyEvent.VK_X,         this, ev_cut);
            OurUtil.makeMenuItem(editmenu,"Copy"          , true,                 KeyEvent.VK_C,        KeyEvent.VK_C,         this, ev_copy);
            OurUtil.makeMenuItem(editmenu,"Paste"         , !mode_externalEditor, KeyEvent.VK_V,        KeyEvent.VK_V,         this, ev_paste);
            editmenu.addSeparator();
            OurUtil.makeMenuItem(editmenu,"Go To..."      , !mode_externalEditor, KeyEvent.VK_T,        KeyEvent.VK_T,         this, ev_goTo);
            OurUtil.makeMenuItem(editmenu,"Previous File" , text.getTabCount()>1, KeyEvent.VK_PAGE_UP,  KeyEvent.VK_PAGE_UP,   this, ev_goToPrevFile);
            OurUtil.makeMenuItem(editmenu,"Next File"     , text.getTabCount()>1, KeyEvent.VK_PAGE_DOWN,KeyEvent.VK_PAGE_DOWN, this, ev_goToNextFile);
            editmenu.addSeparator();
            OurUtil.makeMenuItem(editmenu,"Find..."       , !mode_externalEditor, KeyEvent.VK_F,        KeyEvent.VK_F,         this, ev_find);
            OurUtil.makeMenuItem(editmenu,"Find Next"     , !mode_externalEditor, KeyEvent.VK_G,        KeyEvent.VK_G,         this, ev_findNext);
        }

        if (key==ev_undo && !mode_externalEditor && text.canUndo()) text.undo();

        if (key==ev_redo && !mode_externalEditor && text.canRedo()) text.redo();

        if (key==ev_copy) { if (lastFocusIsOnEditor && !mode_externalEditor) text.text().copy(); else log.copy(); }

        if (key==ev_cut) if (lastFocusIsOnEditor && !mode_externalEditor) text.text().cut();

        if (key==ev_paste) if (lastFocusIsOnEditor && !mode_externalEditor) text.text().paste();

        if (key==ev_logFocused) lastFocusIsOnEditor=false;

        if (key==ev_goToPrevFile) {
            int i=text.getSelectedIndex()-1;
            if (i<0) i=text.getTabCount()-1;
            text.setSelectedIndex(i);
        }

        if (key==ev_goToNextFile) {
            int i=text.getSelectedIndex()+1;
            if (i>=text.getTabCount()) i=0;
            text.setSelectedIndex(i);
        }

        if (key==ev_goTo && !mode_externalEditor) {
            JTextField y=new JTextField();
            JTextField x=new JTextField();
            if (!OurDialog.getInput(frame,"Go To","Line Number:", y, "Column Number (optional):", x)) return false;
            try {
                JTextArea t=text.text();
                int xx=1, yy=Integer.parseInt(y.getText());
                if (yy<1) return false;
                if (yy>t.getLineCount()) {log.logRed("This file only has "+t.getLineCount()+" line(s)."); return false;}
                if (x.getText().length()!=0) xx=Integer.parseInt(x.getText());
                if (xx<1) {log.logRed("If the column number is specified, it must be 1 or greater."); return false;}
                int caret=t.getLineStartOffset(yy-1);
                int len=t.getLineEndOffset(yy-1)-caret;
                if (xx>len) xx=len;
                if (xx<1) xx=1;
                t.setSelectionStart(caret+xx-1);
                t.setSelectionEnd(caret+xx-1);
                t.requestFocusInWindow();
            } catch(NumberFormatException ex) { log.logRed("The number must be 1 or greater."); return false;
            } catch(Throwable ex) { return false;
            }
        }

        if (key==ev_find && !mode_externalEditor) {
            JTextField x=new JTextField(lastFind);
            x.selectAll();
            JCheckBox c=new JCheckBox("Case Sensitive?",lastFindCaseSensitive);
            c.setMnemonic('c');
            JCheckBox b=new JCheckBox("Search Backward?",!lastFindForward);
            b.setMnemonic('b');
            if (!OurDialog.getInput(frame, "Find", "Text:", x, " ", c, b)) return false;
            if (x.getText().length()==0) return false;
            lastFind=x.getText();
            lastFindCaseSensitive=c.getModel().isSelected();
            lastFindForward=!b.getModel().isSelected();
            return run(ev_findNext);
        }

        if (key==ev_findNext && lastFind.length()>0 && !mode_externalEditor) {
            JTextArea t=text.text();
            String all=t.getText();
            int i=Util.indexOf(all, lastFind, t.getCaretPosition()+(lastFindForward?0:-1),lastFindForward,lastFindCaseSensitive);
            if (i<0) {
                i=Util.indexOf(all, lastFind, lastFindForward?0:(all.length()-1), lastFindForward, lastFindCaseSensitive);
                if (i<0) {log.logRed("The specified search string cannot be found."); return false;}
                log.logRed("Search wrapped.");
            } else log.clearError();
            if (lastFindForward) { t.setCaretPosition(i); t.moveCaretPosition(i+lastFind.length()); }
            else { t.setCaretPosition(i+lastFind.length()); t.moveCaretPosition(i); }
            t.requestFocusInWindow();
        }

        if (key==ev_refreshRun) {
            KeyStroke ac=KeyStroke.getKeyStroke(KeyEvent.VK_E, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
            runmenu.removeAll();
            OurUtil.makeMenuItem(runmenu, "Execute Latest Command",
                    true, KeyEvent.VK_E, KeyEvent.VK_E, this, ev_runLatest);
            runmenu.add(new JSeparator());
            OurUtil.makeMenuItem(runmenu, "Show Latest Instance",  latestInstance.length()>0, KeyEvent.VK_L,  KeyEvent.VK_L, this, ev_showLatest);
            OurUtil.makeMenuItem(runmenu, "Show Metamodel",        true,                      KeyEvent.VK_M,  KeyEvent.VK_M, this, ev_showMetaModel);
            OurUtil.makeMenuItem(runmenu, "Open Evaluator",        true,                      KeyEvent.VK_V,  -1,            this, ev_loadEvaluator);
            List<Command> cp = (mode_externalEditor ? null : commands);
            if (cp==null) {
                try {
                    List<Command> u=null;
                    if (!mode_externalEditor)
                        u=CompUtil.parseOneModule_fromString(text.text().getText());
                    else if (text.isFile() && text.getFilename().length()>0)
                        u=CompUtil.parseOneModule_fromFile(text.getFilename());
                    cp=u;
                }
                catch(Err e) {
                    commands = null;
                    runmenu.getItem(0).setEnabled(false);
                    runmenu.getItem(3).setEnabled(false);
                    if (!mode_externalEditor) {
                        Err e2 = new ErrorFatal(new Pos(text.getFilename(), e.pos.x, e.pos.y, e.pos.x2, e.pos.y2),"");
                        text.highlight(e2);
                    }
                    log.logRed(e.toString()+"\n\n");
                    return true;
                }
                catch(Throwable e) {
                    commands = null;
                    runmenu.getItem(0).setEnabled(false);
                    runmenu.getItem(3).setEnabled(false);
                    //e.printStackTrace(System.err); System.err.flush();
                    log.logRed("Cannot parse the model.\n"+e.toString()+"\n\n");
                    return true;
                }
                commands=cp;
            }
            text.removeAllHighlights();
            log.clearError(); // To clear any residual error message
            if (cp==null) {
                runmenu.getItem(0).setEnabled(false);
                runmenu.getItem(3).setEnabled(false);
                return true;
            }
            if (cp.size()==0) {
                runmenu.getItem(0).setEnabled(false);
                return true;
            }
            if (latestCommand>=cp.size()) {
                latestCommand=cp.size()-1;
            }
            runmenu.remove(0);
            for(int i=0; i<cp.size(); i++) {
                JMenuItem y=new JMenuItem(cp.get(i).toString());
                y.addActionListener(new MultiRunner(this, evi_execute, i));
                if (i==latestCommand) { y.setMnemonic(KeyEvent.VK_E); y.setAccelerator(ac); }
                runmenu.add(y,i);
            }
            if (cp.size()>=2) {
                JMenuItem y=new JMenuItem("Execute All");
                y.setMnemonic(KeyEvent.VK_A);
                y.addActionListener(new MultiRunner(this, evi_execute, -1));
                runmenu.add(y,0);
                runmenu.add(new JSeparator(),1);
            }
        }

        if (key==ev_runLatest) {
            run(ev_refreshRun);
            OurUtil.enableAll(runmenu);
            if (commands==null) return false;
            int n=commands.size();
            if (n<=0) { log.logRed("There are no commands to execute.\n\n"); return false; }
            if (latestCommand>=n) latestCommand=n-1;
            if (latestCommand<0) latestCommand=0;
            run(evi_execute, latestCommand);
        }

        if (key==ev_showMetaModel) {
            run(ev_refreshRun);
            OurUtil.enableAll(runmenu);
            if (commands!=null) run(evi_execute, -2);
        }

        if (key==ev_done || key==ev_fail || key==ev_stop) {
            if (key==ev_stop || key==ev_fail) {
                log.escSetProcess(null); // Prevents the subprocess from writing any more text to the log
                log.escReset();
                if (key==ev_stop && subrunning) { log.logBold("\nSolving Stopped.\n"); log.logDivider(); }
                if (subprocess!=null) { subprocess.destroy(); subprocess=null; }
            } else if (subrunning && subprocess!=null) {
                try {
                    int r=subprocess.exitValue();
                    // If we can get to this line, that means the process has terminated abnormally.
                    log.escSetProcess(null); // Prevents the subprocess from writing any more text to the log
                    log.escReset();
                    subprocess.destroy();
                    subprocess=null;
                    if (r==SimpleRunner.EXIT_OUT_OF_MEMORY)
                        log.logBold("Fatal Error: Solver ran out of memory.\n" +
                        "Please go to the \"Options\" menu to specify a different memory size.\n");
                    else
                        log.logBold("Fatal Error: Solver failed due to unknown reason.\n" +
                        "One possible cause is that, in the Options menu, your specified\n" +
                        "memory size is larger than the amount allowed by your OS.\n");
                    log.logDivider();
                } catch(IllegalThreadStateException ex) {}
            }
            subrunning=false;
            runmenu.setEnabled(true);
            runbutton.setVisible(true);
            showbutton.setEnabled(true);
            stopbutton.setVisible(false);
            if (latestAutoInstance.length()==0) return true;
            String f=latestAutoInstance;
            latestAutoInstance="";
            if (subrunningTask==2) viz.run(VizGUI.evs_loadInstanceForcefully, f);
            else if (mode_autoVisualize || subrunningTask==1) run(evs_visualize, "XML: "+f);
        }

        if (key==ev_refreshFile) {
            JMenu recentmenu;
            String open=(mode_externalEditor?"Load":"Open");
            filemenu.removeAll();
            if (!mode_externalEditor) OurUtil.makeMenuItem(filemenu, "New", true,KeyEvent.VK_N,KeyEvent.VK_N, this, ev_new);
            OurUtil.makeMenuItem(filemenu, open+"...",               true,KeyEvent.VK_O,KeyEvent.VK_O, this, ev_open);
            OurUtil.makeMenuItem(filemenu, open+" Sample Models...", true,KeyEvent.VK_B,-1, this, ev_builtin);
            filemenu.add(recentmenu = new JMenu(open+" Recent"));
            if (!mode_externalEditor) {
                OurUtil.makeMenuItem(filemenu, "Save", true,KeyEvent.VK_S,KeyEvent.VK_S, this, ev_save);
                if (Util.onMac())
                    OurUtil.makeMenuItemWithShift(filemenu,"Save As...",KeyEvent.VK_S, new MultiRunner(this, ev_saveAs));
                else
                    OurUtil.makeMenuItem(filemenu, "Save As...", true, KeyEvent.VK_A, -1, this, ev_saveAs);
            }
            OurUtil.makeMenuItem(filemenu, "Close", true, KeyEvent.VK_W, KeyEvent.VK_W, this, ev_close);
            OurUtil.makeMenuItem(filemenu, "Clear Temporary Directory", true, -1, -1, this, ev_clearTemp);
            OurUtil.makeMenuItem(filemenu, "Quit", true, KeyEvent.VK_Q, (Util.onMac()?-1:KeyEvent.VK_Q), this, ev_quit);
            boolean found=false;
            for(Util.StringPref p: new Util.StringPref[]{ Model0, Model1, Model2, Model3 }) {
                final String name = p.get();
                if (name.length()==0) continue;
                found=true;
                OurUtil.makeMenuItem(recentmenu, name, -1, -1, new MultiRunner(this, evs_open, name));
            }
            recentmenu.addSeparator();
            OurUtil.makeMenuItem(recentmenu, "Clear Menu", true, -1, -1, this, ev_clearRecent);
            recentmenu.setEnabled(found);
        }

        if (key==ev_clearRecent) {Model0.set(""); Model1.set(""); Model2.set(""); Model3.set("");}

        if (key==ev_new && !mode_externalEditor) {
            text.newTab();
            notifyChange();
            run(ev_show);
        }

        if (key==ev_showLatest) {
            if (latestInstance.length()==0) log.logRed("No previous instances are available for viewing.\n\n");
            else run(evs_visualize, "XML: "+latestInstance);
        }

        if (key==ev_refreshOption) {
            optmenu.removeAll();
            //
            final boolean showWelcome=Welcome.get() < welcomeLevel;
            OurUtil.makeMenuItem(optmenu, "Welcome Message at Start Up: "+(showWelcome?"Yes":"No")
            ,-1,-1, new Runnable(){
                public void run() { Welcome.set(showWelcome ? welcomeLevel : 0); }
            });
            //
            SatSolver now=SatSolver.get();
            JMenu sat=new JMenu("SAT Solver: "+now);
            for(final SatSolver sc:satChoices) {
                (OurUtil.makeMenuItem(sat, ""+sc, -1, -1, new Runnable() {
                    public final void run() { sc.set(); }
                })).setIcon(sc==now?iconYes:iconNo);
            }
            optmenu.add(sat);
            //
            OurUtil.makeMenuItem(optmenu,"Warnings are Fatal: "+(mode_warningNonFatal?"No":"Yes")
            ,-1,-1,new Runnable(){
                public final void run() { WarningNonfatal.set(mode_warningNonFatal=!mode_warningNonFatal); }
            });
            //
            int mem=SubMemory.get();
            boolean memfound=false;
            JMenu subMemoryMenu=new JMenu("Maximum Memory to Use: "+mem+"M");
            for(final int n: new Integer[]{16,32,64,128,256,512,768,1024,2048,3072,4096}) {
                (OurUtil.makeMenuItem(subMemoryMenu, ""+n+"M", -1, -1, new Runnable() {
                    public final void run() { SubMemory.set(SimpleGUI.this.subMemory = n); }
                })).setIcon(n==mem?iconYes:iconNo);
                if (n==mem) memfound=true;
            }
            (OurUtil.makeMenuItem(subMemoryMenu, "Other...", -1, -1, new Runnable() {
                public final void run() {
                    String ans;
                    while(true) {
                        ans=JOptionPane.showInputDialog(frame, "What amount of memory do you want to use for SAT solving?", "Maximum Memory", JOptionPane.PLAIN_MESSAGE);
                        try {
                            int m=Integer.parseInt(ans);
                            SubMemory.set(SimpleGUI.this.subMemory = m);
                            break;
                        } catch(NumberFormatException ex) {
                            OurDialog.alert(frame, "Error: \""+(ans.trim())+"\" is not a valid integer.", "Error");
                        }
                    }
                }
            })).setIcon(memfound ? iconNo : iconYes);
            optmenu.add(subMemoryMenu);
            //
            Verbosity vnow=Verbosity.get();
            JMenu verb=new JMenu("Message Verbosity: "+vnow);
            for(final Verbosity vb:Verbosity.values()) {
                (OurUtil.makeMenuItem(verb, ""+vb, -1, -1, new Runnable() {
                    public final void run() { vb.set(); }
                })).setIcon(vb==vnow?iconYes:iconNo);
            }
            optmenu.add(verb);
            //
            int fontSize=FontSize.get();
            JMenu size=new JMenu("Font Size: "+fontSize);
            for(final int n: new Integer[]{9,10,11,12,14,16,18,24}) {
                (OurUtil.makeMenuItem(size, ""+n, -1, -1, new Runnable() {
                    public final void run() {
                        FontSize.set(n);
                        String family=FontName.get();
                        text.setFont(new Font(family, Font.PLAIN, n));
                        status.setFont(new Font(family, Font.PLAIN, n));
                        log.setFontSize(n);
                    }
                })).setIcon(n==fontSize?iconYes:iconNo);
            }
            optmenu.add(size);
            //
            String fontname=FontName.get();
            JMenuItem fontnameMenu=new JMenuItem("Font: "+fontname+"...");
            fontnameMenu.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    int size=FontSize.get();
                    String family=OurDialog.askFont(frame);
                    if (family.length()==0) return;
                    FontName.set(family);
                    text.setFont(new Font(family, Font.PLAIN, size));
                    status.setFont(new Font(family, Font.PLAIN, size));
                    log.setFontName(family);
                }
            });
            optmenu.add(fontnameMenu);
            //
            int tabSize=TabSize.get();
            JMenu tabSizeMenu=new JMenu("Tab Size: "+tabSize);
            for(final int n: new Integer[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16}) {
                (OurUtil.makeMenuItem(tabSizeMenu, ""+n, -1, -1, new Runnable() {
                    public final void run() { TabSize.set(n); text.setTabSize(n); }
                })).setIcon(n==tabSize?iconYes:iconNo);
            }
            optmenu.add(tabSizeMenu);
            //
            int skDepth=SkolemDepth.get();
            JMenu skDepthMenu=new JMenu("Skolem Depth: "+skDepth);
            for(final int n: new Integer[]{0,1,2}) {
                (OurUtil.makeMenuItem(skDepthMenu, ""+n, -1, -1, new Runnable() {
                    public final void run() { SkolemDepth.set(n); }
                })).setIcon(n==skDepth?iconYes:iconNo);
            }
            optmenu.add(skDepthMenu);
            //
            Runnable ext=new Runnable() {
                public void run() {
                    text.setDefaultDirectory(Dir.get());
                    boolean r=text.closeAll();
                    Dir.set(text.defaultDirectory());
                    if (!r) return;
                    log.clearError();
                    ExternalEditor.set(mode_externalEditor = !mode_externalEditor);
                    if (mode_externalEditor) text.disableIO(); else text.enableIO();
                    Container all=frame.getContentPane();
                    all.removeAll();
                    newbutton.setVisible(!mode_externalEditor);
                    openbutton.setVisible(!mode_externalEditor);
                    savebutton.setVisible(!mode_externalEditor);
                    loadbutton.setVisible(mode_externalEditor);
                    if (mode_externalEditor) {
                        log.setBackground(Color.WHITE);
                        ((JPanel)(splitpane.getTopComponent())).remove(toolbar);
                        splitpane.setBottomComponent(null);
                        all.add(toolbar, BorderLayout.NORTH);
                        all.add(logpane, BorderLayout.CENTER);
                    } else {
                        log.setBackground(background);
                        ((JPanel)(splitpane.getTopComponent())).add(toolbar, BorderLayout.NORTH);
                        splitpane.setBottomComponent(logpane);
                        all.add(splitpane, BorderLayout.CENTER);
                    }
                    all.add(status, BorderLayout.SOUTH);
                    notifyChange();
                    frame.validate();
                    SimpleGUI.this.run(ev_refreshFile);
                    OurUtil.enableAll(filemenu);
                    if (mode_externalEditor) logpane.requestFocusInWindow(); else text.text().requestFocusInWindow();
                    lastFocusIsOnEditor = !mode_externalEditor;
                }
            };
            OurUtil.makeMenuItem(optmenu,"Use an External Editor: "+(mode_externalEditor?"Yes":"No")
                ,-1,-1,ext);
            OurUtil.makeMenuItem(optmenu,"Visualize Automatically: "+(mode_autoVisualize?"Yes":"No")
                ,-1,-1,new Runnable(){
                public final void run() { AutoVisualize.set(mode_autoVisualize=!mode_autoVisualize); }
            });
            final boolean recordKK=RecordKodkod.get();
            OurUtil.makeMenuItem(optmenu, "Record the Kodkod Input/Output: "+(recordKK?"Yes":"No")
                ,-1,-1, new Runnable(){
                public void run() { RecordKodkod.set(!recordKK); }
            });
            // The following line is needed if we need to "go into EXTERNAL TEXT EDITOR MODE" upon program start up
            if (mode_externalEditor && splitpane.getBottomComponent()!=null) {mode_externalEditor=false; ext.run();}
        }

        if (key==ev_minimize) frame.setExtendedState(JFrame.ICONIFIED);

        if (key==ev_maximize) {
            if ((frame.getExtendedState() & JFrame.MAXIMIZED_BOTH)==JFrame.MAXIMIZED_BOTH)
                frame.setExtendedState(JFrame.NORMAL);
            else
                frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        }

        if (key==ev_refreshWindow || key==ev_refreshWindow2) {
            JMenu w=(key==ev_refreshWindow)?windowmenu:windowmenu2;
            w.removeAll();
            if (key==ev_refreshWindow) {
                OurUtil.makeMenuItem(w, "Minimize", true, KeyEvent.VK_M, -1, this, ev_minimize).setIcon(iconNo);
                OurUtil.makeMenuItem(w, "Zoom", true, -1, -1, this, ev_maximize).setIcon(iconNo);
            } else {
                OurUtil.makeMenuItem(w, "Minimize", true, KeyEvent.VK_M, -1, viz, VizGUI.ev_minimize).setIcon(iconNo);
                OurUtil.makeMenuItem(w, "Zoom", true, -1, -1, viz, VizGUI.ev_maximize).setIcon(iconNo);
            }
            w.addSeparator();
            List<String> filenames=text.getFilenames();
            for(int i=0; i<filenames.size(); i++) {
                String f=filenames.get(i);
                JMenuItem it;
                if (mode_externalEditor && !text.isFile(i))
                    it = new JMenuItem("Alloy Analyzer");
                else
                    it = new JMenuItem("Model: "+slightlyShorterFilename(f)+(text.modified(i) ? " *" : ""));
                it.setIcon((f.equals(text.getFilename()) && key==ev_refreshWindow) ? iconYes : iconNo);
                if (f.equals(text.getFilename()))
                  it.addActionListener(new MultiRunner(this, ev_show));
                else
                  it.addActionListener(new MultiRunner(this, evs_open, f));
                w.add(it);
            }
            if (viz!=null) for(String f:viz.getInstances()) {
                JMenuItem it=new JMenuItem("Instance: "+viz.getInstanceTitle(f));
                it.setIcon( (key==ev_refreshWindow2 && f.equals(viz.getXMLfilename())) ? iconYes : iconNo);
                it.addActionListener(new MultiRunner(this, evs_visualize, "XML: "+f));
                w.add(it);
            }
        }

        if (key==ev_show) {
            bringup(frame);
            if (mode_externalEditor) logpane.requestFocusInWindow(); else text.text().requestFocusInWindow();
        }

        if (key==ev_help) try {
            int w=OurUtil.getScreenWidth(), h=OurUtil.getScreenHeight();
            final JEditorPane html1,html2;
            final JFrame frame = new JFrame();
            URL url1=this.getClass().getResource("/help/Nav.html");
            URL url2=this.getClass().getResource("/help/index.html");
            html1=new JEditorPane(url1);
            html2=new JEditorPane(url2);
            HyperlinkListener hl=new HyperlinkListener() {
                public final void hyperlinkUpdate(HyperlinkEvent e) {
                    try {
                        if (e.getEventType()!=HyperlinkEvent.EventType.ACTIVATED) return;
                        if (e.getURL().getPath().endsWith("quit.htm")) { frame.dispose(); return; }
                        html2.setPage(e.getURL()); html2.requestFocusInWindow();
                    } catch(Throwable ex) { }
                }
            };
            html1.setEditable(false); html1.setBorder(new EmptyBorder(3,3,3,3)); html1.addHyperlinkListener(hl);
            html2.setEditable(false); html2.setBorder(new EmptyBorder(3,3,3,3)); html2.addHyperlinkListener(hl);
            JScrollPane scroll1 = OurUtil.scrollpane(html1);
            JScrollPane scroll2 = OurUtil.scrollpane(html2);
            JSplitPane split = OurUtil.splitpane(JSplitPane.HORIZONTAL_SPLIT, scroll1, scroll2, 150);
            split.setResizeWeight(0d);
            frame.setTitle("Alloy Analyzer Online Guide");
            frame.getContentPane().setLayout(new BorderLayout());
            frame.getContentPane().add(split, BorderLayout.CENTER);
            frame.pack();
            frame.setSize(w-w/10, h-h/10);
            frame.setLocation(w/20, h/20);
            frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            frame.setVisible(true);
            html2.requestFocusInWindow();
        } catch(Throwable ex) { return false; }

        if (key==ev_license) {
            JButton dismiss = new JButton(Util.onMac() ? "Dismiss" : "Close");
            String alloytxt;
            try {alloytxt=Util.readAll(true,"LICENSES/Alloy.txt");} catch(IOException ex) {return false;}
            final JTextArea text = new JTextArea(alloytxt,15,85);
            text.setEditable(false);
            text.setLineWrap(false);
            text.setBorder(new EmptyBorder(2,2,2,2));
            text.setFont(new Font("Monospaced", Font.PLAIN, 12));
            final JComboBox combo = new JComboBox(new Object[]{"Alloy","Kodkod","GraphViz","Grappa","NanoXML","JavaCup","SAT4J","ZChaff","MiniSat"});
            combo.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    Object value = combo.getSelectedItem();
                    if (value instanceof String) {
                        try {
                            String content = Util.readAll(true,"LICENSES/"+value+".txt");
                            text.setText(content);
                        } catch(IOException ex) {
                            text.setText("Sorry: an error has occurred in displaying the license file.");
                        }
                    }
                    text.setCaretPosition(0);
                }
            });
            JScrollPane scroll = OurUtil.scrollpane(text);
            scroll.setBorder(new LineBorder(Color.DARK_GRAY, 1));
            Object[] array = {
                    "The source code for the Alloy Analyzer is available",
                    "under the GNU General Public License version 2.",
                    " ",
                    "The Alloy Analyzer utilizes several third-party packages whose code may",
                    "be distributed under a different license. We are extremely grateful to",
                    "the authors of these packages for making their source code freely available.",
                    " ",
                    OurUtil.makeH(null, "See the copyright notice for: ", combo, null),
                    " ",
                    scroll,
                    OurUtil.makeH(null, dismiss, null)};
            final JOptionPane about = new JOptionPane(array,
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, null, new Object[]{});
            final JDialog dialog = about.createDialog(null, "The Copyright Notices");
            dismiss.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) { dialog.dispose(); }
            });
            dialog.setVisible(true);
        }

        if (key==ev_about) {
            Icon icon=OurUtil.loadIcon("images/logo.gif");
            JButton dismiss = new JButton(Util.onMac() ? "Dismiss" : "Close");
            Object[] array = {
                    icon,
                    "Alloy Analyzer "+Version.version(),
                    "Build date: "+Version.buildDate(),
                    " ",
                    "Lead developer: Felix Chang",
                    "Engine developer: Emina Torlak",
                    "Graphic design: Julie Pelaez",
                    "Project lead: Daniel Jackson",
                    " ",
                    "More information at: http://alloy.mit.edu/",
                    "Comments and questions to: alloy@mit.edu",
                    " ",
                    "Thanks to: Ilya Shlyakhter, Manu Sridharan, Derek Rayside, Jonathan Edwards, Gregory Dennis,",
                    "Robert Seater, Edmond Lau, Vincent Yeung, Sam Daitch, Andrew Yip, Jongmin Baek, Ning Song,",
                    "Arturo Arizpe, Li-kuo (Brian) Lin, Joseph Cohen, Jesse Pavel, Ian Schechter, and Uriel Schafer.",
                    OurUtil.makeH(null,dismiss,null)};
            final JOptionPane about = new JOptionPane(array,
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, null, new Object[]{});
            final JDialog dialog = about.createDialog(null, "About Alloy Analyzer "+Version.version());
            dismiss.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) { dialog.dispose(); }
            });
            dialog.setVisible(true);
        }

        return true;
    }

    //====== Event Handler ========================================================================================//

    /**
     * The event handler for events with an int argument.
     */
    public boolean run(final int key, final int index) {
        if (key != evi_execute) return true;
        if (subrunning) return false;
        if (index==(-2)) subrunningTask=1; else subrunningTask=0;
        latestAutoInstance="";
        if (index>=0) latestCommand=index;
        if (index==-1 && commands!=null) {
            latestCommand=commands.size()-1;
            if (latestCommand<0) latestCommand=0;
        }
        // To update the accelerator to point to the command actually chosen
        run(ev_refreshRun); OurUtil.enableAll(runmenu);
        if (commands==null) return false;
        if (commands.size()==0 && index!=-2) { log.logRed("There are no commands to execute.\n\n"); return false; }
        int i=index;
        if (i>=commands.size()) i=commands.size()-1;
        // Now we begin the execution; make sure you trap all possible exceptions, so that we can turn subrunning to false
        subrunning=true;
        runmenu.setEnabled(false);
        runbutton.setVisible(false);
        showbutton.setEnabled(false);
        stopbutton.setVisible(true);
        final SatSolver sc = SatSolver.get();
        final A4Options opt = new A4Options();
        opt.recordKodkod = RecordKodkod.get();
        opt.skolemDepth = SkolemDepth.get();
        opt.originalFilename = Util.canon(text.getFilename());
        opt.solver = sc;
        opt.solverDirectory = Helper.alloyHome()+fs+"binary";
        if (1==0 && Verbosity.get()==Verbosity.FULLDEBUG) {
            try {
                final String tempdir = Helper.maketemp();
                SimpleReporter.performRegularCommand(null, text.takeSnapshot(), i, opt, WarningNonfatal.get(), tempdir, Verbosity.get().ordinal());
                System.err.flush();
            } catch(Throwable ex) {
                ErrorFatal err=new ErrorFatal(ex.getMessage(), ex);
                log.logBold("Unknown exception: "+ex+"\nStackTrace:\n"+err.getTotalTrace());
                System.err.flush();
            }
            run(ev_done);
            return true;
        }
        if (!prepareSubJVM()) {
            log.logBold("Error launching the sub-JVM.\n\"java\" is not in your current program search path.\n");
            log.flush();
            run(ev_stop);
            return false;
        }
        try {
            final String tempdir = Helper.maketemp();
            final String cache = tempdir + File.separatorChar + "cache";
            final SimpleRunnerBundle b = new SimpleRunnerBundle(
                opt,
                text.takeSnapshot(),
                i,
                Verbosity.get().ordinal(),
                WarningNonfatal.get());
            (new File(cache)).deleteOnExit();
            b.write(cache);
            byte[] bytes=("S"+tempdir).getBytes("UTF-8");
            subprocess.getOutputStream().write(bytes, 0, bytes.length);
            subprocess.getOutputStream().write(new byte[]{0}, 0, 1);
            subprocess.getOutputStream().flush();
        } catch(Throwable ex) {
            log.logBold("Fatal Error: Solver failed due to unknown reason.\n" +
            "One possible cause is that, in the Options menu, your specified\n" +
            "memory size is larger than the amount allowed by your OS.\n");
            log.logDivider();
            log.flush();
            subrunning=false;
            run(ev_stop);
        }
        return true;
    }

    //====== Event Handler ========================================================================================//

    /**
     * The event handler for events with a String argument.
     * <p> Eg. evs_visualize, evs_open...
     */
    public boolean run(final int key, final String arg) {

        if (key<0) {
            // Solution Enumeration
            bringup(frame);
            if (subrunning)
                throw new RuntimeException("Alloy4 is currently executing a SAT solver command. Please wait until that command has finished.");
            if (!prepareSubJVM())
                throw new RuntimeException("Error launching the sub-JVM.\n\"java\" is not in your current program search path.\n");
            try {
                byte[] bytes=("N"+arg).getBytes("UTF-8");
                subprocess.getOutputStream().write(bytes, 0, bytes.length);
                subprocess.getOutputStream().write(new byte[]{0}, 0, 1);
                subprocess.getOutputStream().flush();
            } catch(Throwable ex) {
                throw new RuntimeException("Solver failed due to unknown reason.\n" +
                "One possible cause is that, in the Options menu,\n" +
                "your specified memory size is larger than the\n" +
                "amount allowed by your OS.\n");
            }
            subrunning=true;
            subrunningTask=2;
            runmenu.setEnabled(false);
            runbutton.setVisible(false);
            showbutton.setEnabled(false);
            stopbutton.setVisible(true);
            return true;
        }

        if (key==evs_update) {
            String result=arg;
            int num=0;
            int len=result.length();
            boolean found=false;
            for(int i=0; ; i++) {
                if (i>=len) return true;
                char c=result.charAt(i);
                if (!(c>='0' && c<='9')) { if (!found) continue; result=result.substring(i); break; }
                found=true;
                num=num*10+(int)(c-'0');
            }
            latestAlloyVersionName=result.trim();
            latestAlloyVersion=num;
            exitReporter.setLatestAlloyVersion(latestAlloyVersion, latestAlloyVersionName);
            if (latestAlloyVersion<=Version.buildNumber()) return true;
            log.logBold("An updated version of Alloy Analyzer has been released.\n");
            log.log("Please visit alloy.mit.edu to download the latest version:\nVersion "+latestAlloyVersionName+"\n");
            log.logDivider();
            log.flush();
        }

        if (key==evs_setLatest) {
            latestInstance=arg;
            latestAutoInstance=arg;
        }

        if (key==evs_visualize) {
            text.removeAllHighlights();
            if (arg.startsWith("MSG: ")) {
                OurDialog.showtext("Detailed Message", arg.substring(5), false);
            }
            if (arg.startsWith("CORE: ")) {
                String filename = Util.canon(arg.substring(6));
                String alloyfilename = null;
                IdentitySet<Pos> core = new IdentitySet<Pos>();
                InputStream is=null;
                ObjectInputStream ois=null;
                try {
                    is = new FileInputStream(filename);
                    ois = new ObjectInputStream(is);
                    int n = (Integer) (ois.readObject());
                    alloyfilename = Util.canon((String) (ois.readObject()));
                    for(int i=0; i<n; i++) { core.add((Pos) (ois.readObject())); }
                } catch(Throwable ex) {
                    log.logRed("Error reading or parsing the core \""+filename+"\"\n");
                    return false;
                } finally {
                    Util.close(ois);
                    Util.close(is);
                }
                text.newTab(alloyfilename);
                text.removeAllHighlights();
                for(Pos p:core) text.highlight(p,false);
            }
            if (arg.startsWith("POS: ")) {
                Scanner s = new Scanner(arg.substring(5));
                int x1 = s.nextInt();
                int y1 = s.nextInt();
                int x2 = s.nextInt();
                int y2 = s.nextInt();
                String f = s.nextLine();
                if (f.length()>0 && f.charAt(0)==' ') f=f.substring(1); // Get rid of the space after Y2
                Pos p=new Pos(Util.canon(f), x1, y1, x2, y2);
                if (!mode_externalEditor) text.highlight(new ErrorSyntax(p,""));
                return true;
            }
            if (arg.startsWith("CNF: ")) {
                String filename=Util.canon(arg.substring(5));
                String text;
                try { text=Util.readAll(filename); OurDialog.showtext("Text Viewer", text, false); }
                catch(IOException ex) { log.logRed("Error reading the file \""+filename+"\"\n"); }
            }
            if (arg.startsWith("XML: ")) {
                String filename=Util.canon(arg.substring(5));
                viz.run(VizGUI.evs_loadInstance, filename);
            }
        }

        if (key==evs_open) {
            String f=Util.canon(arg);
            if (!text.newTab(f)) {
                run(ev_show);
                log.logRed("Cannot open the file "+f+"\n\n");
                return false;
            }
            if (text.isFile()) addHistory(f);
            run(ev_show);
            if (mode_externalEditor) logpane.requestFocusInWindow(); else text.text().requestFocusInWindow();
            return true;
        }

        return true;
    }

    //====== Evaluator ======================================================//

    private static Computer evaluator = new Computer() {
        private String filename=null;
        private A4Solution instance=null;
        public final void setSourceFile(String filename) {
            this.filename=filename;
        }
        public final String compute(String input) throws Exception {
            try {
                if (filename==null) throw new RuntimeException("Internal Error: filename==null.");
                instance=A4Solution.readXML(filename, Helper.alloyHome());
                if (input.trim().length()==0) return ""; // Empty line
                Expr e=CompUtil.parseOneExpression_fromString(instance.getWorld(), input);
                e=instance.getWorld().typecheck(e);
                return instance.eval(e).toString();
            } catch(HigherOrderDeclException ex) {
                throw new ErrorType("Higher-order quantification is not allowed in the evaluator.");
            }
        }
    };

    //====== Main Method ====================================================//

    /** Main method that launches the program; this method might be called by an arbitrary thread. */
    public static final void main(final String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public final void run() { new SimpleGUI(args); }
        });
    }

    //====== Constructor ====================================================//

    /** The constructor; this method will be called by the AWT event thread, using the "invokeLater" method. */
    private SimpleGUI(String[] args) {

        // Register an exception handler for uncaught exceptions
        exitReporter = new MailBug();
        Thread.setDefaultUncaughtExceptionHandler(exitReporter);

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

        // Figure out the desired x, y, width, and height
        int screenWidth=OurUtil.getScreenWidth(), screenHeight=OurUtil.getScreenHeight();
        int width=AnalyzerWidth.get();
        if (width<=0) width=screenWidth/10*8; else if (width<100) width=100;
        if (width>screenWidth) width=screenWidth;
        int height=AnalyzerHeight.get();
        if (height<=0) height=screenHeight/10*8; else if (height<100) height=100;
        if (height>screenHeight) height=screenHeight;
        int x=AnalyzerX.get(); if (x<0) x=screenWidth/10; if (x>screenWidth-100) x=screenWidth-100;
        int y=AnalyzerY.get(); if (y<0) y=screenHeight/10; if (y>screenHeight-100) y=screenHeight-100;

        // Put up a slash screen
        frame=new JFrame("Alloy Analyzer");
        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new MultiRunner(this, ev_dispose));
        frame.addComponentListener(this);
        frame.pack();
        frame.setSize(width,height);
        frame.setLocation(x,y);
        frame.setVisible(true);
        frame.setTitle("Alloy Analyzer "+Version.version()+" loading... please wait...");
        // We intentionally call setVisible(true) first before settings the "please wait" title,
        // since we want the minimized window title on Linux/FreeBSD to just say Alloy Analyzer

        // Choose the appropriate font
        int fontSize=FontSize.get();
        String fontName=FontName.get();
        while(true) {
            if (!OurDialog.hasFont(fontName)) fontName="Lucida Grande"; else break;
            if (!OurDialog.hasFont(fontName)) fontName="Verdana"; else break;
            if (!OurDialog.hasFont(fontName)) fontName="Courier New"; else break;
            if (!OurDialog.hasFont(fontName)) fontName="Lucida Grande";
            break;
        }
        FontName.set(fontName);

        // Copy required files from the JAR
        copyFromJAR();
        final String binary = Helper.alloyHome()+fs+"binary";

        // Create the menu bar
        JMenuBar bar=new JMenuBar();
        filemenu = OurUtil.makeMenu(bar, "File", KeyEvent.VK_F, this, ev_refreshFile);
        editmenu = OurUtil.makeMenu(bar, "Edit", KeyEvent.VK_E, this, ev_refreshEdit);
        runmenu = OurUtil.makeMenu(bar, "Execute", KeyEvent.VK_X, this, ev_refreshRun);
        optmenu = OurUtil.makeMenu(bar, "Options", KeyEvent.VK_O, this, ev_refreshOption);
        windowmenu = OurUtil.makeMenu(bar, "Window", KeyEvent.VK_W, this, ev_refreshWindow);
        windowmenu2 = OurUtil.makeMenu(null, "Window", KeyEvent.VK_W, this, ev_refreshWindow2);
        helpmenu = OurUtil.makeMenu(bar, "Help", KeyEvent.VK_H, null, 0);
        if (!Util.onMac()) OurUtil.makeMenuItem(helpmenu, "About Alloy...", true, KeyEvent.VK_A, -1, this, ev_about);
        OurUtil.makeMenuItem(helpmenu, "Quick Guide", true, KeyEvent.VK_Q, -1, this, ev_help);
        OurUtil.makeMenuItem(helpmenu, "See the Copyright Notices...", true, KeyEvent.VK_L, -1, this, ev_license);

        // Pre-load the visualizer
        viz=new VizGUI(false, "", windowmenu2, this, evaluator);

        // Create the toolbar
        toolbar=new JToolBar();
        toolbar.setFloatable(false);
        if (!Util.onMac()) toolbar.setBackground(background);
        toolbar.add(newbutton=OurUtil.button("New","Starts a new blank model","images/24_new.gif",this,ev_new));
        toolbar.add(openbutton=OurUtil.button("Open","Opens an existing model","images/24_open.gif",this,ev_open));
        toolbar.add(loadbutton=OurUtil.button("Load","Chooses a model to analyze","images/24_open.gif",this,ev_open));
        loadbutton.setVisible(false);
        toolbar.add(savebutton=OurUtil.button("Save","Saves the current model","images/24_save.gif",this,ev_save));
        toolbar.add(runbutton=OurUtil.button("Execute","Executes the latest command","images/24_execute.gif",this,ev_runLatest));
        toolbar.add(stopbutton=OurUtil.button("Stop","Stops the current analysis","images/24_execute_abort2.gif",this,ev_stop));
        stopbutton.setVisible(false);
        toolbar.add(showbutton=OurUtil.button("Show","Shows the latest instance","images/24_graph.gif",this,ev_showLatest));
        toolbar.add(Box.createHorizontalGlue());
        toolbar.setBorder(new OurBorder(false,false,false,false));

        // Create the message area
        logpane = OurUtil.scrollpane();
        log = new SwingLogPanel(logpane, fontName, fontSize, background, Color.BLACK, new Color(.7f,.2f,.2f), this, viz);

        // Create the text area
        text = new OurTabbedEditor(this, frame, new Font(fontName, Font.PLAIN, fontSize), TabSize.get(), Dir.get());

        // Add everything to the frame, then display the frame
        Container all=frame.getContentPane();
        all.setLayout(new BorderLayout());
        all.removeAll();
        JPanel lefthalf=new JPanel();
        lefthalf.setLayout(new BorderLayout());
        lefthalf.add(toolbar, BorderLayout.NORTH);
        lefthalf.add(text.getUI(), BorderLayout.CENTER);
        splitpane = OurUtil.splitpane(JSplitPane.HORIZONTAL_SPLIT, lefthalf, logpane, width/2);
        all.add(splitpane, BorderLayout.CENTER);
        all.add(status=OurUtil.label(new Font(fontName, Font.PLAIN, fontSize)," "), BorderLayout.SOUTH);
        status.setBackground(background);
        status.setOpaque(true);
        status.setBorder(new OurBorder(true,false,false,false));

        // Generate some informative log messages
        log.logBold("Alloy Analyzer "+Version.version()+" (build date: "+Version.buildDate()+")\n\n");

        // If on Mac, then register an application listener
        if (Util.onMac()) MacUtil.registerApplicationListener(this, ev_show, ev_about, evs_open, ev_quit);

        // Add the new JNI location to the java.library.path
        try {
            System.setProperty("java.library.path", binary);
            // The above line is actually useless on Sun JDK/JRE (see Sun's bug ID 4280189)
            // The following 4 lines should work for Sun's JDK/JRE (though they probably won't work for others)
            String[] newarray = new String[]{binary};
            Field old = ClassLoader.class.getDeclaredField("usr_paths");
            old.setAccessible(true);
            old.set(null,newarray);
        } catch (Throwable ex) { }

        // Testing the platform-dependent SAT solvers
        if (1==1) {
            SatSolver now=SatSolver.get();
            satChoices = new ArrayList<SatSolver>(SatSolver.values());
            try { System.loadLibrary("minisat"); } catch(UnsatisfiedLinkError e) {
                log.logBold("Warning: the platform-specific JNI library failed.\n");
                log.log("This means you cannot use any JNI-based SAT solver.\n"+
                        "Please visit the troubleshooting page on\n"+
                        "http://alloy.mit.edu/alloy4/");
                log.logBold("\nThe exact error message is:\n");
                log.logIndented(e.toString().trim());
                log.log("\n");
                log.logDivider();
                log.flush();
                satChoices.remove(SatSolver.MiniSatJNI);
            }
            try { System.loadLibrary("minisatprover"); } catch(UnsatisfiedLinkError e) {
                satChoices.remove(SatSolver.MiniSatProverJNI);
            }
            try { System.loadLibrary("zchaff"); } catch(UnsatisfiedLinkError e) {
                satChoices.remove(SatSolver.ZChaffJNI);
            }
            Subprocess test = new Subprocess(8000, new String[]{binary+fs+"minisat1", binary+fs+"tmp.cnf"});
            String output = test.getStandardOutput();
            if (!output.startsWith("s SATISFIABLE")) {
                log.logBold("Warning: the platform-specific SAT solvers failed.\n");
                log.log("This means the only SAT solver you can use is SAT4J.\n"+
                        "Please visit the troubleshooting page on\n"+
                        "http://alloy.mit.edu/alloy4/");
                log.logBold("\nThe exact error message is:\n");
                log.logIndented(test.getStandardOutputAndError());
                log.log("\n");
                log.logDivider();
                log.flush();
                satChoices.clear();
                satChoices.add(SatSolver.SAT4J);
                satChoices.add(SatSolver.FILE);
            }
            if (!satChoices.contains(now)) now=SatSolver.ZChaffJNI;
            if (!satChoices.contains(now)) now=SatSolver.MiniSatPIPE;
            if (!satChoices.contains(now)) now=SatSolver.SAT4J;
            now.set();
        }

        // Testing the platform-dependent "dot" program
        do {
            if (!Util.onWindows()) {
                Subprocess test1 = new Subprocess(8000, new String[]{"chmod","700",binary+fs+"dotbin"});
                String out = test1.getStandardOutputAndError();
                if (out.length()>0) {
                    log.logBold("Warning: unable to install the platform-specific graph generator \"dot\".\n");
                    log.log("You can solve formulas but cannot visualize the results.\n"+
                            "Please visit the troubleshooting page on\n"+
                    "http://alloy.mit.edu/alloy4/");
                    log.logBold("\nThe exact error message is:\n");
                    log.logIndented(out);
                    log.log("\n");
                    log.logDivider();
                    log.flush();
                    break;
                }
            }
            Subprocess test2 = new Subprocess(8000, new String[]{binary+fs+"dotbin", binary+fs+"tmp.dot"}, 0);
            String out2=test2.getStandardOutput().trim();
            if (!out2.startsWith("digraph")) {
                Subprocess test3 = new Subprocess(8000, new String[]{"dot", binary+fs+"tmp.dot"}, 0);
                String out3 = test3.getStandardOutput().trim();
                if (!out3.startsWith("digraph")) {
                    log.logBold("Warning: the platform-specific graph generator \"dot\" failed.\n");
                    log.log("You can solve formulas but cannot visualize the results.\n"+
                            "Please visit the troubleshooting page on\n"+
                            "http://alloy.mit.edu/alloy4/");
                    log.logBold("\nThe exact error message is:\n");
                    out2 = test2.getStandardOutputAndError();
                    out3 = test3.getStandardOutputAndError();
                    log.logIndented(out2.length()>0 && out3.length()>0 ? (out2+"\n"+out3) : (out2+out3));
                    log.log("\n");
                    log.logDivider();
                    log.flush();
                }
            }
        } while(false);

        // If the temporary directory has become too big, then tell the user they can "clear temporary directory".
        long space = Helper.computeTemporarySpaceUsed();
        if (space<0 || space>=20*1024768) {
            if (space<0) log.logBold("Warning: Alloy4's temporary directory has exceeded 1024M.\n");
            else log.logBold("Warning: Alloy4's temporary directory now uses "+(space/1024768)+"M.\n");
            log.log("To clear the temporary directory,\n"
            +"go to the File menu and click \"Clear Temporary Directory\"\n");
            log.logDivider();
            log.flush();
        }

        // Refreshes all the menu items
        run(ev_refreshFile); OurUtil.enableAll(filemenu);
        run(ev_refreshEdit); OurUtil.enableAll(editmenu);
        run(ev_refreshRun); OurUtil.enableAll(runmenu);
        run(ev_refreshOption);
        run(ev_refreshWindow); OurUtil.enableAll(windowmenu);
        frame.setJMenuBar(bar);

        // Open the given file, if a filename is given in the command line
        if (args.length==1) {
            File f=new File(args[0]);
            if (f.exists()) run(evs_open, f.getPath());
        } else if (args.length==2 && args[0].equals("-open")) {
            File f=new File(args[1]);
            if (f.exists()) run(evs_open, f.getPath());
        }

        // Update the title and status bar
        notifyChange();

        // Start a separate thread to query alloy.mit.edu to see if an updated version of Alloy has been released or not
        Runnable r = new Runnable() {
            public final void run() {
                long now=System.currentTimeMillis();
                String result=checkForUpdate();
                if (System.currentTimeMillis()-now >= 5000 || !result.startsWith("Alloy Build ")) return;
                // Now that we're online, try to remove the old ill-conceived "Java WebStart" versions of Alloy4 (which consists of Alloy4 BETA1..BETA7)
                new Subprocess(10000, new String[]{"javaws","-silent","-offline","-uninstall","http://alloy.mit.edu/alloy4/download/alloy4.jnlp"});
                // Now, display the result of the alloy.mit.edu version polling
                SwingUtilities.invokeLater(new MultiRunner(SimpleGUI.this, evs_update, result));
            }
        };
        (new Thread(r)).start();

        // Launch the welcome screen if needed
        if (Welcome.get() < welcomeLevel) {
            String dismiss = Util.onMac() ? "Dismiss" : "Close";
            JCheckBox again = new JCheckBox("Show this message every time you start Alloy 4");
            again.setSelected(true);
            JOptionPane.showOptionDialog(frame, new Object[]{
                "Thank you for using the Alloy Analyzer 4.0.",
                " ",
                "Version 4.0 of the Alloy Analyzer is a complete rewrite,",
                "offering improvements in robustness, performance and usability.",
                "Models written in Alloy 3 will require some small alterations to run in Alloy 4.",
                " ",
                "Here are some quick tips:",
                " ",
                "* Function calls now use [ ] instead of ( )",
                "  For more details, please see http://alloy.mit.edu/alloy4/quickguide/",
                " ",
                "* The Execute button always executes the latest command.",
                "  To choose which command to execute, go to the Execute menu.",
                " ",
                "* The Alloy Analyzer comes with a variety of sample models.",
                "  To see them, go to the File menu and click Open Sample Models.",
                " ",
                "* To project over a signature in the visualizer, open the themes",
                "  panel, click on the signature you want, then click \"project\".",
                " ",
                again
            },"Welcome", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null, new Object[]{dismiss}, dismiss);
            if (!again.isSelected()) Welcome.set(welcomeLevel);
        }
    }

    //=============================================================================================================//

    private boolean prepareSubJVM() {
        if (subMemory!=subMemoryNow && subprocess!=null) { subprocess.destroy(); subprocess=null; }
        try {
            if (subprocess!=null) { subprocess.exitValue(); subprocess=null; }
        } catch(IllegalThreadStateException ex) {
            // If we can get to this line, that means the subprocess is still alive. So no work is needed.
            return true;
        }
        try {
            subprocess=Runtime.getRuntime().exec(new String[]{
                    "java",
                    "-Xmx"+subMemory+"m",
                    "-Djava.library.path="+Helper.alloyHome()+fs+"binary",
                    "-cp",
                    System.getProperty("java.class.path"),
                    "edu.mit.csail.sdg.alloy4whole.SimpleRunner",
                    Integer.toString(Version.buildNumber()),
                    Integer.toString(latestAlloyVersion),
                    latestAlloyVersionName
            });
            subMemoryNow=subMemory;
        } catch (IOException ex) {
            return false;
        }
        log.escSetProcess(subprocess);
        log.escReset();
        Thread thread1=new Thread(new OutPipe(subprocess, subprocess.getInputStream(), true));
        Thread thread2=new Thread(new OutPipe(subprocess, subprocess.getErrorStream(), false));
        thread1.start();
        thread2.start();
        return true;
    }

    //=======================================================================//

    private final class OutPipe implements Runnable {
        private final Process process;
        private final InputStream input;
        private final boolean isStdout;
        public OutPipe(Process process, InputStream input, boolean isStdout) {
            this.process=process;
            this.input=input;
            this.isStdout=isStdout;
        }
        public void run() {
            try {
                while(true) {
                    byte[] buffer=new byte[1024]; // Must re-allocate each time, since "log.esc" will return before it reads buffer
                    int n=input.read(buffer);
                    if (n<=0) break;
                    if (!isStdout) continue;
                    log.esc(process, buffer, n);
                }
            } catch (IOException ex) {
                log.escReset();
                try {
                    byte[] msg=("Error: "+ex.getMessage()+"\n").getBytes("UTF-8");
                    log.esc(process, msg, msg.length);
                } catch(UnsupportedEncodingException ex2) {}
            }
            log.esc(process, new byte[]{SwingLogPanel.FLUSH}, 1);
            Util.close(input);
            if (isStdout) SwingUtilities.invokeLater(new MultiRunner(SimpleGUI.this, ev_done));
        }
    }

}
