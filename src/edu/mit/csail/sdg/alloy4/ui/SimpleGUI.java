package edu.mit.csail.sdg.alloy4.ui;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.ArrayList;
import java.util.List;
import java.util.prefs.Preferences;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import kodkod.AlloyBridge;
import edu.mit.csail.sdg.alloy4.helper.Err;
import edu.mit.csail.sdg.alloy4.helper.Log;
import edu.mit.csail.sdg.alloy4.helper.LogToTextPane;
import edu.mit.csail.sdg.alloy4.node.ParaSig;
import edu.mit.csail.sdg.alloy4.node.Unit;
import edu.mit.csail.sdg.alloy4.node.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.parser.AlloyParser;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod;
import edu.mit.csail.sdg.alloy4.translator.TranslateAlloyToKodkod.SolverChoice;
import edu.mit.csail.sdg.alloy4.translator.ViaPipe;
import edu.mit.csail.sdg.alloy4util.Func0;
import edu.mit.csail.sdg.alloy4util.Func1;
import edu.mit.csail.sdg.alloy4util.OurDialog;
import edu.mit.csail.sdg.alloy4util.MacUtil;
import edu.mit.csail.sdg.alloy4util.OurMenu;
import edu.mit.csail.sdg.alloy4util.OurMenuBar;
import edu.mit.csail.sdg.alloy4util.OurMenuItem;
import edu.mit.csail.sdg.alloy4util.OurSplitPane;
import edu.mit.csail.sdg.alloy4util.OurUtil;
import edu.mit.csail.sdg.alloy4util.Util;
import edu.mit.csail.sdg.alloy4util.Version;
import edu.mit.csail.sdg.alloy4viz.gui.KodVizGUI;
import edu.mit.csail.sdg.alloy4viz.gui.KodVizGUIFactory;

/**
 * The GUI; except otherwise noted, methods in this class can only be called by the AWT thread.
 * @author Felix Chang
 */

public final class SimpleGUI {

    //====== static readonly fields =========================================//

    /** The current change log. */
    private static final String[] changelog = new String[]{
        "2006 Sep 29 6:30PM:",
        "  Major improvements to the look-and-feel components on Mac OS X.",
        "2006 Spe 29 10AM:",
        "  1) Unconnected Int atoms are now hidden by default.",
        "  2) Projection and Unprojection buttons are changed into hyperlinks in the titlebar.",
        "  3) Projection Panel and Tree View will now use the label from the theme.",
        "2006 Sep 21, 3:55PM:",
        "  Added a new builtin method: disj",
        "  disj is a predicate that takes 2 or more arguments.",
        "  For example, disj[x,y,z] means no(x&y) && no(y&z) && no(x&z)",
        "  Note: for historical reasons, disjoint[ ] is an alias for disj[ ]",
        "2006 Sep 21, 3:15PM:",
        "  Added a new builtin method: int",
        "  int[x] converts a set of Int atoms into a primitive integer.",
        "  The old Alloy 3 syntax ``int x'' and ``sum x'' are still allowed.",
        "  Note: for historical reasons, sum[ ] is an alias for int[ ]",
        "2006 Sep 21, 2:30PM:",
        "  Added a new builtin method: Int",
        "  Int[x] converts a primitive integer into a SIGINT atom.",
        "  The old Alloy 3 syntax ``Int x'' must now be written as ``Int[x]'' or ``x.Int''",
        "2006 Sep 21, 1:10PM:",
        "  Alloy4 Alpha1 released."
    };

    /** The icon for a "checked" menu item. */
    private static final ImageIcon iconYes=OurUtil.loadIcon("images/menu1.gif");

    /** The icon for an "unchecked" menu item. */
    private static final ImageIcon iconNo=OurUtil.loadIcon("images/menu0.gif");

    /** The system-specific file separator (forward-slash on UNIX, back-slash on Windows, etc.) */
    private static final String fs=System.getProperty("file.separator");

    /** The darker background color (for the MessageLog window and the Toolbar and the Status Bar, etc.) */
    private static final Color gray=Color.getHSBColor(0f,0f,0.90f);

    /** FileChooser filter that forces ".ALS" extension. */
    private static final FileFilter filterALS = new FileFilter() {
        @Override public final boolean accept(File f) { return !f.isFile() || f.getPath().endsWith(".als"); }
        @Override public final String getDescription() { return ".als files"; }
    };

    //====== static methods =================================================//

    /** Sets a persistent property. */
    private static void propertySet(String key, String value) {
        Preferences pref=Preferences.userNodeForPackage(SimpleGUI.class);
        pref.put(key,value);
    }

    /** Reads a persistent property. */
    private static String propertyGet(String key) {
        Preferences pref=Preferences.userNodeForPackage(SimpleGUI.class);
        return pref.get(key,"");
    }

    /** Remove the directory part from a pathname (for example, on UNIX, shortFileName("/abc/x") returns "x") */
    public static String shortFileName(String filename) {
        int index=filename.lastIndexOf(File.separatorChar);
        if (index>=0) return filename.substring(index+1); else return filename;
    }

    //====== instance fields (access to these must be synchronized) =========//

    /** The JFrame for the main window. */
    private JFrame frame;

    /** The "File", "Window", and "Run" menus. */
    private OurMenu filemenu, windowmenu, runmenu;

    /** The "Run" and "Stop" buttons. */
    private JButton runbutton, stopbutton;

    /** The JLabel that displays the current line/column position, etc. */
    private JLabel status;

    /** The JTextArea containing the editor buffer. */
    private JTextArea text;

    /** The JTextPane containing the error messages and success messages. */
    private JTextPane log;

    /** The rich styles to use when writing into the JTextPane. */
    private Style styleRegular,styleBold,styleRed,styleGreen,styleGray;

    /** The filename for the content currently in the text editor. ("" if the text editor is unnamed) */
    private String latestName="";

    /** The most-recently-used directory (this is the directory we use when launching a FileChooser next time) */
    private String fileOpenDirectory=(new File(System.getProperty("user.home"))).getAbsolutePath();

    /** The latest command executed by the user (0 if it's the first command, 1 if it's the second command...) */
    private int latestCommand=0;

    /**
     * If it's not "", then it is the first part of the filename for the latest instance.
     * <p/> In particular, latestInstance+".dot" is the DOT file, and latestInstance+".xml" is the XML file.
     */
    private String latestInstance="";

    /** This field is true iff the text in the text buffer hasn't been modified since the last time it was compiled */
    private boolean compiled=false;

    /** This field is true iff the text in the text buffer hasn't been modified since the last time it was saved */
    private boolean modified=false;

    /** The separate thread that is running the SAT solver (null if there is no SAT solver currently running) */
    private Thread current_thread=null;

    /** The current choice of SAT solver. */
    private SolverChoice satOPTION=SolverChoice.MiniSatSimpPIPE;

    /** The factory that constructs Visualizer windows. */
    private KodVizGUIFactory factory;

    //====== helper methods =================================================//

    /** Inserts "filename" as into the "recently opened file list". */
    private synchronized void addHistory(String filename) {
        String name0=propertyGet("history0"), name1=propertyGet("history1"), name2=propertyGet("history2");
        if (name0.equals(filename)) return; else {propertySet("history0",filename); propertySet("history1",name0);}
        if (name1.equals(filename)) return; else propertySet("history2",name1);
        if (name2.equals(filename)) return; else propertySet("history3",name2);
    }

    /** Updates the status bar at the bottom of the screen. */
    private synchronized final void my_caret() {
        try {
            if (Util.onMac()) {
                if (modified) frame.getRootPane().putClientProperty("windowModified",Boolean.TRUE);
                else frame.getRootPane().putClientProperty("windowModified",Boolean.FALSE);
            }
            int c=text.getCaretPosition();
            int y=text.getLineOfOffset(c)+1;
            int x=c-text.getLineStartOffset(y-1)+1;
            status.setText("<html>&nbsp; Line "+y+", Column "+x+(modified?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        } catch(BadLocationException ex) {
            status.setText("<html>&nbsp; Line ?, Column ?"+(modified?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        }
    }

    /**
     * If the text editor content is not modified since the last save, return true; otherwise, ask the user.
     * <p/> If the user says to save it, we will attempt to save it, then return true iff the save succeeded.
     * <p/> If the user says to discard it, this method returns true.
     * <p/> If the user says to cancel it, this method returns false.
     */
    private synchronized final boolean my_confirm() {
        if (!modified) return true;
        Boolean ans=OurDialog.questionSaveDiscardCancel(frame);
        if (ans==null) return false;
        if (!ans.booleanValue()) return true; else return a_save.run();
    }

    /** Call SwingUtilities.invokeLater() on the runnable. */
    private static void invokeLater(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) { r.run(); return; }
        SwingUtilities.invokeLater(r);
    }

    /** Call SwingUtilities.invokeLater() on the function. */
    private static void invokeLater(final Func0 func) {
        if (SwingUtilities.isEventDispatchThread()) { func.run(); return; }
        SwingUtilities.invokeLater(new Runnable() { public final void run() { func.run(); }});
    }

    /** Call SwingUtilities.invokeLater() on the function with the given argument. */
    private static void invokeLater(final Func1 func, final String arg) {
        if (SwingUtilities.isEventDispatchThread()) { func.run(arg); return; }
        SwingUtilities.invokeLater(new Runnable() { public final void run() { func.run(arg); }});
    }

    /** Print a text message into the log window using the provided style.
     * <br/> NOTE: this method can be called by any thread (hence the extra precaution) */
    private synchronized void log(final String msg, final Style style) {
        if (!SwingUtilities.isEventDispatchThread()) {
            invokeLater(new Runnable() { public final void run() { log(msg,style); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        String realmsg = (doc.getLength()==0) ? msg.trim() : msg;
        try { doc.insertString(doc.getLength(), realmsg+"\n", style); } catch (BadLocationException e) { }
        log.setCaretPosition(doc.getLength());
    }

    /** Print msg1 into the log window using the provided style, then print msg2 using the default style.
     * <br/> NOTE: this method can be called by any thread (hence the extra precaution) */
    private synchronized void log(final String msg1, final Style style1, final String msg2) {
        if (!SwingUtilities.isEventDispatchThread()) {
            invokeLater(new Runnable() { public final void run() { log(msg1,style1,msg2); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        String realmsg1 = (doc.getLength()==0) ? msg1.trim() : msg1;
        try {
            doc.insertString(doc.getLength(),realmsg1,style1);
            doc.insertString(doc.getLength(),msg2+"\n",styleRegular);
        } catch (BadLocationException e) { }
        log.setCaretPosition(doc.getLength());
    }

    /**
     * Add a "visualize this" button to the log window.
     * <br/> NOTE: this method can be called by any thread (hence the extra precaution).
     * @param label - the label to show on the button
     * @param dotName - the DOT filename corresponding to the instance to show
     * @param xmlName - the XML filename corresponding to the instance to show
     */
    private synchronized void logButton(final String label, final String dotName, final String xmlName) {
        if (!SwingUtilities.isEventDispatchThread()) {
            invokeLater(new Runnable() { public final void run() { logButton(label,dotName,xmlName); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JButton b=new JButton(label);
        if (Util.onMac()) b.setBackground(gray);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont());
        b.setForeground(Color.BLUE);
        b.addActionListener(new ActionListener(){
            public final void actionPerformed(ActionEvent e) {
                factory.create(dotName, new File(xmlName), frame);
            }
        });
        StyleConstants.setComponent(s,b);
        log("",s);
    }

    /**
     * Add a "visualize this" hyperlink to the log window.
     * <br/> NOTE: this method can be called by any thread (hence the extra precaution).
     * @param label - the label to show on the link
     * @param dotName - the DOT filename corresponding to the instance to show
     * @param xmlName - the XML filename corresponding to the instance to show
     */
    private synchronized void logLink(final String label, final String dotName, final String xmlName) {
        if (!SwingUtilities.isEventDispatchThread()) {
            invokeLater(new Runnable() { public final void run() { logLink(label,dotName,xmlName); } });
            return;
        }
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JLabel b=new JLabel(label);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(OurUtil.getFont());
        b.setForeground(Color.BLUE);
        b.addMouseListener(new MouseListener(){
            public final void mouseClicked(MouseEvent e) { factory.create(dotName, new File(xmlName), frame); }
            public final void mousePressed(MouseEvent e) { }
            public final void mouseReleased(MouseEvent e) { }
            public final void mouseEntered(MouseEvent e) { }
            public final void mouseExited(MouseEvent e) { }
        });
        StyleConstants.setComponent(s, b);
        log("",s);
    }

    //====== Message handlers ===============================================//

    /**
     * Called then the user expands the "File" menu.
     * @return true
     */
    private final Func0 a_file = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                boolean hasEntries=false;
                int n=filemenu.getItemCount();
                for(int i=0; i<n; i++) {
                    // When we've found the separator (which is always null), we delete it and all entries after it.
                    if (filemenu.getItem(i)==null) { while(i<n) {filemenu.remove(i); n--;} break; }
                }
                for(int i=0; i<=3; i++) {
                    final String name = propertyGet("history"+i);
                    if (name.length()==0) continue;
                    if (!hasEntries) { hasEntries=true; filemenu.addSeparator(); }
                    JMenuItem x=new JMenuItem(name);
                    x.addActionListener(new ActionListener() {
                        public final void actionPerformed(ActionEvent e) {if (my_confirm()) a_openFileIfOk.run(name);}
                    });
                    filemenu.add(x);
                }
            }
            return true;
        }
    };

    /**
     * Called when the user clicks "File", then "New".
     * @return true
     */
    private final Func0 a_new = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (!my_confirm()) return false;
                latestName="";
                text.setText("");
                frame.setTitle("Alloy4: build date "+Version.buildDate());
                compiled=false;
                if (modified) {modified=false; my_caret();}
            }
            return true;
        }
    };

    /**
     * Called when the user requests that a specific file should be opened; this method will check
     * if the current text editor needs to be saved or not.
     * @return true iff the file was opened successfully.
     */
    private final Func1 a_openFileIfOk = new Func1() {
        public final boolean run(String arg) {
            synchronized (SimpleGUI.this) {
                if (!my_confirm()) return false;
                return a_openFile.run(arg);
            }
        }
    };

    /**
     * Called when the user requests that a specific file should be opened.
     * @return true iff the file was opened successfully.
     */
    private final Func1 a_openFile = new Func1() {
        public final boolean run(String f) {
            synchronized (SimpleGUI.this) {
                try {
                    // The following is needed, in case this message came from another window.
                    frame.setExtendedState(JFrame.NORMAL); frame.requestFocus(); frame.toFront();
                    FileReader fr=new FileReader(f);
                    BufferedReader br=new BufferedReader(fr);
                    StringBuffer sb=new StringBuffer();
                    while(true) { String s=br.readLine(); if (s==null) break; sb.append(s); sb.append('\n'); }
                    br.close();
                    fr.close();
                    text.setText(sb.toString());
                    text.setCaretPosition(0);
                    log("\nFile \""+shortFileName(f)+"\" successfully loaded.", styleGreen);
                    frame.setTitle("Alloy File: "+f);
                    latestName=f;
                    addHistory(f);
                    compiled=false;
                    if (modified) {modified=false; my_caret();}
                    // The following is needed, in case this message came from another window.
                    frame.setExtendedState(JFrame.NORMAL); frame.requestFocus(); frame.toFront();
                } catch(FileNotFoundException e) { log("\nCannot open the file! "+e.toString(), styleGreen); return false;
                } catch(IOException e) { log("\nCannot open the file! "+e.toString(), styleGreen); return false;
                }
            }
            return true;
        }
    };

    /**
     * Called when the user clicks "File", then "Open".
     * @return true iff a file was chosen and opened successfully.
     */
    private final Func0 a_open = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (!my_confirm()) return false;
                JFileChooser open=new JFileChooser(fileOpenDirectory);
                open.setFileFilter(filterALS);
                if (open.showOpenDialog(frame)!=JFileChooser.APPROVE_OPTION) return false;
                fileOpenDirectory=open.getSelectedFile().getParent();
                return a_openFile.run(open.getSelectedFile().getPath());
            }
        }
    };

    /**
     * Called when the user clicks "File", then "Open Builtin Models".
     * @return true iff a file was chosen and opened successfully.
     */
    private final Func0 a_openBuiltin = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (!my_confirm()) return false;
                JFileChooser open=new JFileChooser(Util.alloyHome()+File.separatorChar+"models");
                open.setFileFilter(filterALS);
                if (open.showOpenDialog(frame)!=JFileChooser.APPROVE_OPTION) return false;
                return a_openFile.run(open.getSelectedFile().getPath());
            }
        }
    };

    /**
     * Called when the user requests that the text editor content be saved to a specific filename.
     * @return true iff the file was saved successfully.
     */
    private final Func1 a_saveFile = new Func1() {
        public final boolean run(String filename) {
            synchronized (SimpleGUI.this) {
                try {
                    FileWriter fw=new FileWriter(filename);
                    BufferedWriter bw=new BufferedWriter(fw);
                    PrintWriter out=new PrintWriter(bw);
                    out.println(text.getText());
                    out.flush();
                    out.close();
                    bw.close();
                    fw.close();
                    if (modified) {modified=false; my_caret();}
                    log("\nFile \""+shortFileName(filename)+"\" successfully saved.", styleGreen);
                    latestName=filename;
                    addHistory(filename);
                    frame.setTitle("Alloy File: "+latestName);
                } catch(IOException e) {
                    log("\nCannot write to the file \""+filename+"\"! "+e.toString(), styleRed);
                    return false;
                }
            }
            return true;
        }
    };

    /**
     * Called when the user clicks "File", then "Save As".
     * @return true iff a file was chosen and saved successfully.
     */
    private final Func0 a_saveAs = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                JFileChooser open=new JFileChooser(fileOpenDirectory);
                open.setFileFilter(filterALS);
                if (open.showSaveDialog(frame)!=JFileChooser.APPROVE_OPTION) return false;
                File file=open.getSelectedFile();
                String filename=file.getAbsolutePath();
                if (file.exists() && !OurDialog.questionOverwrite(frame,filename)) return false;
                if (!a_saveFile.run(filename)) return false;
                fileOpenDirectory=open.getSelectedFile().getParent();
                return true;
            }
        }
    };

    /**
     * Called when the user clicks "File", then "Save".
     * @return true iff the file was saved successfully.
     */
    private final Func0 a_save = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (latestName.length()>0) return a_saveFile.run(latestName); else return a_saveAs.run();
            }
        }
    };

    /**
     * Called when the user clicks "File", then "Quit".
     * @return false iff the user cancels the Quit operation.
     */
    private final Func0 a_quit = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (my_confirm()) System.exit(0);
                return false;
            }
        }
    };

    /**
     * Called when we need to recompile the current text buffer (to see how many commands there are).
     * @return nonnull iff the current text buffer is successfully parsed (the resulting Unit object is returned)
     */
    private synchronized Unit tryCompile() {
        Unit u;
        try {
            runmenu.removeAll();
            Reader isr=new StringReader(text.getText());
            u=AlloyParser.alloy_parseStream(isr);
            if (u.runchecks.size()==0) {
                runmenu.add(new JMenuItem("There are no commands in this model!"));
                return u;
            }
            if (u.runchecks.size()>1) {
                JMenuItem y=new JMenuItem("All");
                y.addActionListener(new RunListener(-1));
                runmenu.add(y);
                runmenu.add(new JSeparator());
            }
            for(int i=0; i<u.runchecks.size(); i++) {
                JMenuItem y=new JMenuItem(u.runchecks.get(i).toString());
                y.addActionListener(new RunListener(i));
                runmenu.add(y);
            }
            return u;
        }
        catch(Err e) {
            runmenu.add(new JMenuItem("Cannot run any commands! See the error message for details!"));
            if (e.pos!=null && e.pos.y>0 && e.pos.x>0) try {
                int c=text.getLineStartOffset(e.pos.y-1)+e.pos.x-1;
                text.setSelectionStart(c);
                text.setSelectionEnd(c+1);
            } catch(BadLocationException ex) {}
            String msg=e.toString();
            if (msg.matches("^.*There are [0-9]* possible tokens that can appear here:.*$")) {
                // Special handling, to display that particular message in a clearer style.
                String head=msg.replaceAll("^(.*There are [0-9]* possible tokens that can appear here:).*$","$1");
                String tail=msg.replaceAll("^.*There are [0-9]* possible tokens that can appear here:(.*)$","$1");
                log("\nCannot parse the model! "+head, styleRed, tail);
            }
            else log("\nCannot parse the model! "+e.toString(), styleRed);
            return null;
        }
        catch(Exception e) {
            runmenu.add(new JMenuItem("Cannot run any commands! See the error message for details!"));
            log("\nCannot parse the model! "+e.toString(), styleRed);
            return null;
        }
    }

    /**
     * Called when the user expands the "Run" menu.
     * @return true
     */
    private final Func0 a_run = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (compiled) return true;
                Unit u=tryCompile();
                if (u!=null) {
                    compiled=true;
                    log("\nParser succeeded: there are "+u.runchecks.size()+" command(s) in this model.", styleGreen);
                }
                return true;
            }
        }
    };

    /**
     * Called when the user clicks the "Run the latest command" button.
     * @return true
     */
    private final Func0 a_runLatest = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                Unit u=tryCompile();
                if (u!=null) {
                    if (latestCommand>=u.runchecks.size()) latestCommand=u.runchecks.size()-1;
                    if (latestCommand<0) {latestCommand=0;log("\nThere are no commands in this model!", styleRed);}
                    else (new RunListener(latestCommand)).actionPerformed(null);
                }
                return true;
            }
        }
    };

    /**
     * Called when the user clicks the "Stop" button.
     * @return true
     */
    private final Func0 a_stop = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                if (current_thread!=null) { AlloyBridge.stopped=true; ViaPipe.forceTerminate(); }
                return true;
            }
        }
    };

    /**
     * Called when the user clicks the "Show the latest instance" button.
     * @return true iff there was a latest instance to show.
     */
    private final Func0 a_showLatestInstance = new Func0() {
        public final boolean run() {
            String filename;
            synchronized (SimpleGUI.this) { filename=latestInstance; }
            if (filename.length()==0) {
                    log("\nNo previous instances are available for viewing.", styleRed); return false;
            } else {
                    factory.create(filename+".dot", new File(filename+".xml"), frame); return true;
            }
        }
    };

    /**
     * Called when the user wishes to bring this window to the foreground.
     * @return true
     */
    private final Func0 a_windowRaise = new Func0() {
        public final boolean run() {
            JFrame topframe;
            synchronized(SimpleGUI.this) { topframe=frame; }
            topframe.setExtendedState(JFrame.NORMAL);
            topframe.requestFocus();
            topframe.toFront();
            return true;
        }
    };

    /**
     * Called when the user expands the "Window" menu.
     * @return true
     */
    private final Func0 a_window = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                windowmenu.removeAll();
                JMenuItem et=new JMenuItem("Editor Window");
                et.setIcon(iconYes);
                et.addActionListener(new ActionListener() {
                    public final void actionPerformed(ActionEvent e) {
                        frame.setExtendedState(JFrame.NORMAL); frame.requestFocus(); frame.toFront();
                    }
                });
                windowmenu.add(et);
                for(final KodVizGUI g: factory.windowList()) {
                    et=new JMenuItem(g.getTitle());
                    et.setIcon(iconNo);
                    et.addActionListener(new ActionListener() {
                        public final void actionPerformed(ActionEvent e) {
                            g.setExtendedState(JFrame.NORMAL); g.requestFocus(); g.toFront();
                        }
                    });
                    windowmenu.add(et);
                }
                return true;
            }
        }
    };

    /**
     * Called when the user clicks "Help", then "About".
     * @return true
     */
    private final Func0 a_showAbout = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                Icon icon=OurUtil.loadIcon("images/logo.gif");
                Object[] array = {
                        icon,
                        "Thank you for using Alloy 4 (build date "+Version.buildDate()+").",
                        "If you have any suggestions or bug reports, please visit our website: alloy.mit.edu",
                        " ",
                        "If you installed Alloy4 using WebStart, it should have created an Alloy4 icon",
                        "on your desktop. Clicking on it will load the Alloy4 graphical interface.",
                        " ",
                        "If you installed the jar file manually, please see alloy.mit.edu for more",
                        "information on the available command line options.",
                        " ", "Thank you."};
                OurDialog.alert(frame, array, "About Alloy 4");
                return true;
            }
        }
    };

    /**
     * Called when the user clicks "Help", then "Show the change log".
     * @return true
     */
    private final Func0 a_showChangeLog = new Func0() {
        public final boolean run() {
            synchronized (SimpleGUI.this) {
                int screenWidth=OurUtil.getScreenWidth(), width=screenWidth/3*2;
                int screenHeight=OurUtil.getScreenHeight(), height=screenHeight/3*2;
                JTextPane log=new JTextPane();
                log.setBackground(gray);
                log.setEditable(false);
                StyledDocument doc=log.getStyledDocument();
                Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
                Style styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
                Style styleBold=doc.addStyle("bold", styleRegular); StyleConstants.setBold(styleBold, true);
                Style styleGreen=doc.addStyle("green", styleBold); StyleConstants.setForeground(styleGreen, new Color(0.2f,0.7f,0.2f));
                for(int i=0; i<changelog.length; i++) {
                    try {
                        if (changelog[i].startsWith("20"))
                            doc.insertString(doc.getLength(), (i==0?"":"\n")+changelog[i]+"\n", styleGreen);
                        else
                            doc.insertString(doc.getLength(), changelog[i]+"\n", styleRegular);
                    } catch(BadLocationException ex) {
                        // This should not happen
                    }
                }
                log.setCaretPosition(0);
                JScrollPane textPane=new JScrollPane(log,
                        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                textPane.setMinimumSize(new Dimension(50, 50));
                final JFrame frame=new JFrame("Alloy change log");
                frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                Container all=frame.getContentPane();
                all.setLayout(new BorderLayout());
                all.add(textPane, BorderLayout.CENTER);
                if (Util.onMac()) all.add(new JLabel(" "), BorderLayout.SOUTH); // Make room for the Mac "grow box"
                frame.pack();
                frame.setSize(new Dimension(width,height));
                frame.setLocation(screenWidth/6, screenHeight/6);
                frame.setVisible(true);
                return true;
            }
        }
    };

    //====== Constructor ====================================================//

    /** The constructor; this method will be called by the AWT thread, using the "invokeLater" method. */
    private SimpleGUI(String[] args) {

        // Enable better look-and-feel
        System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Alloy 4");
        System.setProperty("com.apple.mrj.application.growbox.intrudes","true");
        System.setProperty("com.apple.mrj.application.live-resize","true");
        System.setProperty("com.apple.macos.useScreenMenuBar","true");
        System.setProperty("apple.laf.useScreenMenuBar","true");
        try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()); } catch (Exception e) { }

        // Find out the appropriate Alloy directory
        final String alloyHome=Util.alloyHome();
        final String binary=alloyHome+fs+"binary";

        // Copy the JAR file
        Util.copy(false, alloyHome, "alloy4.jar");

        // Copy the JNI files
        Util.copy(true,binary,"libminisat.so", "libminisat4.so", "libminisat6.so", "libminisat.jnilib");
        Util.copy(true,binary,"libzchaff_basic.so", "libzchaff_basic4.so", "libzchaff_basic6.so", "libzchaff_basic.jnilib");
        Util.copy(false,binary,"minisat.dll", "zchaff_basic.dll");

        // Copy the platform-dependent binaries
        Util.copy(true, binary,"dotbin6", "minisatsimp6", "minisatcore6", "minisat6", "berkmin6");
        Util.copy(true, binary,"dotbin4", "minisatsimp4", "minisatcore4", "minisat4", "berkmin4");
        Util.copy(true, binary,"dotbin", "minisatsimp", "minisatcore", "minisat", "berkmin");
        Util.copy(false,binary,"dotbin.exe", "minisatsimp.exe", "minisatcore.exe", "minisat.exe", "berkmin.exe");
        Util.copy(false,binary,"jpeg.dll","libexpat.dll","libexpatw.dll","zlib1.dll","z.dll","freetype6.dll","png.dll");

        // Copy the model files
        Util.copy(false,alloyHome,
                "models/examples/algorithms/dijkstra.als",
                "models/examples/algorithms/messaging.als",
                "models/examples/algorithms/opt_spantree.als",
                "models/examples/algorithms/peterson.als",
                "models/examples/algorithms/ringlead.als",
                "models/examples/algorithms/s_ringlead.als",
                "models/examples/algorithms/stable_mutex_ring.als",
                "models/examples/algorithms/stable_orient_ring.als",
                "models/examples/algorithms/stable_ringlead.als",
                "models/examples/case_studies/INSLabel.als",
                "models/examples/case_studies/chord.als",
                "models/examples/case_studies/chord2.als",
                "models/examples/case_studies/chordbugmodel.als",
                "models/examples/case_studies/com.als",
                "models/examples/case_studies/firewire.als",
                "models/examples/case_studies/ins.als",
                "models/examples/case_studies/iolus.als",
                "models/examples/case_studies/sync.als",
                "models/examples/case_studies/syncimpl.als",
                "models/examples/puzzles/farmer.als",
                "models/examples/puzzles/handshake.als",
                "models/examples/puzzles/hanoi.als",
                "models/examples/systems/file_system.als",
                "models/examples/systems/javatypes_soundness.als",
                "models/examples/systems/lists.als",
                "models/examples/systems/marksweepgc.als",
                "models/examples/systems/views.als",
                "models/examples/toys/birthday.als",
                "models/examples/toys/ceilingsAndFloors.als",
                "models/examples/toys/genealogy.als",
                "models/examples/toys/grandpa.als",
                "models/examples/toys/javatypes.als",
                "models/examples/toys/life.als",
                "models/examples/toys/numbering.als",
                "models/examples/toys/railway.als",
                "models/examples/toys/trivial.als",
                "models/examples/tutorial/farmer.als",
                "models/util/boolean.als",
                "models/util/graph.als",
                "models/util/integer.als",
                "models/util/natural.als",
                "models/util/ordering.als",
                "models/util/relation.als",
                "models/util/seqrel.als",
                "models/util/sequence.als",
                "models/util/ternary.als"
        );

        // Construct the JFrame object
        int screenWidth=OurUtil.getScreenWidth(), width=screenWidth/10*8;
        int screenHeight=OurUtil.getScreenHeight(), height=screenHeight/10*8;
        frame=new JFrame("Alloy4: build date "+Version.buildDate());
        factory=new KodVizGUIFactory(false);

        // Create the menu bar
        OurMenuBar bar=new OurMenuBar();
        frame.setJMenuBar(bar);

        if (1==1) { // File menu
            filemenu = bar.addMenu("File", true, KeyEvent.VK_F, a_file);
            filemenu.addMenuItem(null, "New",                     true, KeyEvent.VK_N, KeyEvent.VK_N, a_new);
            filemenu.addMenuItem(null, "Open",                    true, KeyEvent.VK_O, KeyEvent.VK_O, a_open);
            filemenu.addMenuItem(null, "Open Builtin Models",     true, KeyEvent.VK_B, -1,            a_openBuiltin);
            filemenu.addMenuItem(null, "Save",                    true, KeyEvent.VK_S, KeyEvent.VK_S, a_save);
            filemenu.addMenuItem(null, "Save As",                 true, KeyEvent.VK_A, -1,            a_saveAs);
            if (!Util.onMac()) filemenu.addMenuItem(null, "Quit", true, KeyEvent.VK_Q, -1,            a_quit);
        }

        if (1==1) { // Run menu
            runmenu = bar.addMenu("Run", true, KeyEvent.VK_R, a_run);
        }

        if (1==1) { // Options menu
            Error ex=null;
            List<SolverChoice> choices=new ArrayList<SolverChoice>();
            for(SolverChoice sc:SolverChoice.values()) choices.add(sc);
            try {               System.load(binary+fs+"libminisat6.so");    ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat4.so");    ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat.so");     ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat.jnilib"); ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"minisat.dll");       ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) choices.remove(SolverChoice.MiniSatJNI);
            try {               System.load(binary+fs+"libzchaff_basic6.so");    ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic4.so");    ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic.so");     ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic.jnilib"); ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"zchaff_basic.dll");       ex=null; } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) choices.remove(SolverChoice.ZChaffJNI);
            final OurMenu optmenu = bar.addMenu("Options", true, KeyEvent.VK_O, null);
            for(final SolverChoice sc:choices) {
                final OurMenuItem item = optmenu.addMenuItem(sc==satOPTION?iconYes:iconNo, "Use "+sc, true, null);
                item.addActionListener(new ActionListener() {
                    public final void actionPerformed(ActionEvent e) {
                        synchronized(SimpleGUI.this) {
                            optmenu.setIconForChildren(iconNo); item.setIcon(iconYes); satOPTION=sc;
                        }
                    }
                });
            }
        }

        if (1==1) { // Window menu
            windowmenu=bar.addMenu("Window", true, KeyEvent.VK_W, a_window);
        }

        if (1==1) { // Help menu
            OurMenu helpmenu = bar.addMenu("Help", true, KeyEvent.VK_H, null);
            if (!Util.onMac()) helpmenu.addMenuItem(null, "About Alloy4", true, KeyEvent.VK_A, -1, a_showAbout);
            helpmenu.addMenuItem(null, "See the Change Log", true, KeyEvent.VK_V, -1, a_showChangeLog);
        }

        // Create the toolbar
        JButton showbutton;
        JToolBar toolbar=new JToolBar();
        toolbar.setFloatable(false);
        if (!Util.onMac()) toolbar.setBackground(gray);
        toolbar.add(OurUtil.makeJButton("New","Starts a new blank model","images/24_new.gif", a_new));
        toolbar.add(OurUtil.makeJButton("Open","Opens an existing model","images/24_open.gif", a_open));
        toolbar.add(OurUtil.makeJButton("Save","Saves the current model","images/24_save.gif", a_save));
        toolbar.add(runbutton=OurUtil.makeJButton("Run","Runs the latest command","images/24_execute.gif", a_runLatest));
        toolbar.add(stopbutton=OurUtil.makeJButton("Stop","Stops the current analysis","images/24_execute_abort2.gif", a_stop));
        toolbar.add(showbutton=OurUtil.makeJButton("Visualize","Visualize the latest instance","images/24_graph.gif", a_showLatestInstance));
        toolbar.add(Box.createHorizontalGlue());
        runbutton.setMnemonic(KeyEvent.VK_N);
        showbutton.setMnemonic(KeyEvent.VK_V);
        stopbutton.setVisible(false);

        // Create the text editor
        text=new JTextArea();
        text.setLineWrap(false);
        text.setEditable(true);
        text.setTabSize(3);
        text.setFont(OurUtil.getFont());
        text.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) {my_caret();}
        });
        text.getDocument().addDocumentListener(new DocumentListener(){
            public final void insertUpdate(DocumentEvent e) {
                synchronized(SimpleGUI.this) { compiled=false; if (!modified) {modified=true;my_caret();} }
            }
            public final void removeUpdate(DocumentEvent e) {
                synchronized(SimpleGUI.this) { compiled=false; if (!modified) {modified=true;my_caret();} }
            }
            public final void changedUpdate(DocumentEvent e) {
                synchronized(SimpleGUI.this) { compiled=false; if (!modified) {modified=true;my_caret();} }
            }
        });
        JComponent textPane = OurUtil.makeJScrollPane(text);

        // Create the message area
        log=new JTextPane();
        log.setBackground(gray);
        log.setEditable(false);
        log.setFont(OurUtil.getFont());
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        styleBold=doc.addStyle("bold", styleRegular); StyleConstants.setBold(styleBold, true);
        styleGreen=doc.addStyle("green", styleBold); StyleConstants.setForeground(styleGreen, new Color(0.2f,0.7f,0.2f));
        styleRed=doc.addStyle("red", styleBold); StyleConstants.setForeground(styleRed, new Color(0.7f,0.2f,0.2f));
        styleGray=doc.addStyle("gray", styleBold); StyleConstants.setBackground(styleGray, new Color(0.8f,0.8f,0.8f));
        JScrollPane statusPane=new JScrollPane(log,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setMinimumSize(new Dimension(50, 50));

        // Add everything to the frame, then display the frame
        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter() {
            public final void windowClosing(WindowEvent e) { if (my_confirm()) System.exit(0); }
        });
        Container all=frame.getContentPane();
        all.setLayout(new BorderLayout());
        JPanel lefthalf=new JPanel();
        lefthalf.setLayout(new BorderLayout());
        lefthalf.add(toolbar, BorderLayout.NORTH);
        lefthalf.add(textPane, BorderLayout.CENTER);
        all.add(new OurSplitPane(JSplitPane.HORIZONTAL_SPLIT, lefthalf, statusPane, width/2), BorderLayout.CENTER);
        all.add(status=OurUtil.makeJLabel(" ",OurUtil.getFont()), BorderLayout.SOUTH);
        status.setBackground(gray);
        status.setOpaque(true);
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setLocation(screenWidth/10, screenHeight/10);
        frame.setVisible(true);

        // Generate some informative log messages
        log("Alloy4: build date "+Version.buildDate(), styleGreen);
        log("\nSolver: "+satOPTION, styleGreen);
        if (Util.onMac()) {
            log("\nMac OS X detected.", styleGreen);
            MacUtil.addApplicationListener(a_showAbout, a_openFileIfOk, a_quit);
        }

        // log("\nCurrent directory = " + (new File(".")).getAbsolutePath(), styleGreen);
        // On Mac, it will be the directory that contains "Alloy4.app". Problem: people can RENAME "Alloy4.app"...
        //log("ARGS = "+args.length+"\n", styleGreen);
        //for(String a:args) log("# = "+a+"\n",styleGreen);
        // If commandline tells you to load a file, load it.

        // Open the given file, if a filename is given in the command line
        if (args.length==1) {
            File f=new File(args[0]);
            if (f.exists()) a_openFile.run(f.getAbsolutePath());
        } else if (args.length==2 && args[0].equals("-open")) {
            File f=new File(args[1]);
            if (f.exists()) a_openFile.run(f.getAbsolutePath());
        }

        // Start a separate thread to watch for other invocations of Alloy4
        Runnable r = new Runnable() {
            public final void run() {
                while(true) {
                    try {
                        String msg=Util.lockThenReadThenErase(Util.alloyHome()+File.separatorChar+"msg");
                        if (msg.equals("open:")) invokeLater(a_windowRaise);
                        else if (msg.startsWith("open:")) invokeLater(a_openFileIfOk,msg.substring(5));
                    } catch(IOException e) { }
                    try {Thread.sleep(400);} catch (InterruptedException e) {}
                }
            }
        };
        Thread t=new Thread(r);
        t.start();
    }

    //=======================================================================//

    @SuppressWarnings("unused")
    /** The lock object is static, to ensure that it does not get garbage-collected. */
    private static FileLock lock=null;

    /** Main method that launches the program; this method might be called by an arbitrary thread. */
    public static final void main(final String[] args) {
        try {
            File lockfile = new File(Util.alloyHome() + File.separatorChar + "lock");
            FileChannel lockchannel = new RandomAccessFile(lockfile, "rw").getChannel();
            lock = lockchannel.tryLock();
            if (lock==null) {
                // If Alloy4 is already launched, then write the commandline argument into a msg file then exit.
                // The existing Alloy4 will watch this msg file, and responds accordingly when it is nonempty.
                String dest = Util.alloyHome() + File.separatorChar + "msg";
                if (args.length==1) {
                    File f=new File(args[0]);
                    if (f.exists()) Util.lockThenWrite(dest, "open:"+f.getAbsolutePath());
                } else if (args.length==2 && args[0].equals("-open")) {
                    File f=new File(args[1]);
                    if (f.exists()) Util.lockThenWrite(dest, "open:"+f.getAbsolutePath());
                } else {
                    Util.lockThenWrite(dest, "open:");
                }
                return;
            }
        } catch(IOException ex) {
            OurDialog.fatal(null, "A fatal error has occurred! "+ex.getMessage());
        }
        SwingUtilities.invokeLater(new Runnable() {
            public final void run() { new SimpleGUI(args); }
        });
    }

    //=======================================================================//

    /** This Runnable is used to execute a SAT query; this allows the main GUI to remain responsive. */
    private final class Runner implements Runnable {
        /** The Alloy model to be anaylzed */
        private final Reader source;
        /** The command that this runner should run (0 is first, 1 is second..) (-1 means all of them) */
        private final int index;
        /** Constructs a runner that runs the i-th command (0 is first, 1 is second...) (-1 means all of them) */
        public Runner(Reader source, int i) { this.source=source; this.index=i; }
        /** The run() method that actually runs the SAT query. */
        public void run() {
            try {
                // It's safe to pass "log" to "reallog", since the LogToTextPane class
                // is careful about only doing GUI operations via the AWT thread.
                Log reallog=new LogToTextPane(log,styleRegular,styleGreen);
                Log blanklog=new Log();
                ArrayList<Unit> units=AlloyParser.alloy_totalparseStream(Util.alloyHome(), source);
                ArrayList<ParaSig> sigs=VisitTypechecker.check(blanklog, units);
                String tempdir = Util.maketemp();
                SolverChoice satopt;
                synchronized (SimpleGUI.this) { satopt=satOPTION; }
                List<TranslateAlloyToKodkod.Result> result=TranslateAlloyToKodkod.codegen(index,reallog,units,sigs,satopt,tempdir);
                reallog.flush(); // To make sure everything is flushed.
                (new File(tempdir)).delete(); // In case it was UNSAT, or was TRIVIALLY SAT, or cancelled.
                if (result.size()==1 && result.get(0)==TranslateAlloyToKodkod.Result.SAT) {
                    logButton("Click here to display this instance", tempdir+fs+(index+1)+".dot", tempdir+fs+(index+1)+".xml");
                    synchronized (SimpleGUI.this) { latestInstance=tempdir+fs+(index+1); }
                }
                if (result.size()>1) {
                    log("\n" + result.size() + " commands were completed. The results are:", styleGreen);
                    int i=0;
                    for(TranslateAlloyToKodkod.Result b:result) {
                        i++;
                        if (b==null) {log("#"+i+": CANCELED", styleRegular); continue;}
                        switch(b) {
                        case SAT:
                            logLink("#"+i+": SAT (Click here to see the instance)", tempdir+fs+i+".dot", tempdir+fs+i+".xml");
                            synchronized (SimpleGUI.this) { latestInstance=tempdir+fs+i; }
                            break;
                        case TRIVIALLY_SAT: log("#"+i+": SAT (trivially SAT)", styleRegular); break;
                        case UNSAT: case TRIVIALLY_UNSAT: log("#"+i+": UNSAT", styleRegular); break;
                        default: log("#"+i+": CANCELED", styleRegular);
                        }
                    }
                }
            }
            catch(UnsatisfiedLinkError e) {
                log("\nCannot run the command! The required JNI library cannot be found! "+e.toString(), styleRed);
            }
            catch(Err e) {
                log("\nCannot run the command! "+e.toString(), styleRed);
            }
            invokeLater(new Runnable() {
                public final void run() {
                    runmenu.setEnabled(true);
                    runbutton.setVisible(true);
                    stopbutton.setVisible(false);
                    synchronized(SimpleGUI.this) {current_thread=null;}
                }
            });
        }
    }

    //=======================================================================//

    /** This ActionListener is called when the user indicates a particular command to execute. */
    private class RunListener implements ActionListener {
        /** The index number of the command that the user wishes to execute (0..) (-1 means ALL of them). */
        private final int index;
        /** The constructor. */
        public RunListener(int index) {this.index=index;}
        /** The event handler that gets called when the user clicked on one of the menu item. */
        public void actionPerformed(ActionEvent e) {
            synchronized(SimpleGUI.this) {
                if (current_thread!=null) return;
                latestCommand=index;
                AlloyBridge.stopped=false;
                Runner r=new Runner(new StringReader(text.getText()), index);
                Thread t=new Thread(r);
                current_thread=t;
                runmenu.setEnabled(false);
                runbutton.setVisible(false);
                stopbutton.setVisible(true);
                t.start();
            }
        }
    }

    //=======================================================================//
}
