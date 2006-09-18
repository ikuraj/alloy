package edu.mit.csail.sdg.alloy4.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.prefs.Preferences;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import edu.mit.csail.sdg.alloy4.core.ParaSig;
import edu.mit.csail.sdg.alloy4.core.Unit;
import edu.mit.csail.sdg.alloy4.core.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.util.Err;
import edu.mit.csail.sdg.alloy4.util.Log;
import edu.mit.csail.sdg.alloy4.util.LogToTextPane;
import edu.mit.csail.sdg.kodviz.gui.KodVizGUIFactory;
import edu.mit.csail.sdg.kodviz.gui.KodVizInstaller;

@SuppressWarnings("serial")
public final class SimpleGUI {

    private synchronized void log(String x, Style s) {
        StyledDocument doc=log.getStyledDocument();
        if (doc.getLength()==0) x=x.trim();
        try { doc.insertString(doc.getLength(), x+"\n", s); } catch (BadLocationException e) { }
        log.setCaretPosition(doc.getLength());
    }

    private synchronized void log(String x, Style s, String y) {
        StyledDocument doc=log.getStyledDocument();
        if (doc.getLength()==0) x=x.trim();
        try {
            doc.insertString(doc.getLength(),x,s);
            doc.insertString(doc.getLength(),y+"\n",styleRegular);
        } catch (BadLocationException e) { }
        log.setCaretPosition(doc.getLength());
    }

    /**
     * This Runnable is used to execute a SAT query.
     * By having a separate runnable, we allow the main GUI to remain responsive.
     */
    private final class Runner implements Runnable {

        /** The Alloy model to be anaylzed */
        private final Reader source;

        /** The current directory (with a trailing FILE SEPARATOR) */
        private final String cwd;

        /** The command that this runner should run (0..) (-1 means all of them) */
        private final int index;

        /**
         * Constructor for this runner.
         *
         * @param cwd - the current directory (with a trailing FILE SERPATOR)
         * @param index - the command that this runner will run
         */
        public Runner(Reader source, String cwd, int index) {
            this.source=source;
            this.cwd=cwd;
            this.index=index;
        }

        /** The run() method to start this runner. */
        public void run() {
            try {
                Log blanklog=new Log();
                Log reallog=new LogToTextPane(log,styleRegular,styleGreen);
                ArrayList<Unit> units=AlloyParser.alloy_totalparseStream(source);
                ArrayList<ParaSig> sigs=VisitTypechecker.check(blanklog,units);
                List<TranslateAlloyToKodkod.Result> result=TranslateAlloyToKodkod.codegen(index,reallog,units,sigs, minisat?2:(zchaff_basic?1:0));
                if (result.size()==1 && result.get(0)==TranslateAlloyToKodkod.Result.SAT) {
                    log("Visualizer loading... please wait...", styleRegular);
                    String newcwd = new File(cwd+".."+fs+"kodviz").getAbsolutePath();
                    System.setProperty("kodviz.dir",newcwd);
                    KodVizGUIFactory factory=new KodVizGUIFactory(false);
                    factory.create(new File(cwd+".alloy.xml"));
                    log("Visualizer loaded.", styleRegular);
                }
                if (result.size()>1) {
                    log("\n" + result.size() + " command" + (result.size()>1?"s were":" was") + " completed", styleGreen);
                    String summary = "The result" + (result.size()>1?"s are":" is");
                    for(TranslateAlloyToKodkod.Result b:result) {
                        switch(b) {
                        case SAT: TRIVIALLY_SAT: summary+=" SAT"; break;
                        case UNSAT: TRIVIALLY_UNSAT: summary+=" UNSAT"; break;
                        default: summary+=" CANCELED";
                        }
                    }
                    log(summary+".", styleRegular);
                }
            }
            catch(UnsatisfiedLinkError e) { log("\nCannot run the command! The required JNI library cannot be found! "+e.toString(), styleRed); }
            catch(Err e) { log("\nCannot run the command! "+e.toString(), styleRed); }
            thread_reportTermination();
        }
    }

    /**
     * Synchronized helper method that writes the content of the editor to a file.
     * (If this method fails, it will output an error message to the JTextArea that displays messages)
     *
     * @return true if the method succeeds; false if the method fails.
     */
    private synchronized boolean my_save(String filename, boolean alwaysOverwrite) {
        if (!alwaysOverwrite) {
            File file=new File(filename);
            if (file.exists()) {
                String cancel="Cancel"; // This ensures that the same object is used, regardless of the compiler interning.
                int ans=JOptionPane.showOptionDialog(frame,
                        "The file \""+filename+"\" already exists. Do you wish to overwrite it?",
                        "Warning: the file already exists!",
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.WARNING_MESSAGE,
                        null,
                        new Object[]{"Overwrite",cancel},
                        cancel);
                if (ans!=JOptionPane.YES_OPTION) return false;
            }
        }
        try {
            FileWriter fw=new FileWriter(filename);
            BufferedWriter bw=new BufferedWriter(fw);
            PrintWriter out=new PrintWriter(bw);
            out.println(text.getText());
            out.flush();
            out.close();
            bw.close();
            fw.close();
            modified(false);
            log("\nContent saved to file \""+filename+"\"", styleGreen);
            latestName=filename;
            frame.setTitle(title+": "+latestName);
            return true;
        } catch(IOException e) {
            log("\nCannot write to the file \""+filename+"\"! "+e.toString(), styleRed);
            return false;
        }
    }

    /**
     * @return true iff the content of the text editor has been saved to a file.
     */
    private synchronized boolean my_save() {
        if (latestName.length()!=0) return my_save(latestName,true);
        return my_saveAs();
    }

    /**
     * @return true iff the content of the text editor has been saved to a file.
     */
    private synchronized boolean my_saveAs() {
        FileFilter filter=new FileFilter() {
            @Override public boolean accept(File f) {
                if (f.isFile() && !f.getPath().endsWith(".als")) return false;
                return true;
            }
            @Override public String getDescription() {
                return ".als files";
            }
        };
        JFileChooser open=new JFileChooser(fileOpenDirectory);
        open.setFileFilter(filter);
        if (open.showSaveDialog(frame)!=JFileChooser.APPROVE_OPTION) return false;
        fileOpenDirectory=open.getSelectedFile().getParent(); set("lastdir",fileOpenDirectory);
        String f=open.getSelectedFile().getPath();
        return my_save(f,false);
    }

    private void set(String key, String value) {
        Preferences pref= Preferences.userNodeForPackage(this.getClass());
        pref.put(key,value);
    }

    private String get(String key) {
        Preferences pref= Preferences.userNodeForPackage(this.getClass());
        return pref.get(key,"");
    }

    /** The default title */
    private final String title = "Alloy Analyzer";

    private static final String fs=System.getProperty("file.separator");

    /** The current directory (plus a trailing FILE SEPARATOR) */
    private final String cwd = new File(".").getAbsolutePath() + System.getProperty("file.separator");

    /** This field is true iff the text in the text buffer hasn't been modified since the last time it was compiled */
    private boolean compiled=false;
    /** Synchronized helper method that sets or clears the "compiled" flag. */
    private synchronized void compiled(boolean x) { compiled=x; }
    /** Synchronized helper method that returns true if and only if the "compiled" flag is true */
    private synchronized boolean compiled() { return compiled; }

    /** This field is true iff the text in the text buffer hasn't been modified */
    private boolean modified=false;
    /** Synchronized helper method that sets or clears the "modified" flag. */
    private synchronized void modified(boolean x) { if (modified!=x) {modified=x; my_caret();} }
    /** Synchronized helper method that returns true if and only if the "modified" flag is true */
    private synchronized boolean modified() { return modified; }

    private Thread current_thread=null;
    private synchronized void thread_reportTermination() { current_thread=null; }
    private synchronized boolean thread_stillRunning() { return current_thread!=null; }
    private synchronized void thread_stop() { if (current_thread!=null) Stopper.stopped=true; }

    /** The filename of the file most-recently-opened ("" if there is no loaded file) */
    private String latestName = "";

    /** The latest FileOpen directory. */
    private String fileOpenDirectory = new File("models").getAbsolutePath();

    /** The JFrame for the main window. */
    private JFrame frame;

    /** The JTextArea containing the editor buffer. */
    private JTextArea text;

    /** The JTextArea containing the error messages and success messages. */
    private JTextPane log;
    private Style styleRegular,styleBold,styleRed,styleGreen,styleGray;

    /** The JMenu that contains the list of RUN and CHECK commands in the current file. */
    private JMenu runmenu;

    /** The JLabel that displays the current line/column position, etc. */
    private JLabel status;

    /** Main method that launches the program. */
    public static final void main (String[] args) {
        new SimpleGUI(args);
    }

    /**
     * The constructor. To ensure thread safety, we move all initialization
     * code into a synchronized helper method named "my_setup".
     */
    public final String alloyhome;
    public SimpleGUI(String[] args) {
        my_setup(args);
        alloyhome=get("basedir");
        System.setProperty("alloyhome",alloyhome);
    }

    /** An ActionListener that is called when the user indicates a particular command to execute. */
    private class RunListener implements ActionListener {
        /** The current working directory (plus a trailing FILE SEPARATOR) */
        private final String cwd;
        /** The index number of the command that the user wishes to execute (0..) (-1 means ALL of them). */
        private final int index;
        /** The constructor. */
        public RunListener(String c,int i) {cwd=c; index=i;}
        /** The event handler that gets called when the user clicked on one of the menu item. */
        public void actionPerformed(ActionEvent e) {
            if (thread_stillRunning()) {
                log("...The previous analysis is still running...", styleRed);
                return;
            }
            Stopper.stopped=false;
            Runner r=new Runner(new StringReader(text.getText()), cwd, index);
            Thread t=new Thread(r);
            t.start();
            current_thread=t;
        }
    }

    /**
     * Synchronized helper method that gets called whenever the user tries to expand the RUN menu.
     *
     * <p/> When this happens, we first check if the current text buffer has been changed
     * since the last compilation. If it has, we write it to a temporary file named ".alloy"
     * in the current directory, and then parse it minimally (without doing typechecking or CNF generation).
     *
     * <p/>We then set the "compiled" flag to true. And populate the RUN menu with the list of commands
     * that are defined in the model.
     */
    private synchronized void my_run() {
        if (thread_stillRunning()) {
            compiled(false);
            runmenu.removeAll();
            JMenuItem y=new JMenuItem("The current analysis is still running...");
            y.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    thread_stop();
                }
            });
            runmenu.add(y);
            return;
        }
        if (compiled()) return;
        compiled(true);
        runmenu.removeAll();
        JMenuItem y=new JMenuItem("Cannot run any commands! See the error box below for details!");
        runmenu.add(y);
        Unit u;
        try {
            Reader isr=new StringReader(text.getText());
            u=AlloyParser.alloy_parseStream(isr);
        }
        catch(Err e) {
            if (e.pos!=null && e.pos.y>0 && e.pos.x>0) try {
                int c=text.getLineStartOffset(e.pos.y-1)+e.pos.x-1;
                text.setSelectionStart(c);
                text.setSelectionEnd(c+1);
            } catch(BadLocationException ex) {}
            String msg=e.toString();
            if (msg.matches("^.*There are [0-9]* possible tokens that can appear here:.*$")) {
                String head=msg.replaceAll("^(.*There are [0-9]* possible tokens that can appear here:).*$","$1");
                String tail=msg.replaceAll("^.*There are [0-9]* possible tokens that can appear here:(.*)$","$1");
                log("\nCannot parse the model! "+head, styleRed, tail);
            }
            else
                log("\nCannot parse the model! "+e.toString(), styleRed);
            return;
        }
        catch(Exception e) {
            log("\nCannot parse the model! "+e.toString(), styleRed);
            return;
        }
        log("\nParser succeeded: there are "+u.runchecks.size()+" command(s) in this model.", styleGreen);
        runmenu.removeAll();
        if (u.runchecks.size()==0) {
            y=new JMenuItem("There are no commands in this model!");
            runmenu.add(y);
            return;
        }
        if (u.runchecks.size()>1) {
            y=new JMenuItem("All");
            y.addActionListener(new RunListener(cwd,-1));
            runmenu.add(y);
            runmenu.add(new JSeparator());
        }
        for(int i=0; i<u.runchecks.size(); i++) {
            String label=u.runchecks.get(i).toString();
            y=new JMenuItem(label);
            y.addActionListener(new RunListener(cwd,i));
            runmenu.add(y);
        }
    }

    private synchronized final boolean my_confirm() {
        if (!modified()) return true;
        String cancel="Cancel"; // This ensures that the same object is used, regardless of the compiler interning.
        int ans=JOptionPane.showOptionDialog(frame,
                "The text in the text editor has not been saved yet! Do you wish to save it, discard it, or cancel?",
                "Warning! The text editor content has not been saved!",
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{"Save","Discard",cancel},
                cancel);
        if (ans==JOptionPane.YES_OPTION) { if (!my_save()) return false; }
        if (ans!=JOptionPane.NO_OPTION) return false;
        return true;
    }

    /**
     * Synchronized helper method that clears the text editor.
     */
    private synchronized final void my_new() {
        if (!my_confirm()) return;
        latestName="";
        text.setText("");
        frame.setTitle(title);
        compiled(false);
        modified(false);
    }

    /**
     * Synchronized helper method that creates a "File Open" dialog box, and open a file if the user chooses one.
     *
     * <p/> If it's successful, it will load the file into the text buffer,
     * then clear the "compiled" flag and the "modified" flag.
     *
     * <p/> If it's unsuccessful, it will report an error message.
     */
    private synchronized final void my_open() {
        if (!my_confirm()) return;
        FileFilter filter=new FileFilter() {
            @Override public boolean accept(File f) {
                if (f.isFile() && !f.getPath().endsWith(".als")) return false;
                return true;
            }
            @Override public String getDescription() {
                return ".als files";
            }
        };
        JFileChooser open=new JFileChooser(fileOpenDirectory);
        open.setFileFilter(filter);
        if (open.showOpenDialog(frame)!=JFileChooser.APPROVE_OPTION) return;
        fileOpenDirectory=open.getSelectedFile().getParent(); set("lastdir",fileOpenDirectory);
        my_open(open.getSelectedFile().getPath());
    }

    /**
     * Synchronized helper method that opens a new file.
     *
     * <p/> If it's successful, it will load the file into the text buffer,
     * then clear the "compiled" flag and the "modified" flag.
     *
     * <p/> If it's unsuccessful, it will report an error message.
     */
    private synchronized final void my_open(String f) {
        try {
            FileReader fr=new FileReader(f);
            BufferedReader br=new BufferedReader(fr);
            StringBuffer sb=new StringBuffer();
            while(true) {
                String s=br.readLine();
                if (s==null) break;
                sb.append(s);
                sb.append('\n');
            }
            br.close();
            fr.close();
            text.setText(sb.toString());
            text.setCaretPosition(0);
            log("\nFile \""+f+"\" successfully loaded.", styleGreen);
            frame.setTitle(title+": "+f);
            latestName=f;
            compiled(false);
            modified(false);
        } catch(FileNotFoundException e) { log("\nCannot open the file! "+e.toString(), styleGreen);
        } catch(IOException e) { log("\nCannot open the file! "+e.toString(), styleGreen);
        }
    }

    private synchronized final void my_caret() {
        try {
            int c=text.getCaretPosition();
            int y=text.getLineOfOffset(c)+1;
            int x=c-text.getLineStartOffset(y-1)+1;
            status.setText("<html>&nbsp; Line "+y+", Column "+x+(modified()?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        } catch(BadLocationException ex) {
            status.setText("<html>&nbsp; Line ?, Column ?"+(modified()?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        }
    }

    private JMenuItem make_JMenuItem(String label, int key, String accelerator, ActionListener al) {
        JMenuItem ans = new JMenuItem(label,key);
        if (accelerator!=null) ans.setAccelerator(KeyStroke.getKeyStroke(accelerator));
        if (al!=null) ans.addActionListener(al);
        return ans;
    }


    /**
     * Synchronized helper method that actually initializes everything.
     *
     * <p/> This method is called by the SimpleGUI's constructor to actually initialize everything.
     * It will create a GUI window, and populate it with two JTextArea and one JMenuBar.
     */
    private boolean minisat=true;
    private boolean zchaff_basic=true;
    private synchronized void my_setup(String[] args) {

        String basedir=get("basedir");
        if (basedir==null) basedir="";
        String binary=basedir+fs+"binary";
        File bindir=new File(binary);
        File modeldir=new File(basedir+fs+"models");
        
        if (basedir.length()==0 || (args.length==1 && args[0].equals("-jaws")) || !bindir.isDirectory() || !modeldir.isDirectory()) {
            basedir=KodVizInstaller.install(basedir);
            if (basedir==null || basedir.length()==0) System.exit(1);
            binary=basedir+fs+"binary";
            bindir=new File(binary);
            modeldir=new File(basedir+fs+"models");
            if (!bindir.isDirectory() || !modeldir.isDirectory()) System.exit(1);
        }
        
        // The following files we want to overwrite each time, to keep them up-to-date.
        KodVizInstaller.copy("alloy4.jar", basedir, false);
        KodVizInstaller.copy("libminisat.so", binary, true);
        KodVizInstaller.copy("libminisat4.so", binary, true);
        KodVizInstaller.copy("libminisat6.so", binary, true);
        KodVizInstaller.copy("libzchaff_basic.so", binary, true);
        KodVizInstaller.copy("libzchaff_basic4.so", binary, true);
        KodVizInstaller.copy("libzchaff_basic6.so", binary, true);
        KodVizInstaller.copy("libminisat.jnilib", binary, true);
        KodVizInstaller.copy("libzchaff_basic.jnilib", binary, true);
        KodVizInstaller.copy("minisat.dll", binary, false);
        KodVizInstaller.copy("zchaff_basic.dll", binary, false);
        set("basedir",basedir);

        try { System.load(binary+fs+"libminisat6.so"); } catch(UnsatisfiedLinkError ex) {
         try { System.load(binary+fs+"libminisat4.so"); } catch(UnsatisfiedLinkError ex2) {
          try { System.load(binary+fs+"libminisat.so"); } catch(UnsatisfiedLinkError ex3) {
           try { System.load(binary+fs+"libminisat.jnilib"); } catch(UnsatisfiedLinkError ex4) {
            try { System.load(binary+fs+"minisat.dll"); } catch(UnsatisfiedLinkError ex5) {
             minisat=false;
            }
           }
          }
         }
        }

        try { System.load(binary+fs+"libzchaff_basic6.so"); } catch(UnsatisfiedLinkError ex) {
         try { System.load(binary+fs+"libzchaff_basic4.so"); } catch(UnsatisfiedLinkError ex2) {
          try { System.load(binary+fs+"libzchaff_basic.so"); } catch(UnsatisfiedLinkError ex3) {
           try { System.load(binary+fs+"libzchaff_basic.jnilib"); } catch(UnsatisfiedLinkError ex4) {
            try { System.load(binary+fs+"zchaff_basic.dll"); } catch(UnsatisfiedLinkError ex5) {
             zchaff_basic=false;
            }
           }
          }
         }
        }

        fileOpenDirectory=basedir+fs+"models";

        int width=1000, height=600;
        Font font=new Font("Monospaced",0,12);
        JMenuBar bar=new JMenuBar();
        bar.setVisible(true);

        // Create the File menu
        JMenu filemenu=new JMenu("File",true);
        filemenu.setMnemonic(KeyEvent.VK_F);
        filemenu.add(make_JMenuItem("New", KeyEvent.VK_N, "ctrl N", new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_new(); }
        }));
        filemenu.add(make_JMenuItem("Open", KeyEvent.VK_O, "ctrl O", new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_open(); }
        }));
        filemenu.add(make_JMenuItem("Save", KeyEvent.VK_S, "ctrl S", new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_save(); }
        }));
        filemenu.add(make_JMenuItem("Save As", KeyEvent.VK_A, null, new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_saveAs(); }
        }));
        filemenu.add(make_JMenuItem("Load Standalone Visualizer", KeyEvent.VK_V, null, new ActionListener() {
            public void actionPerformed(ActionEvent e) { new KodVizGUIFactory(false).create(null); }
        }));
        filemenu.add(make_JMenuItem("Exit", KeyEvent.VK_X, null,new ActionListener() {
            public void actionPerformed(ActionEvent e) { if (my_confirm()) System.exit(1); }
        }));
        bar.add(filemenu);

        // Create the Run menu
        runmenu=new JMenu("Run",true);
        runmenu.setMnemonic(KeyEvent.VK_R);
        runmenu.addMenuListener(new MenuListener() {
            public void menuSelected(MenuEvent e) { my_run(); }
            public void menuDeselected(MenuEvent e) { }
            public void menuCanceled(MenuEvent e) { }
        });
        bar.add(runmenu);

        // Create the text editor
        text=new JTextArea();
        text.setLineWrap(false);
        text.setEditable(true);
        text.setTabSize(3);
        text.setFont(font);
        text.addCaretListener(new CaretListener() {
            public void caretUpdate(CaretEvent e) { my_caret(); }
        });
        text.getDocument().addDocumentListener(new DocumentListener(){
            public void insertUpdate(DocumentEvent e) {compiled(false); modified(true);}
            public void removeUpdate(DocumentEvent e) {compiled(false); modified(true);}
            public void changedUpdate(DocumentEvent e) {compiled(false); modified(true);}
        });
        JScrollPane textPane=new JScrollPane(text,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        textPane.setMinimumSize(new Dimension(50, 50));

        // Create the message area
        log=new JTextPane();
        log.setBackground(Color.getHSBColor(0f,0f,0.95f));
        log.setEditable(false);
        log.setFont(font);
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, "Monospaced");
        styleBold=doc.addStyle("bold", styleRegular); StyleConstants.setBold(styleBold, true);
        styleGreen=doc.addStyle("green", styleBold); StyleConstants.setForeground(styleGreen, new Color(0.2f,0.7f,0.2f));
        styleRed=doc.addStyle("red", styleBold); StyleConstants.setForeground(styleRed, new Color(0.7f,0.2f,0.2f));
        styleGray=doc.addStyle("gray", styleBold); StyleConstants.setBackground(styleGray, new Color(0.8f,0.8f,0.8f));
        JScrollPane statusPane=new JScrollPane(log,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setMinimumSize(new Dimension(50, 50));

        // Create a JSplitPane to hold the text editor and the message area
        JSplitPane split=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, textPane, statusPane);
        split.setDividerLocation(500);
        split.setOneTouchExpandable(false);

        // Make the JFrame, and put the JSplitPane and a JLabel into it.
        frame=new JFrame(title);
        frame.setBackground(Color.lightGray);
        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) { if (my_confirm()) System.exit(0); }
        });
        Container all=frame.getContentPane();
        all.setLayout(new BorderLayout());
        all.add(split, BorderLayout.CENTER);
        status=new JLabel(" ");
        status.setFont(font);
        all.add(status, BorderLayout.SOUTH);
        frame.setJMenuBar(bar);
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setVisible(true);

        if (args.length==2 && args[0].equals("-open") && new File(args[1]).exists()) my_open(args[1]);
    }
}
