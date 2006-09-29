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
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.prefs.Preferences;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
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
import javax.swing.SwingUtilities;
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
import edu.mit.csail.sdg.alloy4.util.AlloyVersion;
import edu.mit.csail.sdg.kodviz.gui.KodVizGUIFactory;
import edu.mit.csail.sdg.kodviz.gui.KodVizInstaller;

public final class SimpleGUI {

    private void addHistory(String f) {
        String name0=get("history0");
        String name1=get("history1");
        String name2=get("history2");
        if (name0.equals(f)) return;
        if (name1.equals(f)) { set("history1",name0); set("history0",f); return; }
        if (name2.equals(f)) { set("history2",name1); set("history1",name0); set("history0",f); return ;}
        set("history3",name2); set("history2",name1); set("history1",name0); set("history0",f);
    }

    public static int getScreenWidth() {
        return Toolkit.getDefaultToolkit().getScreenSize().width;
    }

    public static int getScreenHeight() {
        return Toolkit.getDefaultToolkit().getScreenSize().height;
    }

    private static final String[] changelog = new String[]{
    	"2006 Spe 28 9PM:",
    	"  1) Unconnected Int atoms are now hidden by default.",
    	"  2) Projection and Unprojection buttons are changed into hyperlinks in the titlebar.",
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

    private static final void showChangeLog() {
        int screenWidth=getScreenWidth();
        int screenHeight=getScreenHeight();
        int width=screenWidth/3*2, height=screenHeight/3*2;
        JTextPane log=new JTextPane();
        log.setBackground(Color.getHSBColor(0f,0f,0.95f));
        log.setEditable(false);
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        Style styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, "Monospaced");
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
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setLocation(screenWidth/6, screenHeight/6);
        frame.setVisible(true);
    }

    private synchronized void logButton(String label, final String tmpdir, final String f) {
    	StyledDocument doc=log.getStyledDocument();
    	Style s=doc.addStyle("link", styleRegular);
    	JButton b=new JButton(label);
    	b.setMaximumSize(b.getPreferredSize());
    	b.setFont(new Font("Monospaced", Font.PLAIN, 11));
    	b.setForeground(Color.BLUE);
    	b.addActionListener(new ActionListener(){
			public final void actionPerformed(ActionEvent e) {
                KodVizGUIFactory factory=new KodVizGUIFactory(alloyhome,false);
                factory.create(tmpdir, new File(f));
			}
		});
    	StyleConstants.setComponent(s, b);
    	log("", s);
	}

    private synchronized void logLink(String label, final String tmpdir, final String f) {
    	StyledDocument doc=log.getStyledDocument();
    	Style s=doc.addStyle("link", styleRegular);
    	JLabel b=new JLabel(label);
    	b.setMaximumSize(b.getPreferredSize());
    	b.setFont(new Font("Monospaced", Font.PLAIN, 11));
    	b.setForeground(Color.BLUE);
    	b.addMouseListener(new MouseListener(){
			public void mouseClicked(MouseEvent e) {
                KodVizGUIFactory factory=new KodVizGUIFactory(alloyhome,false);
                factory.create(tmpdir, new File(f));
			}
			public void mousePressed(MouseEvent e) { }
			public void mouseReleased(MouseEvent e) { }
			public void mouseEntered(MouseEvent e) { }
			public void mouseExited(MouseEvent e) { }
    	});
    	StyleConstants.setComponent(s, b);
    	log("", s);
	}

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

    private String maketemp() {
    	Random r=new Random(new Date().getTime());
    	while(true) {
    		int i=r.nextInt(100000);
    		String dest=alloyhome+fs+"tmp"+fs+i;
    		File f=new File(dest);
    		if (f.exists()) continue;
    		f.mkdirs();
    		return dest+fs;
    	}
    }

    /**
     * This Runnable is used to execute a SAT query.
     * By having a separate runnable, we allow the main GUI to remain responsive.
     */
    private final class Runner implements Runnable {

        /** The Alloy model to be anaylzed */
        private final Reader source;

        /** The command that this runner should run (0..) (-1 means all of them) */
        private final int index;

        /**
         * Constructor for this runner.
         * @param index - the command that this runner will run
         */
        public Runner(Reader source, int index) {
            this.source=source;
            this.index=index;
        }

        /** The run() method to start this runner. */
        public void run() {
            try {
                Log blanklog=new Log();
                Log reallog=new LogToTextPane(log,styleRegular,styleGreen);
                ArrayList<Unit> units=AlloyParser.alloy_totalparseStream(alloyhome, source);
                ArrayList<ParaSig> sigs=VisitTypechecker.check(blanklog,units);
                String tempdir=maketemp();
                List<TranslateAlloyToKodkod.Result> result=TranslateAlloyToKodkod.codegen(index,reallog,units,sigs, minisat?2:(zchaff_basic?1:0), tempdir);
                new File(tempdir).delete(); // In case it was UNSAT, or was TRIVIALLY SAT. Or cancelled.
                if (result.size()==1 && result.get(0)==TranslateAlloyToKodkod.Result.SAT) {
                    //log("Visualizer loading... please wait...", styleRegular);
                    //KodVizGUIFactory factory=new KodVizGUIFactory(alloyhome,false);
                    //factory.create(tempdir,new File(tempdir+(index+1)+".xml"));
                    //log("Visualizer loaded.", styleRegular);
                    logButton("Click here to display this instance", tempdir, tempdir+(index+1)+".xml");
                }
                if (result.size()>1) {
                    log("\n" + result.size() + " commands were completed. The results are:", styleGreen);
                    int i=0;
                    for(TranslateAlloyToKodkod.Result b:result) {
                    	i++;
                        switch(b) {
                        case SAT: logLink("#"+i+": SAT (Click here to see the instance)", tempdir, tempdir+i+".xml"); break;
                        case TRIVIALLY_SAT: log("#"+i+": SAT", styleRegular); break;
                        case UNSAT: case TRIVIALLY_UNSAT: log("#"+i+": UNSAT", styleRegular); break;
                        default: log("#"+i+": CANCELED", styleRegular);
                        }
                    }
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
            addHistory(filename);
            frame.setTitle("Alloy File: "+latestName);
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

    private static final String fs=System.getProperty("file.separator");

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
    private JMenu runmenu,filemenu;

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
    public String alloyhome="";
    public SimpleGUI(final String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() { my_setup(args); }
        });
    }

    /** An ActionListener that is called when the user indicates a particular command to execute. */
    private class RunListener implements ActionListener {
        /** The index number of the command that the user wishes to execute (0..) (-1 means ALL of them). */
        private final int index;
        /** The constructor. */
        public RunListener(int i) {index=i;}
        /** The event handler that gets called when the user clicked on one of the menu item. */
        public void actionPerformed(ActionEvent e) {
            if (thread_stillRunning()) {
                log("...The previous analysis is still running...", styleRed);
                return;
            }
            Stopper.stopped=false;
            Runner r=new Runner(new StringReader(text.getText()), index);
            Thread t=new Thread(r);
            t.start();
            current_thread=t;
        }
    }

    /**
     * Synchronized helper method that gets called whenever the user tries to expand the FILE menu.
     */
    private synchronized void my_file() {
        boolean hasEntries=false;
        while(filemenu.getItemCount()>5) filemenu.remove(5);
        for(int i=0; i<=3; i++) {
            final String n=get("history"+i);
            if (n.length()>0) {
                if (!hasEntries) {
                    hasEntries=true;
                    filemenu.addSeparator();
                }
                JMenuItem x=new JMenuItem(n);
                x.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        if (!my_confirm()) return;
                        my_open(n);
                    }
                });
                filemenu.add(x);
            }
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
            y.addActionListener(new RunListener(-1));
            runmenu.add(y);
            runmenu.add(new JSeparator());
        }
        for(int i=0; i<u.runchecks.size(); i++) {
            String label=u.runchecks.get(i).toString();
            y=new JMenuItem(label);
            y.addActionListener(new RunListener(i));
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
        if (ans==JOptionPane.YES_OPTION) { if (!my_save()) return false; return true; }
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
        frame.setTitle(AlloyVersion.version());
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
            frame.setTitle("Alloy File: "+f);
            latestName=f;
            addHistory(f);
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
        alloyhome=KodVizInstaller.install(get("basedir"));
        fileOpenDirectory=alloyhome+fs+"models";
        System.setProperty("alloyhome",alloyhome);
        String binary=alloyhome+fs+"binary";
        // The following files we want to overwrite each time, to keep them up-to-date.
        KodVizInstaller.copy("alloy4.jar", alloyhome, false);
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
        set("basedir",alloyhome);

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

        int screenWidth=getScreenWidth();
        int screenHeight=getScreenHeight();
        int width=screenWidth/10*8, height=screenHeight/10*8;
        Font font=new Font("Monospaced",0,12);
        JMenuBar bar=new JMenuBar();
        bar.setVisible(true);

        // Create the File menu
        filemenu=new JMenu("File",true);
        filemenu.setMnemonic(KeyEvent.VK_F);
        filemenu.addMenuListener(new MenuListener() {
            public void menuSelected(MenuEvent e) { my_file(); }
            public void menuDeselected(MenuEvent e) { }
            public void menuCanceled(MenuEvent e) { }
        });
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

        // Create the About menu
        JMenu helpmenu=new JMenu("Help",true);
        helpmenu.setMnemonic(KeyEvent.VK_H);
        helpmenu.add(make_JMenuItem("See Alloy4 Change Log", KeyEvent.VK_C, null, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                showChangeLog();
            }
        }));
        helpmenu.add(make_JMenuItem("See Alloy4 Version", KeyEvent.VK_V, null, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(null,AlloyVersion.version());
            }
        }));
        bar.add(helpmenu);

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
        split.setDividerLocation(width/2);
        split.setOneTouchExpandable(false);

        // Make the JFrame, and put the JSplitPane and a JLabel into it.
        frame=new JFrame(AlloyVersion.version());
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
        frame.setLocation(screenWidth/10, screenHeight/10);
        frame.setVisible(true);

        log(AlloyVersion.version(), styleGreen);

        if (minisat) log("\nSolver: MiniSAT using JNI", styleGreen);
        else if (zchaff_basic) log("\nSolver: ZChaff using JNI", styleGreen);
        else log("\nSolver: SAT4J", styleGreen);

        if (args.length==1 && new File(args[0]).exists()) my_open(args[0]);

        if (args.length==2 && args[0].equals("-open") && new File(args[1]).exists()) my_open(args[1]);
    }
}
