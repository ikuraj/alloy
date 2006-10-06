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
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
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

import edu.mit.csail.sdg.alloy4.core.ParaSig;
import edu.mit.csail.sdg.alloy4.core.Unit;
import edu.mit.csail.sdg.alloy4.core.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.helper.Err;
import edu.mit.csail.sdg.alloy4.helper.Log;
import edu.mit.csail.sdg.alloy4.helper.LogToTextPane;
import edu.mit.csail.sdg.alloy4util.OurDialog;
import edu.mit.csail.sdg.alloy4util.MacUtil;
import edu.mit.csail.sdg.alloy4util.OurMenu;
import edu.mit.csail.sdg.alloy4util.OurMenuItem;
import edu.mit.csail.sdg.alloy4util.OurMenuBar;
import edu.mit.csail.sdg.alloy4util.OurSplitPane;
import edu.mit.csail.sdg.alloy4util.Util;
import edu.mit.csail.sdg.alloy4util.Version;
import edu.mit.csail.sdg.alloy4viz.gui.KodVizGUI;
import edu.mit.csail.sdg.alloy4viz.gui.KodVizGUIFactory;
import edu.mit.csail.sdg.alloy4viz.gui.KodVizInstaller;

public final class SimpleGUI implements Util.MessageHandler {

    private static final ImageIcon iconYes=Util.loadIcon("images/menu1.gif");
    private static final ImageIcon iconNo=Util.loadIcon("images/menu0.gif");

    /** The system-specific file separator (forward-slash on UNIX, back-slash on Windows, etc.) */
    private static final String fs=System.getProperty("file.separator");

    /** The darker background (for the MessageLog window and the Toolbar and the Status Bar, etc.) */
    private static final Color gray=Color.getHSBColor(0f,0f,0.90f);

    /** FileChooser filter that forces ".ALS" extension. */
    private static final FileFilter filterALS = new FileFilter() {
        @Override public final boolean accept(File f) { return !f.isFile() || f.getPath().endsWith(".als"); }
        @Override public final String getDescription() { return ".als files"; }
    };

    private void addHistory(String f) {
        String name0=get("history0");
        String name1=get("history1");
        String name2=get("history2");
        if (name0.equals(f)) return;
        if (name1.equals(f)) { set("history1",name0); set("history0",f); return; }
        if (name2.equals(f)) { set("history2",name1); set("history1",name0); set("history0",f); return ;}
        set("history3",name2); set("history2",name1); set("history1",name0); set("history0",f);
    }

    private KodVizGUIFactory factory;

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

    private static final void showChangeLog() {
        int screenWidth=Toolkit.getDefaultToolkit().getScreenSize().width;
        int screenHeight=Toolkit.getDefaultToolkit().getScreenSize().height;
        int width=screenWidth/3*2, height=screenHeight/3*2;
        JTextPane log=new JTextPane();
        log.setBackground(gray);
        log.setEditable(false);
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        Style styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, Util.getFontName());
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
    }

    private synchronized void logButton(String label, final String tmpdir, final String f) {
        StyledDocument doc=log.getStyledDocument();
        Style s=doc.addStyle("link", styleRegular);
        JButton b=new JButton(label);
        if (Util.onMac()) b.setBackground(gray);
        b.setMaximumSize(b.getPreferredSize());
        b.setFont(Util.getFont());
        b.setForeground(Color.BLUE);
        b.addActionListener(new ActionListener(){
            public final void actionPerformed(ActionEvent e) {
                factory.create(tmpdir, new File(f), frame);
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
        b.setFont(Util.getFont());
        b.setForeground(Color.BLUE);
        b.addMouseListener(new MouseListener(){
            public void mouseClicked(MouseEvent e) {
                factory.create(tmpdir, new File(f), frame);
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

        /** The Alloy model to be anaylzed */  private final Reader source;

        /** The command that this runner should run (0..) (-1 means all of them) */ private final int index;

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
                List<TranslateAlloyToKodkod.Result> result=TranslateAlloyToKodkod.codegen(index,reallog,units,sigs, satOPTION(), tempdir);
                reallog.flush(); // To make sure everything is flushed.
                new File(tempdir).delete(); // In case it was UNSAT, or was TRIVIALLY SAT. Or cancelled.
                if (result.size()==1 && result.get(0)==TranslateAlloyToKodkod.Result.SAT) {
                    logButton("Click here to display this instance", tempdir, tempdir+(index+1)+".xml");
                }
                if (result.size()>1) {
                    log("\n" + result.size() + " commands were completed. The results are:", styleGreen);
                    int i=0;
                    for(TranslateAlloyToKodkod.Result b:result) {
                        i++;
                        if (b==null) log("#"+i+": CANCELED", styleRegular);
                        else switch(b) {
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
     * @return non-null if the method succeeds; null if the method fails.
     */
    private synchronized Object my_save(String filename, boolean alwaysOverwrite) {
        if (!alwaysOverwrite) {
            File file=new File(filename);
            if (file.exists() && !OurDialog.questionOverwrite(frame,filename)) return null;
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
            return Boolean.TRUE;
        } catch(IOException e) {
            log("\nCannot write to the file \""+filename+"\"! "+e.toString(), styleRed);
            return null;
        }
    }

    private void set(String key, String value) {
        Preferences pref= Preferences.userNodeForPackage(this.getClass());
        pref.put(key,value);
    }

    private String get(String key) {
        Preferences pref= Preferences.userNodeForPackage(this.getClass());
        return pref.get(key,"");
    }

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

    private JButton stopbutton=null;
    private Thread current_thread=null;
    private synchronized void thread_reportStarting(Thread x) { runmenu.setEnabled(false); stopbutton.setVisible(true); current_thread=x; }
    private synchronized void thread_reportTermination() { runmenu.setEnabled(true); stopbutton.setVisible(false); current_thread=null; }
    private synchronized boolean thread_stillRunning() { return current_thread!=null; }
    private synchronized void thread_stop() {
        /*
        System.out.println("START...");
        for(StackTraceElement e: current_thread.getStackTrace()) {
            System.out.println("  ENTRY: " + e.toString());
        }
        System.out.println("Done.\n\n");
        System.out.flush();
        */
        if (current_thread!=null) Stopper.stopped=true;
    }

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
    private OurMenu runmenu;

    /** The JLabel that displays the current line/column position, etc. */
    private JLabel status;

    /** Main method that launches the program. */
    public static final void main(final String[] args) {
        System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Alloy 4");
        System.setProperty("com.apple.mrj.application.growbox.intrudes","true");
        System.setProperty("com.apple.mrj.application.live-resize","true");
        System.setProperty("com.apple.macos.useScreenMenuBar","true");
        System.setProperty("apple.laf.useScreenMenuBar","true");
        try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()); } catch (Exception e) { }
        SwingUtilities.invokeLater(new Runnable() {
            public final void run() { new SimpleGUI(args); }
        });
    }

    /**
     * The constructor. To ensure thread safety, we move all initialization
     * code into a synchronized helper method named "my_setup".
     */
    public String alloyhome="";

    /** An ActionListener that is called when the user indicates a particular command to execute. */
    private class RunListener implements ActionListener {
        /** The index number of the command that the user wishes to execute (0..) (-1 means ALL of them). */
        private final int index;
        /** The constructor. */
        public RunListener(int index) {this.index=index;}
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
            thread_reportStarting(t);
        }
    }

    /**
     * Synchronized helper method that gets called whenever the user tries to expand the FILE menu.
     */
    private synchronized void my_file(OurMenu filemenu) {
        boolean hasEntries=false;
        int itemCount=(Util.onMac() ? 4 : 5); // On Mac, we don't have "File->Quit". Elsewhere, we do.
        while(filemenu.getItemCount()>itemCount) filemenu.remove(itemCount);
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

    private synchronized final boolean my_confirm() {
        if (!modified()) return true;
        Boolean ans=OurDialog.questionSaveDiscardCancel(frame);
        if (ans==null) return false;
        if (!ans.booleanValue()) return true;
        return handleMessage(this,"save")!=null;
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
            if (Util.onMac()) {
                if (modified()) frame.getRootPane().putClientProperty("windowModified",Boolean.TRUE);
                else frame.getRootPane().putClientProperty("windowModified",Boolean.FALSE);
            }
            int c=text.getCaretPosition();
            int y=text.getLineOfOffset(c)+1;
            int x=c-text.getLineStartOffset(y-1)+1;
            status.setText("<html>&nbsp; Line "+y+", Column "+x+(modified()?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        } catch(BadLocationException ex) {
            status.setText("<html>&nbsp; Line ?, Column ?"+(modified()?" <b style=\"color:red;\">[modified]</b></html>":"</html>"));
        }
    }

    /** Convention for this method: return==null means failure, return!=null means success. */
    public synchronized Object handleMessage(Object caller, String x) {
        if ("new".equals(x)) {
            if (!my_confirm()) return null;
            latestName="";
            text.setText("");
            frame.setTitle("Alloy4: build date "+Version.buildDate());
            compiled(false);
            modified(false);
            return Boolean.TRUE;
        }
        if ("open".equals(x)) {
            if (!my_confirm()) return null;
            JFileChooser open=new JFileChooser(fileOpenDirectory);
            open.setFileFilter(filterALS);
            if (open.showOpenDialog(frame)!=JFileChooser.APPROVE_OPTION) return null;
            fileOpenDirectory=open.getSelectedFile().getParent(); set("lastdir",fileOpenDirectory);
            my_open(open.getSelectedFile().getPath());
            return Boolean.TRUE;
        }
        if (x instanceof String && ((String)x).startsWith("open:")) {
            if (!my_confirm()) return null;
            my_open(((String)x).substring(5));
            return Boolean.TRUE;
        }
        if ("saveas".equals(x)) {
            JFileChooser open=new JFileChooser(fileOpenDirectory);
            open.setFileFilter(filterALS);
            if (open.showSaveDialog(frame)!=JFileChooser.APPROVE_OPTION) return null;
            fileOpenDirectory=open.getSelectedFile().getParent(); set("lastdir",fileOpenDirectory);
            String f=open.getSelectedFile().getPath();
            return my_save(f,false);
        }
        if ("run".equals(x)) {
            if (compiled()) return Boolean.TRUE;
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
                return Boolean.TRUE;
            }
            catch(Exception e) {
                log("\nCannot parse the model! "+e.toString(), styleRed);
                return Boolean.TRUE;
            }
            log("\nParser succeeded: there are "+u.runchecks.size()+" command(s) in this model.", styleGreen);
            runmenu.removeAll();
            if (u.runchecks.size()==0) {
                y=new JMenuItem("There are no commands in this model!");
                runmenu.add(y);
                return Boolean.TRUE;
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
        if ("window".equals(x)) {
            OurMenu windowmenu=(OurMenu)caller;
            windowmenu.removeAll();
            JMenuItem et=new JMenuItem("Editor Window");
            et.setIcon(iconYes);
            et.addActionListener(new ActionListener() {
                public final void actionPerformed(ActionEvent e) {
                    frame.setExtendedState(JFrame.NORMAL);
                    frame.toFront();
                }
            });
            windowmenu.add(et);
            for(final KodVizGUI g: factory.windowList()) {
                et=new JMenuItem(g.getTitle());
                et.setIcon(iconNo);
                et.addActionListener(new ActionListener() {
                    public final void actionPerformed(ActionEvent e) {factory.show(g);}
                });
                windowmenu.add(et);
            }
        }
        if ("showchange".equals(x)) showChangeLog();
        if ("showversion".equals(x)) JOptionPane.showMessageDialog(null,"Alloy 4: build date "+Version.buildDate());
        if ("quit".equals(x)) if (my_confirm()) System.exit(0);
        if ("stop".equals(x)) thread_stop();
        if ("sat=sat4j".equals(x)) { OurMenuItem m=(OurMenuItem)caller; m.parent.setIconForChildren(iconNo); m.setIcon(iconYes); satOPTION=0; }
        if ("sat=zchaff".equals(x)) { OurMenuItem m=(OurMenuItem)caller; m.parent.setIconForChildren(iconNo); m.setIcon(iconYes); satOPTION=1; }
        if ("sat=minisat".equals(x)) { OurMenuItem m=(OurMenuItem)caller; m.parent.setIconForChildren(iconNo); m.setIcon(iconYes); satOPTION=2; }
        if ("sat=file".equals(x)) { OurMenuItem m=(OurMenuItem)caller; m.parent.setIconForChildren(iconNo); m.setIcon(iconYes); satOPTION=(-1); }
        if ("save".equals(x)) return latestName.length()!=0 ? my_save(latestName,true) : handleMessage(caller,"saveas");
        if ("file".equals(x)) my_file((OurMenu)caller);
        return Boolean.TRUE;
    }

    /**
     * Synchronized helper method that actually initializes everything.
     *
     * <p/> This method is called by the SimpleGUI's constructor to actually initialize everything.
     * It will create a GUI window, and populate it with two JTextArea and one JMenuBar.
     */
    private int satOPTION=2;
    private synchronized int satOPTION() { return satOPTION; }

    private SimpleGUI(String[] args) {

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

        int screenWidth=Toolkit.getDefaultToolkit().getScreenSize().width;
        int screenHeight=Toolkit.getDefaultToolkit().getScreenSize().height;
        int width=screenWidth/10*8, height=screenHeight/10*8;
        Font font=Util.getFont();
        frame=new JFrame("Alloy4: build date "+Version.buildDate());
        factory=new KodVizGUIFactory(alloyhome, false);

        // Create the menu
        OurMenuBar bar=new OurMenuBar(this);
        frame.setJMenuBar(bar);

        if (1==1) { // File menu
            OurMenu filemenu = bar.addMenu("File", true, KeyEvent.VK_F, "file");
            filemenu.addMenuItem(null, "New",     true, KeyEvent.VK_N, KeyEvent.VK_N, "new");
            filemenu.addMenuItem(null, "Open",    true, KeyEvent.VK_O, KeyEvent.VK_O, "open");
            filemenu.addMenuItem(null, "Save",    true, KeyEvent.VK_S, KeyEvent.VK_S, "save");
            filemenu.addMenuItem(null, "Save As", true, KeyEvent.VK_A, -1,            "saveas");
            if (!Util.onMac()) filemenu.addMenuItem(null, "Quit", true, KeyEvent.VK_Q, -1, "quit");
        }

        if (1==1) { // Run menu
            runmenu = bar.addMenu("Run", true, KeyEvent.VK_R, "run");
        }

        if (Util.keepMenuConsistent && Util.onMac()) { // Theme menu
            bar.addMenu("Theme", false, -1, null);
        }

        if (1==1) { // Options menu
            Error ex=null;
            boolean minisat;
            try {               System.load(binary+fs+"libminisat6.so");    } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat4.so");    } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat.so");     } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libminisat.jnilib"); } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"minisat.dll");       } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) { minisat=false; satOPTION=1; } else minisat=true;
            boolean zchaff;
            try { ex=null;      System.load(binary+fs+"libzchaff_basic6.so");    } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic4.so");    } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic.so");     } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"libzchaff_basic.jnilib"); } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) try { System.load(binary+fs+"zchaff_basic.dll");       } catch(UnsatisfiedLinkError e) {ex=e;}
            if (ex!=null) { zchaff=false; if (satOPTION==1) satOPTION=0; } else zchaff=true;
            OurMenu optmenu = bar.addMenu("Options", true, KeyEvent.VK_O, "");
            optmenu.addMenuItem(satOPTION==0?iconYes:iconNo, "Use SAT4J",       true,                       "sat=sat4j");
            optmenu.addMenuItem(satOPTION==1?iconYes:iconNo, "Use ZChaff",      zchaff,  KeyEvent.VK_Z, -1, "sat=zchaff");
            optmenu.addMenuItem(satOPTION==2?iconYes:iconNo, "Use MiniSat",     minisat, KeyEvent.VK_M, -1, "sat=minisat");
            optmenu.addMenuItem(iconNo,                      "Use CommandLine", true,    KeyEvent.VK_C, -1, "sat=file");
        }

        if (1==1) { // Window menu
            bar.addMenu("Window", true, KeyEvent.VK_W, "window");
        }

        if (1==1) { // Help menu
            OurMenu helpmenu = bar.addMenu("Help", true, KeyEvent.VK_H, null);
            helpmenu.addMenuItem(null, "See Alloy4 Change Log", true, KeyEvent.VK_C, -1, "showchange");
            helpmenu.addMenuItem(null, "See Alloy4 Version",    true, KeyEvent.VK_V, -1, "showversion");
        }

        // Create the text editor
        text=new JTextArea();
        text.setLineWrap(false);
        text.setEditable(true);
        text.setTabSize(3);
        text.setFont(Util.getFont());
        text.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) {my_caret();}
        });
        text.getDocument().addDocumentListener(new DocumentListener(){
            public final void insertUpdate(DocumentEvent e) {compiled(false); modified(true);}
            public final void removeUpdate(DocumentEvent e) {compiled(false); modified(true);}
            public final void changedUpdate(DocumentEvent e) {compiled(false); modified(true);}
        });
        JComponent textPane = Util.makeJScrollPane(text);

        // Create the toolbar
        JPanel toolbar=Util.makeH();
        if (!Util.onMac()) toolbar.setBackground(gray);
        toolbar.add(Util.makeJButton("New","Starts a new blank model","images/24_new.gif", this, "new"));
        toolbar.add(Util.makeJButton("Open","Opens an existing model","images/24_open.gif", this, "open"));
        toolbar.add(Util.makeJButton("Save","Saves the current model","images/24_save.gif", this, "save"));
        toolbar.add(stopbutton=Util.makeJButton("Stop","Stops the current analysis","images/24_execute_abort2.gif", this, "stop"));
        stopbutton.setVisible(false);
        JPanel lefthalf=new JPanel();
        lefthalf.setLayout(new BorderLayout());
        lefthalf.add(toolbar, BorderLayout.NORTH);
        lefthalf.add(textPane, BorderLayout.CENTER);
        textPane=lefthalf;

        // Create the message area
        log=new JTextPane();
        log.setBackground(gray);
        log.setEditable(false);
        log.setFont(Util.getFont());
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        styleRegular=doc.addStyle("regular", old);  StyleConstants.setFontFamily(styleRegular, Util.getFontName());
        styleBold=doc.addStyle("bold", styleRegular); StyleConstants.setBold(styleBold, true);
        styleGreen=doc.addStyle("green", styleBold); StyleConstants.setForeground(styleGreen, new Color(0.2f,0.7f,0.2f));
        styleRed=doc.addStyle("red", styleBold); StyleConstants.setForeground(styleRed, new Color(0.7f,0.2f,0.2f));
        styleGray=doc.addStyle("gray", styleBold); StyleConstants.setBackground(styleGray, new Color(0.8f,0.8f,0.8f));
        JScrollPane statusPane=new JScrollPane(log,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setMinimumSize(new Dimension(50, 50));

        frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) { if (my_confirm()) System.exit(0); }
        });

        Container all=frame.getContentPane();
        all.setLayout(new BorderLayout());
        all.add(new OurSplitPane(JSplitPane.HORIZONTAL_SPLIT, textPane, statusPane, width/2), BorderLayout.CENTER);
        all.add(status=Util.makeJLabel(" ",font), BorderLayout.SOUTH);
        status.setBackground(gray);
        status.setOpaque(true);
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setLocation(screenWidth/10, screenHeight/10);
        frame.setVisible(true);

        // Generate some informative log messages
        log("Alloy4: build date "+Version.buildDate(), styleGreen);
        if (satOPTION==2) log("\nSolver: MiniSAT using JNI", styleGreen);
            else if (satOPTION==1) log("\nSolver: ZChaff using JNI", styleGreen);
            else if (satOPTION==0) log("\nSolver: SAT4J", styleGreen);
        if (Util.onMac()) {
            log("\nMac OS X detected.", styleGreen);
            MacUtil.addApplicationListener(this);
        }

        // log("\nCurrent directory = " + (new File(".")).getAbsolutePath(), styleGreen);
        // On Mac, it will be the directory that contains "Alloy4.app". Problem: people can RENAME "Alloy4.app"...
        //log("ARGS = "+args.length+"\n", styleGreen);
        //for(String a:args) log("# = "+a+"\n",styleGreen);
        // If commandline tells you to load a file, load it.
        //if (args.length==1 && new File(args[0]).exists()) my_open(args[0]);
        //else if (args.length==2 && args[0].equals("-open") && new File(args[1]).exists()) my_open(args[1]);
    }
}
