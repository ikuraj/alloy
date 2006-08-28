package edu.mit.csail.sdg.alloy4;

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
import java.util.List;
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

import edu.mit.csail.sdg.kodviz.gui.KodVizGUIFactory;

@SuppressWarnings("serial")
public final class SimpleGUI {
	
	private synchronized void log(String x) {
		if (log.getDocument().getLength()==0) x=x.trim();
		log.append(x+"\n");
		log.setCaretPosition(log.getDocument().getLength());
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
              String ps=System.getProperty("file.separator");
              List<Boolean> result=Main.run(index,source,new Log(log));
              if (result.size()==1 && result.get(0)==true) {
            	  log("Visualizing...");
            	  String newcwd = new File(cwd+".."+ps+"kodviz").getAbsolutePath();
            	  System.setProperty("kodviz.dir",newcwd);
            	  KodVizGUIFactory factory=new KodVizGUIFactory(false);
            	  factory.create(new File(cwd+".alloy.xml"));
              }
            }
            catch(UnsatisfiedLinkError e) { log("Cannot run the command! The required JNI library cannot be found! "+e.toString()); }
            catch(ErrorWithPos e) { log("Cannot run the command! "+e.toString()); }
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
            log("\nContent saved to file \""+filename+"\"");
            latestName=filename;
            frame.setTitle(title+": "+latestName);
            return true;
        } catch(IOException e) {
            log("\nCannot write to the file \""+filename+"\"! "+e.toString());
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
        fileOpenDirectory=open.getSelectedFile().getParent();
        String f=open.getSelectedFile().getPath();
        return my_save(f,false);
    }

    /** The default title */
    private final String title = "Alloy Analyzer";

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

    /** The filename of the file most-recently-opened ("" if there is no loaded file) */
    private String latestName = "";

    /** The latest FileOpen directory. */
    private String fileOpenDirectory = new File("models").getAbsolutePath();
    
    /** The JFrame for the main window. */
    private JFrame frame;
    
    /** The JTextArea containing the editor buffer. */
    private JTextArea text;
    
    /** The JTextArea containing the error messages and success messages. */
    private JTextArea log;
    
    /** The JMenu that contains the list of RUN and CHECK commands in the current file. */
    private JMenu runmenu;

    /** The JLabel that displays the current line/column position, etc. */
    private JLabel status;
    
    /** Main method that launches the program. */
    public static final void main (String[] unused) {
        new SimpleGUI();
    }
    
    /**
     * The constructor. To ensure thread safety, we move all initialization
     * code into a synchronized helper method named "my_setup".
     */
    public SimpleGUI() {
        my_setup();
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
            log("\nCompiling...");
            Runner r=new Runner(new StringReader(text.getText()), cwd, index);
            Thread t=new Thread(r);
            t.start();
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
        catch(ErrorWithPos e) {
            if (e.pos!=null && e.pos.y>0 && e.pos.x>0) try {
               int c=text.getLineStartOffset(e.pos.y-1)+e.pos.x-1;
               text.setSelectionStart(c);
               text.setSelectionEnd(c+1);
            } catch(BadLocationException ex) {}
            log("\nCannot parse the model! "+e.toString());
            return;
        }
        catch(Exception e) {
            log("\nCannot parse the model! "+e.toString());
            return;
        }
        log("\nParser succeeded: there are "+u.runchecks.size()+" command(s) in this model.");
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
        log.setText("");
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
        fileOpenDirectory=open.getSelectedFile().getParent();
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
            log("\nFile \""+f+"\" successfully loaded.");
            frame.setTitle(title+": "+f);
            latestName=f;
            compiled(false);
            modified(false);
        } catch(FileNotFoundException e) { log("\nCannot open the file! "+e.toString());
        } catch(IOException e) { log("\nCannot open the file! "+e.toString());
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

    /**
     * Synchronized helper method that actually initializes everything.
     * 
     * <p/> This method is called by the SimpleGUI's constructor to actually initialize everything.
     * It will create a GUI window, and populate it with two JTextArea and one JMenuBar.
     */
    private synchronized final void my_setup() {
        int width=1000, height=600;
        Font font=new Font("Monospaced",0,12);

        frame=new JFrame(title);
        frame.setBackground(Color.lightGray);
		frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) { if (my_confirm()) System.exit(0); }
		});
		
		Container all=frame.getContentPane();

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

        log=new JTextArea();
        log.setBackground(Color.getHSBColor(0f,0f,0.95f));
        log.setLineWrap(true);
        log.setWrapStyleWord(true);
        log.setEditable(false);
        log.setFont(font);
        JScrollPane statusPane=new JScrollPane(log,
            ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
            ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setMinimumSize(new Dimension(50, 50));
        
        all.setLayout(new BorderLayout());
        
        JSplitPane split=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, textPane, statusPane);
        split.setDividerLocation(500);
        split.setOneTouchExpandable(false);
        all.add(split, BorderLayout.CENTER);
        
        status=new JLabel(" ");
        status.setFont(font);
        all.add(status, BorderLayout.SOUTH);
        
        JMenu filemenu=new JMenu("File",true);
        filemenu.setMnemonic(KeyEvent.VK_F);
        
        JMenuItem filenew=new JMenuItem("New", KeyEvent.VK_N);
        filenew.setAccelerator(KeyStroke.getKeyStroke("ctrl N"));
        filenew.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_new(); }
        });
        filemenu.add(filenew);

        JMenuItem fileopen=new JMenuItem("Open", KeyEvent.VK_O);
        fileopen.setAccelerator(KeyStroke.getKeyStroke("ctrl O"));
        fileopen.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_open(); }
        });
        filemenu.add(fileopen);

        JMenuItem filesave=new JMenuItem("Save", KeyEvent.VK_S);
        filesave.setAccelerator(KeyStroke.getKeyStroke("ctrl S"));
        filesave.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_save(); }
        });
        filemenu.add(filesave);

        JMenuItem filesaveas=new JMenuItem("Save As", KeyEvent.VK_A);
        filesaveas.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { my_saveAs(); }
        });
        filemenu.add(filesaveas);

        JMenuItem fileexit=new JMenuItem("Exit", KeyEvent.VK_X);
        fileexit.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) { if (my_confirm()) System.exit(0); }
        });
        filemenu.add(fileexit);

        runmenu=new JMenu("Run",true);
        runmenu.setMnemonic(KeyEvent.VK_R);
        runmenu.addMenuListener(new MenuListener() {
            public void menuSelected(MenuEvent e) { my_run(); }
            public void menuDeselected(MenuEvent e) { }
            public void menuCanceled(MenuEvent e) { }
        });

        JMenuBar bar=new JMenuBar();
        bar.add(filemenu);
        bar.add(runmenu);
        bar.setVisible(true);
        frame.setJMenuBar(bar);
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setVisible(true);
    }
}
