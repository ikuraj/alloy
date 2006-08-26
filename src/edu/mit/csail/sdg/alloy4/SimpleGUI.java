package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileFilter;

import edu.mit.csail.sdg.kodviz.gui.KodVizGUIFactory;

@SuppressWarnings("serial")
public final class SimpleGUI {

	/**
	 * This Runnable is used to execute a SAT query.
	 * By having a separate runnable, we allow the main GUI to remain responsive.
	 */
    private static final class Runner implements Runnable {
    	
    	/** The command that this runner will run (0..) (-1 means all of them) */
    	private final int index;
    	
    	/** The JTextArea that will display the progress */
        private final JTextArea status;
        
        /**
         * Constructor for this runner.
         * 
         * @param index - the command that this runner will run
         * @param text - the textual summary of the command that this runner will run
         * @param status - the JTextArea that will display the progress
         */
        public Runner(int index, String text, JTextArea status) {
            this.index=index;
            this.status=status;
        }
        
        /**
         * Helper method that appends an error message to the bottom of the JTextArea.
         * 
         * @param x - the error message
         */
        private void addlog(String x) {
            status.append("Cannot run the command! "+x);
            status.setCaretPosition(status.getDocument().getLength());
        }
        
        /**
         * The run() method to start this runner
         */
        public void run() {
            String[] args={".alloy"};
            Log log=new Log(status);
            try {
              new Main(index,args,log);
              //status.append("Visualizing...\n");
              //status.setCaretPosition(status.getDocument().getLength());
          	  //KodVizGUIFactory factory=new KodVizGUIFactory(false);
          	  //factory.create(new File(".alloy.xml"));
            }
            catch(FileNotFoundException e) { addlog("One of the required source file cannot be found! "+e.toString()); }
            catch(UnsatisfiedLinkError e) { addlog("The required JNI library cannot be found! "+e.toString()); }
            catch(ErrorInternal e) { addlog("An internal error has occurred! Please report this to the Alloy developers. "+e.toString()); }
            catch(ErrorType e) { addlog("Type Error! "+e.toString()); }
            catch(ErrorSyntax e) { addlog("Syntax Error! "+e.toString()); }
        }
    }

    /**
     * Synchronized helper method that writes the content of the editor
     * into a temporary file in the current directory named ".alloy".
     * (If this method fails, it will output an error message to the JTextArea that displays messages)
     *
     * @return true if the method succeeds; false if the method fails.
     */
	private synchronized boolean writetemp() {
        try {
            FileWriter fw=new FileWriter(".alloy");
            BufferedWriter bw=new BufferedWriter(fw);
            PrintWriter out=new PrintWriter(bw);
            out.println(text.getText());
            out.flush();
            out.close();
            bw.close();
            fw.close();
            return true;
        } catch(IOException e) {
        	status.setText("Cannot write the temporary file \".alloy\"! "+e.toString());
        	return false;
        }
	}

	/** This field is true iff the text in the text buffer hasn't been modified since the last time it was compiled */
    private boolean compiled=false;
    
    /** Synchronized helper method that sets or clears the "compiled" flag. */
    private synchronized void compiled(boolean x) { compiled=x; }
    
    /** Synchronized helper method that returns true if and only if the "compiled" flag is true */
    private synchronized boolean compiled() { return compiled; }

    /** The current directory. */
    private String currentDirectory=".";
    
    /** The JFrame for the main window. */
    private JFrame frame;
    
    /** The JTextArea containing the editor buffer. */
    private JTextArea text;
    
    /** The JTextArea containing the error messages and success messages. */
    private JTextArea status;
    
    /** The JMenu that contains the list of RUN and CHECK commands in the current file. */
    private JMenu runmenu;

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

    /**
     * An ActionListener that is called when the user indicates a particular command to execute.
     */
    private class RunListener implements ActionListener {
    	/** The index number of the command that the user wishes to execute (0..) (-1 means ALL of them). */
    	private final int index;
    	/** The human-readable summary of the command that the user wishes to execute. */
    	private final String label;
    	/** The constructor. */
    	public RunListener(int i,String la) {index=i; label=la;}
    	/** The event handler that gets called when the user clicked on one of the menu item. */
		public void actionPerformed(ActionEvent e) {
	    	status.setText("Running "+label);
	        Runner r=new Runner(index, text.getText(), status);
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
   		status.setText("");
   		runmenu.removeAll();
		JMenuItem y=new JMenuItem("Cannot run any commands! See the error box below for details!");
 		runmenu.add(y);
   		if (!writetemp()) return;
   		Unit u;
		try { u=AlloyParser.alloy_parseFile(".alloy",""); }
		catch(Exception e) { status.setText("Cannot parse the model! "+e.toString()); return; }
   		runmenu.removeAll();
   		if (u.runchecks.size()==0) {
   			y=new JMenuItem("There are no commands in this model!");
   	 		runmenu.add(y);
   	 		return;
   		}
   		if (u.runchecks.size()>1) {
   			y=new JMenuItem("All");
   			y.addActionListener(new RunListener(-1,"All"));
   			runmenu.add(y);
   	 		runmenu.add(new JSeparator());
   		}
		for(int i=0; i<u.runchecks.size(); i++) {
			String label=u.runchecks.get(i).toString();
    		y=new JMenuItem(label);
    		y.addActionListener(new RunListener(i,label));
    		runmenu.add(y);
		}
    }

    /**
     * Synchronized helper method that clears the text editor.
     */
    private synchronized final void my_new() {
    	text.setText("");
    	status.setText("");
    	compiled(false);
    }

    /**
     * Synchronized helper method that creates a "File Open" dialog box, and open a new file if the user chooses one.
     * 
     * <p/> If it's successful, it will load the file into the text buffer, then clear the "compiled" flag.
     * 
     * <p/> If it's unsuccessful, it will report an error message. 
     */
    private synchronized final void my_open() {
        FileFilter filter=new FileFilter() {
            @Override public boolean accept(File f) {
                if (f.isFile() && !f.getPath().endsWith(".als")) return false;
                return true;
            }
            @Override public String getDescription() {
                return ".als files";
            }
        };
        JFileChooser open=new JFileChooser(currentDirectory);
        open.setFileFilter(filter);
        int ans=open.showOpenDialog(frame);
        if (ans!=JFileChooser.APPROVE_OPTION) return;
        try {
            String f=open.getSelectedFile().getPath();
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
            currentDirectory=open.getSelectedFile().getParent();
            compiled(false);
            text.setText(sb.toString());
      	    text.setCaretPosition(0);
            status.setText("File \""+f+"\" successfully loaded.");
        } catch(FileNotFoundException e) { status.setText("Cannot open the file! "+e.toString());
        } catch(IOException e) { status.setText("Cannot open the file! "+e.toString());
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

        frame=new JFrame("Alloy--");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setBackground(Color.lightGray);

        Container all=frame.getContentPane();

        text=new JTextArea();
        text.setLineWrap(false);
        text.setEditable(true);
        text.setFont(font);
        text.getDocument().addDocumentListener(new DocumentListener(){
			public void insertUpdate(DocumentEvent e) {compiled(false);}
			public void removeUpdate(DocumentEvent e) {compiled(false);}
			public void changedUpdate(DocumentEvent e) {compiled(false);}
        });
        JScrollPane textPane=new JScrollPane(text,
            ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
            ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        textPane.setMinimumSize(new Dimension(50, 50));

        status=new JTextArea();
        status.setBackground(Color.getHSBColor(0f,0f,0.9f));
        status.setLineWrap(true);
        status.setEditable(false);
        status.setFont(font);
        JScrollPane statusPane=new JScrollPane(status,
            ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
            ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setMinimumSize(new Dimension(50, 50));
        
        JSplitPane split=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, textPane, statusPane);
        split.setDividerLocation(500);
        split.setOneTouchExpandable(true);
        all.add(split);

        JMenu filemenu=new JMenu("File",true);
		filemenu.setMnemonic(KeyEvent.VK_F);
		
		JMenuItem filenew=new JMenuItem("New", KeyEvent.VK_N);
		filenew.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) { my_new(); }
		});
		filemenu.add(filenew);

		JMenuItem fileopen=new JMenuItem("Open", KeyEvent.VK_O);
		fileopen.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) { my_open(); }
		});
		filemenu.add(fileopen);
		
		JMenuItem fileexit=new JMenuItem("Exit", KeyEvent.VK_X);
		fileexit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) { System.exit(0); }
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
