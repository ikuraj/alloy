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
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileFilter;

import kodviz.gui.KodVizGUIFactory;

@SuppressWarnings("serial")
public final class SimpleGUI {

    private static final class Runner implements Runnable {
    	private final int index;
        private final JTextArea status;
        public Runner(int index, String text, JTextArea status) {
            this.index=index;
            this.status=status;
        }
        private void addlog(String x) {
            status.append("Cannot run the command! "+x);
            status.setCaretPosition(status.getDocument().getLength());
        }
        public void run() {
            String[] args={".alloy"};
            Log log=new Log(status);
            try {
            	new Main(index,args,log);
            	KodVizGUIFactory.main(new String[]{});
            }
            catch(FileNotFoundException e) { addlog("FileNotFoundException! "+e.toString()); }
            catch(UnsatisfiedLinkError e) { addlog("The required JNI library cannot be found! "+e.toString()); }
            catch(ErrorInternal e) { addlog("An internal error has occurred! Please report this to the Alloy developers. "+e.toString()); }
            catch(ErrorType e) { addlog("Type Error! "+e.toString()); }
            catch(ErrorSyntax e) { addlog("Syntax Error! "+e.toString()); }
        }
    }

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

    private boolean compiled=false;
    private synchronized void compiled(boolean x) { compiled=x; }
    private synchronized boolean compiled() { return compiled; }

    private String currentDirectory=".";
    private JFrame frame;
    private JTextArea text;
    private JTextArea status;
    private JMenu runmenu;

    public static final void main (String[] unused) {
        new SimpleGUI();
    }

    private SimpleGUI() {
        my_setup();
    }

    private class RunListener implements ActionListener {
    	private final int index;
    	private final String label;
    	public RunListener(int i,String la) {index=i; label=la;}
		public void actionPerformed(ActionEvent e) {
	    	status.setText("Running "+label);
	        Runner r=new Runner(index, text.getText(), status);
	        Thread t=new Thread(r);
	        t.start();
		}
    }
    
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
   			y=new JMenuItem("Run every command");
   			y.addActionListener(new RunListener(-1,"Run every commmand"));
   			runmenu.add(y);
   		}
		for(int i=0; i<u.runchecks.size(); i++) {
			String label=u.runchecks.get(i).toString();
    		y=new JMenuItem(label);
    		y.addActionListener(new RunListener(i,label));
    		runmenu.add(y);
		}
    }

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

    private synchronized final void my_setup() {
        int width=1000, height=600;
        Font font=new Font("Monospaced",0,12);

        frame=new JFrame("Alloy--");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setBackground(Color.lightGray);

        Container all=frame.getContentPane();
        all.setBackground(Color.lightGray);
        all.setLayout(null);

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
        textPane.setBounds(5, 5, width-20, 400);
        all.add(textPane);

        status=new JTextArea();
        status.setLineWrap(true);
        status.setEditable(false);
        status.setFont(font);
        JScrollPane statusPane=new JScrollPane(status,
            ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
            ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        statusPane.setBounds(5, 410, width-20, height-470);
        all.add(statusPane);
        
        JMenu filemenu=new JMenu("File",true);
		filemenu.setMnemonic(KeyEvent.VK_F);
		
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
