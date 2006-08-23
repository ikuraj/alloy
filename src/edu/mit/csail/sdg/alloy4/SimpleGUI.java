package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.JScrollPane;
import javax.swing.JButton;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;

@SuppressWarnings("serial")
public final class SimpleGUI {

	private static final class Runner implements Runnable {
		private final JTextArea status;
		public Runner(String text, JTextArea status) {
			this.status=status;
			try {
				FileWriter fw=new FileWriter(".alloy");
				BufferedWriter bw=new BufferedWriter(fw);
				PrintWriter out=new PrintWriter(bw);
				out.println(text);
				out.flush();
				out.close();
				bw.close();
				fw.close();
			} catch(IOException e) { status.setText(e.toString()); }
		}
		public void run() {
			String[] args={".alloy"};
			Log log=new Log(status);
			try { new Main(true,args,log); }
			catch(FileNotFoundException e) { status.setText(e.toString()); }
			catch(UnsatisfiedLinkError e) { status.setText(e.toString()); }
			catch(ErrorInternal e) { status.setText(e.toString()); }
			catch(ErrorType e) { status.setText(e.toString()); }
			catch(ErrorSyntax e) { status.setText(e.toString()); }
		}
	}

	private JFrame frame;
	private JTextArea text;
	private JTextArea status;
	
	public static final void main (String[] unused) {
		new SimpleGUI();
	}
	
	private SimpleGUI() {
		my_setup();
	}
	
	private synchronized final void add (Container a,Component c,int x,int y,int w,int h,Font f) {
		c.setFont(f);
		c.setBounds(x,y,w,h);
		a.add(c);
	}
	
	private synchronized final void my_button2() {
		status.setText("");
		Runner r=new Runner(text.getText(), status);
		Thread t=new Thread(r);
		t.start();
	}
	
	private synchronized final void my_button1() {
		FileFilter filter=new FileFilter() {
			@Override public boolean accept(File f) {
				if (f.isFile() && !f.getPath().endsWith(".als")) return false;
				return true;
			}
			@Override public String getDescription() {
				return ".als files";
			}
		};
		JFileChooser open=new JFileChooser();
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
			text.setText(sb.toString());
			status.setText("File \""+f+"\" successfully loaded.");
		} catch(FileNotFoundException e) { status.setText(e.toString());
		} catch(IOException e) { status.setText(e.toString());
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
		
		JButton my_b1=new JButton("Open");
		add(all, my_b1,  5, 5, 80, 25, font);
		my_b1.addActionListener(new ActionListener()
				{public final void actionPerformed(ActionEvent v) {my_button1();}});
		
		JButton my_b2=new JButton("Run");
		add(all, my_b2, 90, 5, 70, 25, font);
		my_b2.addActionListener(new ActionListener()
				{public final void actionPerformed(ActionEvent v) {my_button2();}});
		
		text=new JTextArea();
		text.setFont(font);
		JScrollPane textPane=new JScrollPane(text,
			ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		textPane.setBounds(5, 35, width-20, 400);
		all.add(textPane);
		
		status=new JTextArea();
		status.setLineWrap(true);
		status.setEditable(false);
		status.setFont(font);
		JScrollPane statusPane=new JScrollPane(status,
 			ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
 			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		statusPane.setBounds(5, 441, width-20, height-477);
		all.add(statusPane);
		
		frame.pack();
		frame.setSize(new Dimension(width,height));
		frame.setVisible(true);
	}
}
