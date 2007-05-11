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

package edu.mit.csail.sdg.alloy4;

import java.io.File;
import java.io.FilenameFilter;
import java.util.StringTokenizer;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import static javax.swing.JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED;
import static javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * Graphical dialog methods for asking the user some questions.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurDialog {

    /** The constructor is private, since this utility class never needs to be instantiated. */
    private OurDialog() { }

    /** Popup the given error message. */
    public static void alert(Frame parentFrame, Object message, String title) {
        JOptionPane.showMessageDialog(parentFrame, message, title, JOptionPane.PLAIN_MESSAGE);
    }

    /** Popup the given error message, then terminate the program. */
    public static void fatal(Frame parentFrame, Object message) {
        JOptionPane.showMessageDialog(parentFrame, message, "Fatal Error", JOptionPane.ERROR_MESSAGE);
        System.exit(1);
    }

    /**
     * Ask if the user wishes to save the file, discard the file, or cancel the entire operation (default==cancel).
     * @return null if cancel, true if save, false if discard
     */
    public static Boolean askSaveDiscardCancel(Frame parentFrame, String description) {
        String save="Save", discard="Don\'t Save", cancel="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame,
                new String[]{
                description+" has not been saved. Do you want to",
                "cancel the operation, close the file without saving, or save it and close?"},
                "Warning",
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{save,discard,cancel},
                cancel);
        if (ans==JOptionPane.YES_OPTION) {
            return Boolean.TRUE;
        }
        if (ans!=JOptionPane.NO_OPTION) {
            return null;
        }
        return Boolean.FALSE;
    }

    /** Ask if the user really wishes to overwrite the file (default=no). */
    public static boolean askOverwrite(Frame parentFrame, String filename) {
        String yes="Overwrite", no="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame, new String[]{
                "The file \""+filename+"\"",
                "already exists. Do you wish to overwrite it?"
                },
                "Warning: file already exists",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{yes,no},
                no);
        return ans==JOptionPane.YES_OPTION;
    }

    /** This caches the result of the call to get all fonts. */
    private static String[] allFonts=null;

    /** Returns true if a font with that name exists on the system. */
    public static boolean hasFont(String fontname) {
        String[] fonts;
        synchronized (OurDialog.class) {
            if (allFonts==null) {
                allFonts=GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
            }
            fonts=allFonts;
        }
        for(int i=0; i<fonts.length; i++) {
            if (fontname.compareToIgnoreCase(fonts[i])==0) {
                return true;
            }
        }
        return false;
    }

    /** Asks the user to choose a font; returns "" if the user cancels the request. */
    public static String askFont(Frame parentFrame) {
        String[] fonts;
        synchronized (OurDialog.class) {
            if (allFonts==null) {
                allFonts=GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
            }
            fonts=allFonts;
        }
        JComboBox jcombo = new JComboBox(fonts);
        int ans=JOptionPane.showOptionDialog(parentFrame, new Object[]{
                "Please choose the new font:",
                jcombo
                },
                "Font",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{"Ok","Cancel"},
                "Cancel");
        Object value=jcombo.getSelectedItem();
        if (ans!=JOptionPane.YES_OPTION || !(value instanceof String)) {
            return "";
        }
        return ((String)value);
    }

    /**
     * Use the platform's preferred file chooser to ask the user to select a file.
     * <br> Note: if it is a save operation, and the user didn't include an extension, then we'll add the extension.
     * @param parentFrame - the parent frame
     * @param isOpen - true means this is an Open operation; false means this is a Save operation
     * @param dir - the initial directory
     * @param ext - the file extension (including "."; using lowercase letters; for example, ".als") or ""
     * @param description - the description for the given extension
     * @return null if the user didn't choose anything, otherwise it returns the selected file
     */
    public static File askFile
    (Frame parentFrame, boolean isOpen, String dir, final String ext, final String description) {
        String ans;
        if (Util.onMac()) {
            FileDialog f = new FileDialog(parentFrame, isOpen?"Open...":"Save...");
            f.setMode(isOpen ? FileDialog.LOAD : FileDialog.SAVE);
            f.setDirectory(dir);
            if (ext.length()>0) f.setFilenameFilter(new FilenameFilter() {
                public boolean accept(File dir, String name) {
                    return name.toLowerCase().endsWith(ext);
                }
            });
            f.setVisible(true); // This method blocks until the user either chooses something or cancels the dialog.
            if (f.getFile()==null) {
                return null;
            }
            ans=f.getDirectory()+File.separatorChar+f.getFile();
        } else {
            JFileChooser open=new JFileChooser(dir);
            open.setDialogTitle(isOpen?"Open...":"Save...");
            open.setApproveButtonText(isOpen?"Open":"Save");
            if (ext.length()>0) open.setFileFilter(new FileFilter() {
                public boolean accept(File f) {
                    return !f.isFile() || f.getPath().toLowerCase().endsWith(ext);
                }
                public String getDescription() {
                    return description;
                }
            });
            if (open.showOpenDialog(parentFrame)!=JFileChooser.APPROVE_OPTION || open.getSelectedFile()==null) {
                return null;
            }
            ans=open.getSelectedFile().getPath();
        }
        if (!isOpen && ans.lastIndexOf('.')<0) {
            ans=ans+ext;
        }
        return new File(Util.canon(ans));
    }

    /** Display "msg" in a dialogbox, and ask the user to choose yes versus no (default==no). */
    public static boolean yesno(Frame parentFrame, String msg, String yes, String no) {
        return JOptionPane.showOptionDialog(parentFrame,
                msg,
                "Question",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{yes,no},
                no) == JOptionPane.YES_OPTION;
    }

    /** Display "msg" in a dialogbox, and ask the user to choose "Yes" versus "No" (default==no). */
    public static boolean yesno(Frame parentFrame, String msg) {
        return yesno(parentFrame, msg, "Yes", "No");
    }

    /** Display a modal dialog window containing the "objects"; returns true iff the user clicks Ok. */
    public static boolean getInput(Frame parentFrame, String title, Object... objects) {
        Object main="Ok";
        for(int i=0; i<objects.length; i++) {
            if (objects[i] instanceof JTextField || objects[i] instanceof JTextArea) {
                main=objects[i];
                break;
            }
        }
        final JOptionPane pane=new JOptionPane(
            objects,
            JOptionPane.QUESTION_MESSAGE,
            JOptionPane.YES_NO_OPTION,
            null,
            new Object[]{"Ok","Cancel"},
            main);
        final JDialog jd=pane.createDialog(parentFrame,title);
        for (int i=0; i<objects.length; i++) {
            if (!(objects[i] instanceof JTextField || objects[i] instanceof JCheckBox)) {
                continue;
            }
            JComponent x=(JComponent)(objects[i]);
            x.addKeyListener(new KeyListener() {
                public final void keyPressed(KeyEvent e) {
                    if (e.getKeyCode()==KeyEvent.VK_ENTER) {
                        pane.setValue("Ok");
                        jd.dispose();
                    }
                }
                public final void keyTyped(KeyEvent e) {}
                public final void keyReleased(KeyEvent e) {}
            });
        }
        jd.setVisible(true);
        return pane.getValue()=="Ok";
    }

    /** Display a simple window showing some text. */
    public static JFrame showtext(String title, String text, boolean autoLineWrap) {
        final JFrame window = new JFrame(title);
        final JButton done = new JButton("Close");
        done.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) {
                window.dispose();
            }
        });
        JTextArea textarea = new JTextArea(text);
        textarea.setBackground(Color.WHITE);
        textarea.setEditable(false);
        textarea.setLineWrap(autoLineWrap);
        textarea.setWrapStyleWord(autoLineWrap);
        JScrollPane scrollPane = new JScrollPane(textarea, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
        window.getContentPane().setLayout(new BorderLayout());
        window.getContentPane().add(scrollPane, BorderLayout.CENTER);
        window.getContentPane().add(done, BorderLayout.SOUTH);
        window.pack();
        window.setLocation(100,100);
        window.setSize(500,500);
        window.setVisible(true);
        return window;
    }

    // Returns a simple console that allows the user to interactively send queries and get answers from the "computer"
    public static JTextComponent showConsole(String title, String welcomeHtmlMessage, final Computer computer) {
        final SimpleAttributeSet bold = new SimpleAttributeSet();
        StyleConstants.setBold(bold, true);
        final SimpleAttributeSet green = new SimpleAttributeSet();
        StyleConstants.setForeground(green, Color.GREEN);
        final SimpleAttributeSet blue = new SimpleAttributeSet();
        StyleConstants.setForeground(blue, Color.BLUE);
        final SimpleAttributeSet red = new SimpleAttributeSet();
        StyleConstants.setForeground(red, Color.RED);
        final JEditorPane textarea = new JEditorPane(
            "text/html",
            "<html><body style=\"font:14pt Verdana;\">"
            +(welcomeHtmlMessage.trim())+"<br><b>Eval&gt;</b>&nbsp;</body></html>");
        final Document doc = textarea.getDocument();
        final IntegerBox box = new IntegerBox(doc.getLength()); // Stores the caret position right after the latest "Eval>&nbsp;"
        textarea.addCaretListener(new CaretListener() {
            public final void caretUpdate(CaretEvent e) {
                // Ensure existing text are not editable
                int a=e.getDot(), b=e.getMark(), n=box.get();
                textarea.setEditable(a>=n && b>=n);
            }
        });
        textarea.addKeyListener(new KeyListener() {
            public final void keyTyped(KeyEvent e) {
                // If the caret is right after "Eval>", and the user hits BACKSPACE, this will restore the deleted space
                while(doc.getLength()<box.get()) {
                    try {
                        doc.insertString(doc.getLength()," ",null);
                    } catch(BadLocationException ex) {
                        // Should not happen
                    }
                }
                // If the caret is before "Eval>", this will move the cursor to the end of the document before processing the key
                int c=textarea.getCaretPosition(), b=box.get(), d=doc.getLength();
                if (c<b) textarea.setCaretPosition(d);
                // If the user hit ENTER, then...
                if (e.getKeyChar()!='\n' && e.getKeyChar()!='\r') return;
                try {
                    // Send the query to the computer
                    String ans=null, err=null;
                    try {
                        ans=linewrap(computer.compute(doc.getText(b,d-b)));
                    } catch(RuntimeException ex) {
                        err=linewrap("Error: "+ex.getMessage());
                    } catch(Exception ex) {
                        err=linewrap("Error: "+ex.toString());
                    }
                    // Find out whether there was already a linebreak at the end of the user input or not
                    String n=doc.getText(d-1,1);
                    n = (n.charAt(0)=='\n' || n.charAt(0)=='\r') ? "\n   " : "\n\n   ";
                    // Add a linebreak if needed, then add the answer, then add another "Eval> " prompt.
                    if (err!=null) doc.insertString(d, n+err+"\n", red); else doc.insertString(d, n+ans+"\n", blue);
                    d=doc.getLength();
                    doc.insertString(d, "\nEval> ", null);
                    if (doc instanceof DefaultStyledDocument) {
                        ((DefaultStyledDocument)doc).setCharacterAttributes(d, 6, bold, false);
                    }
                    d=doc.getLength();
                    textarea.setCaretPosition(d);
                    box.set(d);
                } catch(Exception ex) { }
            }
            public final void keyPressed(KeyEvent e) { }
            public final void keyReleased(KeyEvent e) { }
        });
        textarea.setBackground(Color.WHITE);
        textarea.setEditable(true);
        return textarea;
    }

    // Try to wrap the input (but if a token is too longer, we won't break it)
    private static String linewrap(String msg) {
        int lb=msg.indexOf('\n');
        if (lb>=0) return linewrap(msg.substring(0,lb))+"\n   "+linewrap(msg.substring(lb+1));
        StringBuilder sb=new StringBuilder();
        StringTokenizer tokenizer=new StringTokenizer(msg.trim(), "\n\r\t ");
        int max=50, now=0;
        while(tokenizer.hasMoreTokens()) {
            String x=tokenizer.nextToken();
            if (now>0) {
                if (now+1+x.length() > max) {now=0; sb.append("\n   ");} else {now++; sb.append(' ');}
            }
            now=now+x.length();
            sb.append(x);
        }
        return sb.toString();
    }
}
