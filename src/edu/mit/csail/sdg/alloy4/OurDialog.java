package edu.mit.csail.sdg.alloy4;

import java.io.File;
import java.io.FilenameFilter;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Frame;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

/**
 * This class provides convenient static functions for asking the user some questions.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurDialog {

    /** The constructor is private, since this utility class never needs to be instantiated. */
    private OurDialog() { }

    /** Popup the given text in a scrollable window with the given background color. */
    public static void popup(Color background, String[] text) {
        int screenWidth=OurUtil.getScreenWidth(), width=screenWidth/3*2;
        int screenHeight=OurUtil.getScreenHeight(), height=screenHeight/3*2;
        JTextPane log=new JTextPane();
        log.setBackground(background);
        log.setEditable(false);
        StyledDocument doc=log.getStyledDocument();
        Style old=StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);
        Style styleRegular=doc.addStyle("regular", old);
        StyleConstants.setFontFamily(styleRegular, OurUtil.getFontName());
        Style styleBold=doc.addStyle("bold", styleRegular);
        StyleConstants.setBold(styleBold, true);
        Style styleGreen=doc.addStyle("green", styleBold);
        StyleConstants.setForeground(styleGreen, new Color(0.2f,0.7f,0.2f));
        for(int i=0; i<text.length; i++) {
            try {
                if (text[i].startsWith("20"))
                    doc.insertString(doc.getLength(), (i==0?"":"\n")+text[i]+"\n", styleGreen);
                else
                    doc.insertString(doc.getLength(), text[i]+"\n", styleRegular);
            } catch(BadLocationException ex) {
                Util.harmless("This should not happen; harmless anyway",ex);
            }
        }
        log.setCaretPosition(0);
        final JFrame frame=new JFrame("Alloy change log");
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        Container all=frame.getContentPane();
        all.setLayout(new BorderLayout());
        all.add(OurUtil.makeJScrollPane(log), BorderLayout.CENTER);
        if (Util.onMac()) all.add(new JLabel(" "), BorderLayout.SOUTH); // Make room for the Mac OS X "grow box"
        frame.pack();
        frame.setSize(new Dimension(width,height));
        frame.setLocation(screenWidth/6, screenHeight/6);
        frame.setVisible(true);
    }

    /** Popup the given error message. */
    public static void alert(JFrame parent, Object message, String title) {
        JOptionPane.showMessageDialog(parent, message, title, JOptionPane.PLAIN_MESSAGE);
    }

    /** Popup the given error message, then terminate the program. */
    public static void fatal(JFrame parent, Object message) {
        JOptionPane.showMessageDialog(parent, message, "A fatal error has occurred!", JOptionPane.ERROR_MESSAGE);
        System.exit(1);
    }

    /**
     * Ask if the user wishes to save the file, discard the file, or cancel the entire operation (default=cancel).
     * @return null if cancel, true if save, false if discard
     */
    public static Boolean questionSaveDiscardCancel(JFrame parentFrame, String description) {
        String save="Save", discard="Don\'t Save", cancel="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame,
                description+" has not been saved. Do you wish to save it, discard it, or cancel the operation?",
                "Warning: "+description+" has not been saved!",
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{save,discard,cancel},
                cancel);
        if (ans==JOptionPane.YES_OPTION) return Boolean.TRUE;
        if (ans!=JOptionPane.NO_OPTION) return null; else return Boolean.FALSE;
    }

    /** Ask if the user really wishes to overwrite the file (default=no). */
    public static boolean questionOverwrite(JFrame parentFrame, String filename) {
        String yes="Overwrite", no="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame,
                "The file \""+filename+"\" already exists. Do you wish to overwrite it?",
                "Warning: the file already exists!",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{yes,no},
                no);
        return ans==JOptionPane.YES_OPTION;
    }

    /**
     * Use the platform's preferred file chooser to ask the user to select a file.
     * Note: if it is a save operation, and the user didn't include an extension, then we'll add the extension.
     * @param frame - the parent frame
     * @param isOpen - true means this is an Open operation; false means this is a Save operation
     * @param dir - the initial directory
     * @param ext - the file extension including "." using lowercase letters; for example, ".als"
     * @param extDescription - the description for the given extension
     * @return null if the user didn't choose anything, otherwise it returns the selected file
     */
    public static File showFileChooser(
            Frame frame, boolean isOpen, String dir, final String ext, final String extDescription) {
        File ans;
        if (Util.onMac()) ans=showAWTchooser(frame,isOpen,dir,ext,extDescription);
        else ans=showSwingChooser(frame,isOpen,dir,ext,extDescription);
        if (ans!=null && !isOpen && ans.getName().lastIndexOf('.')<0) ans=new File(ans.getAbsolutePath()+ext);
        return ans;
    }

    /**
     * Use Swing's JFileChooser to ask the user to select a file.
     * @param frame - the parent frame
     * @param isOpen - true means this is an Open operation; false means this is a Save operation
     * @param dir - the initial directory
     * @param ext - the file extension including "." using lowercase letters; for example, ".als"
     * @param extDescription - the description for the given extension
     * @return null if the user didn't choose anything, otherwise it returns the selected file
     */
    private static File showSwingChooser(
            Frame frame, boolean isOpen, String dir, final String ext, final String extDescription) {
        JFileChooser open=new JFileChooser(dir);
        open.setDialogTitle(isOpen?"Open...":"Save...");
        open.setApproveButtonText(isOpen?"Open":"Save");
        FileFilter filter = new FileFilter() {
            public final boolean accept(File f) { return !f.isFile() || f.getPath().endsWith(ext); }
            public final String getDescription() { return extDescription; }
        };
        open.setFileFilter(filter);
        if (open.showOpenDialog(frame)!=JFileChooser.APPROVE_OPTION) return null;
        return open.getSelectedFile();
    }

    /**
     * Use AWT's FileDialog to ask the user to select a file.
     * @param frame - the parent frame
     * @param isOpen - true means this is an Open operation; false means this is a Save operation
     * @param dir - the initial directory
     * @param ext - the file extension including "." using lowercase letters; for example, ".als"
     * @param extDescription - the description for the given extension
     * @return null if the user didn't choose anything, otherwise it returns the selected file
     */
    private static File showAWTchooser(
            Frame frame, boolean isOpen, String dir, final String ext, final String extDescription) {
        FileDialog f = new FileDialog(frame, isOpen?"Open...":"Save...");
        f.setMode(isOpen ? FileDialog.LOAD : FileDialog.SAVE);
        f.setDirectory(dir);
        f.setFilenameFilter(new FilenameFilter() {
            public final boolean accept(File dir, String name) { return name.endsWith(ext); }
        });
        f.setVisible(true);
        if (f.getFile()==null) return null; else return new File(f.getDirectory()+File.separatorChar+f.getFile());
    }

    /** Display "msg" in a dialogbox, and ask the user to choose yes versus no (default=no). */
    public static boolean yesno(JFrame parentFrame, String msg, String yes, String no) {
        return JOptionPane.showOptionDialog(parentFrame, msg, "Question!", JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE, null, new Object[]{yes,no}, no)==JOptionPane.YES_OPTION;
    }

    /** Display "msg" in a dialogbox, and ask the user to choose "Yes" versus "No" (default=no). */
    public static boolean yesno(JFrame parentFrame, String msg) { return yesno(parentFrame, msg, "Yes", "No"); }
}
