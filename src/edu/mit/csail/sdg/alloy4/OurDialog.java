package edu.mit.csail.sdg.alloy4;

import java.io.File;
import java.io.FilenameFilter;
import java.awt.FileDialog;
import java.awt.Frame;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 * Graphical dialog methods for asking the user some questions.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 */

public final class OurDialog {

    /** The constructor is private, since this utility class never needs to be instantiated. */
    private OurDialog() { }

    /** Popup the given error message. */
    public static void alert(JFrame parentFrame, Object message, String title) {
        JOptionPane.showMessageDialog(parentFrame, message, title, JOptionPane.PLAIN_MESSAGE);
    }

    /** Popup the given error message, then terminate the program. */
    public static void fatal(JFrame parentFrame, Object message) {
        JOptionPane.showMessageDialog(parentFrame, message, "A fatal error has occurred.", JOptionPane.ERROR_MESSAGE);
        System.exit(1);
    }

    /**
     * Ask if the user wishes to save the file, discard the file, or cancel the entire operation (default==cancel).
     * @return null if cancel, true if save, false if discard
     */
    public static Boolean askSaveDiscardCancel(JFrame parentFrame, String description) {
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
        if (ans==JOptionPane.YES_OPTION) return Boolean.TRUE;
        if (ans!=JOptionPane.NO_OPTION) return null; else return Boolean.FALSE;
    }

    /** Ask if the user really wishes to overwrite the file (default=no). */
    public static boolean askOverwrite(JFrame parentFrame, String filename) {
        String yes="Overwrite", no="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame,
                "The file \""+filename+"\" already exists. Do you wish to overwrite it?",
                "Warning: the file already exists.",
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
     * @param parentFrame - the parent frame
     * @param isOpen - true means this is an Open operation; false means this is a Save operation
     * @param dir - the initial directory
     * @param ext - the file extension (including ".") (using lowercase letters); for example, ".als"
     * @param description - the description for the given extension
     * @return null if the user didn't choose anything, otherwise it returns the selected file
     */
    public static File askFile(
            Frame parentFrame, boolean isOpen, String dir,
            final String ext, final String description) {
        File ans;
        if (Util.onMac()) {
            FileDialog f = new FileDialog(parentFrame, isOpen?"Open...":"Save...");
            f.setMode(isOpen ? FileDialog.LOAD : FileDialog.SAVE);
            f.setDirectory(dir);
            f.setFilenameFilter(new FilenameFilter() {
                public boolean accept(File dir, String name) { return name.toLowerCase().endsWith(ext); }
            });
            f.setVisible(true); // This method blocks until the user either chooses something or cancels the dialog.
            if (f.getFile()==null) return null;
            ans=new File(f.getDirectory()+File.separatorChar+f.getFile());
        } else {
            JFileChooser open=new JFileChooser(dir);
            open.setDialogTitle(isOpen?"Open...":"Save...");
            open.setApproveButtonText(isOpen?"Open":"Save");
            FileFilter filter = new FileFilter() {
                public boolean accept(File f) { return !f.isFile() || f.getPath().toLowerCase().endsWith(ext); }
                public String getDescription() { return description; }
            };
            open.setFileFilter(filter);
            if (open.showOpenDialog(parentFrame)!=JFileChooser.APPROVE_OPTION) return null;
            ans=open.getSelectedFile();
        }
        if (ans!=null && !isOpen && ans.getName().lastIndexOf('.')<0) ans=new File(ans.getAbsolutePath()+ext);
        return ans;
    }

    /** Display "msg" in a dialogbox, and ask the user to choose yes versus no (default==no). */
    public static boolean yesno(JFrame parentFrame, String msg, String yes, String no) {
        return JOptionPane.showOptionDialog(parentFrame, msg, "Question", JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE, null, new Object[]{yes,no}, no)==JOptionPane.YES_OPTION;
    }

    /** Display "msg" in a dialogbox, and ask the user to choose "Yes" versus "No" (default==no). */
    public static boolean yesno(JFrame parentFrame, String msg) { return yesno(parentFrame, msg, "Yes", "No"); }
}
