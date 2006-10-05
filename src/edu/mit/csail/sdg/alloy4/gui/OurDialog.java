package edu.mit.csail.sdg.alloy4.gui;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

public final class OurDialog {

	/** This constructor is private, since this utility class never needs to be instantiated. */
	private OurDialog() { }
	
    /** Default is no. */
    public static boolean yesno(JFrame parentFrame, String message, String yes, String no) {
        int ans=JOptionPane.showOptionDialog(parentFrame, message, "Warning!",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{yes,no},
                no);
        return ans==JOptionPane.YES_OPTION;
    }

    /** Default is no. */
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

    /** Default is CANCEL; return null if cancel, TRUE if save, FALSE if discard. */
    public static Boolean questionSaveDiscardCancel(JFrame parentFrame) {
        String save="Save", discard="Discard", cancel="Cancel";
        int ans=JOptionPane.showOptionDialog(parentFrame,
                "The content has not been saved. Do you wish to save it, discard it, or cancel the operation?",
                "Warning: the content has not been saved!",
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE,
                null,
                new Object[]{save,discard,cancel},
                cancel);
        if (ans==JOptionPane.YES_OPTION) return Boolean.TRUE;
        if (ans!=JOptionPane.NO_OPTION) return null; else return Boolean.FALSE;
    }
}
