package edu.mit.csail.sdg.alloy4.util;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.PrintWriter;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/**
 * Utility class for doing common I/O and XML and GUI operations.
 *
 * @author Felix Chang
 */
public final class Util {

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private Util() { }

    /** Returns true iff running on a Mac OS X, with look and feel of Aqua **/
    public static boolean onMac() {
        return System.getProperty("mrj.version") != null
            && UIManager.getSystemLookAndFeelClassName().equals(UIManager.getLookAndFeel().getClass().getName());
    }

    /** Returns the recommended font name to use, based on the OS. */
    public static String getFontName() { if (onMac()) return "LucidaGrande"; else return "Monospaced"; }

    /** Returns the recommended font size to use, based on the OS. */
    public static int getFontSize() { if (onMac()) return 12; else return 12; }

    /** Returns the recommended Font to use, based on the OS. */
    public static Font getFont() { return new Font(getFontName(), Font.PLAIN, getFontSize()); }

    /** Make a JScrollPane. */
    public static JScrollPane makeJScrollPane(Component component) {
        JScrollPane ans = new JScrollPane(component,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        ans.setMinimumSize(new Dimension(50, 50));
        return ans;
    }

    /** Make a JMenu. */
    public static JMenu makeJMenu(JMenuBar parent, String label, boolean enabled, int mnemonic, final MessageHandler handler, final String message) {
        JMenu ans=new JMenu(label,false);
        if (!onMac()) ans.setMnemonic(mnemonic);
        if (handler!=null) ans.addMenuListener(new MenuListener() {
            public final void menuSelected(MenuEvent e) { handler.handleMessage(message); }
            public final void menuDeselected(MenuEvent e) { }
            public final void menuCanceled(MenuEvent e) { }
        });
        ans.setEnabled(enabled);
        parent.add(ans);
        return ans;
    }

    /** Make a JMenu. */
    public static JMenuItem makeJMenuItem(JMenu parent, String label, int key, int accel, final MessageHandler handler, final String message) {
        JMenuItem ans = new JMenuItem(label,key);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        if (accel>=0) ans.setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        if (handler!=null) ans.addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { handler.handleMessage(message); }
        });
        parent.add(ans);
        return ans;
    }

    /** Make a JLabel. */
    public static JLabel makeJLabel(String label, Font font) {
        JLabel ans = new JLabel(label);
        ans.setFont(font);
        return ans;
    }

    /**
     * Write a String into a PrintWriter, and encode special characters are XML-encoded.
     *
     * <p/>
     * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
     * into the lt; gt; amp; apos; and quot; encoding. And it turns any characters outside of 32..126 range
     * into the #xHHHH encoding (where HHHH is the 4 digit hexadecimal representation of the character value).
     *
     * @param out - the PrintWriter to write into
     * @param str - the String to write out
     */
    public static void encodeXML(PrintWriter out, String str) {
        int n=str.length();
        for(int i=0; i<n; i++) {
            char c=str.charAt(i);
            if (c=='<') { out.write("&lt;"); continue; }
            if (c=='>') { out.write("&gt;"); continue; }
            if (c=='&') { out.write("&amp;"); continue; }
            if (c=='\'') { out.write("&apos;"); continue; }
            if (c=='\"') { out.write("&quot;"); continue; }
            if (c>=32 && c<127) { out.write(c); continue; }
            out.write("&#x");
            String v=Integer.toString((int)c, 16);
            while(v.length()<4) v="0"+v;
            out.write(v);
            out.write(';');
        }
    }

    /**
     * Write a list of Strings into a PrintWriter, where strs[2n] are written as-is, and strs[2n+1] are XML-encoded.
     *
     * <p/> For example, if you call encodeXML(out, A, B, C, D, E), it desugars into the following:
     * <br/> out.print(A);
     * <br/> out.encodeXML(B);
     * <br/> out.print(C);
     * <br/> out.encodeXML(D);
     * <br/> out.print(E);
     * <br/> In other words, it writes the even entries as-is, and print the odd entries using XML encoding.
     *
     * @param out - the PrintWriter to write into
     * @param strs - the list of Strings to write out
     */
    public static void encodeXMLs(PrintWriter out, String... strs) {
        for(int i=0; i<strs.length; i++) {
            if ((i%2)==0) out.print(strs[i]); else encodeXML(out,strs[i]);
        }
    }
    
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
