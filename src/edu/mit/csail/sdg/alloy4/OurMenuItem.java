package edu.mit.csail.sdg.alloy4;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

/**
 * Graphical menu item that extends JMenuItem.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurMenuItem extends JMenuItem {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Construct a new MenuItem then add it to an existing Menu.
     * @param parent - the Menu to add this MenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want a mnemonic
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want accelerator
     * @param func - the runnable to run if the user clicks this item (or null if there is no runnable to run)
     */
    public OurMenuItem(JMenu parent, String label, int mnemonic, int accel, final Runnable func) {
        super(label);
        if (mnemonic!=-1) setMnemonic(mnemonic);
        if (accel!=-1) {
            int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
            setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        }
        if (func!=null) addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { func.run(); }
        });
        if (parent!=null) parent.add(this);
    }

    /**
     * Construct a new MenuItem then add it to an existing Menu with SHIFT+accelerator.
     * @param parent - the Menu to add this MenuItem into (or null if you don't want to add it to any JMenu yet)
     * @param label - the text to show on the menu
     * @param accel - the accelerator (eg. KeyEvent.VK_F); we will add the "SHIFT" mask on top of it
     * @param func - the action listener to call if the user clicks this item (or null if there is no action to do)
     */
    public OurMenuItem(JMenu parent, String label, int accel, ActionListener func) {
        super(label);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        setAccelerator(KeyStroke.getKeyStroke(accel, accelMask | InputEvent.SHIFT_MASK));
        if (func!=null) addActionListener(func);
        if (parent!=null) parent.add(this);
    }
}
