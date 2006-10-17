package edu.mit.csail.sdg.alloy4;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

/**
 * This wrapper around JMenuItem provides additional convenience methods.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurMenuItem extends JMenuItem {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Construct a new MenuItem then add it to an existing Menu.
     * @param parent - the Menu to add this MenuItem into
     * @param label - the text to show on the menu
     * @param accel - the accelerator (eg. KeyEvent.VK_F); we will add the "SHIFT" mask on top of it
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     */
    public OurMenuItem(JMenu parent, String label, int accel, final OurFunc0 func) {
        super(label);
        int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        setAccelerator(KeyStroke.getKeyStroke(accel, accelMask | InputEvent.SHIFT_MASK));
        if (func!=null) addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { func.run(); }
        });
        parent.add(this);
    }

    /**
     * Construct a new MenuItem then add it to an existing Menu.
     * @param parent - the Menu to add this MenuItem into
     * @param label - the text to show on the menu
     * @param key - the mnemonic (eg. KeyEvent.VK_F)
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want accelerator
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     */
    public OurMenuItem(JMenu parent, String label, int key, int accel, final OurFunc0 func) {
        super(label,key);
        if (accel!=-1) {
            int accelMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
            setAccelerator(KeyStroke.getKeyStroke(accel, accelMask));
        }
        if (func!=null) addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { func.run(); }
        });
        parent.add(this);
    }

    /**
     * Construct a new MenuItem then add it to an existing Menu.
     * @param parent - the Menu to add this MenuItem into
     * @param label - the text to show on the menu
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     */
    public OurMenuItem(JMenu parent, String label, final OurFunc0 func) {
        super(label);
        if (func!=null) addActionListener(new ActionListener() {
            public final void actionPerformed(ActionEvent e) { func.run(); }
        });
        parent.add(this);
    }
}
