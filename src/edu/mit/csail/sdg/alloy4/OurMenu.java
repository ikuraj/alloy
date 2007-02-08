package edu.mit.csail.sdg.alloy4;

import java.awt.Component;
import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;

/**
 * Graphical menu that extends JMenu.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurMenu extends JMenu {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Construct a new menu and add it to an existing JMenuBar.
     *
     * <p> Note: every time the user expands then collapses this menu,
     * it will automatically enable all JMenu and JMenuItem objects inside it.
     *
     * @param parent - the JMenuBar to add this Menu into (or null if we don't want to add it to a JMenuBar yet)
     * @param label - the label to show on screen
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want mnemonic
     * @param func - the function to call if the user expands this menu (or null if there is no function to call)
     * @param key - the argument to pass to func() when the user expands this menu
     */
    public OurMenu
    (JMenuBar parent, String label, int mnemonic, final MultiRunnable func, final int key) {
        super(label,false);
        if (mnemonic!=-1 && !Util.onMac()) setMnemonic(mnemonic);
        addMenuListener(new MenuListener() {
            public final void menuSelected(MenuEvent e) { if (func!=null) func.run(key); }
            public final void menuDeselected(MenuEvent e) { enableChildren(OurMenu.this); }
            public final void menuCanceled(MenuEvent e) { enableChildren(OurMenu.this); }
        });
        if (parent!=null) parent.add(this);
    }

    /**
     * Convenience method that creates a new MenuItem and add it to this Menu.
     * @param icon - the icon to show on the left of the label (or null if you don't want an icon)
     * @param label - the label for the new MenuItem
     * @param enabled - whether the new MenuItem should be initially enabled or disabled
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want mnemonic
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want accelerator
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     * @param key - the argument to pass to func() when the user clicks this item
     * @return the newly constructed OurMenuItem object
     */
    public OurMenuItem addMenuItem
    (Icon icon, String label, boolean enabled, int mnemonic, int accel, MultiRunnable func, int key) {
        // OurMenuItem's constructor will add the new item into the list, so we don't have to call add() here.
        OurMenuItem ans = new OurMenuItem(this, label, mnemonic, accel, new MultiRunner(func,key));
        if (!enabled) ans.setEnabled(false);
        if (icon!=null) ans.setIcon(icon);
        return ans;
    }

    /**
     * Convenience method that creates a new MenuItem and add it to this Menu.
     * @param icon - the icon to show on the left of the label (or null if you don't want an icon)
     * @param label - the label for the new MenuItem
     * @param enabled - whether the new MenuItem should be initially enabled or disabled
     * @param accel - the accelerator (eg. KeyEvent.VK_F); we will add the SHIFT mask on top of it
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     * @param key - the argument to pass to func() when the user clicks this item
     * @return the newly constructed OurMenuItem object
     */
    public OurMenuItem addMenuItem
    (Icon icon, String label, boolean enabled, int accel, MultiRunnable func, int key) {
        // OurMenuItem's constructor will add the new item into the list, so we don't have to call add() here.
        OurMenuItem ans = new OurMenuItem(this, label, accel, new MultiRunner(func,key));
        if (!enabled) ans.setEnabled(false);
        if (icon!=null) ans.setIcon(icon);
        return ans;
    }

    /** Convenience method that recursively enables every JMenu and JMenuItem inside. */
    public void enableChildren() { enableChildren(this); }

    /**
     * Convenience method that recursively enables every JMenu and JMenuItem inside "menu".
     * @param menu - the menu to start the recursive search
     */
    private static void enableChildren(JMenu menu) {
        for(int i=0; i<menu.getMenuComponentCount(); i++) {
            Component obj=menu.getMenuComponent(i);
            if (obj instanceof JMenuItem) ((JMenuItem)obj).setEnabled(true);
            else if (obj instanceof JMenu) enableChildren((JMenu)obj);
        }
    }
}
