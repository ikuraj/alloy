package edu.mit.csail.sdg.alloy4util;

import java.awt.Component;
import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/**
 * This wrapper around JMenu provides additional convenience methods.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public final class OurMenu extends JMenu {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new Menu and add it to an existing MenuBar.
     * @param parent - the MenuBar to add this Menu into (or null if we don't want to add it to a JMenuBar yet)
     * @param label - the label to show on screen
     * @param mnemonic - the mnemonic (eg. KeyEvent.VK_F), or -1 if you don't want mnemonic
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     */
    public OurMenu(JMenuBar parent, String label, int mnemonic, final Func0 func) {
        super(label,false);
        if (mnemonic!=-1 && !Util.onMac()) setMnemonic(mnemonic);
        if (func!=null) addMenuListener(new MenuListener() {
            public final void menuSelected(MenuEvent e) { func.run(); }
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
     * @param key - the mnemonic (eg. KeyEvent.VK_F)
     * @param accel - the accelerator (eg. KeyEvent.VK_F), or -1 if you don't want accelerator
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     * @return the newly constructed OurMenuItem object
     */
    public OurMenuItem addMenuItem(Icon icon, String label, boolean enabled, int key, int accel, Func0 func) {
        // OurMenuItem's constructor will add the new item into the list, so we don't have to call add() here.
        OurMenuItem ans = new OurMenuItem(this,label,key,accel,func);
        ans.setEnabled(enabled);
        if (icon!=null) ans.setIcon(icon);
        return ans;
    }

    /**
     * Convenience method that creates a new MenuItem and add it to this Menu.
     * @param icon - the icon to show on the left of the label (or null if you don't want an icon)
     * @param label - the label for the new MenuItem
     * @param enabled - whether the new MenuItem should be initially enabled or disabled
     * @param func - the function to call if the user clicks this item (or null if there is no function to call)
     * @return the newly constructed OurMenuItem object
     */
    public OurMenuItem addMenuItem(Icon icon, String label, boolean enabled, Func0 func) {
        // OurMenuItem's constructor will add the new item into the list, so we don't have to call add() here.
        OurMenuItem ans = new OurMenuItem(this,label,func);
        ans.setEnabled(enabled);
        if (icon!=null) ans.setIcon(icon);
        return ans;
    }

    /**
     * Convenience method that recursively changes all MenuItem objects under "this menu" to use "icon" as the icon.
     * @param icon - the new icon to use
     */
    public void setIconForChildren(Icon icon) { setIconForChildren(this,icon); }

    /**
     * Convenience method that recursively changes all MenuItem objects under "menu" to use "icon" as the icon.
     * @param menu - the menu to start the search
     * @param icon - the new icon to use
     */
    private static void setIconForChildren(JMenu menu, Icon icon) {
        for(int i=0; i<menu.getMenuComponentCount(); i++) {
            Component obj=menu.getMenuComponent(i);
            if (obj instanceof JMenuItem) ((JMenuItem)obj).setIcon(icon);
            else if (obj instanceof JMenu) setIconForChildren((JMenu)obj, icon);
        }
    }

    /**
     * Convenience method that recursively enable every Menu and MenuItem under "menu".
     * @param menu - the menu to start the search
     */
    private static void enableChildren(JMenu menu) {
        for(int i=0; i<menu.getMenuComponentCount(); i++) {
            Component obj=menu.getMenuComponent(i);
            if (obj instanceof JMenuItem) ((JMenuItem)obj).setEnabled(true);
            else if (obj instanceof JMenu) enableChildren((JMenu)obj);
        }
    }
}
