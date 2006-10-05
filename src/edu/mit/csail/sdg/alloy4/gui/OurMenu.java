package edu.mit.csail.sdg.alloy4.gui;

import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import edu.mit.csail.sdg.alloy4.util.Util;

public final class OurMenu extends JMenu {

	private static final long serialVersionUID = 1L;
	
	public final OurMenubar parent;
	
	public OurMenu(OurMenubar in_parent, String label, boolean enabled, int mnemonic, final String message) {
		super(label,false);
		parent=in_parent;
        if (!Util.onMac()) setMnemonic(mnemonic);
        if (message!=null) addMenuListener(new MenuListener() {
            public final void menuSelected(MenuEvent e) { parent.handleMessage(OurMenu.this, message); }
            public final void menuDeselected(MenuEvent e) { }
            public final void menuCanceled(MenuEvent e) { }
        });
        setEnabled(enabled);
	}

	public void handleMessage(Object caller, String message) { parent.handleMessage(caller,message); }

    public OurMenuItem addMenuItem(Icon icon, String label, boolean enabled, int key, int accel, final String message) {
        OurMenuItem ans = new OurMenuItem(this,label,key,accel,message);
        ans.setEnabled(enabled);
        add(ans);
        ans.setIcon(icon);
        return ans;
    }

    public OurMenuItem addMenuItem(String label, boolean enabled, int key, int accel, final String message) {
        OurMenuItem ans = new OurMenuItem(this,label,key,accel,message);
        ans.setEnabled(enabled);
        add(ans);
        return ans;
    }
    
    public void setIconForChildren(Icon icon) {
    	for(int i=0; i<getItemCount(); i++) {
    		Object obj=getItem(i);
    		if (obj instanceof JMenuItem) ((JMenuItem)obj).setIcon(icon);
    	}
    }
}
