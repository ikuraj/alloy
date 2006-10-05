package edu.mit.csail.sdg.alloy4.gui;

import javax.swing.JMenuBar;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import edu.mit.csail.sdg.alloy4.util.MessageHandler;
import edu.mit.csail.sdg.alloy4.util.Util;

public final class OurMenubar extends JMenuBar {

	private static final long serialVersionUID = 1L;
	
	private final MessageHandler handler;
	
	public OurMenubar(MessageHandler handler) { this.handler=handler; }
	
    public OurMenu addMenu(String label, boolean enabled, int mnemonic, final String message) {
        OurMenu ans=new OurMenu(handler,label,false);
        if (!Util.onMac()) ans.setMnemonic(mnemonic);
        if (handler!=null) ans.addMenuListener(new MenuListener() {
            public final void menuSelected(MenuEvent e) { handler.handleMessage(message); }
            public final void menuDeselected(MenuEvent e) { }
            public final void menuCanceled(MenuEvent e) { }
        });
        ans.setEnabled(enabled);
        this.add(ans);
        return ans;
    }
}

