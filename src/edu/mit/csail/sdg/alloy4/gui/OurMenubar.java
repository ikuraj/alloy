package edu.mit.csail.sdg.alloy4.gui;

import javax.swing.JMenuBar;
import edu.mit.csail.sdg.alloy4.util.MessageHandler;

public final class OurMenubar extends JMenuBar {

	private static final long serialVersionUID = 1L;
	
	private final MessageHandler handler;
	
	public OurMenubar(MessageHandler in_handler) {
		super();
		handler=in_handler;
	}

	public void handleMessage(Object caller, String message) { handler.handleMessage(caller,message); }

    public OurMenu addMenu(String label, boolean enabled, int mnemonic, final String message) {
        OurMenu ans=new OurMenu(this,label,enabled,mnemonic,message);
        this.add(ans);
        return ans;
    }
}
