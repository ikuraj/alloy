package edu.mit.csail.sdg.alloy4.client;

import com.apple.eawt.Application;
import com.apple.eawt.ApplicationEvent;
import com.apple.eawt.ApplicationListener;

import edu.mit.csail.sdg.alloy4util.Util;

public final class MacListener extends Application implements ApplicationListener {

	private final Util.MessageHandler handler;
	
	public MacListener(Util.MessageHandler handler) {
		super();
		this.handler=handler;
		addApplicationListener(this);
	}
	
	public void handleAbout(ApplicationEvent arg0) { }
	public void handleOpenApplication(ApplicationEvent arg0) { }
	public void handleOpenFile(ApplicationEvent arg0) {	}
	public void handlePreferences(ApplicationEvent arg0) { }
	public void handlePrintFile(ApplicationEvent arg0) { }
	public void handleQuit(ApplicationEvent arg0) { arg0.setHandled(false); handler.handleMessage(null,"quit"); } 
	public void handleReOpenApplication(ApplicationEvent arg0) { }
}
