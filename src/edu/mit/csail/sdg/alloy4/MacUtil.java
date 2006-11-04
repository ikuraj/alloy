package edu.mit.csail.sdg.alloy4;

import javax.swing.SwingUtilities;
import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;
import com.apple.eawt.ApplicationListener;

/**
 * This class provides useful methods that may be called only on Mac OS X.
 *
 * <p/> You must not call any methods here if you're not on a Mac,
 * since that will trigger the loading of com.apple.eawt.* which are not available on other platforms.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 */
public final class MacUtil {

    /** Constructor is private, since this class never needs to be instantiated. */
    private MacUtil() { }

    /** The cached Application object. */
    private static Application application=null;

    /** The previous listener (null if there was none). */
    private static ApplicationListener listener=null;

    /**
     * Register a Mac OS X "ApplicationListener"; if there was a previous listener, it will be removed.
     * @param reopen - when the user clicks on the Dock icon, this function will be called
     * @param about - when the user clicks on About, this function will be called
     * @param open - when a file needs to be opened, this function will be called with the filename
     * @param quit - when the user clicks on Quit, this function will be called
     */
    public synchronized static void registerApplicationListener
        (final OurFunc0 reopen, final OurFunc0 about, final OurFunc1 open, final OurFunc0 quit) {
        if (application==null) application=new Application();
        if (listener!=null) application.removeApplicationListener(listener);
        listener=new ApplicationAdapter() {
            @Override public void handleReOpenApplication(ApplicationEvent arg) {
                SwingUtilities.invokeLater(new Runnable() { public void run() { reopen.run(); } });
            }
            @Override public void handleAbout(ApplicationEvent arg0) {
                arg0.setHandled(true);
                SwingUtilities.invokeLater(new Runnable() { public void run() { about.run(); } });
            }
            @Override public void handleOpenFile(ApplicationEvent arg0) {
                final String filename=arg0.getFilename();
                SwingUtilities.invokeLater(new Runnable() { public void run() { open.run(filename); } });
            }
            @Override public void handleQuit(ApplicationEvent arg0) {
                arg0.setHandled(false); // "false" is correct; some documentation on apple.com claims otherwise.
                SwingUtilities.invokeLater(new Runnable() { public void run() { quit.run(); } });
            }
        };
        application.addApplicationListener(listener);
    }
}
