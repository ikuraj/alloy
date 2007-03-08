package edu.mit.csail.sdg.alloy4;

import javax.swing.SwingUtilities;
import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;
import com.apple.eawt.ApplicationListener;
import edu.mit.csail.sdg.alloy4.MultiRunner.MultiRunnable;

/**
 * This class provides useful methods that may be called only on Mac OS X.
 *
 * <p> You must not call any methods here if you're not on a Mac,
 * since that will trigger the loading of com.apple.eawt.* which are not available on other platforms.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class MacUtil {

    /** Constructor is private, since this class never needs to be instantiated. */
    private MacUtil() { }

    /** The cached Application object. */
    private static Application application=null;

    /** The previous listener (or null if there was none). */
    private static ApplicationListener listener=null;

    /**
     * Register a Mac OS X "ApplicationListener"; if there was a previous listener, it will be removed first.
     * @param handler - the application listener
     * @param reopen - when the user clicks on the Dock icon, we'll call handler.run(reopen)
     * @param about - when the user clicks on About Alloy4, we'll call handler.run(about)
     * @param open - when a file needs to be opened, we'll call handler.run(open,filename)
     * @param quit - when the user clicks on Quit, we'll call handler.run(quit)
     */
    public synchronized static void registerApplicationListener
    (final MultiRunnable handler, final int reopen, final int about, final int open, final int quit) {
        if (application==null) {
            application=new Application();
        }
        if (listener!=null) {
            application.removeApplicationListener(listener);
        }
        listener=new ApplicationAdapter() {
            @Override public void handleReOpenApplication (ApplicationEvent arg) {
                SwingUtilities.invokeLater(new MultiRunner(handler, reopen));
            }
            @Override public void handleAbout (ApplicationEvent arg0) {
                arg0.setHandled(true);
                SwingUtilities.invokeLater(new MultiRunner(handler, about));
            }
            @Override public void handleOpenFile (ApplicationEvent arg0) {
                SwingUtilities.invokeLater(new MultiRunner(handler, open, arg0.getFilename()));
            }
            @Override public void handleQuit (ApplicationEvent arg0) {
                arg0.setHandled(false); // "false" is correct; some documentation on apple.com claimed otherwise.
                SwingUtilities.invokeLater(new MultiRunner(handler, quit));
            }
        };
        application.addApplicationListener(listener);
    }
}
