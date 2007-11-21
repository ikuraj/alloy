/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import javax.swing.SwingUtilities;
import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationListener;
import com.apple.eawt.ApplicationEvent;

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
    private static Application application = null;

    /** The previous listener (or null if there was none). */
    private static ApplicationListener listener = null;

    /**
     * Register a Mac OS X "ApplicationListener"; if there was a previous listener, it will be removed first.
     * @param reopen - when the user clicks on the Dock icon, we'll call reopen.run() using SwingUtilities.invokeLater
     * @param about - when the user clicks on About Alloy4, we'll call about.run() using SwingUtilities.invokeLater
     * @param open - when a file needs to be opened, we'll call open.run(filename) using SwingUtilities.invokeLater
     * @param quit - when the user clicks on Quit, we'll call quit.run() using SwingUtilities.invokeAndWait
     */
    public synchronized static void registerApplicationListener
    (final Runnable reopen, final Runnable about, final Runner open, final Runnable quit) {
        if (application==null) {
            application=new Application();
        } else if (listener!=null) {
            application.removeApplicationListener(listener);
        }
        listener=new ApplicationAdapter() {
            @Override public final void handleReOpenApplication (final ApplicationEvent arg) {
                SwingUtilities.invokeLater(reopen);
            }
            @Override public final void handleAbout (final ApplicationEvent arg) {
                arg.setHandled(true);
                SwingUtilities.invokeLater(about);
            }
            @Override public final void handleOpenFile (final ApplicationEvent arg) {
                SwingUtilities.invokeLater(new Runnable() {
                    public final void run() { open.run(arg.getFilename()); }
                });
            }
            @Override public final void handleQuit (final ApplicationEvent arg) {
                OurUtil.invokeAndWait(quit);
                arg.setHandled(false); // "false" is correct; some documentation on apple.com claimed otherwise.
            }
        };
        application.addApplicationListener(listener);
    }
}
