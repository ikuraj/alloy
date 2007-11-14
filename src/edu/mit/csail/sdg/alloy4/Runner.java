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

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/** This class converts a Runnable into an AbstractAction, WindowListener, and MenuListener also. */

public abstract class Runner extends AbstractAction implements Runnable, WindowListener, MenuListener {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** Constructs a new runner to encapsulate the given runnable that takes an int argument. */
    public Runner() { }

    /** This method should be overriden to provide the default action that this Runner would perform. */
    public abstract void run();

    /** This method should be overriden to provide the default action that this Runner would perform given an argument. */
    public abstract void run(Object arg);

    /** This method is defined in java.awt.event.ActionListener; (this implementation calls this.run()) */
    public final void actionPerformed(ActionEvent e) { run(); }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation calls this.run()) */
    public final void menuSelected(MenuEvent e) { run(); }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation does nothing) */
    public final void menuDeselected(MenuEvent e) { }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation does nothing) */
    public final void menuCanceled(MenuEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation calls this.run()) */
    public final void windowClosing(WindowEvent e) { run(); }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowClosed(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowOpened(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowIconified(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowDeiconified(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowActivated(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public final void windowDeactivated(WindowEvent e) { }

    /** This helper method returns a Runnable whose run() method will call window.dispose() */
    public static final Runner createDispose(final Window window) {
        return new Runner() {
            private static final long serialVersionUID = 1L;
            public void run() { window.dispose(); }
            public void run(Object arg) { window.dispose(); }
        };
    }
}
