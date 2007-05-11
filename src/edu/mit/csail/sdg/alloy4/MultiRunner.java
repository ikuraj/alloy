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

import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/** This class encapsulates a Runnable. */

public final class MultiRunner extends AbstractAction implements MenuListener, WindowListener, Runnable {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** This defines a Runnable that can be encapsulated in a MultiRunner. */
    public interface MultiRunnable {
        /** Returns true if the method succeeds; you can call this method only from the AWT event thread. */
        public boolean run(int key);
        /** Returns true if the method succeeds; you can call this method only from the AWT event thread. */
        public boolean run(int key, int arg);
        /** Returns true if the method succeeds; you can call this method only from the AWT event thread. */
        public boolean run(int key, String arg);
    }

    /** The runnable encapsulated inside this runner. */
    private final MultiRunnable runnable;

    /** The key to pass to the runnable. */
    private final int key;

    /** 0 if there is no argument; 1 if the argument is integer; 2 if the argument is String. */
    private final int arg;

    /** The integer argument to pass to the runnable. */
    private final int intArg;

    /** The String argument to pass to the runnable. */
    private final String stringArg;

    /** Constructs a new runner to encapsulate the given runnable that takes an int argument. */
    public MultiRunner(MultiRunnable runnable, int key) {
        this.runnable=runnable;
        this.key=key;
        this.arg=0;
        this.intArg=0;
        this.stringArg="";
    }

    /** Constructs a new runner to encapsulate the given runnable that takes two int arguments. */
    public MultiRunner(MultiRunnable runnable, int key, int arg) {
        this.runnable=runnable;
        this.key=key;
        this.arg=1;
        this.intArg=arg;
        this.stringArg="";
    }

    /** Constructs a new runner to encapsulate the given runnable that takes an int argument and a String argument. */
    public MultiRunner(MultiRunnable runnable, int key, String arg) {
        this.runnable=runnable;
        this.key=key;
        this.arg=2;
        this.intArg=0;
        this.stringArg=arg;
    }

    /** This method is defined in java.lang.Runnable */
    public void run() {
        if (arg==0) {
            runnable.run(key);
        } else if (arg==1) {
            runnable.run(key,intArg);
        } else {
            runnable.run(key,stringArg);
        }
    }

    /** This method is defined in java.awt.event.ActionListener; (this implementation calls this.run()) */
    public void actionPerformed(ActionEvent e) { run(); }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation calls this.run()) */
    public void menuSelected(MenuEvent e) { run(); }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation does nothing) */
    public void menuDeselected(MenuEvent e) { }

    /** This method is defined in javax.swing.event.MenuListener; (this implementation does nothing) */
    public void menuCanceled(MenuEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation calls this.run()) */
    public void windowClosing(WindowEvent e) { run(); }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowClosed(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowOpened(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowIconified(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowDeiconified(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowActivated(WindowEvent e) { }

    /** This method is defined in java.awt.event.WindowListener; (this implementation does nothing) */
    public void windowDeactivated(WindowEvent e) { }
}
