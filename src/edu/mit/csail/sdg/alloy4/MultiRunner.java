package edu.mit.csail.sdg.alloy4;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/** This class encapsulates a Runnable. */

@SuppressWarnings("serial")
public final class MultiRunner extends AbstractAction implements MenuListener, ActionListener, WindowListener, Runnable {

    /** This defines a Runnable that can be encapsulated in a MultiRunner. */
    public interface MultiRunnable {
        /** Returns true if the method succeeds; you can call this method only from the AWT event thread. */
        public boolean run(int key);
        /** Returns true if the method succeeds; you can call this method only from the AWT event thread. */
        public boolean run(int key,String arg);
    }

    /** The runnable encapsulated inside this runner. */
    private final MultiRunnable runnable;

    /** The key to pass to the runnable. */
    private final int key;

    /** The argument to pass to the runnable. */
    private final String arg;

    /** Constructs a new runner to encapsulate the given runnable that takes an int argument. */
    public MultiRunner(MultiRunnable runnable, int key) {
        this.key=key; this.runnable=runnable; this.arg=null;
    }

    /** Constructs a new runner to encapsulate the given runnable that takes an int argument and a String argument. */
    public MultiRunner(MultiRunnable runnable, int key, String arg) {
        this.key=key; this.runnable=runnable; this.arg=arg;
    }

    /** This method is defined in java.lang.runnable */
    public void run() { if (arg==null) runnable.run(key); else runnable.run(key,arg); }

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
