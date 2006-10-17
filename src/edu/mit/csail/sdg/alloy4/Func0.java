package edu.mit.csail.sdg.alloy4util;

/**
 * This interface defines a runnable with a "boolean run()" method.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public interface Func0 {

    /** Note: this method must be called only by the AWT thread. */
    public boolean run();
}
