package edu.mit.csail.sdg.alloy4;

/**
 * This interface defines a runnable with a "boolean run()" method.
 *
 * <p/><b>Thread Safety:</b> Can be called only by the AWT thread.
 *
 * @author Felix Chang
 */

public interface OurFunc0 {

    /** Returns false if the method failed; Note: this method can be called only by the AWT thread. */
    public boolean run();
}
