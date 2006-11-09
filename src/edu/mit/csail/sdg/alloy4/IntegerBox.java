package edu.mit.csail.sdg.alloy4;

/**
 * Mutable; this class holds a mutable integer.
 *
 * <p/><b>Thread Safety:</b> Safe
 */

public class IntegerBox {

    /** The integer value. */
    private int value;

    /** Constructs an IntegerBox with the given initial value. */
    public IntegerBox(int initialValue) {value=initialValue;}

    /** Reads the current value. */
    public synchronized int get() {return value;}

    /** Sets the current value. */
    public synchronized void set(int newValue) {value=newValue;}
}
