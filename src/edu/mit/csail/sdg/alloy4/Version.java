package edu.mit.csail.sdg.alloy4util;

/**
 * This class holds the buildDate String.
 *
 * The release build script
 * will generate a customized Version.java with the correct date.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 *
 * @author Felix Chang
 */

public final class Version {

    /** The constructor is private, since this class never needs to be instantiated. */
    private Version() { }

    /** Returns the build date. */
    public static String buildDate() { return "unknown"; }
}
