package edu.mit.csail.sdg.alloy4;

/**
 * This class holds the buildDate String.
 *
 * The release build script
 * will generate a customized Version.java with the correct date.
 *
 * <p/><b>Thread Safety:</b>  Safe.
 */

public final class Version {

    /** The constructor is private, since this class never needs to be instantiated. */
    private Version() { }

    /** Returns the version string. */
    public static String version() { return "4.0 Beta1"; }

    /** Returns the build date. */
    public static String buildDate() { return "unknown"; }
}
