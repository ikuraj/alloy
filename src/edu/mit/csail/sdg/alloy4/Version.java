package edu.mit.csail.sdg.alloy4;

/**
 * This holds the buildDate String.
 *
 * The release build script
 * will generate a customized Version.java with the correct buildnumber and date.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class Version {

    /** The constructor is private, since this class never needs to be instantiated. */
    private Version() { }

    /** Returns the build number. */
    public static int buildNumber() { return 0; }

    /** Returns the version string. */
    public static String version() { return "4.0"; }

    /** Returns the build date. */
    public static String buildDate() { return "unknown"; }
}
