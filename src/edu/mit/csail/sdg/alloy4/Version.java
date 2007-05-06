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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

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
    public static int buildNumber() {
        return 0;
    }

    /** Returns the version string. */
    public static String version() {
        return "4.0";
    }

    /** Returns the build date. */
    public static String buildDate() {
        return "unknown";
    }
}
