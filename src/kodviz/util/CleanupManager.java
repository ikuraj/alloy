/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

/** Manages cleanup by keeping a list of callbacks to be called at shutdown time,
    and calling them before terminating the JVM.   Similar to the
    "shutdown hook" service provided by JDK1.3; implemented here to let us
    use JDK1.2.  Defines an {@link #exit} method which calls the cleanup
    hooks and then calls {@link System#exit}. */

@SuppressWarnings("unchecked")
public class CleanupManager {
    /** List of cleanup hooks to be called at shutdown.  {@link Map} from the
	{@link Integer} order number of the cleanup method to its {@link Runnable}
	implementation. Cleanup methods with lower order numbers will be called
	first by {@link #exit}. */
    private static Map _cleanupMethods = new TreeMap();

    /** Add a {@link Runnable} to be called at shutdown time. */
    public static void addCleanupMethod(int orderNumber_, Runnable r_) {
	_cleanupMethods.put(new Integer(orderNumber_), r_);
	// when JDK1.3 is standard, we can use the cleaner
	// "shutdown hook" mechanism:
//         Runtime.getRuntime().addShutdownHook(new Thread() {
//                 public void run() {
//                     try { r_.run(); }
//                     catch (Exception e) {
//                         Dbg.warn("Exception in cleanup method: " + e);
//                         e.printStackTrace();
//                     }
//                 }
//             });
    }

    /** Call any registered cleanup methods, then exit the JVM. */
    public static void exit(final int exitCode_) {
	Dbg.info("Calling " + _cleanupMethods.size() + " cleanup methods.");
	for (Iterator cleanupMethodIter = _cleanupMethods.keySet().iterator();
	     cleanupMethodIter.hasNext();) {
	    Integer cleanupOrderNumber = (Integer)cleanupMethodIter.next();
	    Dbg.info("Cleanup order #" + cleanupOrderNumber);
	    Runnable cleanupMethod = (Runnable)_cleanupMethods.get(cleanupOrderNumber);
	    try { cleanupMethod.run(); }
	    catch (Exception e) {
		Dbg.warn("Exception in cleanup method: " + e);
		e.printStackTrace();
	    }
	}
	System.exit(exitCode_);
    }
}  // class CleanupManager
