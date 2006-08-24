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

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/** Manages temporary files created by the application.  Gives out names of
    temporary files, keeps track of the temporary files created,
    erases them at the end.  Avoids doing a dangerous wildcard erasure
    of tempfiles at the end; keeps track of individual file names
    we create and erases them one by one at the end. */
@SuppressWarnings("unchecked")
public class TmpFiles {

    /** Temporary file directory.  Temporary files created by this class
    go into this directory and are deleted on exit. */
    private static File TMP_DIR = null;

    /** Cleanup order number of the {@link #removeTempFiles} method.
    If you need to do clean up that is run *before* {@link #removeTempFiles}
    (e.g. that ensures that certain temporary files are no longer
    in use and may be safely deleted), give a cleanup order number
    <em>lower</em> than {@link #CLEANUP_ORDER_NUMBER} when
    registering your cleanup method with {@link CleanupManager#addCleanupMethod}. */
    public static final int CLEANUP_ORDER_NUMBER = 1000;

    /** Prefix (usually program name) used to identify temporary directory
    for this program.  Temporary directory will be named
    prognameXXXX where XXXX is a unique integer.  This lets several
    copies of the program run on the same machine without interferance. */
    private static final String TMP_DIR_PREFIX = "alloy";

    /** Return the name of the temporary directory.  If tempdir
    is not allocated yet, allocate it now. */
    public static File getTmpDir() { _setupTmpDir(); return TMP_DIR; }

    /** Set up the temporary directory */
    private static void _setupTmpDir() {
        if (TMP_DIR != null) return;
        TMP_DIR = new File(System.getProperty("java.io.tmpdir"));
        //TMP_DIR = new File("/tmp/bad ass/");
        if (!(TMP_DIR.isAbsolute() && TMP_DIR.exists() && TMP_DIR.isDirectory())) {
            // try to create the temporary directory under the Alloy program directory
            TMP_DIR = new File("../tmp");
            if (!TMP_DIR.exists())
            TMP_DIR.mkdir();
        }
        Dbg.chk(TMP_DIR.exists() && TMP_DIR.isDirectory(),
            "Could not find temporary directory " + TMP_DIR.getAbsolutePath());

        File alloyTmpSubdir = null;
        try {
            alloyTmpSubdir = File.createTempFile(TMP_DIR_PREFIX, "", TMP_DIR);
        }
        catch (IOException e) {
            Dbg.chk(false,
                "setUpTmpDir: Temporary directory not writeable: " + alloyTmpSubdir.getAbsolutePath());

        }
        Dbg.chk(alloyTmpSubdir.exists() && alloyTmpSubdir.isFile(),
            "setUpTmpDir: Temporary directory not writeable: " + alloyTmpSubdir.getAbsolutePath());
        alloyTmpSubdir.delete();
        alloyTmpSubdir.mkdir();

        Dbg.chk(alloyTmpSubdir.exists() && alloyTmpSubdir.isDirectory(),
            "setUpTmpDir: Temporary directory not writeable: " + alloyTmpSubdir.getAbsolutePath());
        TMP_DIR = alloyTmpSubdir.getAbsoluteFile();
        Dbg.info("Temporary directory: " + TMP_DIR.getAbsolutePath());
    }

    private static final Set _tempFileNames = new HashSet();
    public static boolean keepTempFiles = false;

    public static void removeTempFiles() {
        if (Boolean.getBoolean("alloy.keeptmp")) return;
        if (TMP_DIR == null) return;
        if (keepTempFiles) {
            Dbg.info("Keeping all temporary files in " + TMP_DIR.getAbsolutePath());
            return;
        }
        Iterator tfname = _tempFileNames.iterator();
        while (tfname.hasNext()) {
            final File tfile = (File) tfname.next();
            Dbg.info("Deleting temporary file:  " + tfile);
            tfile.delete();
        }
        try { Thread.sleep(750); } catch (InterruptedException e) { }
        TMP_DIR.deleteOnExit();
        TMP_DIR.delete();
        Dbg.info("Removing temporary directory:  " + TMP_DIR);
    }

    {
    CleanupManager.addCleanupMethod(CLEANUP_ORDER_NUMBER, new Runnable()
        { public void run() { removeTempFiles(); } });
    }

    /**
     * Return a full pathname to a temporary file
     * with the given basename, in the temporary directory.
     * The file will be {@link #removeTempFiles deleted} on
     * {@link CleanupManager#exit exit}.  Can be called repeatedly
     * with the same file name (will return the same temporary
     * file that will be deleted on exist).
     */
    /*
    public static String createTempName(String name) {
        File tmpFileName = new File(getTmpDir(), name);
        String absPath = tmpFileName.getAbsolutePath();
        _tempFileNames.add(absPath);
        return absPath;
    }
    */
    public static File createHandleToATempFile(final String name) {
		final File handle = new File(getTmpDir(), name);
		_tempFileNames.add(handle);
        Dbg.info("Creating temporary file:  " + handle);
		return handle;
    }

    /** Record the name of a temporary file (in the temporary
    directory) to be deleted on exit.  Useful if you've
    used {@link #createTempName} to create a tempfile,
    and then derived some new tempfile names from that name. */
    //public static void recordTempName(String name_) {   _tempFileNames.add(name_); }

    /** Private constructor, to create one instance so that the static
    initialization will be called. */
    private TmpFiles () { }
    /** One instance to force the static initialization to happen. */

    @SuppressWarnings("unused")
	private static TmpFiles _theInstance = new TmpFiles();

    /** unit test */
    public static void main(String[] args) {
        System.out.println(getTmpDir());
    }

}  // class TmpFiles
