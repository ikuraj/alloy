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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * Utility methods for files.
 */

@SuppressWarnings("serial")
public class FileUtil {

    /** standard file extension for Alloy modules.  Convention
     * dictates that a module be contained in a file
     * module_name.FILE_EXTENSION, and this convention is
     * assumed when finding modules for open / uses statements.
     */
    public static final String FILE_EXTENSION = ".als";

    /** directories to search when looking for a module */
    private static File[] _modulePaths;

    /** Module path, as a single string. */
    private static String _modulePath;


    /**
    * initialize the setting of which paths to search
    * for modules.
    * @param paths_ paths of directories containing modules.  paths_
    * must be specified as the PATH or CLASSPATH variables would be on
    * the running system.  For example, on Windows, ".;c:\foo", and on
    * Unix, ".:/foo"
    * @return <code>true</code> if all directories in path are valid; <code>false</code>
    * otherwise
    */
    public static boolean setModulePath(String paths_) {
    boolean ret = true;
    if ((paths_ == null) || (paths_.length() == 0)) {
        // default is just current directory
        paths_ = System.getProperty("user.dir");
    }
    StringTokenizer tokenizer = new StringTokenizer(paths_, File.pathSeparator);
    int pathCount = 0;
    _modulePaths = new File[tokenizer.countTokens()];
    while (tokenizer.hasMoreTokens()) {
        String path = tokenizer.nextToken();
        File dir = new File(path);
        if (dir.isDirectory()) {
        _modulePaths[pathCount++] = dir;
        } else {
        Dbg.warn("invalid directory " + path + " in module paths");
        ret = false;
        }
    }
    _modulePath = paths_;
    return ret;
    }


    /** Move a file to the given destination, possibly overwriting the target file. */
    public static void move(String fromName_, String toName_) {
        File fromFile = new File(fromName_);
        File toFile = new File(toName_);
        fromFile.renameTo(toFile);
    }

    /** Copy a file */
    public static void copy(String fromName_, String toName_) throws IOException {
        File in = new File(fromName_);
        File out = new File(toName_);
        InputStream fis  = new BufferedInputStream(new FileInputStream(in));
        OutputStream fos = new BufferedOutputStream(new FileOutputStream(out));
        byte[] buf = new byte[32768];
        int i = 0;

        while((i=fis.read(buf))!=-1) {
            fos.write(buf, 0, i);
        }

        fis.close();
        fos.close();
        // System.out.println("copied " + fromName_ + " to " + toName_);
    }

    /** If the last character of the directory name is not a slash, add the slash. */
    public static String addSlash(String dirName_) {
    return dirName_.charAt(dirName_.length()-1) == '/' ? dirName_ : (dirName_ + '/');
    }

    /** Return the current module path, as a string. */
    public static String getModulePath() { return _modulePath; }

    /**
     * @return the first File corresponding to a module found in a search
     * of the module paths (in the order in which they were specified).
     * @exception FileNotFoundException if no file can be found for the module
     */
    public static File getModuleFile(String moduleName_) throws FileNotFoundException {
    String relativeModulePath = _convertToRelativePath(moduleName_);
    for (int i = 0; i < _modulePaths.length; i++) {
        File curPath = _modulePaths[i];
            if (curPath != null) {
                String modulePath = curPath.getPath() + File.separator + relativeModulePath;
                File module = new File(modulePath);
                if (module.isFile()) {
                    return module;
                }
            }
    }
    // no file found
    throw new FileNotFoundException();
    }

    /**
     * Get all modules in a particular package.  Used to implement
     * the use / open * functionality.
     * @return a (possibly empty) Set of Files corresponding to modules in the
     * package
     * @exception NoDirsFoundException if no directories corresponding to the package
     * are found
     */
    @SuppressWarnings("unchecked")
    public static Set getModulesInPackage(String package_) throws NoDirsFoundException {
    Set ret = new HashSet();
    String relativePath = package_.replace('/', File.separatorChar);
    for (int i = 0; i < _modulePaths.length; i++) {
        File curPath = _modulePaths[i];
        String packagePath = curPath.getPath() + File.separator + relativePath;
        File modulePackage = new File(packagePath);
        if (modulePackage.isDirectory()) {
        ret.addAll(Arrays.asList(modulePackage.listFiles(new ModuleFileFilter(ret))));
        }
    }
    if (ret.size() == 0) {
        throw new NoDirsFoundException();
    }
    return ret;
    }

    public static class NoDirsFoundException extends Exception {}

    private static final class ModuleFileFilter implements FileFilter {

    private Set _modules;

    public ModuleFileFilter(Set modules_) {
        _modules = modules_;
    }

    /**
     * @return true if file_ is a file ending with proper extension and
     * no file in existing set has the same name
     */
    public boolean accept(File file_) {
        String name = file_.getName();
        if (file_.isFile() && name.endsWith(FILE_EXTENSION)) {
        for (Iterator iter = _modules.iterator(); iter.hasNext();) {
            File element = (File) iter.next();
            if (element.getName().equals(name)) return false;
        }
        return true;
        }
        return false;
    }
    }
    /**
     * converts a module name to a relative path to the corresponding file
     */
    private static String _convertToRelativePath(String moduleName_) {
    // convert "/" to system-specific directory separator
    String ret = moduleName_.replace('/', File.separatorChar);
    // append standard extension
    ret += FILE_EXTENSION;
    return ret;
    }

    /**
     * read a text file's contents into a String.  Assumes that file exists.
     */
    public static String readFile(File file_) {
    String ret = null;
    try {
        ret = readIntoString(new FileReader(file_));
    } catch (FileNotFoundException e) {
        Dbg.fatal("non-existent file passed into readFile()");
    }
    return ret;
    }

    /**
     * read the contents of a Reader into a String
     */
    public static String readIntoString(Reader reader_) {
    BufferedReader br =
        (reader_ instanceof BufferedReader)
        ? ((BufferedReader) reader_)
        : new BufferedReader(reader_);
    String nextLine = "";
    StringBuffer sb = new StringBuffer();
    try {
        while ((nextLine = br.readLine()) != null) {
        sb.append(nextLine);
        //BufferedReader strips the EOL character so we add it back
        sb.append("\n");
        }
    } catch (IOException e) {
        // should not happen normally
        Dbg.fatal("IOException: " + e.getMessage());
    }
    return sb.toString();
    }

    /*
     * Get the extension of a file.
     */
    public static String getExtension(File f) {
        String ext = null;
        String s = f.getName();
        int i = s.lastIndexOf('.');

        if (i > 0 &&  i < s.length() - 1) {
            ext = s.substring(i+1).toLowerCase();
        }
        return ext;
    }

    /** Test whether we're running on a Windows machine */
    public static boolean runningOnWindows() {
    return System.getProperty("os.name").toLowerCase().startsWith("win");
    }

    /**
     * Get the correct name of an executable, given its base name.
     * On windows, this adds .exe.
     */
    public static String getExeName(String baseName_) {
    return runningOnWindows() ? baseName_ + ".exe" : baseName_;
    }
}
