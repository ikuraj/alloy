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

import javax.swing.filechooser.FileFilter;


/**
 * A {@link FileFilter} that allows only files with a
 * particular extension, and all directories.
 */
public class ExtensionFileFilter extends FileFilter {
    /** The extension which we accept */
    private String _extension;

    /**
     * Construct a filter accepting the given extension.
     *
     * @param extension_ extension to accept, or <code>null</code>
     *                   to accept only files without extension.
     */
    public ExtensionFileFilter(String extension_) { _extension = extension_; }
    
    // Accept all directories and all gif, jpg, or tiff files.
    public boolean accept(File file) {
        if (file.isDirectory()) return true;
        String extension = FileUtil.getExtension(file);
	return _extension == null ? extension == null : _extension.equals(extension);
    }

    /**
     * Utility method that returns the name of the specified file with
     * the extension if the file has no extension.
     */
    public File addExtensionIfNone(File file) {
	String fileName = file.getName();
	if (fileName.indexOf(".") < 0) {
	    return new File(file.getParent(), fileName + "." + _extension);
	} else {
	    return file;
        }
    }
    
    // The description of this filter
    public String getDescription() {
        return "." + _extension + " files";
    }
}


