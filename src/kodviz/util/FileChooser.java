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

import java.awt.Component;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import java.io.FilenameFilter;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import kodviz.gui.AlloySwingUtilities;


/**
   Utility class which abstracts choosing the best file dialog
   for a platform.  {@link java.awt.FileDialog} seems to work better
   on Mac, but {@link javax.swing.JFileChooser} is fine on other platforms.
   We might change our minds about the other platforms in the future as well.

   Here, we try to mimic the JFileChooser interface.
*/

public class FileChooser {

    /** flag indicating a file was successfully approved in the dialog */
    public static final int APPROVE_OPTION = 0;

    /** flag indicating the file operation was cancelled */
    public static final int CANCEL_OPTION = 1;
    

    /** the directory in which to start the dialog */
    private String _curDirPath;

    /** file filter to use for the dialog */
    private FileFilter _fileFilter;

    /** the selected file; <code>null</code> if dialog has not
        yet been shown or if selection was cancelled */
    private File _selectedFile;

    /** title for dialog */
    private String _title;
    
    public FileChooser() {
        this(null);
    }
    
    public FileChooser(String curDirPath_) {
        _curDirPath = curDirPath_;
    }

                                                         
    public void setFileFilter(FileFilter fileFilter_) {
        _fileFilter = fileFilter_;
    }

    public void setSelectedFile(File selectedFile_) {
        _selectedFile = selectedFile_;
    }
    
    public File getSelectedFile() {
        return _selectedFile;
    }

    public void setDialogTitle(String title_) {
        _title = title_;
    }
    
    /**
       Show a dialog for opening a file.
       @param parentFrame_ parent frame for dialog.  We need to take a {@link Frame}
       instead of a {@link Component} to accomodate the more restrictive
       {@link java.awt.FileDialog}.
       @return {@link APPROVE_OPTION} if file operation was approved; otherwise
       {@link CANCEL_OPTION}
    */
    public int showOpenDialog(Frame parentFrame_) {
        if (parentFrame_ != null && AlloySwingUtilities.onMac()) {
            return _showAWTDialog(parentFrame_, true);
        } else {
            return _showSwingDialog(parentFrame_, true);
        }
    }

    /**
       Show a dialog for saving a file.
       @param parentFrame_ parent frame for dialog.  We need to take a {@link Frame}
       instead of a {@link Component} to accomodate the more restrictive
       {@link java.awt.FileDialog}.
       @return {@link APPROVE_OPTION} if file operation was approved; otherwise
       {@link CANCEL_OPTION}
    */
    public int showSaveDialog(Frame parentFrame_) {
        if (parentFrame_ != null && AlloySwingUtilities.onMac()) {
            return _showAWTDialog(parentFrame_, false);
        } else {
            return _showSwingDialog(parentFrame_, false);
        }
    }

    private int _showAWTDialog(Frame parentFrame_,
                               boolean isOpen_) {
        int ret;
        FileDialog f = null;
        if (_title == null) {
            f = new FileDialog(parentFrame_);
        } else {
            f = new FileDialog(parentFrame_, _title);
        }
        if (isOpen_) {
            f.setMode(FileDialog.LOAD);
        } else {
            f.setMode(FileDialog.SAVE);
        }
        if (_selectedFile != null) {
            f.setFile(_selectedFile.getName());
        }
        if (_selectedFile != null && _selectedFile.isAbsolute()) {
            f.setDirectory(_selectedFile.getParent());
        } else if (_curDirPath != null) {
            f.setDirectory(_curDirPath);
        }
        
        if (_fileFilter != null) {
            f.setFilenameFilter(new FilenameFilter() {
                    public boolean accept(File dir, String name) {
                        return _fileFilter.accept(new File(dir, name));
                    }
                });
        }
        
        f.setVisible(true);
        String selectedItem = f.getFile();
        if (selectedItem == null) {
            ret = CANCEL_OPTION;
        } else {
            _selectedFile = new File(f.getDirectory(), f.getFile());
            ret = APPROVE_OPTION;
        }
        return ret;
    }

    private int _showSwingDialog(Frame parentFrame_,
                                 boolean isOpen_) {
        JFileChooser f =  new JFileChooser(_curDirPath);
        if (_fileFilter != null) {
            f.setFileFilter(_fileFilter);
        }
        if (_selectedFile != null) {
            f.setSelectedFile(_selectedFile);
        }
        if (_title != null) {
            f.setDialogTitle(_title);
        }
        int result = 0;
        if (isOpen_) {
            result = f.showOpenDialog(parentFrame_);
        } else { // save
            result = f.showSaveDialog(parentFrame_);
        }
        if (result == JFileChooser.APPROVE_OPTION) {
            _selectedFile = f.getSelectedFile();
        }
        return result;
    }
}
    

 




