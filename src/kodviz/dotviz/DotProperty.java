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
package kodviz.dotviz;

import java.io.Serializable;

import javax.swing.Icon;

/**
 * DotProperty is the interface that all properties for use with dot objects
 * must implement.
 */
public abstract class DotProperty implements Serializable {

    /*
     * _displayedName is the name that will be displayed in the GUI.    ("Orientation")
     * _dotName is the name that dot uses to represent a property.      ("rankdir")
     * _displayedText is the option that will be displayed in the GUI.  ("Vertical" or "Horizontal")
     * _dotText is the text that dot uses to represent that option.     ("TB" or "LR")
     */
    protected String _displayedName;
    protected String _dotName;
    protected String _displayedText;
    protected String _dotText;

    protected Icon _icon;
    
    /**
     * Returns the name of this DotProperty that will be displayed in the GUI.
     */
    public String getDisplayedName() {
	return _displayedName;
    }

    /**
     * Returns the name that dot uses for this DotProperty.
     */
    public String getDotName() {
	return _dotName;
    }

    /**
     * Returns the String that will be displayed in the GUI to represent this
     * DotProperty.
     */
    public String getDisplayedText() {
	return _displayedText;
    }
    
    /**
     * Returns the String that dot uses to represent this DotProperty.
     */
    public String getDotText() {
	return _dotText;
    }

    /**
     * Returns the Icon that will be displayed in the GUI to represent this
     * DotProperty.
     */
    public Icon getIcon() {
	return _icon;
    }

    /**
     * Returns a String representation of this DotProperty.
     */
    public String toString() {
	return getDisplayedText();
    }
    

}

