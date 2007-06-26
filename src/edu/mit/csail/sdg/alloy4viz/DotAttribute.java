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

package edu.mit.csail.sdg.alloy4viz;

import javax.swing.Icon;

/**
 * Immutable; this is the abstract class for all attributes used in generating dot files.
 *
 * <p><b>Thread Safety:</b>  Safe (since the fields here are immutable, and every subclass is immutable too)
 */

abstract class DotAttribute {

    /** This is the label to display in the GUI. */
    private final String displayedText;

    /** This is the value to write into the actual dot file, unless overridden by the subclass. */
    private final String dotText;

    /** This is the icon to display in the GUI, unless overridden by the subclass; can be null if there's no icon. */
    private final Icon icon;

    /**
     * Constructs a new DotAttribute.
     * @param displayedText - the label to display in the GUI for this value
     * @param dotText - the value to write into the actual dot file for this value
     * @param icon - the icon to display on the GUI for this value (Can be null if no icon is needed)
     */
    DotAttribute(String displayedText, String dotText, Icon icon) {
        this.displayedText=displayedText;
        this.dotText=dotText;
        this.icon=icon;
    }

    /** Returns the String that will be displayed in the GUI to represent this value. */
    public final String getDisplayedText() { return displayedText; }

    /** Returns the String that should be written into the dot file for this value. */
    public String getDotText(DotPalette pal) { return dotText; }

    /** Returns the Icon that will be displayed in the GUI to represent this value. */
    public Icon getIcon(DotPalette pal) { return icon; }
}
