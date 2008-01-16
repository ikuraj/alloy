/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4viz;

import javax.swing.Icon;

/**
 * Immutable; this is the abstract class for all attributes used in generating dot files.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
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
