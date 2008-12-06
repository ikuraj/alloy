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
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4graph.VizStyle;

/** Immutable; this defines the set of line styles that dot can produce.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public enum DotStyle {

   SOLID("Solid", "solid", "icons/StyleIcons/solid.gif", VizStyle.SOLID),
   DASHED("Dashed", "dashed", "icons/StyleIcons/dashed.gif", VizStyle.DASHED),
   DOTTED("Dotted", "dotted", "icons/StyleIcons/dotted.gif", VizStyle.DOTTED),
   BOLD("Bold", "bold", "icons/StyleIcons/bold.gif", VizStyle.BOLD);

   /** The text to display. */
   private final String displayText;

   /** The text to output to dot. */
   private final String dotText;

   /** The icon to display. */
   private final Icon icon;

   /** The visual style corresponding to this DotStyle. */
   public final VizStyle vizStyle;

   /** Construct a new DotStyle. */
    private DotStyle(String displayText, String dotText, String icon, VizStyle vizStyle) {
       this.displayText = displayText;
       this.dotText = dotText;
       this.icon = OurUtil.loadIcon(icon);
       this.vizStyle = vizStyle;
    }

    /** This method is used in parsing the XML value into a valid DotStyle; returns null if there is no match. */
    public static DotStyle parse(String x) {
        if (x != null) for(DotStyle d: values()) if (d.toString().equals(x)) return d;
        return null;
    }

    /** Returns the String that will be displayed in the GUI to represent this value. */
    public String getDisplayedText() { return displayText; }

    /** Returns the String that should be written into the dot file for this value, when used with the given palette. */
    public String getDotText(DotPalette pal) { return dotText; }

    /** Returns the Icon that will be displayed in the GUI to represent this value, when used with the given palette. */
    public Icon getIcon(DotPalette pal) { return icon; }

    /** This value is used in writing XML. */
    @Override public String toString() { return displayText; }
}
