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

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4graph.VizStyle;

/**
 * Immutable; this defines the set of line styles that dot can produce.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DotStyle extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    private static final List<Object> values;

    public static final DotStyle SOLID = new DotStyle("Solid", "solid", "icons/StyleIcons/solid.gif", VizStyle.SOLID);
    public static final DotStyle DASHED = new DotStyle("Dashed", "dashed", "icons/StyleIcons/dashed.gif", VizStyle.DASHED);
    public static final DotStyle DOTTED = new DotStyle("Dotted", "dotted", "icons/StyleIcons/dotted.gif", VizStyle.DOTTED);
    public static final DotStyle BOLD = new DotStyle("Bold", "bold", "icons/StyleIcons/bold.gif", VizStyle.BOLD);

    static {
        List<Object> list = new ArrayList<Object>();
        list.add(SOLID);
        list.add(DASHED);
        list.add(DOTTED);
        list.add(BOLD);
        values=Collections.unmodifiableList(list);
    }

    /**
     * Construct a new DotStyle.
     * @param displayedText - the label to show when the user selects a style from a combobox
     * @param dotText - the actual attribute that we will write into the DOT file
     * @param icon - the icon to use for this
     */
    private DotStyle(String displayedText, String dotText, String icon, VizStyle vizStyle) {
        super(displayedText, dotText, OurUtil.loadIcon(icon));
        this.vizStyle=vizStyle;
    }

    public final VizStyle vizStyle;

    /** Returns the default value. */
    public static DotStyle getDefault() { return SOLID; }

    /** Returns the list of values that the user is allowed to select from. */
    public static List<Object> values() { return values; }

    /** This method is used in parsing the XML value into a valid DotStyle; returns null if there is no match. */
    public static DotStyle valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotStyle)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
