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

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.OurUtil;

/**
 * Immutable; this defines the set of line styles that dot can produce.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotStyle extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    private static final List<Object> values;

    public static final DotStyle SOLID = new DotStyle("Solid", "solid", "icons/StyleIcons/solid.gif");
    public static final DotStyle DASHED = new DotStyle("Dashed", "dashed", "icons/StyleIcons/dashed.gif");
    public static final DotStyle DOTTED = new DotStyle("Dotted", "dotted", "icons/StyleIcons/dotted.gif");
    public static final DotStyle BOLD = new DotStyle("Bold", "bold", "icons/StyleIcons/bold.gif");

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
    private DotStyle(String displayedText, String dotText, String icon) {
        super(displayedText, dotText, OurUtil.loadIcon(icon));
    }

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
