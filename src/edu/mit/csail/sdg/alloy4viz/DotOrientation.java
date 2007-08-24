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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; this defines the set of orientations that dot can use: horizontal and vertical.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotOrientation extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    private static final List<Object> values;

    private static final DotOrientation TOP_TO_BOTTOM = new DotOrientation("Top to Bottom", "TB");

    private static final DotOrientation LEFT_TO_RIGHT = new DotOrientation("Left to Right", "LR");

    static {
        List<Object> list = new ArrayList<Object>();
        list.add(TOP_TO_BOTTOM);
        list.add(LEFT_TO_RIGHT);
        values = Collections.unmodifiableList(list);
    }

    /**
     * Constructs a new DotOrientation object.
     * @param displayedText - the label to show when the user is selecting a value
     * @param dotText - the attribute to write into the .dot file
     */
    private DotOrientation(String displayedText, String dotText) { super(displayedText, dotText, null); }

    /** Returns the default value. */
    public static DotOrientation getDefault() { return TOP_TO_BOTTOM; }

    /** Returns the list of values that the user is allowed to select from. */
    public static List<Object> values() { return values; }

    /** This method is used in parsing the XML value into a valid DotOrientation; returns null if there is no match. */
    public static DotOrientation valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotOrientation)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
