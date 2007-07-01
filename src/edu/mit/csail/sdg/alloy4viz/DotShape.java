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
 * Immutable; this defines the set of shapes that dot can produce.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotShape extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    static final List<DotShape> values;

    public static final DotShape ELLIPSE = new DotShape("Ellipse", "ellipse");
    public static final DotShape BOX = new DotShape("Box", "box");
    public static final DotShape CIRCLE = new DotShape("Circle", "circle");

    static {
        List<DotShape> list = new ArrayList<DotShape>();
        list.add(ELLIPSE);
        list.add(BOX);
        list.add(CIRCLE);
        list.add(new DotShape("Egg", "egg"));
        list.add(new DotShape("Triangle", "triangle"));
        list.add(new DotShape("Diamond", "diamond"));
        list.add(new DotShape("Trapezoid", "trapezium"));
        list.add(new DotShape("Parallelogram", "parallelogram"));
        list.add(new DotShape("House", "house"));
        list.add(new DotShape("Hexagon", "hexagon"));
        list.add(new DotShape("Octagon", "octagon"));
        list.add(new DotShape("Dbl Circle", "doublecircle"));
        list.add(new DotShape("Dbl Octagon", "doubleoctagon"));
        list.add(new DotShape("Tpl Octagon", "tripleoctagon"));
        list.add(new DotShape("Inv Triangle", "invtriangle"));
        list.add(new DotShape("Inv Trapezoid", "invtrapezium"));
        list.add(new DotShape("Inv House", "invhouse"));
        list.add(new DotShape("Lined Diamond", "Mdiamond"));
        list.add(new DotShape("Lined Square", "Msquare"));
        list.add(new DotShape("Lined Circle", "Mcircle"));
        values=Collections.unmodifiableList(list);
    }

    /**
     * Construct a new DotShape.
     * @param displayedText - the label to show when the user selects a style from a combobox
     * @param dotText - the actual attribute that we will write into the DOT file
     */
    private DotShape(String displayedText, String dotText) {
        super(displayedText, dotText, OurUtil.loadIcon("icons/ShapeIcons/"+dotText+".gif"));
    }

    /** Returns the default value. */
    public static DotShape getDefault() { return ELLIPSE; }

    /** Returns the list of values that the user is allowed to select from. */
    @SuppressWarnings("unchecked")
	public static List<Object> values() { 
    	final List raw = values;
    	return raw; 
    }

    /** This method is used in parsing the XML value into a valid DotShape; returns null if there is no match. */
    public static DotShape valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotShape)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
