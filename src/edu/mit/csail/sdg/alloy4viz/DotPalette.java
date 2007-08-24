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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Immutable; this defines the set of color palettes that the user can choose from.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotPalette extends DotAttribute {

    /**
     * The list of values that the user can select from a combobox.
     * If you change the order, you must also change the ordering of the colors in DotColor class.
     */
    private static final List<Object> values;

    /** Classic palette. */    public static final DotPalette CLASSIC  = new DotPalette("Classic");
    /** Standard palette. */   public static final DotPalette STANDARD = new DotPalette("Standard");
    /** Martha palette. */     public static final DotPalette MARTHA   = new DotPalette("Martha");
    /** Neon palette. */       public static final DotPalette NEON     = new DotPalette("Neon");

    static {
        List<Object> list=new ArrayList<Object>();
        list.add(CLASSIC); list.add(STANDARD); list.add(MARTHA); list.add(NEON);
        values=Collections.unmodifiableList(list);
    }

    /** Constructs a DotPalette object with the given label. */
    private DotPalette(String displayedText) { super(displayedText,"",null); }

    /** Returns the default value. */
    public static DotPalette getDefault() { return CLASSIC; }

    /** Returns the list of values that the user is allowed to select from. */
    public static List<Object> values() { return values; }

    /** This method is used in parsing the XML value into a valid DotPalette; returns null if there is no match. */
    public static DotPalette valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotPalette)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
