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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Immutable; this defines the set of color palettes that the user can choose from.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
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
