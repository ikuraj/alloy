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

/**
 * Immutable; this defines the set of orientations that dot can use: horizontal and vertical.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
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
