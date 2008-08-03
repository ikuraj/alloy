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
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4graph.VizShape;

/**
 * Immutable; this defines the set of shapes that dot can produce.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DotShape extends DotAttribute {

    /** The list of values that the user can select from a combobox. */
    static final List<DotShape> values;

    public static final DotShape ELLIPSE = new DotShape("Ellipse", "ellipse", VizShape.ELLIPSE);
    public static final DotShape BOX = new DotShape("Box", "box", VizShape.BOX);
    public static final DotShape CIRCLE = new DotShape("Circle", "circle", VizShape.CIRCLE);
    private static final DotShape EGG = new DotShape("Egg", "egg", VizShape.EGG);
    private static final DotShape TRIANGLE = new DotShape("Triangle", "triangle", VizShape.TRIANGLE);
    private static final DotShape DIAMOND = new DotShape("Diamond", "diamond", VizShape.DIAMOND);
    private static final DotShape TRAPEZOID = new DotShape("Trapezoid", "trapezium", VizShape.TRAPEZOID);
    private static final DotShape PARALLELOGRAM = new DotShape("Parallelogram", "parallelogram", VizShape.PARALLELOGRAM);
    private static final DotShape HOUSE = new DotShape("House", "house", VizShape.HOUSE);
    private static final DotShape HEXAGON = new DotShape("Hexagon", "hexagon", VizShape.HEXAGON);
    private static final DotShape OCTAGON = new DotShape("Octagon", "octagon", VizShape.OCTAGON);
    private static final DotShape DOUBLE_CIRCLE = new DotShape("Dbl Circle", "doublecircle", VizShape.DOUBLE_CIRCLE);
    private static final DotShape DOUBLE_OCTAGON = new DotShape("Dbl Octagon", "doubleoctagon", VizShape.DOUBLE_OCTAGON);
    private static final DotShape TRIPLE_OCTAGON = new DotShape("Tpl Octagon", "tripleoctagon", VizShape.TRIPLE_OCTAGON);
    private static final DotShape INV_TRIANGLE = new DotShape("Inv Triangle", "invtriangle", VizShape.INV_TRIANGLE);
    private static final DotShape INV_TRAPEZOID = new DotShape("Inv Trapezoid", "invtrapezium", VizShape.INV_TRAPEZOID);
    private static final DotShape INV_HOUSE = new DotShape("Inv House", "invhouse", VizShape.INV_HOUSE);
    private static final DotShape M_DIAMOND = new DotShape("Lined Diamond", "Mdiamond", VizShape.M_DIAMOND);
    private static final DotShape M_SQUARE = new DotShape("Lined Square", "Msquare", VizShape.M_SQUARE);
    private static final DotShape M_CIRCLE = new DotShape("Lined Circle", "Mcircle", VizShape.M_CIRCLE);

    /** Initialize values. */
    static {
        final List<DotShape> list = new ArrayList<DotShape>();
        list.add(ELLIPSE);
        list.add(BOX);
        list.add(CIRCLE);
        list.add(EGG);
        list.add(TRIANGLE);
        list.add(DIAMOND);
        list.add(TRAPEZOID);
        list.add(PARALLELOGRAM);
        list.add(HOUSE);
        list.add(HEXAGON);
        list.add(OCTAGON);
        list.add(DOUBLE_CIRCLE);
        list.add(DOUBLE_OCTAGON);
        list.add(TRIPLE_OCTAGON);
        list.add(INV_TRIANGLE);
        list.add(INV_HOUSE);
        list.add(INV_TRAPEZOID);
        list.add(M_DIAMOND);
        list.add(M_SQUARE);
        list.add(M_CIRCLE);
        values=Collections.unmodifiableList(list);
    }

    static final List<List<DotShape>> families;

    /** Initialize families. */
    static {
        final List<List<DotShape>> f = new ArrayList<List<DotShape>>();
        f.add(ula(BOX, TRAPEZOID, HOUSE));
        f.add(ula(ELLIPSE, EGG));
        f.add(ula(HEXAGON, OCTAGON, DOUBLE_OCTAGON, TRIPLE_OCTAGON));
        f.add(ula(INV_TRIANGLE, INV_HOUSE, INV_TRAPEZOID));
        f.add(ula(M_DIAMOND, M_SQUARE, M_CIRCLE));
        f.add(ula(PARALLELOGRAM, DIAMOND));
        //f.add(ula(CIRCLE, DOUBLE_CIRCLE));
        families = Collections.unmodifiableList(f);
    }

    private static List<DotShape> ula(DotShape... shapes) {
        return Util.asList(shapes);
    }

    /**
     * Construct a new DotShape.
     * @param displayedText - the label to show when the user selects a style from a combobox
     * @param dotText - the actual attribute that we will write into the DOT file
     */
    private DotShape(String displayedText, String dotText, VizShape vizShape) {
        super(displayedText, dotText, OurUtil.loadIcon("icons/ShapeIcons/"+dotText+".gif"));
        this.vizShape=vizShape;
    }

    public final VizShape vizShape;

    /** Returns the default value. */
    public static DotShape getDefault() { return ELLIPSE; }

    /** Returns the list of values that the user is allowed to select from. */
    public static List<?> values() { return values; }

    /** This method is used in parsing the XML value into a valid DotShape; returns null if there is no match. */
    public static DotShape valueOf(String x) {
        if (x!=null) for(Object d:values) if (d.toString().equals(x)) return (DotShape)d;
        return null;
    }

    /** This value is used in writing XML. */
    @Override public String toString() { return getDisplayedText(); }
}
