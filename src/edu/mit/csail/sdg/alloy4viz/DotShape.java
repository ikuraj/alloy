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

import java.util.List;
import javax.swing.Icon;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4graph.VizShape;

/** Immutable; this defines the set of shapes that dot can produce.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public enum DotShape {

    ELLIPSE("Ellipse", "ellipse", VizShape.ELLIPSE),
    BOX("Box", "box", VizShape.BOX),
    CIRCLE("Circle", "circle", VizShape.CIRCLE),
    EGG("Egg", "egg", VizShape.EGG),
    TRIANGLE("Triangle", "triangle", VizShape.TRIANGLE),
    DIAMOND("Diamond", "diamond", VizShape.DIAMOND),
    TRAPEZOID("Trapezoid", "trapezium", VizShape.TRAPEZOID),
    PARALLELOGRAM("Parallelogram", "parallelogram", VizShape.PARALLELOGRAM),
    HOUSE("House", "house", VizShape.HOUSE),
    HEXAGON("Hexagon", "hexagon", VizShape.HEXAGON),
    OCTAGON("Octagon", "octagon", VizShape.OCTAGON),
    DOUBLE_CIRCLE("Dbl Circle", "doublecircle", VizShape.DOUBLE_CIRCLE),
    DOUBLE_OCTAGON("Dbl Octagon", "doubleoctagon", VizShape.DOUBLE_OCTAGON),
    TRIPLE_OCTAGON("Tpl Octagon", "tripleoctagon", VizShape.TRIPLE_OCTAGON),
    INV_TRIANGLE("Inv Triangle", "invtriangle", VizShape.INV_TRIANGLE),
    INV_HOUSE("Inv House", "invhouse", VizShape.INV_HOUSE),
    INV_TRAPEZOID("Inv Trapezoid", "invtrapezium", VizShape.INV_TRAPEZOID),
    M_DIAMOND("Lined Diamond", "Mdiamond", VizShape.M_DIAMOND),
    M_SQUARE("Lined Square", "Msquare", VizShape.M_SQUARE),
    M_CIRCLE("Lined Circle", "Mcircle", VizShape.M_CIRCLE);

    /** The list of shape families. */
    static final List<ConstList<DotShape>> families;
    static {
       TempList<ConstList<DotShape>> list = new TempList<ConstList<DotShape>>();
       list.add(Util.asList(BOX, TRAPEZOID, HOUSE));
       list.add(Util.asList(ELLIPSE, EGG));
       list.add(Util.asList(HEXAGON, OCTAGON, DOUBLE_OCTAGON, TRIPLE_OCTAGON));
       list.add(Util.asList(INV_TRIANGLE, INV_HOUSE, INV_TRAPEZOID));
       list.add(Util.asList(M_DIAMOND, M_SQUARE, M_CIRCLE));
       list.add(Util.asList(PARALLELOGRAM, DIAMOND));
       families = list.makeConst();
    }

    /** The text to display. */
    private final String displayText;

    /** The text to output to dot. */
    private final String dotText;

    /** The icon to display. */
    private final Icon icon;

    /** The visual shape corresponding to this shape. */
    public final VizShape vizShape;

    /** Construct a new DotShape. */
    private DotShape(String displayText, String dotText, VizShape vizShape) {
        this.displayText = displayText;
        this.dotText = dotText;
        this.icon = OurUtil.loadIcon("icons/ShapeIcons/" + dotText + ".gif");
        this.vizShape=vizShape;
    }

    /** This method is used in parsing the XML value into a valid DotShape; returns null if there is no match. */
    public static DotShape parse(String x) {
        if (x != null) for(DotShape d: values()) if (d.toString().equals(x)) return d;
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
