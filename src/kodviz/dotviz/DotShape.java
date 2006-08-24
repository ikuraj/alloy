/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.dotviz;

import java.io.ObjectStreamException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;

/**
 * DotShape is a typesafe enumeration that represents the different shapes that dot
 * can produce.
 */
public class DotShape extends DotProperty {

	private static final long serialVersionUID = 1L;

	// Ordinal of next DotShape to be created
    private static int nextOrdinal = 0;

    // Assign an ordinal to this shape
    private final int ordinal = nextOrdinal++;

    public static final String DISPLAYED_NAME = "Shape";
    public static final String DOT_NAME = "shape";

    public static final DotShape BOX = new DotShape("Box", "box", "icons/ShapeIcons/box.gif");
    public static final DotShape ELLIPSE =
        new DotShape("Ellipse", "ellipse", "icons/ShapeIcons/ellipse.gif");
    public static final DotShape CIRCLE =
        new DotShape("Circle", "circle", "icons/ShapeIcons/circle.gif");
    //public static final DotShape POINT = new DotShape("Point", "point", "icons/ShapeIcons/point.gif");
    public static final DotShape EGG = new DotShape("Egg", "egg", "icons/ShapeIcons/egg.gif");
    public static final DotShape TRIANGLE =
        new DotShape("Triangle", "triangle", "icons/ShapeIcons/triangle.gif");
    public static final DotShape PLAIN_TEXT =
        new DotShape("Plain Text", "plaintext", "icons/ShapeIcons/plaintext.gif");
    public static final DotShape DIAMOND =
        new DotShape("Diamond", "diamond", "icons/ShapeIcons/diamond.gif");
    public static final DotShape TRAPEZOID =
        new DotShape("Trapezoid", "trapezium", "icons/ShapeIcons/trapezoid.gif");
    public static final DotShape PARALLELOGRAM =
        new DotShape("Parallelogram", "parallelogram", "icons/ShapeIcons/parallelogram.gif");
    public static final DotShape HOUSE =
        new DotShape("House", "house", "icons/ShapeIcons/house.gif");
    public static final DotShape HEXAGON =
        new DotShape("Hexagon", "hexagon", "icons/ShapeIcons/hexagon.gif");
    public static final DotShape OCTAGON =
        new DotShape("Octagon", "octagon", "icons/ShapeIcons/octagon.gif");
    public static final DotShape DOUBLE_CIRCLE =
        new DotShape("Dbl Circle", "doublecircle", "icons/ShapeIcons/doublecircle.gif");
    public static final DotShape DOUBLE_OCTAGON =
        new DotShape("Dbl Octagon", "doubleoctagon", "icons/ShapeIcons/doubleoctagon.gif");
    public static final DotShape TRIPLE_OCTAGON =
        new DotShape("Tpl Octagon", "tripleoctagon", "icons/ShapeIcons/tripleoctagon.gif");
    public static final DotShape INV_TRIANGLE =
        new DotShape("Inv Triangle", "invtriangle", "icons/ShapeIcons/invtriangle.gif");
    public static final DotShape INV_TRAPEZOID =
        new DotShape("Inv Trapezoid", "invtrapezium", "icons/ShapeIcons/invtrapezoid.gif");
    public static final DotShape INV_HOUSE =
        new DotShape("Inv House", "invhouse", "icons/ShapeIcons/invhouse.gif");
    public static final DotShape MDIAMOND =
        new DotShape("Lined Diamond", "Mdiamond", "icons/ShapeIcons/lineddiamond.gif");
    public static final DotShape MSQUARE =
        new DotShape("Lined Square", "Msquare", "icons/ShapeIcons/linedsquare.gif");
    public static final DotShape MCIRCLE =
        new DotShape("Lined Circle", "Mcircle", "icons/ShapeIcons/linedcircle.gif");

    private static final DotShape[] VALS =
        {
            BOX,
            ELLIPSE,
            CIRCLE,
            EGG,
            TRIANGLE,
            PLAIN_TEXT,
            DIAMOND,
            TRAPEZOID,
            PARALLELOGRAM,
            HOUSE,
            HEXAGON,
            OCTAGON,
            DOUBLE_CIRCLE,
            DOUBLE_OCTAGON,
            TRIPLE_OCTAGON,
            INV_TRIANGLE,
            INV_TRAPEZOID,
            INV_HOUSE,
            MDIAMOND,
            MSQUARE,
            MCIRCLE };

    // Construct a new DotShape with the given displayedText and dotText
    private DotShape(String displayedText_, String dotText_, String iconPath_) {
        super();
        _displayedText = displayedText_;
        _dotText = dotText_;
        _displayedName = DISPLAYED_NAME;
        _dotName = DOT_NAME;
        _icon = new ImageIcon(ImageHandler.loadImage(iconPath_));
    }

    @SuppressWarnings("unchecked")
    public static List getValidValues() {
        List values = new ArrayList(22);

        values.add(BOX);
        values.add(ELLIPSE);
        values.add(CIRCLE);
        //values.add(POINT);
        values.add(EGG);
        values.add(TRIANGLE);
        //values.add(PLAIN_TEXT);
        values.add(DIAMOND);
        values.add(TRAPEZOID);
        values.add(PARALLELOGRAM);
        values.add(HOUSE);
        values.add(HEXAGON);
        values.add(OCTAGON);
        values.add(DOUBLE_CIRCLE);
        values.add(DOUBLE_OCTAGON);
        values.add(TRIPLE_OCTAGON);
        values.add(INV_TRIANGLE);
        values.add(INV_TRAPEZOID);
        values.add(INV_HOUSE);
        values.add(MDIAMOND);
        values.add(MSQUARE);
        values.add(MCIRCLE);

        return Collections.unmodifiableList(values);
    }

    // used for deserialization
    private Object readResolve() throws ObjectStreamException {
        return VALS[ordinal];
    }
}
