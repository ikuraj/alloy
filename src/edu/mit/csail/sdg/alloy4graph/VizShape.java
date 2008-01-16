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

package edu.mit.csail.sdg.alloy4graph;

/**
 * Immutable; enumerates the possible node shapes (BOX, CIRCLE, ELLIPSE...)
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public enum VizShape {

    /** Box                */ BOX("Box", "box"),
    /** Circle             */ CIRCLE("Circle", "circle"),
    /** Ellipse            */ ELLIPSE("Ellipse", "ellipse"),
    /** Egg                */ EGG("Egg", "egg"),
    /** Triangle           */ TRIANGLE("Triangle", "triangle"),
    /** Diamond            */ DIAMOND("Diamond", "diamond"),
    /** Trapezoid          */ TRAPEZOID("Trapezoid", "trapezium"),
    /** Parallelogram      */ PARALLELOGRAM("Parallelogram", "parallelogram"),
    /** House              */ HOUSE("House", "house"),
    /** Hexagon            */ HEXAGON("Hexagon", "hexagon"),
    /** Octagon            */ OCTAGON("Octagon", "octagon"),
    /** Double Circle      */ DOUBLE_CIRCLE("Dbl Circle", "doublecircle"),
    /** Double Octagon     */ DOUBLE_OCTAGON("Dbl Octagon", "doubleoctagon"),
    /** Triple Octagon     */ TRIPLE_OCTAGON("Tpl Octagon", "tripleoctagon"),
    /** Inverted Triangle  */ INV_TRIANGLE("Inv Triangle", "invtriangle"),
    /** Inverted Trapezoid */ INV_TRAPEZOID("Inv Trapezoid", "invtrapezium"),
    /** Inverted House     */ INV_HOUSE("Inv House", "invhouse"),
    /** Lined Diamond      */ M_DIAMOND("Lined Diamond", "Mdiamond"),
    /** Lined Square       */ M_SQUARE("Lined Square", "Msquare"),
    /** Lined Circle       */ M_CIRCLE("Lined Circle", "Mcircle");

    /** The brief description of this node shape. */
    private final String longName;

    /** Constructs a VizShape with the given long name and short name. */
    private VizShape(String longName, String shortName) { this.longName=longName; }

    /** Returns a brief description of this line style. */
    @Override public String toString() { return longName; }
}
