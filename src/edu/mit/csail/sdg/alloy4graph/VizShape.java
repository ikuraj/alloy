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
