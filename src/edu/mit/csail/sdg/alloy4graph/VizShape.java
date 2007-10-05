package edu.mit.csail.sdg.alloy4graph;

/** Immutable; enumerates the possible node shapes (BOX, CIRCLE, ELLIPSE...) */

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
