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

package kodviz.graph;

import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;

/**
 * A Node represents an element of a Graph that can be connected to other nodes
 * by edges.  It contains information about how a Node should appear on screen,
 * including its label, shape, color, and style.
 */
public class Node extends GraphElement {

    //
    // DEFAULTS
    //
    
    public static final DotShape DEF_SHAPE = DotShape.ELLIPSE;
    public static final DotColor DEF_COLOR = DotColor.WHITE;
    public static final DotStyle DEF_STYLE = DotStyle.SOLID;


    //
    // MEMBER VARIABLES
    //

    private DotShape _shape;

    
    //
    // CONSTRUCTORS
    //
    
    /**
     * Construct a new Node with the given id, and default shape (ellipse),
     * color (white), and style (solid).  The label is the
     * same as the id.
     */
    public Node(String id_) {
	this(id_, id_, DEF_SHAPE, DEF_COLOR, DEF_STYLE);
    }

    /**
     * Construct a new Node with the given id, label, shape, color, and style.
     */
    public Node(String id_, String label_, DotShape shape_, DotColor color_,
		DotStyle style_) {
	_id = id_;
	_label = label_;
	_shape = shape_;
	_color = color_;
	_style = style_;
    }


    //
    // ACCESSORS
    //

    /**
     * Returns the shape of this Node.
     */
    public DotShape getShape() {
	return _shape;
    }
    

    //
    // MUTATORS
    //

    /**
     * Sets the label of this Node to the specified value.  This is necessary
     * for Nodes because when the edges are created, if they are attributes this
     * needs to change the label of the Node.
     */
    public void setLabel(String label) {
	_label = label;
    }

    //
    // OTHER METHODS
    //

    /**
     * Returns a String representation of this Node.
     */
    public String toString() {
	return "Node: " + super.toString() +
	    "Shape: " + _shape.getDisplayedText() + "\n" +
	    "Fill Color: " + _color.getDisplayedText();
    }

    /**
     * Tells if this Node is equal to the specified Object.  Because IDs are
     * unique, two Nodes are equal if they have the same id.
     */
    public boolean equals(Object o) {
	if ((o == null) || !(o instanceof Node)) {
	    return false;
	}
	return (this.getID().equals(((Node)o).getID()));
    }
    
    /**
     * Returns a hash code for this Node based on its id.
     */
    public int hashCode() {
	return _id.hashCode();
    }
}
