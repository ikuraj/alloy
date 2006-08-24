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
import kodviz.dotviz.DotStyle;
/**
 * GraphElement is the abstract class from which graph elements (nodes and
 * edges) extend.  It provides functionality for properties shared by both nodes
 * and edges, such as label and style.
 */
public abstract class GraphElement {

    //
    // MEMBER VARIABLES
    //
    
    // _id is a unique identifier; _label is the label that will actually be
    // displayed
    String _id;
    String _label;
    
    DotStyle _style;
    DotColor _color;

    //
    // ACCESSORS
    //
    
    /**
     * Returns the id of this GraphElement.
     */
    public String getID() {
	return _id;
    }

    /**
     * Returns the label of this GraphElement.
     */
    public String getLabel() {
	return _label;
    }

    /**
     * Returns the style of this GraphElement.
     */
    public DotStyle getStyle() {
	return _style;
    }

    /**
     * Returns the color of this GraphElement
     *
     */
    public DotColor getColor() {
	return _color;
    }
    

    //
    // OTHER METHODS
    //

    /**
     * Returns a String Representation of this GraphElement.
     */
    public String toString() {
	return _id + "\n" +
	    "Label: " + _label + "\n" +
	    "Style: " + _style + "\n";
    }
}
