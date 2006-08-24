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

package kodviz.alloyviz;

import java.io.Serializable;

import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;


/**
 * NodeViz is a data structure that holds all of the information about
 * visualizing a type of node.
 */
public class NodeViz implements Serializable {

	private static final long serialVersionUID = 1L;
	private Boolean _visible;
    private String _label;
    private DotColor _color;
    private DotShape _shape;
    private DotStyle _style;
    private Boolean _sameRank;
    // new additions 5/15/03
    private Boolean _showLabel;
    private Boolean _selected; // in gui editor
    // added 7/11/03:
    private Boolean _showInAttr; // a set-specific property
    private Boolean _hideUnconnected; // a type-specific property
    // added 7/18/03:
    private Boolean _numberAtoms; // whether or not to number atoms in labels

    /**
     * Create a new NodeViz with the given label, color, shape, style, and
     * ranking.
     */
    public NodeViz(
        Boolean visible_,
        String label_,
        DotColor color_,
        DotShape shape_,
        DotStyle style_,
        Boolean sameRank_,
        Boolean showLabel_,
        Boolean selected_,
        Boolean showInAttr_,
        Boolean hideUnconnected_,
        Boolean numberAtoms_) {
        	
        _visible = visible_;
        _label = label_;
        _color = color_;
        _shape = shape_;
        _style = style_;
        _sameRank = sameRank_;
        _showLabel = showLabel_;
        _selected = selected_;
        _showInAttr = showInAttr_;
        _hideUnconnected = hideUnconnected_;
        _numberAtoms = numberAtoms_;
    }

    public NodeViz copy() {
        NodeViz nv =
            new NodeViz(
                _visible,
                _label,
                _color,
                _shape,
                _style,
                _sameRank,
                _showLabel,
                _selected,
                _showInAttr,
                _hideUnconnected,
                _numberAtoms);
        return nv;
    }

    /**
     * Returns whether or not this node will be shown.  Null means inherit.
     */
    public Boolean isVisible() {
        return _visible;
    }

    /**
     * Returns the label for this NodeViz.
     */
    public String getLabel() {
        return _label;
    }

    /**
     * Returns the color for this NodeViz.
     */
    public DotColor getColor() {
        return _color;
    }

    /**
     * Returns the shape for this NodeViz.
     */
    public DotShape getShape() {
        return _shape;
    }

    /**
     * Returns the style for this NodeViz.
     */
    public DotStyle getStyle() {
        return _style;
    }

    /**
     * Returns true if this NodeViz is set to Same Rank, false otherwise.
     */
    public Boolean isSameRank() {
        return _sameRank;
    }

    /**
     * Returns true if this NodeViz is set to display/show its label in the
     * graph
     */
    public Boolean showLabel() {
        return _showLabel;
    }

    /**
     * Returns true if the node is a selected one in the gui.  Note value returned is
     * boolean, not Boolean.  null doesn't make sense here.
     */
    public boolean isSelected() {
        if (_selected == null) {
            return true;
        }
        return _selected.booleanValue();
    }
    
    /**
     * Returns true if NodeViz is customized to show (set) label in attributes
     */
    public Boolean showInAttr() {
    	return _showInAttr;
    }
    
    public Boolean hideUnconnected() {
    	return _hideUnconnected;
    }
    
    public Boolean numberAtoms() {
    	return _numberAtoms;
    }

    /**
     * Returns a String representation of this NodeViz.
     */
    public String toString() {
        return "NodeViz:\nLabel: "
            + getLabel()
            + "\nVisible: "
            + isVisible()
            + "\nColor: "
            + getColor()
            + "\nShape: "
            + getShape()
            + "\nStyle: "
            + getStyle()
            + "\nSame Rank: "
            + isSameRank()
            + "\n";
    }
    
    // jbaek added for testing purpose
    public boolean equals(Object o) {
    	if ((o == null) || !(o instanceof NodeViz)) {
    	    return false;
    	}
    	return this.toString().equals(o.toString());
    }
    public int hashCode() {
    	return this.toString().hashCode();
    }
}
