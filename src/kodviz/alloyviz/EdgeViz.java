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
import kodviz.dotviz.DotStyle;


/**
 * EdgeViz is a data structure that holds all of the information about
 * visualizing a type of edge.
 */
public class EdgeViz implements Serializable {

	private static final long serialVersionUID = 1L;
	private Boolean _visible;
    private String _label;
    private DotColor _color;
    private DotStyle _style;
    private int _weight;
    private Boolean _attribute;
    private Boolean _sameRank;
    private Boolean _mergeArrows;
    private Boolean _selected;
    private Boolean _layoutBack;

    /**
     * Create a new EdgeViz with the given settings.  Use null for default ("inherit")
     */
    public EdgeViz(
        Boolean visible_,
        String label_,
        DotColor color_,
        DotStyle style_,
        int weight_,
        Boolean attribute_,
        Boolean sameRank_,
        Boolean mergeArrows_,
        Boolean selected_,
        Boolean layoutBack_) {
        _visible = visible_;
        _label = label_;
        _color = color_;
        _style = style_;
        _weight = weight_;
        _attribute = attribute_;
        _sameRank = sameRank_;
        _mergeArrows = mergeArrows_;
        _selected = selected_;
        _layoutBack = layoutBack_;
    }

    public EdgeViz copy() {
        EdgeViz ev =
            new EdgeViz(
                _visible,
                _label,
                _color,
                _style,
                _weight,
                _attribute,
                _sameRank,
                _mergeArrows,
                _selected,
                _layoutBack);
        return ev;
    }

    //
    // NOTE: return values of null indicates the default option was chosen
    //

    /**
     * Returns whether or not this edge will be shown  
     */
    public Boolean isVisible() {
        return _visible;
    }
    /**
     * Returns the label for this EdgeViz.
     */
    public String getLabel() {
        return _label;
    }

    /**
     * Returns the color for this EdgeViz.
     */
    public DotColor getColor() {
        return _color;
    }

    /**
     * Returns the style for this EdgeViz.
     */
    public DotStyle getStyle() {
        return _style;
    }

    /**
     * Returns the weight of this EdgeViz.
     */
    public int getWeight() {
        return _weight;
    }

    /**
     * Returns whether this EdgeViz is an attribute.
     */
    public Boolean isAttribute() {
        return _attribute;
    }

    /**
     * Returns whether this EdgeViz is set to Same Rank.
     */
    public Boolean isSameRank() {
        return _sameRank;
    }

    /**
     * Returns whether this EdgeViz is set to merge bidirectional arrows.
     */
    public Boolean mergeArrows() {
        return _mergeArrows;
    }

    /**
     * Returns true if edge is selected in cust. editor checkbox     
     */
    public boolean isSelected() {
        if (_selected == null) {
            return true;
        }
        return _selected.booleanValue();
    }

    /**
     * Returns whether or not this edge is to be laid out backwards
     */
    public Boolean layoutBack() {
        return _layoutBack;
    }

    /**
     * Returns a String representation of this EdgeViz.
     */
    public String toString() {
        return "EdgeViz:\nLabel: "
            + getLabel()
            + "\nVisible: "
            + isVisible()
            + "\nColor: "
            + getColor()
            + "\nStyle: "
            + getStyle()
            + "\nWeight: "
            + getWeight()
            + "\nAttribute: "
            + isAttribute()
            + "\nSame Rank: "
            + isSameRank()
            + "\nMerge Arrows: "
            + mergeArrows()
            + "\n";
    }
    // jbaek added for testing purpose
    public boolean equals(Object o) {
    	if ((o == null) || !(o instanceof EdgeViz)) {
    	    return false;
    	}
    	return this.toString().equals(o.toString());
    }
    public int hashCode() {
    	return this.toString().hashCode();
    }
}
