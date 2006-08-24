package kodviz.graph;

import kodviz.dotviz.DotArrowHead;
import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotDirection;
import kodviz.dotviz.DotStyle;
/**
 * The Edge class represents a directed connection between two
 * Node objects.  It also holds information about the visual 
 * display of the edge (label, color, etc.) and drawing "hints"
 * for the layout tool (e.g. weight).
 */
public class Edge extends GraphElement {

    public static final DotStyle DEF_STYLE = DotStyle.SOLID;
    public static final DotArrowHead DEF_ARROWHEAD = DotArrowHead.NORMAL;
    public static final DotColor DEF_COLOR = DotColor.BLACK;
    public static final DotDirection DEF_DIR = DotDirection.FORWARD;
    public static final int DEF_WEIGHT = 1;
    
    
	
    private Node _from, _to;
    private DotDirection _dir;
    private DotArrowHead _arrowHead;
    private int _weight;    
    
    
    //
    // CONSTRUCTORS
    //

    /**
     * Full constructor for an edge
     *
     * @requires params are not null, id_ is *unique*
     * @effects creates a new edge with given parameters
     * @param from_ the origin node
     * @param to_ the destination node
     * @param label_ the textual label of the edge.  Can contain newlines
     * @param style_ the style of the edge
     * @param color_ the color of the edge
     * @param dir_ the direction of the arrow of the edge
     * @param arrowHead_ the text for the type of arrowhead desired
     * @param weight_ the weight of the edge.  The layout tool will attempt
     *        to make edges with higher weights shorter and straighter
     * @param id_ a unique String id for this edge
     */
    public Edge(Node from_, Node to_, String label_, DotStyle style_, DotColor color_,
		DotDirection dir_, DotArrowHead arrowHead_, int weight_, String id_) {
	_from = from_;
	_to = to_;
	_label = label_;
	_style = style_;
	_color = color_;
	_dir = dir_;
	_arrowHead = arrowHead_;
	_weight = weight_;
	_id = id_;
    }
 
    /**
     * Basic constructor
     *
     * @requires params are not null, id_ is *unique*
     * @effects creates a new edge using for the given nodes and label
     *          with default parameters
     * @param from_ the origin node
     * @param to_ the destination node
     * @param label_ the textual label of the edge.  Can contain newlines
     * @param id_ a 
     */
    public Edge(Node from_, Node to_, String label_, String id_) {
	this(from_, to_, label_, DEF_STYLE, DEF_COLOR, DEF_DIR,
	     DEF_ARROWHEAD, DEF_WEIGHT, id_);	
    }

    //
    // ACCESSORS
    //

    /**
     * Returns the origin node of the edge
     *
     * @return origin node
     */
    public Node getFromNode() {
	return _from;
    }

    /**
     * Returns the destination node of the edge
     *
     * @return destination node
     */
    public Node getToNode() {
	return _to;
    }

    /**
     * Returns the direction of the arrowhead of the edge
     *
     * @return direction of arrowhead
     */
    public DotDirection getDirection() {
	return _dir;
    }
    
    /**
     * Returns the type of arrow head of the edge
     *
     * @return arrow head of the edge
     */
    public DotArrowHead getArrowHead() {
	return _arrowHead;
    }

    /**
     * Returns the weight of the edge
     *
     * @return weight of the edge
     */
    public int getWeight() {
	return _weight;
    }


    //
    // OTHER METHODS
    //

    /*
     * this code causes problems when using sets and we don't really need 
     * this type of equality
     */
    /*
    public boolean equals(Object o) {
	if (o!=null && o instanceof Edge) {
	    return (_label.equals(((Edge)o)._label) &&
		    _id.equals(((Edge)o)._id) &&
		    _from.equals(((Edge)o)._from) &&
		    _to.equals(((Edge)o)._to));
	}
	else {
	    return false;
	}
    }*/

    /**
     * Overrides Object.hashCode()
     */
    public int hashCode() {
	return _label.hashCode()+_id.hashCode()+_from.hashCode()+_to.hashCode();
    }

    /**
     * Overrides Object.toString()
     */
    public String toString() {
	return "Edge " + _label.toString() + ": " + _from.toString()+
	    "->" + _to.toString() + "\n id: " + _id;
    }	    				  
	     
}
