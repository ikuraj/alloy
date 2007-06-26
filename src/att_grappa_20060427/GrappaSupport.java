/*
 *  This software may only be used by you under license from AT&T Corp.
 *  ("AT&T").  A copy of AT&T's Source Code Agreement is available at
 *  AT&T's Internet website having the URL:
 *  <http://www.research.att.com/sw/tools/graphviz/license/source.html>
 *  If you received this software without first entering into a license
 *  with AT&T, you have an infringing copy of this software and cannot use
 *  it without violating AT&T's intellectual property rights.
 */

package att_grappa_20060427;

import java.util.*;
import java.awt.geom.*;


/**
 * A class providing some supports function for Grappa.
 *
 * @version 1.2, 27 Apr 2006; Copyright 1996 - 2006 by AT&T Corp.
 * @author  <a href="mailto:john@research.att.com">John Mocenigo</a> and Rich Drechsler, <a href="http://www.research.att.com">Research @ AT&T Labs</a>
 */
@SuppressWarnings("unchecked")
public abstract
class GrappaSupport

    implements GrappaConstants

{


    ///////////////////////////////////////////////////////////////////////////
    //
    // The ctypes type stuff is courtesy of Rich Drechsler of AT&T Labs.
    //
    ///////////////////////////////////////////////////////////////////////////

    private static final short	CN = 0x01;	// control
    private static final short	WS = 0x02;	// white space
    private static final short	SP = 0x04;	// space
    private static final short	PU = 0x08;	// punctuation
    private static final short	DG = 0x10;	// digit
    private static final short	OD = 0x20;	// octal digit
    private static final short	UC = 0x40;	// upper case
    private static final short	HD = 0x80;	// hex digit
    private static final short	LC = 0x100;	// lower case

    private static final short	ctype[] = {
	CN, CN, CN, CN, CN, CN, CN, CN,
	CN, CN|WS, CN|WS, CN|WS, CN|WS, CN|WS, CN, CN,
	CN, CN, CN, CN, CN, CN, CN, CN,
	CN, CN, CN, CN, CN, CN, CN, CN,
	WS|SP, PU, PU, PU, PU, PU, PU, PU,
	PU, PU, PU, PU, PU, PU, PU, PU,
	DG|OD, DG|OD, DG|OD, DG|OD, DG|OD, DG|OD, DG|OD, DG|OD,
	DG, DG, PU, PU, PU, PU, PU, PU,
	PU, UC|HD, UC|HD, UC|HD, UC|HD, UC|HD, UC|HD, UC,
	UC, UC, UC, UC, UC, UC, UC, UC,
	UC, UC, UC, UC, UC, UC, UC, UC,
	UC, UC, UC, PU, PU, PU, PU, PU,
	PU, LC|HD, LC|HD, LC|HD, LC|HD, LC|HD, LC|HD, LC,
	LC, LC, LC, LC, LC, LC, LC, LC,
	LC, LC, LC, LC, LC, LC, LC, LC,
	LC, LC, LC, PU, PU, PU, PU, CN,
    };

    private static final short	ALPHA = (UC|LC);
    private static final short	ALNUM = (UC|LC|DG);
    private static final short	GRAPH = (ALNUM|PU);
    private static final short	PRINT = (GRAPH|SP);
    private static final short	ODIGIT = (OD);
    private static final short	XDIGIT = (DG|HD);

    private static final short	LOWERTOUPPER = 'A' - 'a';
    private static final short	UPPERTOLOWER = 'a' - 'A';

    ///////////////////////////////////////////////////////////////////////////
    //
    // GrappaSupport
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isalnum(int ch) {

	return(isascii(ch) && (ctype[ch]&ALNUM) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isalpha(int ch) {

	return(isascii(ch) && (ctype[ch]&ALPHA) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isascii(int ch) {

	return(ch >= 0 && ch < 128);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	iscntrl(int ch) {

	return(isascii(ch) && (ctype[ch]&CN) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isdigit(int ch) {

	return(isascii(ch) && (ctype[ch]&DG) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isgraph(int ch) {

	return(isascii(ch) && (ctype[ch]&DG) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	islower(int ch) {

	return(isascii(ch) && (ctype[ch]&LC) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isoctal(int ch) {

	return(isascii(ch) && (ctype[ch]&ODIGIT) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isprint(int ch) {

	return(isascii(ch) && (ctype[ch]&PRINT) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	ispunct(int ch) {

	return(isascii(ch) && (ctype[ch]&PU) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isspace(int ch) {

	return(isascii(ch) && (ctype[ch]&WS) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isupper(int ch) {

	return(isascii(ch) && (ctype[ch]&UC) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static boolean
	isxdigit(int ch) {

	return(isascii(ch) && (ctype[ch]&XDIGIT) != 0);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static int
	tolower(int ch) {

	return(isupper(ch) ? ch + UPPERTOLOWER : ch);
    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * for ASCII only
     */
    static int
	toupper(int ch) {

	return(islower(ch) ? ch + LOWERTOUPPER : ch);
    }

    ///////////////////////////////////////////////////////////////////////////

    static String[]
	strsplit(String tuple) throws IllegalArgumentException {

	if(tuple == null) throw new IllegalArgumentException("supplied split string is null");

	StringTokenizer st = new StringTokenizer(tuple,",",false);

	String[] array = new String[st.countTokens()];

	int i = 0;
	while(st.hasMoreTokens()) {
	    array[i++] = st.nextToken();
	}

	return array;
    }

    ///////////////////////////////////////////////////////////////////////////

    static float[]
	floatArrayForTuple(String tuple) throws IllegalArgumentException, NumberFormatException {

	if(tuple == null) throw new IllegalArgumentException("supplied tuple string is null");

	StringTokenizer st = new StringTokenizer(tuple,", \t",false);

	float[] array = new float[st.countTokens()];

	int i = 0;
	while(st.hasMoreTokens()) {
	    array[i++] = Float.valueOf(st.nextToken()).floatValue();
	}

	return array;

    }

    ///////////////////////////////////////////////////////////////////////////

    static double[]
	arrayForTuple(String tuple) throws IllegalArgumentException, NumberFormatException {

	if(tuple == null) throw new IllegalArgumentException("supplied tuple string is null");

	StringTokenizer st = new StringTokenizer(tuple,", \t",false);

	double[] array = new double[st.countTokens()];

	int i = 0;
	while(st.hasMoreTokens()) {
	    array[i++] = Double.valueOf(st.nextToken()).doubleValue();
	}

	return array;

    }

    ///////////////////////////////////////////////////////////////////////////

    /**
     * Converts a string to an integer edge direction.
     * The string is first canonicalized (converted to lower case and
     * non-alphanumerics are removed) then compared to none, back, forward
     * or both.  A match returns GrappaLine.NONE_ARROW_EDGE,
     * GrappaLine.HEAD_ARROW_EDGE, GrappaLine.TAIL_ARROW_EDGE or 
     * GrappaLine.BOTH_ARROW_EDGE, respectively. When there is no match,
     * GrappaLine.NONE_ARROW_EDGE is returned.
     *
     * @param direction a string representing an edge direction
     *
     * @return an integer representation of the supplied edge direction
     */
    public static int xlateDirString(String direction) {
	if(direction != null) {
	    String dir = canonize(direction);
	    if(dir.equals("forward")) {
		return GrappaLine.TAIL_ARROW_EDGE;
	    } else if(dir.equals("back")) {
		return GrappaLine.HEAD_ARROW_EDGE;
	    } else if(dir.equals("both")) {
		return GrappaLine.BOTH_ARROW_EDGE;
	    }
	}
	return GrappaLine.NONE_ARROW_EDGE;
    }

    /**
     * Converts an integer edge direction value to a string representation.
     * Only GrappaLine.NONE_ARROW_EDGE, GrappaLine.HEAD_ARROW_EDGE,
     * GrappaLine.TAIL_ARROW_EDGE and GrappaLine.BOTH_ARROW_EDGE are
     * understood, all others are taken to mean GrappaLine.NONE_ARROW_EDGE.
     *
     * @param direction an integer representing an edge direction
     *
     * @return a string representation of the supplied edge direction
     */
    public static String xlateDir(int direction) {
	String retstr = null;

	if(direction == GrappaLine.TAIL_ARROW_EDGE) {
	    retstr = "forward";
	} else if(direction == GrappaLine.HEAD_ARROW_EDGE) {
	    retstr = "back";
	} else if(direction == GrappaLine.BOTH_ARROW_EDGE) {
	    retstr = "both";
	} else {
	    retstr = "none";
	}

	return retstr;
    }

    /**
     * Converts a string to an integer font style.
     * The string is first canonicalized (converted to lower case and
     * non-alphanumerics are removed) then compared to italic, bold or
     * bolditalic.  A match returns Font.ITALIC, Font.BOLD, or a bitwise
     * OR-ing of the two, respectively. When there is no match, Font.PLAIN
     * is returned.
     *
     * @param fontstyle a string representing a font style
     *
     * @return an integer representation of the supplied font style string
     */
    public static int xlateFontStyleString(String fontstyle) {
	if(fontstyle != null) {
	    String style = canonize(fontstyle);
	    if(style.equals("italic")) {
		return java.awt.Font.ITALIC;
	    } else if(style.equals("bold")) {
		return java.awt.Font.BOLD;
	    } else if(style.equals("bolditalic")) {
		return (java.awt.Font.BOLD|java.awt.Font.ITALIC);
	    }
	}
	return java.awt.Font.PLAIN;
    }

    /**
     * Converts an integer font style value to a string representation.
     * Only Font.ITALIC, Font.BOLD, and (Font.BOLD|Font.ITALIC) are
     * understood, all others are taken to mean Font.PLAIN.
     *
     * @param fontstyle an integer representing a font style
     *
     * @return a string representation of the supplied font style value
     */
    public static String xlateFontStyle(int fontstyle) {
	String retstr = null;

	if(fontstyle == java.awt.Font.ITALIC) {
	    retstr = "italic";
	} else if(fontstyle == java.awt.Font.BOLD) {
	    retstr = "bold";
	} else if(fontstyle == (java.awt.Font.BOLD|java.awt.Font.ITALIC)) {
	    retstr = "bolditalic";
	} else {
	    retstr = "normal";
	}

	return retstr;
    }

    /**
     * Canonize string by converting to lower-case and removing all
     * non-letter, non-digit characters.
     *
     * @param input the string to be canonized
     *
     * @return the canonized string
     */
    public static String canonize(String input) {
	if(input == null) return null;
	char[] array = input.toCharArray();
	int pos = 0;
	for(int i = 0; i < array.length; i++) {
	    if(Character.isLetterOrDigit(array[i])) {
		array[pos++] = Character.toLowerCase(array[i]);
	    }
	}

	if(pos == 0) return("");

	return new String(array,0,pos);
    }

    /**
     * Creates a GrappaBox from the coordinates of any two opposing corners.
     *
     * @param x1 x-coordinate of corner number 1.
     * @param y1 x-coordinate of corner number 1.
     * @param x2 x-coordinate of corner number 2, which is opposite corner 1.
     * @param y2 x-coordinate of corner number 2, which is opposite corner 1.
     * @return a GrappaBox generated the possibly-reordered coordinates.
     */
    public static GrappaBox boxFromCorners(double x1, double y1, double x2, double y2) {
	return(boxFromCorners(null, x1, y1, x2, y2));
    }

    /**
     * Creates a GrappaBox from the coordinates of any two opposing corners.
     *
     * @param box if non-null, the coordinates of this box are changed and this box is returned, otherwise a new box is created.
     * @param x1 x-coordinate of corner number 1.
     * @param y1 x-coordinate of corner number 1.
     * @param x2 x-coordinate of corner number 2, which is opposite corner 1.
     * @param y2 x-coordinate of corner number 2, which is opposite corner 1.
     * @return a GrappaBox generated the possibly-reordered coordinates.
     */
    public static GrappaBox boxFromCorners(GrappaBox box, double x1, double y1, double x2, double y2) {
	if(box == null) {
	    box = new GrappaBox();
	}
	box.x = x1 < x2 ? x1 : x2;
	box.y = y1 < y2 ? y1 : y2;
	box.width = x1 < x2 ? x2 - x1 : x1 - x2;
	box.height = y1 < y2 ? y2 - y1 : y1 - y2;

	return(box);
    }

    /**
     * Find an element in the supplied subgraph that contains the given point.
     * The last element encoutered is returned from a search of first
     * subgraphs, then edges, then nodes. The ordering within a set of elements
     * (e.g., nodes) is indeterminate.
     *
     * @param subg the subgraph to be searched.
     * @param pt the point of the search.
     * @return an element containing the point, or null.
     */
    public static Element findContainingElement(Subgraph subg, Point2D pt) {

	Element elem = null;

	Rectangle2D bb = subg.getBoundingBox();

	GrappaNexus grappaNexus = null;

	if(bb.contains(pt)) {
	    elem = subg;

	    Enumeration enm;
	    Element subelem = null;

	    enm = subg.subgraphElements();
	    while(enm.hasMoreElements()) {
		if((subelem = findContainingElement((Subgraph)(enm.nextElement()), pt)) != null) {
		    elem = subelem;
		}
	    }
	    enm = subg.edgeElements();
	    Edge edge;
	    while(enm.hasMoreElements()) {
		edge = (Edge)enm.nextElement();
		if((grappaNexus = edge.grappaNexus) == null) continue;
		if(grappaNexus.rawBounds2D().contains(pt)) {
		    if(grappaNexus.contains(pt)) {
			elem = edge;
		    }
		}
	    }
	    enm = subg.nodeElements();
	    Node node;
	    while(enm.hasMoreElements()) {
		node = (Node)enm.nextElement();
		if((grappaNexus = node.grappaNexus) == null) continue;
		if(grappaNexus.rawBounds2D().contains(pt)) {
		    if(grappaNexus.contains(pt)) {
			elem = node;
		    }
		}
	    }
	}
	return(elem);
    }

    /**
     * Find the elements in the supplied subgraph that are contained in
     * the given box.
     *
     * @param subg the subgraph to be searched.
     * @param pt the container box.
     * @return a vector whose components may be single elements or another
     *         vector of this same type with the property that all the elements
     *         in the vector (when eventually unravelled) are contained in
     *         the supplied box.
     */
    public static Vector findContainedElements(Subgraph subg, GrappaBox box) {
	Vector elems = null;

	Rectangle2D bb = subg.getBoundingBox();

	GrappaNexus grappaNexus = null;

	if(box.contains(bb)) {
	    return subg.vectorOfElements(Grappa.SUBGRAPH|Grappa.NODE|Grappa.EDGE);
	} else if(box.intersects(bb)) {
	    Enumeration enm;
	    Vector subelems = null;

	    enm = subg.subgraphElements();
	    while(enm.hasMoreElements()) {
		if((subelems = findContainedElements((Subgraph)(enm.nextElement()), box)) != null) {
		    if(elems == null) {
			elems = new Vector();
		    }
		    elems.add(subelems);
		}
	    }
	    enm = subg.edgeElements();
	    Edge edge;
	    while(enm.hasMoreElements()) {
		edge = (Edge)enm.nextElement();
		if((grappaNexus = edge.grappaNexus) == null) continue;
		if(box.contains(grappaNexus.rawBounds2D())) {
		    if(elems == null) {
			elems = new Vector();
		    }
		    elems.add(edge);
		}
	    }
	    enm = subg.nodeElements();
	    Node node;
	    while(enm.hasMoreElements()) {
		node = (Node)enm.nextElement();
		if((grappaNexus = node.grappaNexus) == null) continue;
		if(box.contains(grappaNexus.rawBounds2D())) {
		    if(elems == null) {
			elems = new Vector();
		    }
		    elems.add(node);
		}
	    }
	}
	return(elems);
    }

    /**
     * Set the highlight on an element and, possibly, related elements.
     * Since deletion can affect related elements (i.e., the edges connected
     * to a node or the sub-elements of a subgraph), those elements are
     * affected as well when highlighting.
     *
     * @param elem the element whose highlighting is to be adjusted.
     * @param mode the highlight mode to apply or remove; a mode of
     *             zero indicates all highlighting is turned off regardless
     *             of the setting.
     * @param setting one of HIGHLIGHT_ON, HIGHLIGHT_OFF or HIGHLIGHT_TOGGLE.
     */
    public static void setHighlight(Element elem, int mode, int setting) {
	if(elem == null) return;
	if(mode == 0) {
	    // treat delete specially
	    boolean wasDelete = ((elem.highlight&DELETION_MASK) == DELETION_MASK);
	    elem.highlight = 0;
	    if(wasDelete) {
		if(elem.isNode()) {
		    Enumeration enm = ((Node)elem).edgeElements();
		    while(enm.hasMoreElements()) {
			((Element)(enm.nextElement())).highlight = 0;
		    }
		} else if(elem.isSubgraph()) {
		    Enumeration enm = ((Subgraph)elem).elements();
		    while(enm.hasMoreElements()) {
			((Element)(enm.nextElement())).highlight = 0;
		    }
		}
	    }
	} else {
	    mode &= HIGHLIGHT_MASK;
	    if(setting == HIGHLIGHT_TOGGLE) {
		elem.highlight ^= mode;
	    } else if(setting == HIGHLIGHT_ON) {
		elem.highlight |= mode;
	    } else {
		elem.highlight &= ~mode;
	    }

	    if((mode&DELETION_MASK) == DELETION_MASK) {
		if(elem.isNode()) {
		    if((elem.highlight&DELETION_MASK) == DELETION_MASK) {
			Enumeration enm = ((Node)elem).edgeElements();
			while(enm.hasMoreElements()) {
			    ((Element)(enm.nextElement())).highlight |= DELETION_MASK;
			}
		    } else {
			Enumeration enm = ((Node)elem).edgeElements();
			while(enm.hasMoreElements()) {
			    ((Element)(enm.nextElement())).highlight &= ~DELETION_MASK;
			}
		    }
		} else if(elem.isSubgraph()) {
		    if((elem.highlight&DELETION_MASK) == DELETION_MASK) {
			Enumeration enm = ((Subgraph)elem).elements();
			while(enm.hasMoreElements()) {
			    ((Element)(enm.nextElement())).highlight |= DELETION_MASK;
			}
		    } else {
			Enumeration enm = ((Subgraph)elem).elements();
			while(enm.hasMoreElements()) {
			    ((Element)(enm.nextElement())).highlight &= ~DELETION_MASK;
			}
		    }
		}
	    }
	}
    }

    /**
     * Scroll to the viewport containing the specified GrappaPanel so that
     * it is centered on the given point. If the point is not contained in
     * the subgraph being displayed in the GrappaPanel, no action is taken.
     * If the getParent() method applied to the GrappaPanel argument does not
     * return a JViewport, an error message is displayed.
     *
     * @param cpt the point to place at the center of the GrappaPanel viewport
     * @param gpanel the GrappaPanel displaying the graph
     *
     * @return true for a valid request, false otherwise
     *
     */
    public static boolean centerPanel(Point2D cpt, GrappaPanel gpanel) {
	if(cpt == null || gpanel == null)
	    return false;
	if(!gpanel.getSubgraph().getBoundingBox().contains(cpt.getX(), cpt.getY())) {
	    return false;
	}
	Object prnt = gpanel.getParent();
	if(!(prnt instanceof javax.swing.JViewport)) {
	    Grappa.displayException(new RuntimeException("the parent of the supplied GrappaPanel is not a JViewport"));
	    return false;
	}
	javax.swing.JViewport vport = (javax.swing.JViewport)prnt;
	java.awt.Rectangle b = gpanel.getBounds();
	java.awt.Dimension d = vport.getExtentSize();
	AffineTransform transform = gpanel.getTransform();
	if(transform == null)
	    return false;
	Point2D p = transform.transform(cpt, null);

	Rectangle2D r = new Rectangle2D.Double(p.getX() + (double)b.x - ((double)d.width)/2., p.getY() + (double)b.y - ((double)d.height)/2., (double)d.width, (double)d.height);
	r = r.createIntersection(b);
	vport.scrollRectToVisible(r.getBounds());

	return true;
    }

}
