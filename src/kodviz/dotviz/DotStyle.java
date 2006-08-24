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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;

/**
 * DotStyle is a typesafe enumeration that represents the different line styles
 * that dot can produce.  These styles can be applied to nodes or edges.
 */
public class DotStyle extends DotProperty implements Serializable {

	private static final long serialVersionUID = 1L;
	public static final String DISPLAYED_NAME = "Style";
    public static final String DOT_NAME = "style";
    
    public static final DotStyle SOLID = new DotStyle("Solid", "solid", "icons/StyleIcons/solid.gif");
    public static final DotStyle DASHED = new DotStyle("Dashed", "dashed", "icons/StyleIcons/dashed.gif");
    public static final DotStyle DOTTED = new DotStyle("Dotted", "dotted", "icons/StyleIcons/dotted.gif");
    public static final DotStyle BOLD = new DotStyle("Bold", "bold", "icons/StyleIcons/bold.gif");
    public static final DotStyle INVIS = new DotStyle("Invisible", "invis", null);

    /*********serializing code**************/   
    // Ordinal of next suit to be created
    private static int nextOrdinal = 0;
    // Assign an ordinal to this suit
    private final int ordinal = nextOrdinal++;

    private static final DotStyle[] VALS = 
    { SOLID, DASHED, DOTTED, BOLD, INVIS };

    
    // Construct a new DotStyle with the given displayedText and dotText
    private DotStyle(String displayedText_, String dotText_, String iconPath_) {
	_displayedText = displayedText_;
	_dotText = dotText_;
	_displayedName = DISPLAYED_NAME;
	_dotName = DOT_NAME;
	_icon = new ImageIcon(ImageHandler.loadImage(iconPath_));
    }

    @SuppressWarnings("unchecked")
    public static List getValidValues() {
	List values = new ArrayList(4);
	values.add(SOLID);
	values.add(DASHED);
	values.add(DOTTED);
	values.add(BOLD);

	return Collections.unmodifiableList(values);
    }

    /* for serializing purposes */
    private Object readResolve()
            throws ObjectStreamException {
        return VALS[ordinal]; // Canonicalize
    }	

}
