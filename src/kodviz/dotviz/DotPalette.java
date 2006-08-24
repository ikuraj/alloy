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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * This class holds typesafe enums for different color palettes the user can choose.
 * It's not a "real" dot property in that it doesn't get written.
 */
public class DotPalette extends DotProperty implements Serializable {

	private static final long serialVersionUID = 1L;
	public static final String DISPLAYED_NAME = "Palette";
    public static final String DOT_NAME = "";
    public static final String DOT_TEXT = "";    

    /*********serializing code**************/   
    // Ordinal of next suit to be created
    private static int nextOrdinal = 0;
    // Assign an ordinal to this suit
    private final int ordinal = nextOrdinal++;

    public static final DotPalette STANDARD = new DotPalette("Standard");
    public static final DotPalette CLASSIC  = new DotPalette("Classic");
    public static final DotPalette MARTHA  = new DotPalette("Martha");
    public static final DotPalette NEON  = new DotPalette("Neon");

    private static final DotPalette[] VALS = 
       { STANDARD, CLASSIC, MARTHA, NEON };

    //
    // CONSTRUCTOR
    //

    /**
     */
    private DotPalette(String displayedText_) {
	_displayedText = displayedText_;
	_dotText = DOT_TEXT;	
	_displayedName = DISPLAYED_NAME;
	_dotName = DOT_NAME;	
    }

    public static List getValidValues() {	
	return Collections.unmodifiableList(Arrays.asList(VALS));

    }

    /* for serializing purposes */
    private Object readResolve()
            throws ObjectStreamException {
        return VALS[ordinal]; // Canonicalize
    }

}
   
