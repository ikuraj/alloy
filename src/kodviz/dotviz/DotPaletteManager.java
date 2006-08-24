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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unchecked")
public class DotPaletteManager {

    private Map _nodePalettesToColors, _edgePalettesToColors;

    private static final DotColor[] NODE_STANDARD =
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.STANDARD_RED,
      DotColor.STANDARD_GREEN, DotColor.STANDARD_BLUE, DotColor.STANDARD_YELLOW };
    private static final DotColor[] NODE_CLASSIC =
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.CLASSIC_RED,
      DotColor.CLASSIC_GREEN, DotColor.CLASSIC_BLUE, DotColor.CLASSIC_YELLOW };
    private static final DotColor[] NODE_MARTHA = 
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.MARTHA_RED,
      DotColor.MARTHA_GREEN, DotColor.MARTHA_BLUE, DotColor.MARTHA_YELLOW };
    private static final DotColor[] NODE_NEON = 
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.NEON_RED,
      DotColor.NEON_GREEN, DotColor.NEON_BLUE, DotColor.NEON_YELLOW };

    private static final DotColor[] EDGE_STANDARD =
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.STANDARD_RED,
      DotColor.STANDARD_GREEN, DotColor.STANDARD_BLUE, DotColor.STANDARD_YELLOW };
    private static final DotColor[] EDGE_CLASSIC =
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.CLASSIC_RED,
      DotColor.CLASSIC_GREEN, DotColor.CLASSIC_BLUE, DotColor.CLASSIC_YELLOW };
    private static final DotColor[] EDGE_MARTHA = 
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.MARTHA_RED,
      DotColor.MARTHA_GREEN, DotColor.MARTHA_BLUE, DotColor.MARTHA_YELLOW };
    private static final DotColor[] EDGE_NEON = 
    { DotColor.WHITE, DotColor.GRAY, DotColor.BLACK, DotColor.NEON_RED,
      DotColor.NEON_GREEN, DotColor.NEON_BLUE, DotColor.NEON_YELLOW };    
    
    
    public DotPaletteManager() {
	_nodePalettesToColors = new HashMap();
	_edgePalettesToColors = new HashMap();

	_nodePalettesToColors.put(DotPalette.STANDARD, Arrays.asList(NODE_STANDARD));
	_nodePalettesToColors.put(DotPalette.CLASSIC, Arrays.asList(NODE_CLASSIC));
	_nodePalettesToColors.put(DotPalette.MARTHA, Arrays.asList(NODE_MARTHA));
	_nodePalettesToColors.put(DotPalette.NEON, Arrays.asList(NODE_NEON));

	_edgePalettesToColors.put(DotPalette.STANDARD, Arrays.asList(EDGE_STANDARD));
	_edgePalettesToColors.put(DotPalette.CLASSIC, Arrays.asList(EDGE_CLASSIC));
	_edgePalettesToColors.put(DotPalette.MARTHA, Arrays.asList(EDGE_MARTHA));
	_edgePalettesToColors.put(DotPalette.NEON, Arrays.asList(EDGE_NEON));
    }

    /**
     * Tries to match by index.  Returns null if newDP is smaller than the index of
     * oldColor dictates.
     */
    public DotColor findBestNodeMatch(DotPalette oldDP, DotPalette newDP, DotColor oldColor) {
	int index = getNodeColors(oldDP).indexOf(oldColor);
	List newColors = getNodeColors(newDP);
	if (index==-1) {	    
	    return null;
	}
	else if (newColors.size()>index) {
	    return (DotColor)newColors.get(index);
	}
	return null;			
    }

    public DotColor findBestEdgeMatch(DotPalette oldDP, DotPalette newDP, DotColor oldColor) {
	int index = getEdgeColors(oldDP).indexOf(oldColor);
	List newColors = getEdgeColors(newDP);
	if (index==-1) {
	    return null;
	}
	else if (newColors.size()>index) {
	    return (DotColor)newColors.get(index);
	}
	return null;
    }
    
    public List getNodeColors(DotPalette dp) {
	return Collections.unmodifiableList((List)_nodePalettesToColors.get(dp));
    }

    public List getEdgeColors(DotPalette dp) {
	return Collections.unmodifiableList((List)_edgePalettesToColors.get(dp));
    }
	
}
