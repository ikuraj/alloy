/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

import java.util.Set;

public class MagicColour {

    /** This method logs the progress */
    private static final void log(String msg) {
        System.err.println(msg);
        System.err.flush();
    }

    /** The VizState object that we're going to configure. */
    private final VizState vizState;


    /**
     * Constructor.
     */
    private MagicColour(final VizState vizState) {
        this.vizState = vizState;
    }

    /**
     * Main method to infer settings.
     */
    public static void magic(final VizState vizState) {
    	vizState.setNodePalette(DotPalette.MARTHA);
        final MagicColour st = new MagicColour(vizState);
        st.nodeNumbering();
        st.nodeNames();
        st.nodeShape();
        st.nodeColour();
        st.skolemColour();
    }


    /**
     * SYNTACTIC/VISUAL: Determine colours for nodes.
     *
     * when do we color things and what is the meaning of color
     * <ul>
     * <li> symmetry breaking: colors only matter up to recoloring (diff from
     * shape!)
     * <li> color substitutes for name/label
     *
     * </ul>
     */
    private void nodeColour() {
    	final Set<AlloyType> visibleUserTypes = MagicUtil.visibleUserTypes(vizState);
    	final Set<AlloyType> uniqueColourTypes;
    	
    	if (visibleUserTypes.size() <= 5) {
    		// can give every visible user type its own shape
    		uniqueColourTypes = visibleUserTypes;
    	} else {
    		// give every top-level visible user type its own shape
    		uniqueColourTypes = MagicUtil.topLevelTypes(vizState, visibleUserTypes);
    	}

    	int index = 0;
    	for (final AlloyType t : uniqueColourTypes) {
    		vizState.nodeColor(t, DotColor.values.get(index));
    		index = (index + 1) % DotColor.values.size();
    	}
    }

    /**
     * SYNTACTIC/VISUAL: Determine colour highlighting for skolem constants.
     *
     */
    private void skolemColour() {
    }

    /**
     * SYNTACTIC/VISUAL: Determine shapes for nodes.
     *
     * <ul>
     * <li> trapezoid, hexagon, rectangle, ellipse, circle, square -- no others
     * <li> actual shape matters -- do not break symmetry as with color
     * <li> ellipse by default
     * <li> circle if special extension of ellipse
     * <li> rectangle if lots of attributes
     * <li> square if special extension of rectangle
     * <li> when to use other shapes?
     *
     * </ul>
     */
    private void nodeShape() {
    	final Set<AlloyType> visibleUserTypes = MagicUtil.visibleUserTypes(vizState);
    	final Set<AlloyType> uniqueShapeTypes;// = new HashSet<AlloyType>();
    	
    	if (visibleUserTypes.size() <= 5) {
    		// can give every visible user type its own shape
    		uniqueShapeTypes = visibleUserTypes;
    	} else {
    		// give every top-level visible user type its own shape
    		uniqueShapeTypes = MagicUtil.topLevelTypes(vizState, visibleUserTypes);
    	}

    	int index = 0;
    	for (final AlloyType t : uniqueShapeTypes) {
    		vizState.shape(t, DotShape.values.get(index));
    		index = (index + 1) % DotShape.values.size();
    	}
    	
    	
//        final AlloyModel model = vizState.getCurrentModel();
//        for (final AlloyType t : model.getTypes()) {
//            if (!t.isBuiltin && MagicUtil.isActuallyVisible(vizState, t)) {
//                final String label = vizState.label(t);
//                if (label == null || label.length() == 0) {
//                    // circles for unlabelled nodes
//                    vizState.shape(t, DotShape.CIRCLE);
//                }
//            }
//        }
    }

    /**
     * SYNTACTIC/VISUAL: Should nodes of a given type be numbered?
     *
     * <ul>
     * <li> only if projecting? (otherwise the topology can distinguish them)
     * <li> need numbering if it is the target of an attribute
     * <li> never for enumerated types or singletons
     * </ul>
     */
    private void nodeNumbering() {
        // this is the default for the viz anyways
//      final AlloyModel model = vizState.getCurrentModel();
//      for (final AlloyType t : model.getTypes()) {
//          if (enumerationTypes.contains(t) || singletonTypes.contains(t)) {
//              vizState.number(t, false);
//          }
//      }
    }



    /**
     * SYNTACTIC/VISUAL: Should the names of nodes be displayed on them?
     *
     * when should names be used?
     * <ul>
     * <li> not when only a single sig (e.g. state machine with only one 'node'
     * sig)
     * <li> not when only a single relation
     * <li> check for single things _after_ hiding things by default
     *
     * </ul>
     *
     */
    private void nodeNames() {
    	final Set<AlloyType> visibleUserTypes = MagicUtil.visibleUserTypes(vizState);
    	
    	// trim names
    	for (final AlloyType t : visibleUserTypes) {
            // trim label before last slash
            MagicUtil.trimLabelBeforeLastSlash(vizState, t);
    	}
    	
        // hide names if there's only one node type visible
        if (1 == visibleUserTypes.size()) {
            vizState.label(visibleUserTypes.iterator().next(), "");
        }

    }

}
