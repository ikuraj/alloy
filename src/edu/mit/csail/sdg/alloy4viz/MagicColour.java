/*
 * Alloy Analyzer 4 -- Copyright (c) 2007-2008, Derek Rayside
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4viz;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * This class implements the automatic visualization inference.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

final class MagicColour {

    /** This method logs the progress */
    static final void log(String msg) {
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
            uniqueColourTypes = MagicUtil.partiallyVisibleUserTopLevelTypes(vizState);
        }

        int index = 0;
        for (final AlloyType t : uniqueColourTypes) {
            vizState.nodeColor.put(t, (DotColor) DotColor.values(DotColor.MAGIC).get(index));
            index = (index + 1) % DotColor.values(DotColor.MAGIC).size();
        }
    }

    /**
     * SYNTACTIC/VISUAL: Determine colour highlighting for skolem constants.
     *
     */
    private void skolemColour() {
        final Set<AlloySet> sets = vizState.getCurrentModel().getSets();
        for (final AlloySet s : sets) {
            // change the style
            vizState.nodeStyle.put(s, DotStyle.BOLD);
            // change the label
            String label = vizState.label.get(s);
            final int lastUnderscore = label.lastIndexOf('_');
            if (lastUnderscore >= 0) {
                label = label.substring(lastUnderscore+1);
            }
            vizState.label.put(s, label);
        }
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
        final Set<List<DotShape>> usedShapeFamilies = new LinkedHashSet<List<DotShape>>();
        final Set<AlloyType> topLevelTypes = MagicUtil.partiallyVisibleUserTopLevelTypes(vizState);

        for (final AlloyType t : topLevelTypes) {

            // get the type family
            final Set<AlloyType> subTypes = MagicUtil.visibleSubTypes(vizState, t);
            final boolean isTvisible = MagicUtil.isActuallyVisible(vizState, t);
            final int size = subTypes.size() + (isTvisible ? 1 : 0);
            //log("TopLevelType:  " + t + " -- " + subTypes + " " + size);

            // match it to a shape family
            // 1. look for exact match
            boolean foundExactMatch = false;
            for (final List<DotShape> shapeFamily : DotShape.families) {
                if (size == shapeFamily.size() && !usedShapeFamilies.contains(shapeFamily)) {
                    // found a match!
                    usedShapeFamilies.add(shapeFamily);
                    assignNodeShape(t, subTypes, isTvisible, shapeFamily);
                    foundExactMatch = true;
                    break;
                }
            }
            if (foundExactMatch) continue;
            // 2. look for approximate match
            List<DotShape> approxShapeFamily = null;
            int approxShapeFamilyDistance = Integer.MAX_VALUE;
            for (final List<DotShape> shapeFamily : DotShape.families) {
                if (size <= shapeFamily.size() && !usedShapeFamilies.contains(shapeFamily)) {
                    // found a potential match
                    final int distance = shapeFamily.size() - size;
                    if (distance < approxShapeFamilyDistance) {
                        // it's a closer fit than the last match, keep it for now
                        approxShapeFamily = shapeFamily;
                        approxShapeFamilyDistance = distance;
                    }
                }
            }
            if (approxShapeFamily != null) {
                // use the best approximate match that we just found
                usedShapeFamilies.add(approxShapeFamily);
                assignNodeShape(t, subTypes, isTvisible, approxShapeFamily);
            }
            // 3. re-use a shape family matched to something else -- just give up for now
        }


//      final Set<AlloyType> visibleUserTypes = MagicUtil.visibleUserTypes(vizState);
//      final Set<AlloyType> uniqueShapeTypes;// = new HashSet<AlloyType>();
//
//      if (visibleUserTypes.size() <= 5) {
//          // can give every visible user type its own shape
//          uniqueShapeTypes = visibleUserTypes;
//      } else {
//          // give every top-level visible user type its own shape
//          uniqueShapeTypes = MagicUtil.topLevelTypes(vizState, visibleUserTypes);
//      }
//
//      int index = 0;
//      for (final AlloyType t : uniqueShapeTypes) {
//          vizState.shape(t, DotShape.values.get(index));
//          index = (index + 1) % DotShape.values.size();
//      }


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
     * Helper for nodeShape().
     * @param t
     * @param subTypes
     * @param isTvisible
     * @param shapeFamily
     */
    private void assignNodeShape(final AlloyType t, final Set<AlloyType> subTypes,
            final boolean isTvisible, final List<DotShape> shapeFamily) {
        int index = 0;
        // shape for t, if visible
        if (isTvisible) {
            final DotShape shape = shapeFamily.get(index++);
            //log("AssignNodeShape " + t + " " + shape);
            vizState.shape.put(t, shape);
        }
        // shapes for visible subtypes
        for (final AlloyType subt : subTypes) {
            final DotShape shape = shapeFamily.get(index++);
            //log("AssignNodeShape " + subt + " " + shape);
            vizState.shape.put(subt, shape);
        }
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
            vizState.label.put(visibleUserTypes.iterator().next(), "");
        }

    }

}
