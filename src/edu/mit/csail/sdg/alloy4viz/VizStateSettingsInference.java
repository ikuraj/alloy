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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.parser.World;

public class VizStateSettingsInference {

    /** The VizState object that we're going to configure. */
    private final VizState vizState;

    private final WorldInfo worldInfo;

    // results of inference
    private Set<AlloyType> enumerationTypes = new HashSet<AlloyType>();
    private Set<AlloyType> singletonTypes = new HashSet<AlloyType>();

    private AlloyType projectionType = null;

    private Set<AlloyRelation> spineRelations = Collections.emptySet();

    private static class WorldInfo {
        //private final World world;
        private final Set<String> abstractSigs = new HashSet<String>();
        private final Set<String> oneSigs = new HashSet<String>();
        private final Set<String> orderedSigs = new HashSet<String>();
        private final Set<String> builtinSigs = new HashSet<String>();

        WorldInfo(final World world) {
            if (world != null) {
                final SafeList<Sig> sigs = world.getAllSigs();
                for (final Sig s : sigs) {
                    if (s.builtin) builtinSigs.add(s.label);
                    if (s.isAbstract) abstractSigs.add(s.label);
                    if (s.isOne) oneSigs.add(s.label);
                    if (s.anno.get("ordering") != null) orderedSigs.add(s.label);
                }
            }
        }

        public boolean isAbstract(final AlloyType t) {
            return contains(abstractSigs, t.getName());
        }

        public boolean isOne(final AlloyType t) {
            return contains(oneSigs, t.getName());
        }

        public boolean isBuiltin(final AlloyType t) {
            if (contains(builtinSigs, t.getName())) {
                return true;
            } else {
                // try harder
                return NAMES_OF_ALLOY_SYSTEM_TYPES.contains(t.getName());
            }
        }

        public boolean isOrdered(AlloyType t) {
            return contains(orderedSigs, t.getName());
        }

        private static final Set<String> NAMES_OF_ALLOY_SYSTEM_TYPES;
        static {
            final Set<String> s = new HashSet<String>();
            s.add("univ");
            s.add("Int");
            s.add("seq/Int");
            NAMES_OF_ALLOY_SYSTEM_TYPES = Collections.unmodifiableSet(s);
        }

        private static boolean contains(final Set<String> set, final String s) {
            if (set.contains(s)) return true;
            if (set.contains("this/" + s)) return true;
            return false;
        }

    }


    /**
     * Constructor.
     *
     * @param vizState
     */
    public VizStateSettingsInference(final VizState vizState) {
        this.vizState = vizState;
        this.worldInfo = new WorldInfo(null);
    }

    /**
     * Main method to infer settings.
     */
    public void infer() {
        vizState.resetTheme();

        identifyEnumerationTypes();
        projection();
        nodeVisibility();
        spine();
        attributes();

        edgeLabels();
        hideImports();
        hideUnconnectedNodes();
        nodeNumbering();
        nodeNames();
        nodeShape();
        nodeColour();
        skolemColour();

    }


    /**
     * SYNTACTIC: An enumeration follows the pattern "abstract sig Colour; one
     * sig Red; one sig Blue".
     *
     */
    private void identifyEnumerationTypes() {
        final AlloyModel model = vizState.getCurrentModel();
        final Set<AlloyType> types = model.getTypes();
        for (final AlloyType t : types) {
            if (enumerationTypes.contains(t)) {
                // we've already checked this one, don't muck with it now
                continue;
            }
            // start investigating
            if (worldInfo.isOne(t)) {
                singletonTypes.add(t);
                //vizState.nodeVisible(t, false);
            }
            if (!worldInfo.isBuiltin(t) && worldInfo.isAbstract(t)) {
                final List<AlloyType> subTypes = model.getSubTypes(t);
                int numberOfSingletonSubtypes = 0;
                for (final AlloyType st : subTypes) {
                    if (worldInfo.isOne(st)) {
                        numberOfSingletonSubtypes++;
                        singletonTypes.add(st);
                        //vizState.nodeVisible(st, false);
                    }
                }
                if (subTypes.size() == numberOfSingletonSubtypes) {
                    // we have a winner
                    enumerationTypes.add(t);
                    enumerationTypes.addAll(subTypes);
                    final Boolean inherited = null;
                    for (final AlloyType st: subTypes) {
                        // all of the subtypes in the enumeration should have visibility inherited
                        // so that the user only needs to make the abstract supertype visible
                        // if we made a mistake hiding these things
                        vizState.nodeVisible(st, inherited);
                    }
                    // hide unless these are the source of some relation
                    boolean visible = false;
                    for (final AlloyRelation r : model.getRelations()) {
                        final AlloyType sourceType = r.getTypes().get(0);
                        if (t.equals(sourceType) || subTypes.contains(sourceType)) {
                            visible = true;
                            break;
                        }
                    }
                    System.err.println("VizInference: visible status of enumeration type " + t + " " + visible);
                    vizState.nodeVisible(t, visible);
                }
            }
        }

    }

    /**
     * SEMANTIC/LAYOUT: Determine at most one relation to project over.
     *
     *
     * When do we project over a sig? Do we ever project over more than one?
     * <ul>
     * <li> pick 0 or 1 things to project over
     * <li> match names: Time, State, Tick, TimeStep
     * <li> if ord is opened over the sig
     * <li> if present in several ternary relations (i.e. if it will help viz
     * high arity relations)
     * <li> position in relation (always end or always beginning)
     * <li> should we try to catch projections such as the one over birthday
     * books?
     * <li> pattern match (b,b') to catch transition relations
     * <li> add combo box in GUI (?)
     * </ul>
     */
    private void projection() {
        // only fiddle with this if it hasn't already been set somewhere else
        if (projectionType == null && vizState.getProjectedTypes().isEmpty()) {
            final AlloyModel model = vizState.getCurrentModel();
            //final Set<AlloyType> candidateTypes = new HashSet<AlloyType>();
            final Map<AlloyType,Integer> scores = new HashMap<AlloyType,Integer>();
            for (final AlloyType t : model.getTypes()) {
                scores.put(t, 0);
                // does it have a name like State, Time, etc
                if (hasLikelyProjectionTypeName(t.getName())) {
                    scores.put(t, scores.get(t)+1 );
                }
                // is it in some ternary relation?
                for (final AlloyRelation r : model.getRelations()) {
                    if (r.getArity() > 2 && r.getTypes().contains(t)) {
                        scores.put(t, scores.get(t)+1 );
                    }
                }
                // is it ordered?
                if (worldInfo.isOrdered(t)) {
                    scores.put(t, scores.get(t)+1 );
                }
            }
            // now we have the scores, see who the winners are:
            int max = 0;
            final Set<AlloyType> winners = new HashSet<AlloyType>();
            for (final Map.Entry<AlloyType,Integer> e : scores.entrySet()) {
                if (e.getValue() == max) {
                    winners.add(e.getKey());
                }
                if (e.getValue() > max) {
                    max = e.getValue();
                    winners.clear();
                    winners.add(e.getKey());
                }
            }
            if (max < 2) {
                // no winner, don't project
                System.err.println("VizInference: no candidate type to project on.");
            } else {
                if (winners.size() > 1) {
                    // we have a tie ... what to do?
                    System.err.println("VizInference: projection tie. " + winners);
                }
                // pick one arbitrarily for now ...
                final AlloyType winner = winners.iterator().next();
                System.err.println("VizInference: projecting on " + max + " " + winner);

                projectionType = winner;
                vizState.project(projectionType);
            }

        }
    }
    private final boolean hasLikelyProjectionTypeName(final String n) {
        if (LIKELY_PROJECTION_TYPE_NAMES.contains(n)) {
            return true;
        }
        for (final String s : LIKELY_PROJECTION_TYPE_NAMES) {
            if (n.endsWith(s) || n.startsWith(s)) {
                return true;
            }
        }
        return false;
    }
    private final static Set<String> LIKELY_PROJECTION_TYPE_NAMES;
    static {
        final Set<String> s = new HashSet<String>();
        s.add("State");
        s.add("TrainState");
        s.add("Time");
        s.add("Tick");
        s.add("TimeStep");
        LIKELY_PROJECTION_TYPE_NAMES = Collections.unmodifiableSet(s);
    }

    /**
     * SEMANTIC/LAYOUT: Determine some relations to be the spine (ie, influence
     * the layout).
     *
     * Which relations should be used to layout? all? none? clever?
     * <ul>
     * <li> interesting example: 2d game grid
     * <li> ex: toplogical sort -- layout tree and list, not cnxn between them
     * <li> look for homogenius binary relation (a -&gt; a)
     * <li> may be several relations defining the spine
     * </ul>
     *
     */
    private void spine() {

        final AlloyModel model = vizState.getCurrentModel();
        final Set<AlloyRelation> relations = model.getRelations();
        if (!relations.isEmpty()) {
            // only mess with the relations if there are some

            // only binary relations are candidates
            final Set<AlloyRelation> spines = new HashSet<AlloyRelation>();
            for (final AlloyRelation r : relations) {
                if (r.getArity() == 2) {
                    final List<AlloyType> rtypes = r.getTypes();
                    //final AlloyType sourceType = rtypes.get(0);
                    final AlloyType targetType = rtypes.get(1);

                    if (!enumerationTypes.contains(targetType)) {
                        // only a spine if the target is not an enumeration type
                        spines.add(r);
                    }

                    // however, binary relations named parent should be layed out backwards
                    if (r.getName().equals("parent")) {
                        vizState.layoutBack(r, true);
                    }
                }
            }

            // do we have any spines? if so, use them, if not use all relations
            spineRelations = spines.isEmpty() ? relations : spines;
        }

        // set everything to not influence layout
        for (final AlloyRelation r : relations) {
            vizState.constraint(r, false);
            vizState.edgeColor(r, DotColor.GRAY);
        }

        // set spines to influence layout
        for (final AlloyRelation s : spineRelations) {
            final Boolean inherit = null;
            vizState.constraint(s, inherit);
            // inherit the default color, which should be black
            final DotColor inheritedDotColor = null;
            vizState.edgeColor(s, inheritedDotColor);
        }

    }

    /**
     * SEMANTIC/LAYOUT: Determine whether non-projection, non-spine relations
     * should be shown as attributes or edges.
     *
     * <ul>
     * <li> binary vs. higher arity -- only make binary attributes
     * <li> use attributes on-demand to reduce clutter, not blindly
     * <li> functional relations should be attributes (what about a tree?)
     * <li> never make something an edge and an attribute
     *
     * </ul>
     *
     */
    private void attributes() {
        final AlloyModel model = vizState.getCurrentModel();
        for (final AlloyRelation r : model.getRelations()) {
            if (r.getArity()==2 && !r.equals(projectionType) && !spineRelations.contains(r)) {
                // it's binary, non-projection and non-spine
                if (enumerationTypes.contains(r.getTypes().get(1))) {
                    // target is an enumeration: we have an attribute
                    vizState.attribute(r, true);
                    vizState.edgeVisible(r, false);
                }
            }
        }
    }


    /**
     * PRESENTATIONAL: Labels for edges.
     */
    private void edgeLabels() {
        final AlloyModel model = vizState.getCurrentModel();
        int relationsAsEdges = 0;
        AlloyRelation visibleRelation = null;
        for (final AlloyRelation r : model.getRelations()) {
            final Boolean v = vizState.edgeVisible(r);
            if (v == null || v.booleanValue()) {
                // it's visible
                relationsAsEdges++;
                visibleRelation = r;
                // remove text before last slash
                trimLabelBeforeLastSlash(r);
            }
        }
        // If there's only one relation visible as an edge, then no need to label it.
        if (1 == relationsAsEdges) {
            vizState.label(visibleRelation, "");
        }
    }

    private void trimLabelBeforeLastSlash(final AlloyElement x) {
        vizState.label(x, trimBeforeLastSlash(vizState.label(x)));
    }
    private static String trimBeforeLastSlash(final String label) {
        final int lastSlash = label.lastIndexOf('/');
        if (lastSlash >= 0) {
            return label.substring(lastSlash+1);
        } else {
            return label;
        }
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
        final AlloyModel model = vizState.getCurrentModel();
        for (final AlloyType t : model.getTypes()) {
            if (!worldInfo.isBuiltin(t) && isActuallyVisible(t)) {
                final String label = vizState.label(t);
                if (label == null || label.length() == 0) {
                    // circles for unlabelled nodes
                    vizState.shape(t, DotShape.CIRCLE);
                }
            }
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
     * SYNTACTIC/VISUAL: Hide some things.
     */
    private void nodeVisibility() {
        final AlloyModel model = vizState.getCurrentModel();
        final Set<AlloyType> types = model.getTypes();

        for (final AlloyType t : types) {
            if (!worldInfo.isBuiltin(t) && isActuallyVisible(t)) {
                if (t.getName().endsWith("/Ord")) {
                    vizState.nodeVisible(t, false);
                }
            }
        }

        for (final AlloySet s : model.getSets()) {
            if (isActuallyVisible(s)) {
                if (s.getName().endsWith("/Ord")) {
                    vizState.nodeVisible(s, false);
                }
            }
        }

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
        final AlloyModel model = vizState.getCurrentModel();
        final Set<AlloyType> types = model.getTypes();

        int visibleUserTypeCount = 0;
        AlloyType visibleUserType = null;
        for (final AlloyType t : types) {
            if (!worldInfo.isBuiltin(t) && isActuallyVisible(t)) {
                visibleUserTypeCount++;
                visibleUserType = t;
                System.err.println("VizInference: visible user type " + t);
                // trim label before last slash
                trimLabelBeforeLastSlash(t);
            }
        }
        // hide names if there's only one node type visible
        if (1 == visibleUserTypeCount) {
            vizState.label(visibleUserType, "");
        }

    }

    /**
     * Determines whether a type is actually visible -- ie, if it has an inherited value,
     * looks up the hierarchy until that is resolved.
     * @param t
     * @return true if this type will be shown to the user, false if this type will be hidden from the user
     */
    private boolean isActuallyVisible(final AlloyType t) {
        final Boolean V = vizState.nodeVisible(t);
        if (V != null) return V.booleanValue();

        // inherited value, find out the real deal
        final AlloyModel model = vizState.getCurrentModel();
        AlloyType parent = model.getSuperType(t);
        while (parent != null) {
            final Boolean pV = vizState.nodeVisible(parent);
            if (pV != null) {
                // found a real setting
                break;
            }
            parent = model.getSuperType(parent);
        }
        if (parent == null) {
            // made it to univ without finding a real setting
            return true;
        } else {
            // found a concrete setting, use it
            return vizState.nodeVisible(parent).booleanValue();
        }
    }

    private boolean isActuallyVisible(final AlloySet s) {
        final Boolean V = vizState.nodeVisible(s);
        if (V != null) return V.booleanValue();

        return isActuallyVisible(s.getType());
    }

    /**
     * SYNTACTIC/VISUAL: Should nodes that are not connected to anything else be
     * hidden?
     * <ul>
     * <li> don't see events that don't occur at this time
     * <li> hide unless skolems or sets
     * </ul>
     *
     */
    private void hideUnconnectedNodes() {
//      // set the default to be hidden
//      vizState.hideUnconnected(null, true);
//      // unhide skolems and sets
//      final AlloyModel model = vizState.getCurrentModel();
//      for (final AlloyType t : model.getTypes()) {
//          if (isActuallyVisible(t)) {
//              vizState.hideUnconnected(t, false);
//          }
//      }
//      for (final AlloySet s : model.getSets()) {
//          if (isActuallyVisible(s)) {
//              vizState.hideUnconnected(s, false);
//          }
//      }
    }

    /**
     * Should imports be hidden?
     * <ul>
     * <li> hide ord, not other imports
     * <li> hide unconnected nodes for imports ?
     * </ul>
     */
    private void hideImports() {
    }
}
