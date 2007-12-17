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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import edu.mit.csail.sdg.alloy4.Util;
import static edu.mit.csail.sdg.alloy4viz.VizGUI.priv;

/**
 * This utility class generates a graph for a particular index of the projection.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class StaticGraphMaker {

    /** The theme customization. */
    private final VizState view;

    /** The projected instance for the graph currently being generated. */
    private final AlloyInstance instance;

    /** The projected model for the graph currently being generated. */
    private final AlloyModel model;

    /** The map that contains all edges and what the AlloyTuple that each edge corresponds to. */
    private final Map<DotEdge,AlloyTuple> edges=new LinkedHashMap<DotEdge,AlloyTuple>();

    /** The map that contains all nodes and what the AlloyAtom that each node corresponds to. */
    private final Map<DotNode,AlloyAtom> nodes=new LinkedHashMap<DotNode,AlloyAtom>();

    /** This maps each atom to the node representing it; if an atom doesn't have a node, it won't be in the map. */
    private final Map<AlloyAtom,DotNode> atom2node=new LinkedHashMap<AlloyAtom,DotNode>();

    /** This stores a set of additional labels we want to add to an existing node. */
    private final Map<DotNode,Set<String>> attribs=new LinkedHashMap<DotNode,Set<String>>();

    /** This stores the resulting graph. */
    private final DotGraph graph;

    /** Produces a single Graph from the given Instance and View and choice of Projection */
    public static DotGraph produceGraph(AlloyInstance instance, VizState view, AlloyProjection proj) {
        view = new VizState(view);
        if (proj == null) proj = new AlloyProjection();
        StaticGraphMaker answer = new StaticGraphMaker(instance,view,proj);
        return answer.graph;
    }

    /** The list of colors, in order, to assign each legend. */
    private static final List<Color> colorsClassic = Collections.unmodifiableList(Arrays.asList(
       new Color(228,26,28)
       ,new Color(166,86,40)
       ,new Color(255,127,0)
       ,new Color(77,175,74)
       ,new Color(55,126,184)
       ,new Color(152,78,163)
       //,new Color(255,255,51)
       //,new Color(247,129,191)
    ));

    /** The list of colors, in order, to assign each legend. */
    private static final List<Color> colorsStandard = Collections.unmodifiableList(Arrays.asList(
       new Color(227,26,28)
       ,new Color(255,127,0)
       ,new Color(251*8/10,154*8/10,153*8/10)
       //new Color(166,206,227)
       //,new Color(178,223,138)
       ,new Color(51,160,44)
       ,new Color(31,120,180)
       //,new Color(253,191,111)
    ));

    /** The list of colors, in order, to assign each legend. */
    private static final List<Color> colorsMartha = Collections.unmodifiableList(Arrays.asList(
       new Color(231,138,195)
       ,new Color(252,141,98)
       ,new Color(166,216,84)
       ,new Color(102,194,165)
       ,new Color(141,160,203)
       //,new Color(255,217,47)
       //,new Color(229,196,148)
       //,new Color(179,179,179)
    ));

    /** The list of colors, in order, to assign each legend. */
    private static final List<Color> colorsNeon = Collections.unmodifiableList(Arrays.asList(
       new Color(231,41,138)
       ,new Color(217,95,2)
       ,new Color(166,118,29)
       ,new Color(102,166,30)
       ,new Color(27,158,119)
       ,new Color(117,112,179)
       //,new Color(230,171,2)
       //,new Color(102,102,102)
    ));

    /** The constructor takes an Instance and a View, then insert the generate graph(s) into a blank cartoon. */
    private StaticGraphMaker (AlloyInstance originalInstance, VizState view, AlloyProjection proj) {
        Map<AlloyRelation,Color> magicColor = new TreeMap<AlloyRelation,Color>();
        Map<AlloyRelation,Integer> rels = new TreeMap<AlloyRelation,Integer>();
        this.view = view;
        instance = StaticProjector.project(originalInstance,proj);
        model = instance.model;
        for (AlloyRelation rel: model.getRelations()) {
            rels.put(rel,null);
        }
        List<Color> colors;
          if (view.getEdgePalette() == DotPalette.CLASSIC) colors = colorsClassic;
          else if (view.getEdgePalette() == DotPalette.STANDARD) colors = colorsStandard;
          else if (view.getEdgePalette() == DotPalette.MARTHA) colors = colorsMartha;
          else colors = colorsNeon;
        int ci=0;
        for (AlloyRelation rel: model.getRelations()) {
            DotColor c = view.edgeColor(rel, model);
            Color cc = (c==DotColor.MAGIC) ? colors.get(ci) : DotColor.name2color(c.getDotText(view.getEdgePalette()));
            int count = view.edgeVisible(rel, model) ? edgesAsArcs(rel, colors.get(ci)) : 0;
            rels.put(rel, count);
            magicColor.put(rel, cc);
            if (count>0) ci=(ci+1)%(colors.size());
        }
        for (AlloyAtom atom:instance.getAllAtoms()) {
            List<AlloySet> sets=instance.atom2sets(atom);
            if (sets.size()>0) {
                for (AlloySet s:sets)
                    if (view.nodeVisible(s, instance.model) && !view.hideUnconnected(s,model))
                       {createNode(atom); break;}
            } else if (view.nodeVisible(atom.getType(),model) && !view.hideUnconnected(atom.getType(),model)) {
                createNode(atom);
            }
        }
        for (AlloyRelation rel:model.getRelations())
            if (view.attribute(rel,model))
                edgesAsAttribute(rel);
        Set<Set<DotNode>> rank=new LinkedHashSet<Set<DotNode>>();
        rankTypes(rank);
        rankSets(rank);
        rankEdges(rank);
        graph=new DotGraph(view.getFontSize(), view.getOrientation(),
                view.getNodePalette(), view.getEdgePalette(), rels, magicColor, nodes, edges, rank, attribs);
    }

    /**
     * Return the node for a specific AlloyAtom (create it if it doesn't exist yet).
     * @return null if the atom is explicitly marked as "Don't Show".
     */
    private DotNode createNode(final AlloyAtom atom) {
        DotNode node=atom2node.get(atom);
        if (node!=null) return node;
        if (priv(atom.getType().getName()) || !view.nodeVisible(atom, instance)) return null;
        // Make the node
        DotColor color = view.nodeColor(atom, instance);
        DotStyle style = view.nodeStyle(atom, instance);
        DotShape shape = view.shape(atom, instance);
        String label = atomname(atom,false);
        node = new DotNode(atom, nodes.size(), label, shape, color, style);
        // Get the label based on the sets and relations
        String setsLabel="";
        boolean showLabelByDefault = view.showAsLabel(null);
        for (AlloySet set: instance.atom2sets(atom)) {
            String x=view.label(set); if (x.length()==0) continue;
            Boolean showLabel=view.showAsLabel(set);
            if ((showLabel==null && showLabelByDefault) || (showLabel!=null && showLabel.booleanValue()))
                setsLabel += ((setsLabel.length()>0?", ":"")+x);
        }
        if (setsLabel.length()>0) {
            Set<String> list=attribs.get(node);
            if (list==null) attribs.put(node, list=new TreeSet<String>());
            list.add("("+setsLabel+")");
        }
        nodes.put(node,atom);
        atom2node.put(atom,node);
        return node;
    }

    /** Create an edge for a given tuple from a relation (if neither start nor end node is explicitly invisible) */
    private boolean createEdge(AlloyRelation rel, AlloyTuple tuple, boolean bidirectional, Color magicColor) {
        // This edge represents a given tuple from a given relation.
        //
        // If the tuple's arity==2, then the label is simply the label of the relation.
        //
        // If the tuple's arity>2, then we append the node labels for all the intermediate nodes.
        // eg. Say a given tuple is (A,B,C,D) from the relation R.
        // An edge will be drawn from A to D, with the label "R [B, C]"
        if (priv(tuple.getStart().getType().getName()) || !view.nodeVisible(tuple.getStart(), instance)) return false;
        if (priv(tuple.getEnd().getType().getName()) || !view.nodeVisible(tuple.getEnd(), instance)) return false;
        DotNode start=createNode(tuple.getStart()), end=createNode(tuple.getEnd());
        if (start==null || end==null) return false;
        boolean layoutBack=view.layoutBack(rel,model);
        String label=view.label(rel);
        if (tuple.getArity() > 2) {
            StringBuilder moreLabel = new StringBuilder();
            List<AlloyAtom> atoms=tuple.getAtoms();
            for (int i=1; i<atoms.size()-1; i++) {
                if (i>1) moreLabel.append(", ");
                moreLabel.append(atomname(atoms.get(i),false));
            }
            if (label.length()==0) { /* label=moreLabel.toString(); */ }
            else { label=label+(" ["+moreLabel+"]"); }
        }
        DotDirection dir = bidirectional ? DotDirection.BOTH : (layoutBack ? DotDirection.BACK:DotDirection.FORWARD);
        DotEdge e=new DotEdge(tuple, edges.size(), (layoutBack?end:start), (layoutBack?start:end), label,
                view.edgeStyle(rel,model), view.edgeColor(rel,model), magicColor,
                dir, view.weight(rel), view.constraint(rel,model), rel);
        edges.put(e, tuple);
        return true;
    }

    /** Create edges for every visible tuple in the given relation. */
    private int edgesAsArcs(AlloyRelation rel, Color magicColor) {
        if (priv(rel.getName())) return 0;
        int count = 0;
        if (!view.mergeArrows(rel,model)) {
            // If we're not merging bidirectional arrows, simply create an edge for each tuple.
            for (AlloyTuple tuple: instance.relation2tuples(rel)) if (createEdge(rel, tuple, false, magicColor)) count++;
            return count;
        }
        // Otherwise, find bidirectional arrows and only create one edge for each pair.
        Set<AlloyTuple> tuples = instance.relation2tuples(rel);
        Set<AlloyTuple> ignore = new LinkedHashSet<AlloyTuple>();
        for (AlloyTuple tuple: tuples) {
            if (!ignore.contains(tuple)) {
                AlloyTuple reverse = tuple.getArity()>2 ? null : tuple.reversed();
                // If the reverse tuple is in the same relation, and it is not a self-edge, then draw it as a <-> arrow.
                if (reverse!=null && tuples.contains(reverse) && !reverse.equals(tuple)) {
                    ignore.add(reverse);
                    if (createEdge(rel,tuple,true,magicColor)) count++;
                } else {
                    if (createEdge(rel,tuple,false,magicColor)) count++;
                }
            }
        }
        return count;
    }

    /** Attach tuple values as attributes to existing nodes. */
    private void edgesAsAttribute(AlloyRelation rel) {
        // If this relation wants to be shown as an attribute,
        // then generate the annotations and attach them to each tuple's starting node.
        // Eg.
        //   If (A,B) and (A,C) are both in the relation F,
        //   then the A node would have a line that says "F: B, C"
        // Eg.
        //   If (A,B,C) and (A,D,E) are both in the relation F,
        //   then the A node would have a line that says "F: B->C, D->E"
        // Eg.
        //   If (A,B,C) and (A,D,E) are both in the relation F, and B belongs to sets SET1 and SET2,
        //   and SET1's "show in relational attribute" is on,
        //   and SET2's "show in relational attribute" is on,
        //   then the A node would have a line that says "F: B (SET1, SET2)->C, D->E"
        //
        if (priv(rel.getName())) return;
        Map<DotNode,String> map = new LinkedHashMap<DotNode,String>();
        for (AlloyTuple tuple: instance.relation2tuples(rel)) {
            DotNode start=atom2node.get(tuple.getStart());
            if (start==null) continue; // null means the node won't be shown, so we can't show any attributes
            String attr="";
            List<AlloyAtom> atoms=tuple.getAtoms();
            for (int i=1; i<atoms.size(); i++) {
                if (i>1) attr+="->";
                attr+=atomname(atoms.get(i),true);
            }
            if (attr.length()==0) continue;
            String oldattr=map.get(start);
            if (oldattr!=null && oldattr.length()>0) attr=oldattr+", "+attr;
            if (attr.length()>0) map.put(start,attr);
        }
        for (Map.Entry<DotNode,String> e:map.entrySet()) {
            DotNode node=e.getKey();
            Set<String> list=attribs.get(node);
            if (list==null) attribs.put(node, list=new TreeSet<String>());
            String attr=e.getValue();
            if (view.label(rel).length()>0) attr=view.label(rel)+": "+attr;
            list.add(attr);
        }
    }

    /** Generate a set of sets of Nodes that should be ranked together based on their type. */
    private void rankTypes(Set<Set<DotNode>> retSet) {
        for(AlloyType type: instance.model.getTypes()) if (view.nodeSameRank(type,model)) {
            Set<DotNode> nodes = new LinkedHashSet<DotNode>();
            for (AlloyAtom atom: instance.type2atoms(type)) {
                DotNode node=atom2node.get(atom);
                if (node!=null) nodes.add(node);
            }
            if (nodes.size()>1) retSet.add(nodes);
        }
    }

    /** Generate a set of sets of Nodes that should be ranked together based on their set. */
    private void rankSets(Set<Set<DotNode>> retSet) {
        for (AlloySet set: instance.model.getSets()) if (view.nodeSameRank(set,model)) {
            Set<DotNode> nodes = new LinkedHashSet<DotNode>();
            for (AlloyAtom atom: instance.set2atoms(set)) {
                DotNode node=atom2node.get(atom);
                if (node!=null) nodes.add(node);
            }
            if (nodes.size()>1) retSet.add(nodes);
        }
    }

    /** Generate a set of sets of Nodes that should be ranked together based on the Edges they are connected by. */
    private void rankEdges(Set<Set<DotNode>> retSet) {
        for (AlloyRelation rel: instance.model.getRelations()) if (view.edgeSameRank(rel,model)) {
            for (AlloyTuple tuple: instance.relation2tuples(rel)) {
                DotNode from=atom2node.get(tuple.getStart());
                DotNode to=atom2node.get(tuple.getEnd());
                if (from!=null && to!=null && from!=to) retSet.add(new LinkedHashSet<DotNode>(Util.asList(from,to)));
            }
        }
    }

    /**
     * Return the label for an atom.
     * @param atom - the atom
     * @param showSets - whether the label should also show the sets that this atom belongs to
     *
     * <p> eg. If atom A is the 3rd atom in type T, and T's label is "Person",
     *      then the return value would be "Person3".
     *
     * <p> eg. If atom A is the only atom in type T, and T's label is "Person",
     *      then the return value would be "Person".
     *
     * <p> eg. If atom A is the 3rd atom in type T, and T's label is "Person",
     *      and T belongs to the sets Set1, Set2, and Set3.
     *      However, only Set1 and Set2 have "show in relational attribute == on",
     *      then the return value would be "Person (Set1, Set2)".
     */
    private String atomname(AlloyAtom atom, boolean showSets) {
        String label=atom.getVizName(view, view.number(atom.getType(),model));
        if (!showSets) return label;
        String attr="";
        boolean showInAttrByDefault = view.showAsAttr(null);
        for (AlloySet set: instance.atom2sets(atom)) {
            String x=view.label(set); if (x.length()==0) continue;
            Boolean showAsAttr=view.showAsAttr(set);
            if ((showAsAttr==null && showInAttrByDefault) || (showAsAttr!=null && showAsAttr))
                attr += ((attr.length()>0?", ":"")+x);
        }
        if (label.length()==0) return (attr.length()>0) ? ("("+attr+")") : "";
        return (attr.length()>0) ? (label+" ("+attr+")") : label;
    }
}
