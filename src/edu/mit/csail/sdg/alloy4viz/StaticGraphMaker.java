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

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * This utility class generates a graph for a particular index of the projection).
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class StaticGraphMaker {

    /** The theme customization. */
    private final VizState view;

    /** The projected instance for the graph currently being generated. */
    private final AlloyInstance instance;

    /** The projected model for the graph currently being generated. */
    private final AlloyModel model;

    /** The set of edges for the graph currently being generated. */
    private final Set<DotEdge> edges=new LinkedHashSet<DotEdge>();

    /** The set of nodes for the graph currently being generated. */
    private final Set<DotNode> nodes=new LinkedHashSet<DotNode>();

    /** This maps each atom to the node representing it; if an atom doesn't have a node, it won't be in the map. */
    private final Map<AlloyAtom,DotNode> atom2node=new LinkedHashMap<AlloyAtom,DotNode>();

    /** This stores a set of additional labels we want to add to an existing node. */
    private final Map<DotNode,Set<String>> attribs=new LinkedHashMap<DotNode,Set<String>>();

    /** This stores the resulting graph. */
    private final DotGraph graph;

    /** Produces a single Graph from the given Instance and View and choice of Projection */
    public static DotGraph produceGraph(AlloyInstance instance, VizState view, AlloyProjection proj) {
        // This allows us to work on our private copy of VizState without worrying about locking and blocking
        // other threads (eg. GUI threads) from displaying stuff on screen, for example.
        view=new VizState(view);
        if (proj==null) proj=new AlloyProjection();
        // Now, generate the cartoon.
        StaticGraphMaker answer = new StaticGraphMaker(instance,view,proj);
        return answer.graph;
    }

    /** The constructor takes an Instance and a View, then insert the generate graph(s) into a blank cartoon. */
    private StaticGraphMaker (AlloyInstance originalInstance, VizState view, AlloyProjection proj) {
        this.view = view;
        instance = StaticProjector.project(originalInstance,proj);
        model = instance.model;
        for (AlloyRelation rel:model.getRelations())
            if (view.edgeVisible(rel,model))
                edgesAsArcs(rel);
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
                view.getNodePalette(), view.getEdgePalette(), nodes, edges, rank, attribs);
    }

    /**
     * Return the node for a specific AlloyAtom (create it if it doesn't exist yet).
     * @return null if the atom is explicitly marked as "Don't Show".
     */
    private DotNode createNode(final AlloyAtom atom) {
        DotNode node=atom2node.get(atom);
        if (node!=null) return node;
        if (!view.nodeVisible(atom, instance)) return null;
        // Make the node
        DotColor color = view.nodeColor(atom, instance);
        DotStyle style = view.nodeStyle(atom, instance);
        DotShape shape = view.shape(atom, instance);
        String label = atomname(atom,false);
        node = new DotNode(nodes.size(), label, shape, color, style);
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
        nodes.add(node);
        atom2node.put(atom,node);
        return node;
    }

    /** Create an edge for a given tuple from a relation (if neither start nor end node is explicitly invisible) */
    private void createEdge(AlloyRelation rel, AlloyTuple tuple, boolean bidirectional) {
        // This edge represents a given tuple from a given relation.
        //
        // If the tuple's arity==2, then the label is simply the label of the relation.
        //
        // If the tuple's arity>2, then we append the node labels for all the intermediate nodes.
        // eg. Say a given tuple is (A,B,C,D) from the relation R.
        // An edge will be drawn from A to D, with the label "R [B, C]"
        if (!view.nodeVisible(tuple.getStart(), instance)) return;
        if (!view.nodeVisible(tuple.getEnd(), instance)) return;
        DotNode start=createNode(tuple.getStart()), end=createNode(tuple.getEnd());
        if (start==null || end==null) return;
        boolean layoutBack=view.layoutBack(rel,model);
        String label=view.label(rel);
        if (tuple.getArity() > 2) {
            StringBuilder moreLabel = new StringBuilder();
            List<AlloyAtom> atoms=tuple.getAtoms();
            for (int i=1; i<atoms.size()-1; i++) {
                if (i>1) moreLabel.append(", ");
                moreLabel.append(atomname(atoms.get(i),false));
            }
            if (label.length()==0) label=moreLabel.toString(); else label=label+(" ["+moreLabel+"]");
        }
        DotDirection dir = bidirectional ? DotDirection.BOTH : (layoutBack ? DotDirection.BACK:DotDirection.FORWARD);
        DotEdge e=new DotEdge(edges.size(), (layoutBack?end:start), (layoutBack?start:end), label,
                view.edgeStyle(rel,model), view.edgeColor(rel,model), dir, view.weight(rel), view.constraint(rel,model));
        edges.add(e);
    }

    /** Create edges for every visible tuple in the given relation. */
    private void edgesAsArcs(AlloyRelation rel) {
        if (!view.mergeArrows(rel,model)) {
            // If we're not merging bidirectional arrows, simply create an edge for each tuple.
            for (AlloyTuple tuple: instance.relation2tuples(rel)) createEdge(rel, tuple, false);
            return;
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
                    createEdge(rel,tuple,true);
                } else {
                    createEdge(rel,tuple,false);
                }
            }
        }
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
