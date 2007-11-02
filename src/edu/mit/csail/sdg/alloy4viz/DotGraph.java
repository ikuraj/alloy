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

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import edu.mit.csail.sdg.alloy4.ConstSet;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4graph.VizGraph;
import edu.mit.csail.sdg.alloy4graph.VizNode;
import edu.mit.csail.sdg.alloy4graph.VizViewer;

/**
 * Immutable; this represents a graph to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class DotGraph {

    /** The font size. */
    private final int fontSize;

    /** The graph orientation. */
    private final DotOrientation orientation;

    /** The palette for nodes in the graph. */
    private final DotPalette nodePalette;

    /** The palette for edges in the graph. */
    private final DotPalette edgePalette;

    /** The set of nodes in the graph. */
    public final ConstSet<DotNode> nodes;

    /** The set of edges in the graph. */
    public final ConstSet<DotEdge> edges;

    /** The set of set of nodes that should be aligned together. */
    private final Set<Set<DotNode>> ranks;

    /** The set of additional attributes to be printed for each node. */
    private final Map<DotNode,Set<String>> attrs;

    /** Creates an empty Graph. */
    public DotGraph() {
        orientation = DotOrientation.getDefault();
        nodePalette = edgePalette = DotPalette.getDefault();
        fontSize = 12;
        nodes = ConstSet.make();
        edges = ConstSet.make();
        ranks = new LinkedHashSet<Set<DotNode>>();
        attrs = new LinkedHashMap<DotNode,Set<String>>();
    }

    /**
     * Creates a new Graph object with specified nodes, edges, attributes, and ranking.
     * @param fontSize - the graph's font size
     * @param orientation - the graph orientation
     * @param nodePalette - the palette for nodes in the graph
     * @param edgePalette - the palette for edges in the graph
     * @param nodes - the map containing all nodes in this graph (and the AlloyAtom each node corresponds to)
     * @param edges - the map containing all edges in this graph (and the AlloyTuple each edge corresponds to)
     * @param ranks - the set of set of nodes that should be aligned on the same rank in the graph
     * @param attrs - this maps nodes to a set of additional attributes to be printed
     */
    public DotGraph(int fontSize, DotOrientation orientation, DotPalette nodePalette, DotPalette edgePalette,
            Map<DotNode,AlloyAtom> nodes, Map<DotEdge,AlloyTuple> edges,
            Set<Set<DotNode>> ranks, Map<DotNode,Set<String>> attrs) {
        this.fontSize = fontSize;
        this.orientation = orientation;
        this.nodePalette = nodePalette;
        this.edgePalette = edgePalette;
        this.ranks = new LinkedHashSet<Set<DotNode>>();
        for(Set<DotNode> set:ranks)
            this.ranks.add(Collections.unmodifiableSet(new LinkedHashSet<DotNode>(set)));
        this.attrs = new LinkedHashMap<DotNode,Set<String>>();
        for(Map.Entry<DotNode,Set<String>> e:attrs.entrySet())
            this.attrs.put(e.getKey(), Collections.unmodifiableSet(new LinkedHashSet<String>(e.getValue())));
        this.nodes = ConstSet.make(nodes.keySet());
        this.edges = ConstSet.make(edges.keySet());
    }

    /** Generate the entire content of the DOT file. */
    public VizGraph write2() {
        // TODO: sb.append("rankdir=" + orientation.getDotText(null) + ";\n");
        VizGraph graph=new VizGraph();
        if (nodes.size()==0) {
            new VizNode(graph, null, "Due to your theme settings, every atom is hidden.", "Please click Theme and adjust your settings.");
        } else {
            IdentityHashMap<DotNode,VizNode> map=new IdentityHashMap<DotNode,VizNode>();
            for (DotNode node:nodes) {
                VizNode n=node.write2(graph, attrs.get(node), nodePalette);
                map.put(node, n);
            }
            for (DotEdge edge:edges) {
                VizNode a = map.get(edge.from);
                VizNode b = map.get(edge.to);
                if (a!=null && b!=null) edge.write2(a,b,edgePalette);
            }
            // TODO for (Set<DotNode> set:ranks) if (set.size()>0) {
            //     sb.append("{ rank=same;");
            //     for (DotNode node:set) sb.append(" \"N" + node.getID() + "\";");
            //     sb.append(" }\n");
            //}
        }
        return graph;
    }

    /** Generate the entire content of the DOT file. */
    public String write() {
        StringBuilder sb = new StringBuilder();
        sb.append("digraph \"graph\" {\n");
        sb.append("graph [fontsize=" + fontSize + "]\n");
        sb.append("node [fontsize=" + fontSize + "]\n");
        sb.append("edge [fontsize=" + fontSize + "]\n");
        sb.append("rankdir=" + orientation.getDotText(null) + ";\n");
        if (nodes.size()==0) {
            sb.append("A [" +
                    " color=\"white\"" +
                    " fontcolor=\"black\"" +
                    " shape=\"plaintext\"" +
                    " style=\"bold\"" +
                    " label=\"Due to your theme settings, every atom is hidden.\\nPlease click Theme and adjust your settings.\"" +
                    "]"
            );
        } else {
            for (DotEdge edge:edges) edge.write(sb, edgePalette);
            for (DotNode node:nodes) node.write(sb, attrs.get(node), nodePalette);
            for (Set<DotNode> set:ranks) if (set.size()>0) {
                sb.append("{ rank=same;");
                for (DotNode node:set) sb.append(" \"N" + node.getID() + "\";");
                sb.append(" }\n");
            }
        }
        sb.append("}\n");
        return sb.toString();
    }

    /**
     * Converts a graph into a Dot string and a displayable JPanel.
     * If there's an error, it will throw an exception.
     */
    public Pair<String,JPanel> visualize() throws ErrorFatal {
        final String result=write();
        final VizGraph graph=write2();
        VizViewer gp = new VizViewer(graph, fontSize/12D);
        gp.setBorder(new EmptyBorder(0,0,0,0));
        gp.alloySetAnnotation(result);
        return new Pair<String,JPanel>(result,gp);
    }

    /** Two graphs are equal if their sets of nodes, edges, attributes, and rankings are equal. */
    public boolean equals(Object other) {
        if (!(other instanceof DotGraph)) return false;
        if (other==this) return true;
        DotGraph g = (DotGraph)other;
        return nodes.equals(g.nodes) && edges.equals(g.edges) && attrs.equals(g.attrs) && ranks.equals(g.ranks);
    }

    /** Compute a hash code based on the nodes, edges, attributes, and ranks of this graph. */
    public int hashCode() { return 3*nodes.hashCode() + 5*edges.hashCode() + 7*ranks.hashCode() + 11*attrs.hashCode(); }
}
