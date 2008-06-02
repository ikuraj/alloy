/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
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

import java.awt.Color;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JPanel;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.ConstSet;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4graph.VizGraph;
import edu.mit.csail.sdg.alloy4graph.VizNode;
import edu.mit.csail.sdg.alloy4graph.VizViewer;

/**
 * Immutable; this represents a graph to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
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

    /** The list of relations (and the number of edges in it) in this graph. */
    public final ConstMap<AlloyRelation,Integer> relations;

    /** The list of relations (and the inferred color to use for it) in this graph. */
    public final ConstMap<AlloyRelation,Color> magicColor;

    /** Creates an empty Graph. */
    public DotGraph() {
        orientation = DotOrientation.getDefault();
        nodePalette = edgePalette = DotPalette.getDefault();
        fontSize = 12;
        nodes = ConstSet.make();
        edges = ConstSet.make();
        ranks = new LinkedHashSet<Set<DotNode>>();
        attrs = new LinkedHashMap<DotNode,Set<String>>();
        relations = ConstMap.make();
        magicColor = ConstMap.make();
    }

    /**
     * Creates a new Graph object with specified nodes, edges, attributes, and ranking.
     * @param fontSize - the graph's font size
     * @param orientation - the graph orientation
     * @param nodePalette - the palette for nodes in the graph
     * @param edgePalette - the palette for edges in the graph
     * @param nodes - the map containing all nodes in this graph (and the AlloyAtom each node corresponds to)
     * @param edges - the map containing all edges in this graph (and the AlloyTuple each edge corresponds to)
     * @param attrs - this maps nodes to a set of additional attributes to be printed
     */
    public DotGraph(int fontSize, DotOrientation orientation, DotPalette nodePalette, DotPalette edgePalette,
            Map<AlloyRelation,Integer> rels,
            Map<AlloyRelation,Color> magicColor,
            Map<DotNode,AlloyAtom> nodes, Map<DotEdge,AlloyTuple> edges,
            Map<DotNode,Set<String>> attrs) {
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
        this.relations = ConstMap.make(rels);
        this.magicColor = ConstMap.make(magicColor);
    }

    /** Generate the entire content of the DOT file. */
    public VizGraph write2() {
        // rankdir = orientation.getDotText(null)
        VizGraph graph=new VizGraph(fontSize / 12.0D);
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
            // for (Set<DotNode> set:ranks) if (set.size()>0) { add the constraints that they should be on same rank }
        }
        for(Map.Entry<AlloyRelation,Integer> e:relations.entrySet()) {
            Color c = magicColor.get(e.getKey());
            if (c==null) c = Color.BLACK;
            int n = e.getValue();
            if (n>0) graph.addLegend(e.getKey(), e.getKey().getName()+": "+n, c);
            else graph.addLegend(e.getKey(), e.getKey().getName(), null);
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
        VizViewer gp = new VizViewer(graph);
        gp.setBorder(null);
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
