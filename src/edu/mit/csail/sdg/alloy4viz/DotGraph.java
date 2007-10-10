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

import att_grappa_20060427.Element;
import att_grappa_20060427.Graph;
import att_grappa_20060427.GrappaAdapter;
import att_grappa_20060427.GrappaConstants;
import att_grappa_20060427.GrappaPanel;
import att_grappa_20060427.GrappaPoint;
import att_grappa_20060427.Parser;
import att_grappa_20060427.Subgraph;
import java.awt.Color;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Subprocess;
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
    private final Set<DotNode> nodes;

    /** The set of edges in the graph. */
    private final Set<DotEdge> edges;

    /** The set of set of nodes that should be aligned together. */
    private final Set<Set<DotNode>> ranks;

    /** The set of additional attributes to be printed for each node. */
    private final Map<DotNode,Set<String>> attrs;

    /** Creates an empty Graph. */
    public DotGraph() {
        orientation = DotOrientation.getDefault();
        nodePalette = edgePalette = DotPalette.getDefault();
        fontSize = 12;
        nodes = new LinkedHashSet<DotNode>();
        edges = new LinkedHashSet<DotEdge>();
        ranks = new LinkedHashSet<Set<DotNode>>();
        attrs = new LinkedHashMap<DotNode,Set<String>>();
    }

    /**
     * Creates a new Graph object with specified nodes, edges, attributes, and ranking.
     * @param fontSize - the graph's font size
     * @param orientation - the graph orientation
     * @param nodePalette - the palette for nodes in the graph
     * @param edgePalette - the palette for edges in the graph
     * @param nodes - the set of nodes in this graph
     * @param edges - the set of edges in this graph
     * @param ranks - the set of set of nodes that should be aligned on the same rank in the graph
     * @param attrs - this maps nodes to a set of additional attributes to be printed
     */
    public DotGraph(int fontSize, DotOrientation orientation, DotPalette nodePalette, DotPalette edgePalette,
            Set<DotNode> nodes, Set<DotEdge> edges, Set<Set<DotNode>> ranks, Map<DotNode,Set<String>> attrs) {
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
        this.nodes = new LinkedHashSet<DotNode>(nodes);
        this.edges = new LinkedHashSet<DotEdge>(edges);
    }

    /** Generate the entire content of the DOT file. */
    public VizGraph write2() {
        // TODO: sb.append("rankdir=" + orientation.getDotText(null) + ";\n");
        VizGraph graph=new VizGraph();
        if (nodes.size()==0) {
            new VizNode(graph, "Due to your theme settings, every atom is hidden.", "Please click Theme and adjust your settings.");
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
        String fontname="Lucida Grande";
        StringBuilder sb = new StringBuilder();
        sb.append("digraph \"graph\" {\n");
        sb.append("graph [fontname=\"" + fontname + "\", fontsize=" + fontSize + "]\n");
        sb.append("node [fontname=\"" + fontname + "\", fontsize=" + fontSize + "]\n");
        sb.append("edge [fontname=\"" + fontname + "\", fontsize=" + fontSize + "]\n");
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
     * Converts a graph into a Dot string and a displayable GrappaPanel.
     * If there's an error, it will throw an exception.
     * <p>
     * It works by calling the "dot" external program to perform the node layouts.
     * It will first search the default execution path; if that fails, it'll try
     * System.getProperty("alloy.dotbin0"), System.getProperty("alloy.dotbin1")...
     * and try each value. It stops the search when one of the value is null or the empty String.
     */
    public Pair<String,JPanel> visualize2() throws ErrorFatal {
        final String dot=write();
        String result="";
        boolean nomore=false;
        for(int i=0; !nomore ; i++) {
            String dotBinary = System.getProperty("alloy.dotbin"+i);
            if (dotBinary==null || dotBinary.length()==0) { dotBinary="dot"; nomore=true; }
            String[] cmdArgs1={dotBinary};
            Subprocess process;
            try {process=new Subprocess(20000,cmdArgs1,-1,dot);} catch (Exception x) {continue;}
            result=process.getStandardOutput();
            if (result.startsWith("digraph")) break;
            result=process.getStandardOutputAndError();
        }
        if (!result.startsWith("digraph")) throw new ErrorFatal("Could not call the dot program.\n\n"+
            "Please email this error message to alloy@mit.edu\nThank you.\n\n"+result+"\n\n"+dot);

        Parser program=null;
        try {
            program=new Parser(new StringReader(result));
            program.parse();
        } catch (Exception e) {
            throw new ErrorFatal("Could not parse the layout generated by dot ("
            +e.toString()+")\n\nPlease email this error message to alloy@mit.edu\nThank you.\n\n"
            +"\n\n"+dot);
        }
        Graph grappa = program.getGraph();
        grappa.setEditable(false);
        grappa.setErrorWriter(new PrintWriter(new StringWriter(), true));
        grappa.setGrappaAttribute(GrappaConstants.GRAPPA_BACKGROUND_COLOR_ATTR, "white");
        GrappaPanel gp = new GrappaPanel(grappa);
        gp.setBackground(Color.WHITE);
        gp.setBorder(new EmptyBorder(0,0,0,0));
        gp.setScaleToFit(false);
        gp.annotation=dot;
        gp.addGrappaListener(new GrappaAdapter() {
            @Override public final String grappaTip(Subgraph subg, Element elem,
                    GrappaPoint pt, int modifiers, GrappaPanel panel) {
                return "";
            }
        });
        return new Pair<String,JPanel>(dot,gp);
    }

    public Pair<String,JPanel> visualize() throws ErrorFatal {
        final String result=write();
        final VizGraph graph=write2();
        VizViewer gp = new VizViewer(graph, fontSize/12D);
        gp.setBorder(new EmptyBorder(0,0,0,0));
        gp.do_setAnnotation(result);
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
