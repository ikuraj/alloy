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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
//import dot.Dot;
//import dot.Dot.Agedge_t_;
//import dot.Dot.Agnode_t_;
//import dot.Dot.Agraph_t_;
//import dot.Dot.Agsym_t_;
//import dot.Dot.point;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Subprocess;

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

//    /** This encodes the " character so that it will be safe to use as an attribute value in a Dot file. */
//    private static String esc(String name) {
//        if (name.indexOf('\"') < 0) return name;
//        StringBuilder out = new StringBuilder();
//        for(int i=0; i<name.length(); i++) {
//            char c=name.charAt(i);
//            if (c=='\"') out.append('\\');
//            out.append(c);
//        }
//        return out.toString();
//    }

    /** Generate the entire content of the DOT file. */
//    public String write2() {
//        final Dot dot = new Dot();
//        final Agraph_t_ g = dot.agopen(Dot.C("graph"));
//        dot.graph_init(g);
//        Agsym_t_ ap;
//        ap=dot.agfindattr(g, Dot.C("rank"));              if (ap==null) ap=dot.agraphattr(g,Dot.C("rank") ,Dot.C(""));     final int g_rank=ap.index;
//        ap=dot.agfindattr(g, Dot.C("rankdir"));           if (ap==null) ap=dot.agraphattr(g,Dot.C("rankdir") ,Dot.C(""));  final int g_rankdir=ap.index;
//        ap=dot.agfindattr(g.proto.n, Dot.C("style"));     if (ap==null) ap=dot.agnodeattr(g,Dot.C("style"),Dot.C(""));     final int n_style=ap.index;
//        ap=dot.agfindattr(g.proto.n, Dot.C("shape"));     if (ap==null) ap=dot.agnodeattr(g,Dot.C("shape"),Dot.C(""));     final int n_shape=ap.index;
//        ap=dot.agfindattr(g.proto.n, Dot.C("label"));     if (ap==null) ap=dot.agnodeattr(g,Dot.C("label"),Dot.C(""));     final int n_label=ap.index;
//        ap=dot.agfindattr(g.proto.n, Dot.C("color"));     if (ap==null) ap=dot.agnodeattr(g,Dot.C("color"),Dot.C(""));     final int n_color=ap.index;
//        ap=dot.agfindattr(g.proto.n, Dot.C("fontcolor")); if (ap==null) ap=dot.agnodeattr(g,Dot.C("fontcolor"),Dot.C("")); final int n_fontcolor=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("label"));     if (ap==null) ap=dot.agedgeattr(g,Dot.C("label"),Dot.C(""));     final int e_label=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("color"));     if (ap==null) ap=dot.agedgeattr(g,Dot.C("color"),Dot.C(""));     final int e_color=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("fontcolor")); if (ap==null) ap=dot.agedgeattr(g,Dot.C("fontcolor"),Dot.C("")); final int e_fontcolor=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("style"));     if (ap==null) ap=dot.agedgeattr(g,Dot.C("style"),Dot.C(""));     final int e_style=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("dir"));       if (ap==null) ap=dot.agedgeattr(g,Dot.C("dir"),Dot.C(""));       final int e_dir=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("weight"));    if (ap==null) ap=dot.agedgeattr(g,Dot.C("weight"),Dot.C(""));    final int e_weight=ap.index;
//        ap=dot.agfindattr(g.proto.e, Dot.C("constraint"));if (ap==null) ap=dot.agedgeattr(g,Dot.C("constraint"),Dot.C(""));final int e_constraint=ap.index;
//        if (nodes.size()==0) {
//            Agnode_t_ x = dot.agnode(g, Dot.C("MESSAGE"));
//            dot.agxset(x, n_color, Dot.C("white"));
//            dot.agxset(x, n_fontcolor, Dot.C("black"));
//            dot.agxset(x, n_shape, Dot.C("plaintext"));
//            dot.agxset(x, n_style, Dot.C("bold"));
//            dot.agxset(x, n_label, Dot.C("Due to your theme settings, every atom is hidden. Please click Theme and adjust your settings."));
//        } else {
//            Map<Integer,Agnode_t_> map=new LinkedHashMap<Integer,Agnode_t_>();
//            dot.agxset(g, g_rankdir, Dot.C(orientation.getDotText(null)));
//            for (DotNode node:nodes) node.write(dot, g, map, n_label, n_shape, n_style, n_color, n_fontcolor, attrs.get(node), nodePalette);
//            for (DotEdge edge:edges) edge.write(dot, g, map, e_label, e_color, e_fontcolor, e_style, e_dir, e_weight, e_constraint, edgePalette);
//            int rank=0;
//            for (Set<DotNode> set:ranks) if (set.size()>1) {
//                //System.err.printf("gg=agsubg(g,\"R%d\");\n",rank);
//                Agraph_t_ gg = dot.agsubg(g, Dot.C("R"+rank)); rank++;
//                dot.agxset(gg, g_rank, Dot.C("same"));
//                //System.err.printf("agxset(gg, g_rank, \"same\");\n");
//                for (DotNode node:set) {
//                    //System.err.printf("agnode(gg,\"N%d\");\n", node.getID());
//                    dot.agnode(gg, Dot.C("N"+node.getID()));
//                }
//            }
//        }
//        dot.dot_layout(g);
//        /*
//        JFrame zf=new JFrame();
//        JPanel zp=new JPanel() {
//            private static final long serialVersionUID = 1L;
//            @Override public void paint(Graphics gx) {
//                int lx=0, ly=0, lz=0;
//                for (Agnode_t_ n = dot.agfstnode(g); n!=null; n = dot.agnxtnode(g,n)) {
//                    gx.drawString(Dot.D(dot.agxget(n,n_label)), n.u.coord.x, 500-n.u.coord.y);
//                    //out.append("\" color=\"" + Dot.D(dot.agxget(n,n_color)));
//                    //out.append("\" fontcolor=\"" + Dot.D(dot.agxget(n,n_fontcolor)));
//                    //out.append("\" shape=\"" + Dot.D(dot.agxget(n,n_shape)));
//                    //out.append("\" style=\"" + Dot.D(dot.agxget(n,n_style)));
//                    //out.append("\" height=\"" + n.u.ht/72.0 + "\" width=\"" + (n.u.lw+n.u.rw)/72.0 + "\"]\n");
//                    for (Agedge_t_ e = dot.agfstout(g,n); e!=null; e = dot.agnxtout(g,e)) {
//                        //out.append("\"" + Dot.D(e.head.name) + "\" -> \"" + Dot.D(e.tail.name) + "\" [pos=\"");
//                        if (e.u.spl != null) {
//                          for (int i = 0; i < e.u.spl.size; i++) {
//                            lz=0;
//                            if (e.u.spl.list.get(i).sflag != 0 )
//                                {lx=e.u.spl.list.get(i).sp.x; ly=e.u.spl.list.get(i).sp.y; lz=1;}
//                            for (int j = 0; j < e.u.spl.list.get(i).size; j++) {
//                                point pt = e.u.spl.list.get(i).list.get(j);
//                                if (lz!=0) { if (j==0) gx.setColor(Color.RED); else gx.setColor(Color.BLACK); gx.drawLine(lx, 500-ly, pt.x, 500-pt.y); }
//                                lx=pt.x; ly=pt.y; lz=1;
//                            }
//                            if (e.u.spl.list.get(i).eflag != 0 && lz!=0)
//                                { gx.setColor(Color.BLUE); gx.drawLine(lx, 500-ly, e.u.spl.list.get(i).ep.x, 500-e.u.spl.list.get(i).ep.y); }
//                          }
//                        }
//                        if (e.u.label!=null) {
//                            //point pt = e.u.label.p;
//                            //out.append("\" lp=\"").append(pt.x).append(',').append(pt.y);
//                        }
//                        //out.append("\" label=\"" + esc(Dot.D(dot.agxget(e,e_label))));
//                        //out.append("\" color=\"" + Dot.D(dot.agxget(e,e_color)));
//                        //out.append("\" fontcolor=\"" + Dot.D(dot.agxget(e,e_fontcolor)));
//                        //out.append("\" style=\"" + Dot.D(dot.agxget(e,e_style)));
//                        //out.append("\" dir=\"" + Dot.D(dot.agxget(e,e_dir)));
//                    }
//                }
//            }
//        };
//        zf.getContentPane().setLayout(new BorderLayout());
//        zf.getContentPane().add(zp, BorderLayout.CENTER);
//        zf.pack();
//        zf.setLocation(10,10);
//        zf.setSize(300,300);
//        zf.setVisible(true);
//        */
//        // Now, generating the Dot text
//        StringBuilder out=new StringBuilder("digraph \"graph\" {\ngraph [fontsize=").append(fontSize).append(", rankdir=");
//        out.append(orientation.getDotText(null)).append("]\n");
//        out.append("node [fontsize=").append(fontSize).append("]\n");
//        out.append("edge [fontsize=").append(fontSize).append("]\n");
//        for (Agnode_t_ n = dot.agfstnode(g); n!=null; n = dot.agnxtnode(g,n)) {
//            out.append('\"').append(Dot.D(n.name));
//            out.append("\" [pos=\"").append(n.u.coord.x).append(',').append(n.u.coord.y);
//            out.append("\" label=\"" + esc(Dot.D(dot.agxget(n,n_label))));
//            out.append("\" color=\"" + Dot.D(dot.agxget(n,n_color)));
//            out.append("\" fontcolor=\"" + Dot.D(dot.agxget(n,n_fontcolor)));
//            out.append("\" shape=\"" + Dot.D(dot.agxget(n,n_shape)));
//            out.append("\" style=\"" + Dot.D(dot.agxget(n,n_style)));
//            out.append("\" height=\"" + n.u.ht/72.0 + "\" width=\"" + (n.u.lw+n.u.rw)/72.0 + "\"]\n");
//            for (Agedge_t_ e = dot.agfstout(g,n); e!=null; e = dot.agnxtout(g,e)) {
//                out.append("\"" + Dot.D(e.head.name) + "\" -> \"" + Dot.D(e.tail.name) + "\" [pos=\"");
//                if (e.u.spl != null) {
//                  for (int i = 0; i < e.u.spl.size; i++) {
//                    if (i > 0) out.append(';');
//                    if (e.u.spl.list.get(i).sflag != 0 )
//                        out.append("s," + e.u.spl.list.get(i).sp.x + "," + e.u.spl.list.get(i).sp.y + " ");
//                    if (e.u.spl.list.get(i).eflag != 0)
//                        out.append("e," + e.u.spl.list.get(i).ep.x + "," + e.u.spl.list.get(i).ep.y + " ");
//                    for (int j = 0; j < e.u.spl.list.get(i).size; j++) {
//                        if (j > 0) out.append(' ');
//                        point pt = e.u.spl.list.get(i).list.get(j);
//                        out.append(pt.x).append(',').append(pt.y);
//                    }
//                  }
//                }
//                if (e.u.label!=null) {
//                    point pt = e.u.label.p;
//                    out.append("\" lp=\"").append(pt.x).append(',').append(pt.y);
//                }
//                out.append("\" label=\"" + esc(Dot.D(dot.agxget(e,e_label))));
//                out.append("\" color=\"" + Dot.D(dot.agxget(e,e_color)));
//                out.append("\" fontcolor=\"" + Dot.D(dot.agxget(e,e_fontcolor)));
//                out.append("\" style=\"" + Dot.D(dot.agxget(e,e_style)));
//                out.append("\" dir=\"" + Dot.D(dot.agxget(e,e_dir)));
//                out.append("\" weight=\"" + Dot.D(dot.agxget(e,e_weight)));
//                out.append("\" constraint=\"" + Dot.D(dot.agxget(e,e_constraint)));
//                out.append("\"]\n");
//            }
//        }
//        out.append("}\n");
//        return out.toString();
//    }

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
    public Pair<String,JPanel> visualize() throws ErrorFatal {
        final String dot=write();
        String result="";
        boolean nomore=false;
        for(int i=0; !nomore ; i++) {
            String dotBinary = System.getProperty("alloy.dotbin"+i);
            if (dotBinary==null || dotBinary.length()==0) { dotBinary="dot"; nomore=true; }
            String[] cmdArgs1={dotBinary};
            Subprocess process;
            try {process=new Subprocess(10000,cmdArgs1,-1,dot);} catch (Exception x) {continue;}
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

//    /**
//     * Converts a graph into a Dot string and a displayable GrappaPanel.
//     * If there's an error, it will throw an exception.
//     * <p>
//     * It works by calling the "dot" external program to perform the node layouts.
//     * It will first search the default execution path; if that fails, it'll try
//     * System.getProperty("alloy.dotbin0"), System.getProperty("alloy.dotbin1")...
//     * and try each value. It stops the search when one of the value is null or the empty String.
//     */
//    public Pair<String,JPanel> visualize2() throws ErrorFatal {
//        final String result=write2();
//        final String dot=result+"\n\n---\n\n"+write();
//        Parser program=null;
//        try {
//            program=new Parser(new StringReader(result));
//            program.parse();
//        } catch (Throwable e) {
//            throw new ErrorFatal("Could not parse the layout generated by dot ("
//            +e.toString()+")\n\nPlease email this error message to alloy@mit.edu\nThank you.\n\n"
//            +dot
//            );
//        }
//        Graph grappa = program.getGraph();
//        grappa.setEditable(false);
//        grappa.setErrorWriter(new PrintWriter(new StringWriter(), false));
//        grappa.setGrappaAttribute(GrappaConstants.GRAPPA_BACKGROUND_COLOR_ATTR, "white");
//        GrappaPanel gp = new GrappaPanel(grappa);
//        gp.setBackground(Color.WHITE);
//        gp.setBorder(new EmptyBorder(0,0,0,0));
//        gp.setScaleToFit(false);
//        gp.annotation=dot;
//        gp.addGrappaListener(new GrappaAdapter() {
//            @Override public final String grappaTip(Subgraph subg, Element elem,
//                    GrappaPoint pt, int modifiers, GrappaPanel panel) {
//                return "";
//            }
//        });
//        return new Pair<String,JPanel>(dot,gp);
//    }

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
