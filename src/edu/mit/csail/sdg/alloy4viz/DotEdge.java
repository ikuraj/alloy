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

//import java.util.Map;
//import dot.Dot;
//import dot.Dot.Agedge_t_;
//import dot.Dot.Agnode_t_;
//import dot.Dot.Agraph_t_;

/**
 * Immutable; this represents an edge to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class DotEdge {

    /** The unique id for this edge. */
    private final int id;

    /** The label to show. */
    private final String label;

    /** The line style. */
    private final DotStyle style;

    /** The color. */
    private final DotColor color;

    /** The starting node. */
    public final DotNode from;

    /** The ending node. */
    public final DotNode to;

    /** The direction of the arrow. */
    private final DotDirection dir;

    /** The weight of this edge. */
    private final int weight;

    /** Whether this edge should constrain the graph layout or not. */
    private final boolean constraint;

    /**
     * Constructs an edge.
     * @param id - a unique id for this edge
     * @param from - the origin node
     * @param to - the destination node
     * @param label - the textual label of the edge.  Can contain newlines
     * @param style - the style of the edge
     * @param color - the color of the edge
     * @param dir - the direction of the arrow of the edge
     * @param weight - the weight of the edge (edges with higher weights will tend to be shorter and straighter)
     * @param constraint - whether this edge should constrain the graph layout or not
     */
    public DotEdge(int id, DotNode from, DotNode to, String label,
            DotStyle style, DotColor color, DotDirection dir,
            int weight, boolean constraint) {
        this.id = id;
        this.from = from;
        this.to = to;
        this.label = label;
        this.style = style;
        this.color = color;
        this.dir = dir;
        this.weight = weight;
        this.constraint = constraint;
    }

//    public Agedge_t_ write(Dot dot, Agraph_t_ dotgraph, Map<Integer,Agnode_t_> dotmap,
//            int iLabel, int iColor, int iFontColor, int iStyle, int iDir, int iWeight, int iConstraint,
//            DotPalette pal) {
//        Agnode_t_ a=dotmap.get(from.getID()), b=dotmap.get(to.getID());
//        if (a==null) dotmap.put(from.getID(), a=dot.agnode(dotgraph, Dot.C("N"+from.getID())));
//        if (b==null) dotmap.put(to.getID(), b=dot.agnode(dotgraph, Dot.C("N"+to.getID())));
//        Agedge_t_ x=dot.agedge(dotgraph,a,b);
//        //System.err.printf("agedge(g,N%d,N%d);\n",from.getID(),to.getID());
//        dot.agxset(x, iColor, Dot.C(color.getDotText(pal)));
//        dot.agxset(x, iFontColor, Dot.C(color.getDotText(pal)));
//        dot.agxset(x, iStyle, Dot.C(style.getDotText(pal)));
//        dot.agxset(x, iLabel, Dot.C(label));
//        dot.agxset(x, iDir, Dot.C(dir.getDotText(pal)));
//        dot.agxset(x, iConstraint, Dot.C(constraint?"true":"false"));
//        if (weight!=0) dot.agxset(x, iWeight, Dot.C(""+weight));
//        return x;
//    }

    /** Write this edge (using the given Edge palette) into a StringBuilder as if writing to a DOT file. */
    public void write(StringBuilder out, DotPalette pal) {
        out.append("\"N" + from.getID() + "\"");
        out.append(" -> ");
        out.append("\"N" + to.getID() + "\"");
        out.append(" [");
        out.append("color = \"" + color.getDotText(pal) + "\"");
        out.append(", fontcolor = \"" + color.getDotText(pal) + "\"");
        out.append(", style = \"" + style.getDotText(pal) + "\"");
        out.append(", label = \"" + esc(label) + "\"");
        out.append(", dir = \"" + dir.getDotText(pal) + "\"");
        out.append(", weight = \"" + weight + "\"");
        out.append(", constraint = \"" + (constraint?"true":"false") + "\"");
        out.append("]\n");
    }

    /** This encodes the " character so that it will be safe to use as an attribute value in a Dot file. */
    private static String esc(String name) {
        if (name.indexOf('\"') < 0) return name;
        StringBuilder out = new StringBuilder();
        for(int i=0; i<name.length(); i++) {
            char c=name.charAt(i);
            if (c=='\"') out.append('\\');
            out.append(c);
        }
        return out.toString();
    }

    /** Returns a human-readable textual output for debugging purporses. */
    @Override public String toString() {
        return "Edge["+from+"->"+to+"]";
    }

    /** Two edges are equal if they have the same id. */
    @Override public boolean equals(Object o) {
        if (!(o instanceof DotEdge)) return false;
        return id==(((DotEdge)o).id);
    }

    /** Computes a hash code based on the id. */
    @Override public int hashCode() { return id; }
}
