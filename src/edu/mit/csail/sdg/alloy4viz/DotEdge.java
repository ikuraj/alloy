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

import edu.mit.csail.sdg.alloy4graph.VizEdge;
import edu.mit.csail.sdg.alloy4graph.VizNode;

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

    /** Write this edge (using the given Edge palette) into a StringBuilder as if writing to a DOT file. */
    public void write2(VizNode from, VizNode to, DotPalette pal) {
        VizEdge e;
        if (dir==DotDirection.FORWARD) e=new VizEdge(from,to).set(false,true);
           else if (dir==DotDirection.BACK) e=new VizEdge(from,to).set(true,false);
           else e=new VizEdge(from,to).set(true,true);
        e.set(DotColor.name2color(color.getDotText(pal))).set(style.vizStyle).set(weight<1 ? 1 : (weight>100 ? 100 : weight));
        //TODO out.append(", label = \"" + esc(label) + "\"");
        //TODO out.append(", constraint = \"" + (constraint?"true":"false") + "\"");
    }

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
