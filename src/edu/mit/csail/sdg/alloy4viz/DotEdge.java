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
import edu.mit.csail.sdg.alloy4graph.VizEdge;
import edu.mit.csail.sdg.alloy4graph.VizNode;

/**
 * Immutable; this represents an edge to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DotEdge {

    /** a user-provided annotation that will be associated with this edge (can be null) */
    public final Object uuid;

    /** The unique id for this edge. */
    private final int id;

    /** The label to show. */
    private final String label;

    /** The line style. */
    private final DotStyle style;

    /** The color. */
    private final DotColor color;

    /** The magic color, if the user specified that this edge should use magic color. */
    private final Color magicColor;

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

    /** The group that this edge belongs to; edges in the same group will be highlighted together. */
    private final Object group;

    /**
     * Constructs an edge.
     * @param uuid - a user-provided annotation that will be associated with this edge (can be null)
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
    public DotEdge(Object uuid, int id, DotNode from, DotNode to, String label,
            DotStyle style, DotColor color, Color magicColor, DotDirection dir,
            int weight, boolean constraint, Object group) {
        this.uuid = uuid;
        this.id = id;
        this.from = from;
        this.to = to;
        this.label = label;
        this.style = style;
        this.color = color;
        this.magicColor = magicColor;
        this.dir = dir;
        this.weight = weight;
        this.constraint = constraint;
        this.group = (group==null ? this : group);
    }

    /** Write this edge (using the given Edge palette) into a DotGraph object. */
    void write2(VizNode from, VizNode to, DotPalette pal) {
        VizEdge e;
        if (dir==DotDirection.FORWARD) e=new VizEdge(from,to,uuid,label,group).set(false,true);
           else if (dir==DotDirection.BACK) e=new VizEdge(from,to,uuid,label,group).set(true,false);
           else e=new VizEdge(from,to,uuid,label,group).set(true,true);
        if (color==DotColor.MAGIC && magicColor!=null) e.set(magicColor); else e.set(DotColor.name2color(color.getDotText(pal)));
        e.set(style.vizStyle);
        if (constraint) e.set(weight<1 ? 1 : (weight>100 ? 10000 : 100*weight)); else e.set(1);
    }

    /** Write this edge (using the given Edge palette) into a StringBuilder as if writing to a DOT file. */
    void write(StringBuilder out, DotPalette pal) {
        out.append("\"N" + from.getID() + "\"");
        out.append(" -> ");
        out.append("\"N" + to.getID() + "\"");
        out.append(" [");
        out.append("uuid = \"" + (uuid==null ? "" : esc(uuid.toString())) + "\"");
        out.append(", color = \"" + color.getDotText(pal) + "\"");
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
