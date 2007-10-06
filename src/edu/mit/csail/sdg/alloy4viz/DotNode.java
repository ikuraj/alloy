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

import java.util.Set;
import edu.mit.csail.sdg.alloy4graph.DiGraph;
import edu.mit.csail.sdg.alloy4graph.VizNode;

/**
 * Immutable; this represents a node to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class DotNode {

    /** The unique id for this edge. */
    private final int id;

    /** The label to show. */
    private final String label;

    /** The shape. */
    private final DotShape shape;

    /** The color. */
    private final DotColor color;

    /** The line style. */
    private final DotStyle style;

    /** Returns the unique id for this node. */
    public int getID() { return id; }

    /**
     * Constructs a node.
     * @param id - the unique ID for this node
     * @param label - the label to show
     * @param shape - the shape
     * @param color - the color
     * @param style - the line style
     */
    public DotNode(int id, String label, DotShape shape, DotColor color, DotStyle style) {
        this.id = id; this.label = label; this.shape = shape; this.color = color; this.style = style;
    }

    public VizNode write2(DiGraph dotgraph, Set<String> attribs, DotPalette pal) {
        VizNode n = new VizNode(dotgraph, label);
        if (attribs!=null) for (String a:attribs) if (a.length()>0) n.add(a);
        n.set(DotColor.name2color(color.getDotText(pal))).set(shape.vizShape).set(style.vizStyle);
        return n;
    }

    /**
     * Writes the node into a StringBuilder as we would writing to a Dot file
     * @param out - the StringBuilder object that will receive the text
     * @param attribs - a set of additional labels to append to the node (can be null if we don't have any to append)
     * @param pal - the color palette to use
     */
    public void write(StringBuilder out, Set<String> attribs, DotPalette pal) {
        out.append("\"N" + id + "\"");
        out.append(" [");
        out.append("label=\"" + esc(label));
        if (attribs!=null) for (String a:attribs) if (a.length()>0) out.append("\\n"+esc(a));
        out.append("\", color=\"" + color.getDotText(pal) + "\"");
        out.append(", fontcolor = \"" + color.getLabelColorText(pal) + "\"");
        out.append(", shape = \"" + shape.getDotText(pal) + "\"");
        out.append(", style = \"filled, " + style.getDotText(pal) + "\"");
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

    /** Returns a human-readable textual output for debugging purposes. */
    @Override public String toString() { return "Node"+id+"("+label+")"; }

    /** Two nodes are equal if they have the same id. */
    @Override public boolean equals(Object o) {
        if (!(o instanceof DotNode)) return false;
        return id==(((DotNode)o).id);
    }

    /** Computes a hashcode based on the id. */
    @Override public int hashCode() { return id; }
}
