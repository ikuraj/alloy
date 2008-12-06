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

import java.util.Set;
import edu.mit.csail.sdg.alloy4graph.DotColor;
import edu.mit.csail.sdg.alloy4graph.DotPalette;
import edu.mit.csail.sdg.alloy4graph.VizGraph;
import edu.mit.csail.sdg.alloy4graph.VizNode;
import edu.mit.csail.sdg.alloy4graph.DotShape;
import edu.mit.csail.sdg.alloy4graph.DotStyle;

/** Immutable; this represents a node to be written out to the DOT file.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class DotNode {

    /** a user-provided annotation that will be associated with this node (can be null) */
    public final Object uuid;

    /** The label to show. */
    private final String label;

    /** The shape. */
    private final DotShape shape;

    /** The color. */
    private final DotColor color;

    /** The line style. */
    private final DotStyle style;

    /** Constructs a node.
     * @param uuid - a user-provided annotation that will be associated with this node (can be null)
     * @param label - the label to show
     * @param shape - the shape
     * @param color - the color
     * @param style - the line style
     */
    public DotNode(Object uuid, String label, DotShape shape, DotColor color, DotStyle style) {
        this.uuid = uuid; this.label = label; this.shape = shape; this.color = color; this.style = style;
    }

    /** Writes the node into a DotGraph object.
     * @param dotgraph - the graph
     * @param attribs - a set of additional labels to append to the node (can be null if we don't have any to append)
     */
    VizNode write2(VizGraph dotgraph, Set<String> attribs) {
       VizNode n = new VizNode(dotgraph, uuid, label);
       if (attribs!=null) for (String a:attribs) if (a.length()>0) n.addAfter(a);
       return n.set(color).set(shape).set(style);
    }

    /** Writes the node into a StringBuilder as we would writing to a Dot file
     * @param out - the StringBuilder object that will receive the text
     * @param attribs - a set of additional labels to append to the node (can be null if we don't have any to append)
     * @param pal - the color palette to use
     */
    void write(DotGraph parent, StringBuilder out, Set<String> attribs, DotPalette pal) {
        out.append("\"N" + parent.node2id(this) + "\"");
        out.append(" [");
        out.append("uuid=\"");
        if (uuid!=null) out.append(esc(uuid.toString()));
        out.append("\", label=\"" + esc(label));
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
}
