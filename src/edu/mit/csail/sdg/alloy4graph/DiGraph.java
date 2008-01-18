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

package edu.mit.csail.sdg.alloy4graph;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

/**
 * Mutable; represents a digraph.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public class DiGraph {

    /** Mutable; represents a node in a digraph. */
    public static abstract class DiNode {

        /** The graph that this node belongs to. */
        final VizGraph graph;

        /** Stores the layer that this node is in. */
        private int layer=0;

        /** Stores the current position of this node in the graph's node list. */
        private int pos;

        /** Stores the "in" edges. */
        private final LinkedList<VizEdge> ins = new LinkedList<VizEdge>();

        /** Stores the out edges. */
        private final LinkedList<VizEdge> outs = new LinkedList<VizEdge>();

        /** Stores the self edges. */
        private final LinkedList<VizEdge> selfs = new LinkedList<VizEdge>();

        /** Stores an unmodifiable view of the list of "in" edges. */
        private final List<VizEdge> insView = Collections.unmodifiableList(ins);

        /** Stores an unmodifiable view of the list of "out" edges. */
        private final List<VizEdge> outsView = Collections.unmodifiableList(outs);

        /** Stores an unmodifiable view of the list of "self" edges. */
        private final List<VizEdge> selfView = Collections.unmodifiableList(selfs);

        /** Returns an unmodifiable view of the list of "in" edges. */
        public final List<VizEdge> inEdges() { return insView; }

        /** Returns an unmodifiable view of the list of "out" edges. */
        public final List<VizEdge> outEdges() { return outsView; }

        /** Returns an unmodifiable view of the list of "self" edges. */
        public final List<VizEdge> selfEdges() { return selfView; }

        /** Constructs a new node. */
        DiNode(VizGraph graph) {
            DiGraph d = graph;
            this.graph = graph;
            this.pos = d.nodelist.size();
            d.nodelist.add((VizNode)this);
            d.layerlist.get(0).add((VizNode)this);
        }

        /** {@inheritDoc} */
        @Override public final int hashCode() { return super.hashCode(); }

        /** {@inheritDoc} */
        @Override public final boolean equals(Object other) { return super.equals(other); }

        /** Returns the node's current position in the node list, which is always between 0 and node.size()-1 */
        public final int pos() { return pos; }

        /** Returns the layer that this node is in. */
        public final int layer() { return layer; }

        /**
         * Changes the layer that this node is in; the new layer must be 0 or greater.
         * <p> If a node is removed from a layer, the order of the other nodes in that layer remain unchanged.
         * <p> If a node is added to a new layer, then it is added to the right of the original rightmost node in that layer.
         */
        public final void setLayer(final int newLayer) {
            DiGraph d = graph;
            if (newLayer<0) throw new IllegalArgumentException("The layer cannot be negative!");
            if (layer==newLayer) return;
            d.layerlist.get(layer).remove(this);
            layer=newLayer;
            while(layer >= d.layerlist.size()) d.layerlist.add(new ArrayList<VizNode>());
            d.layerlist.get(layer).add((VizNode)this);
        }
    }

    /** Mutable; represents an edge in a digraph. */
    public static abstract class DiEdge {

        /** The "from" node. */
        private DiNode a;

        /** The "to" node. */
        private DiNode b;

        /** Constructs a new edge; the two nodes must belong to the same graph. */
        DiEdge(DiNode a, DiNode b) {
            if (a.graph!=b.graph) throw new IllegalArgumentException("You cannot draw an edge between two different graphs.");
            this.a=a; this.b=b;
            if (a==b) { a.selfs.add((VizEdge)this); } else { a.outs.add((VizEdge)this); b.ins.add((VizEdge)this); }
            ((DiGraph)(a.graph)).edgelist.add((VizEdge)this);
        }

        /** {@inheritDoc} */
        @Override public final int hashCode() { return super.hashCode(); }

        /** {@inheritDoc} */
        @Override public final boolean equals(Object other) { return super.equals(other); }

        /** Returns the "from" node. */
        public final VizNode a() { return (VizNode)a; }

        /** Returns the "to" node. */
        public final VizNode b() { return (VizNode)b; }

        /** Swaps the "from" node and "to" node. */
        public final void reverse() {
            if (a==b) return;
            a.outs.remove(this);
            b.ins.remove(this);
            a.ins.add((VizEdge)this);
            b.outs.add((VizEdge)this);
            DiNode x=a; a=b; b=x;
        }

        /** Changes the "from" node to the given node. */
        public final void changeA(DiNode newA) {
            VizEdge me=(VizEdge)this;
            if (a.graph != newA.graph) throw new IllegalArgumentException("You cannot draw an edge between two different graphs.");
            if (a==b) a.selfs.remove(this); else { a.outs.remove(this); b.ins.remove(this); }
            a=newA;
            if (a==b) a.selfs.add(me); else { a.outs.add(me); b.ins.add(me); }
        }

        /** Changes the "to" node to the given node. */
        public final void changeB(DiNode newB) {
            VizEdge me=(VizEdge)this;
            if (b.graph != newB.graph) throw new IllegalArgumentException("You cannot draw an edge between two different graphs.");
            if (a==b) a.selfs.remove(this); else { a.outs.remove(this); b.ins.remove(this); }
            b=newB;
            if (a==b) a.selfs.add(me); else { a.outs.add(me); b.ins.add(me); }
        }
    }

    /** The list of nodes. */
    private final List<VizNode> nodelist = new ArrayList<VizNode>();

    /** The list of edges. */
    private final List<VizEdge> edgelist = new ArrayList<VizEdge>();

    /** The list of layers; always contains at least one layer; every node is always in one and exactly one layer. */
    private final List<List<VizNode>> layerlist = new ArrayList<List<VizNode>>();

    /** An unmodifiable view of the list of nodes. */
    public final List<VizNode> nodes = Collections.unmodifiableList(nodelist);

    /** An unmodifiable view of the list of edges. */
    public final List<VizEdge> edges = Collections.unmodifiableList(edgelist);

    /** An unmodifiable empty list. */
    private final List<VizNode> emptyListOfNodes = Collections.unmodifiableList(new ArrayList<VizNode>(0));

    /** Constructs a new digraph. */
    DiGraph() { layerlist.add(new ArrayList<VizNode>()); }

    /** {@inheritDoc} */
    @Override public final int hashCode() { return super.hashCode(); }

    /** {@inheritDoc} */
    @Override public final boolean equals(Object other) { return super.equals(other); }

    /** Returns an unmodifiable view of the list of nodes in the given layer (0..#layer-1); return an empty list if no such layer. */
    public final List<VizNode> layer(int i) {
       if (i>=0 && i<layerlist.size()) return Collections.unmodifiableList(layerlist.get(i));
       return emptyListOfNodes;
    }

    /** Return the number of layers; always at least 1. */
    public final int layers() { return layerlist.size(); }

    /** Swap the given two nodes in the giver layer. */
    public final void swapNodes(int layer, int node1, int node2) {
        List<VizNode> list = layerlist.get(layer);
        VizNode n1 = list.get(node1), n2 = list.get(node2);
        list.set(node1, n2);
        list.set(node2, n1);
    }

    /** Sort the list of nodes according to the order in the given list. */
    public final void sortNodes(Iterable<VizNode> newOrder) {
       // The nodes that are common to this.nodelist and newOrder are moved to the front of the list, in the given order.
       // The nodes that are in this.nodelist but not in newOrder are moved to the back in an unspecified order.
       // The nodes that are in newOrder but not in this.nodelist are ignored.
       int i=0, n=nodelist.size();
       again:
       for(VizNode x:newOrder) for(int j=i; j<n; j++) if (nodelist.get(j)==x) {
          if (i!=j) { VizNode tmp=nodelist.get(i); nodelist.set(i,x); nodelist.set(j,tmp); }
          i++;
          continue again;
       }
       i=0;
       for(DiNode x:nodelist) { x.pos=i; i++; }
    }

    /** Sort the list of nodes in a given layer (0..#layer-1) using the given comparator. */
    public final void sortLayer(int layer, Comparator<VizNode> comparator) { Collections.sort(layerlist.get(layer), comparator); }
}
