package edu.mit.csail.sdg.alloy4graph;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

/** Mutable; represents a digraph. */

public final class DiGraph {

    /** Mutable; represents a node in a digraph. */
    public static abstract class DiNode {

        /** The graph that this node belongs to. */
        public final DiGraph graph;

        /** Stores the layer that this node is in. */
        private int layer=0;

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
        public DiNode(DiGraph graph) {
            this.graph=graph;
            graph.nodelist.add((VizNode)this);
            graph.layerlist.get(0).add((VizNode)this);
        }

        /** {@inheritDoc} */
        @Override public final int hashCode() { return super.hashCode(); }

        /** {@inheritDoc} */
        @Override public final boolean equals(Object other) { return super.equals(other); }

        /** Return the layer that this node is in. */
        public final int layer() { return layer; }

        /**
         * Changes the layer that this node is in; the new layer must be 0 or greater.
         * <p> If a node is removed from a layer, the order of the other nodes in that layer remain unchanged.
         * <p> If a node is added to a new layer, then it is added to the right of the original rightmost node in that layer.
         */
        public final void setLayer(final int newLayer) {
            if (newLayer<0) throw new IllegalArgumentException("The layer cannot be negative!");
            if (layer==newLayer) return;
            graph.layerlist.get(layer).remove(this);
            layer=newLayer;
            while(layer >= graph.layerlist.size()) graph.layerlist.add(new ArrayList<VizNode>());
            graph.layerlist.get(layer).add((VizNode)this);
        }
    }

    /** Mutable; represents an edge in a digraph. */
    public static abstract class DiEdge {

        /** The "from" node. */
        private DiNode a;

        /** The "to" node. */
        private DiNode b;

        /** Constructs a new edge; the two nodes must belong to the same graph. */
        public DiEdge(DiNode a, DiNode b) {
            if (a.graph!=b.graph) throw new IllegalArgumentException("You cannot draw an edge between two different graphs.");
            this.a=a; this.b=b;
            if (a==b) { a.selfs.add((VizEdge)this); } else { a.outs.add((VizEdge)this); b.ins.add((VizEdge)this); }
            a.graph.edgelist.add((VizEdge)this);
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
    public DiGraph() { layerlist.add(new ArrayList<VizNode>()); }

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

    /** Sort the list of nodes using the given comparator. */
    public final void sortNodes(Comparator<VizNode> comparator) { Collections.sort(nodelist, comparator); }

    /** Sort the list of nodes in a given layer (0..#layer-1) using the given comparator. */
    public final void sortLayer(int layer, Comparator<VizNode> comparator) { Collections.sort(layerlist.get(layer), comparator); }
}
