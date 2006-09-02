package edu.mit.csail.sdg.alloy4.util;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;

/**
 * Mutable; this class implements a graph with nodes and directed edges.
 *
 * <p/> Note: it uses N.equals() for node equality.
 *
 * <p/> <b>Invariant:</b>  (nodes.contains(x)) iff (nodeToTargets.containsKey(x))
 *
 * @param <N> - the node type
 *
 * @author Felix Chang
 */

public final class DirectedGraph<N> {

    /** This field stores the set of nodes in the graph. */
    private final Set<N> nodes = new LinkedHashSet<N>();

    /** This field maps each node in the directed graph to its "directed" neighbors. */
    private final Map<N,List<N>> nodeToTargets = new LinkedHashMap<N,List<N>>();

    /** Default constructor that generates an empty graph. */
    public DirectedGraph() { }

    /** This returns a copy of the set of nodes in the graph. */
    public List<N> getNodes() { return new ArrayList<N>(nodes); }

    /** Returns true if the node is in the graph. */
    public boolean hasNode(N node) { return nodes.contains(node); }

    /** This adds a node to the graph if it is not in the graph already. */
    public void addNode(N newnode){
        if (!nodes.contains(newnode)) {
            nodes.add(newnode);
            nodeToTargets.put(newnode, new ArrayList<N>());
        }
    }

    /**
     * This adds a directed edge from node1 to node2 if there wasn't such an edge already.
     * If node1 and node2 don't exist in the graph yet, they will be added. 
     */
    public void addEdge(N node1, N node2) {
        addNode(node1);
        addNode(node2);
        List<N> targets=nodeToTargets.get(node1);
        if (!targets.contains(node2)) targets.add(node2);
    }

    /** Returns true if and only if there is a directed path from start node to end node. */
    public boolean hasPath (N start, N end) {
        Set<N> visited = new LinkedHashSet<N>();
        Set<N> todo = new LinkedHashSet<N>();
        while(true) {
            if (start.equals(end)) return true;
            visited.add(start);
            for (N next: nodeToTargets.get(start))
                if (!visited.contains(next))
                   todo.add(next);
            if (todo.size()==0) return false;
            start = todo.iterator().next();
            todo.remove(start);
        }
    }
}
