package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;
import java.util.IdentityHashMap;

/**
 * Mutable; this implements a graph with nodes and directed edges.
 * Null node is allowed.
 *
 * <p> Note: it uses reference identity for comparing nodes, rather than using N.equals().
 *
 * <p><b>Invariant:</b>  nodeToTargets.containsKey(x) implies nodeToTargets.get(x)!=null
 *
 * <p><b>Thread Safety:</b>  Safe.
 *
 * @param <N> - the node type
 */

public final class DirectedGraph<N> {

    /** This field maps each node X to a list of "neighbor nodes" that X can reach by following a directed edge. */
    private final IdentityHashMap<N,List<N>> nodeToTargets = new IdentityHashMap<N,List<N>>();

    /** Constructs an empty graph. */
    public DirectedGraph() { }

    /** Adds a directed edge from start node to end node (if there wasn't such an edge already). */
    public synchronized void addEdge(N start, N end) {
        List<N> targets = nodeToTargets.get(start);
        if (targets==null) {
            targets=new ArrayList<N>();
            nodeToTargets.put(start,targets);
        } else {
            for (int i=targets.size()-1; i>=0; i--) {
                if (targets.get(i)==end) {
                    return;
                }
            }
        }
        targets.add(end);
    }

    /**
     * Returns whether there is a directed path from start node to end node
     * by following 0 or more directed edges.
     */
    public synchronized boolean hasPath(N start, N end) {
        if (start==end) {
            return true;
        }
        List<N> todo = new ArrayList<N>();
        IdentityHashMap<N,Object> visited = new IdentityHashMap<N,Object>();
        // The correctness relies on three invariants:
        // (1) Nothing is ever removed from "visited".
        // (2) Every time we add X to "visited", we also simultaneously add X to "todo".
        // (3) Every time we add X to "todo", we also simultaneously add X to "visited".
        // (4) Nothing is added to "todo" more than once
        visited.put(start,null);
        todo.add(start);
        while(todo.size()>0) {
            List<N> targets=nodeToTargets.get(todo.remove(todo.size()-1));
            if (targets!=null) {
                for (int i=targets.size()-1; i>=0; i--) {
                    N next=targets.get(i);
                    if (next==end) {
                        return true;
                    }
                    if (!visited.containsKey(next)) {
                        visited.put(next,null);
                        todo.add(next);
                    }
                }
            }
        }
        return false;
    }
}
