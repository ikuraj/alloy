package edu.mit.csail.sdg.alloy4.util;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.IdentityHashMap;

/**
 * Mutable; this implements a graph with nodes and directed edges.
 *
 * <p/> Note: it uses reference identity for comparing nodes, rather than using N.equals().
 *
 * <p/> <b>Invariant:</b>  nodeToTargets.containsKey(x) => nodeToTargets.get(x)!=null
 *
 * @param <N> - the node type
 *
 * @author Felix Chang
 */

public final class DirectedGraph<N> {

    /** This field maps each node X to a list of "neighbor nodes" that X can reach by following a directed edge. */
    private final Map<N,List<N>> nodeToTargets = new IdentityHashMap<N,List<N>>();

    /** Constructs an empty graph. */
    public DirectedGraph() { }

    /** Adds a directed edge from node1 to node2 if there wasn't such a directed edge already. */
    public void addEdge (N start, N end) {
        List<N> targets=nodeToTargets.get(start);
        if (targets==null) { targets=new ArrayList<N>(); nodeToTargets.put(start,targets); }
        for(int i=targets.size()-1; i>=0; i--)
        	if (targets.get(i)==end)
        		return;
        targets.add(end);
    }

    /** Returns whether there is a directed path from start node to end node by following 0 or more directed edges. */
    public boolean hasPath (N start, N end) {
    	List<N> todo = new ArrayList<N>();
    	IdentitySet<N> visited = new IdentitySet<N>();
    	while(true) {
    		if (start.equals(end)) return true;
    		visited.add(start);
    		List<N> targets=nodeToTargets.get(start);
    		if (targets!=null)
    			for (int i=0, n=targets.size(); i<n; i++) {
    				N next=targets.get(i);
    				if (!visited.contains(next)) todo.add(next);
    			}
    		if (todo.size()==0) return false;
    		start=todo.remove(todo.size()-1);
    	}
    }
}
