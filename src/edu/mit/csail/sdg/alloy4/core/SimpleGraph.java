package edu.mit.csail.sdg.alloy4.core;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map;
import java.util.LinkedHashMap;

/**
 * Mutable; this class implements a graph with nodes and directed edges.
 *
 * <br/> Note: it uses N.equals() for node equality.
 * 
 * @author Felix Chang
 *
 * @param <N> - the node type
 */
public class SimpleGraph<N> {

	/**
	 * This field maps each node in the directed graph to its "directed" neighbors.
	 *
	 * <p/>   More precisely, given a directed graph with N=nodes and E=edges, then
	 * <br/>  (1)        n in N  iff  nodeToTargets.containsKey(n)
	 * <br/>  (2)  (n1,n2) in E  iff  nodeToTargets.get(n1).contains(n2)
	 * <br/>  In particular, if n1 has "m" identical edges going to n2,
	 * then n2 will appear exactly m times in nodeToTargets.get(n1)
	 */
	private Map<N,List<N>> nodeToTargets = new LinkedHashMap<N,List<N>>();
	
	/** This returns the set of nodes in the graph. */
	public Set<N> getNodes() { return nodeToTargets.keySet(); }
	
	/** This adds a node to the graph if it is not in the graph already. */
	public void addNode(N newnode){
		if (!nodeToTargets.containsKey(newnode)) {
			nodeToTargets.put(newnode, new ArrayList<N>());
		}
	}
	
	/** This adds an edge from v1 to v2 (even if there were already edges from v1 to v2). */
	public void addEdge(N v1, N v2) {
		addNode(v1);
		addNode(v2);
		nodeToTargets.get(v1).add(v2);
	}
	
	/**
	 * This returns the list of neighbors reachable via 1 directed edge.
	 * If there are multiple edges going to the same neighbor,
	 * this method will return duplicates.
	 */
	private List<N> getTargets(N node) {
		addNode(node);
		return nodeToTargets.get(node);
	}
	
	/** Returns true if and only if there is a path from start node to end node. */
	public boolean hasPath (N start, N end) {
		Set<N> visited = new LinkedHashSet<N>();
		Set<N> todo = new LinkedHashSet<N>();
		while(true) {
			if (start.equals(end)) return true;
			visited.add(start);
			for (N next: getTargets(start)) if (!visited.contains(next)) todo.add(next);
			if (todo.size()==0) return false;
			start = todo.iterator().next();
			todo.remove(start);
		}
	}
}
