package edu.mit.csail.sdg.alloy4.core;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.Stack;

public class SimpleGraph<N> {
	
	private Map<N,Set<N>> nodeToEdges = new LinkedHashMap<N,Set<N>>();

	public Set<N> getNodes() { return nodeToEdges.keySet(); }

	public void add(N newnode){
		if (nodeToEdges.containsKey(newnode)) return;
		Set<N> emptyset = new LinkedHashSet<N>();
		nodeToEdges.put(newnode, emptyset);
	}
	
	public void addEdge(N v1, N v2) {
		add(v1);
		add(v2);
		nodeToEdges.get(v1).add(v2);
	}
	
	private Set<N> getOutgoingEdges(N node) {
		add(node);
		return nodeToEdges.get(node);
	}

	public boolean hasPath (N startat, N toVertex ) {
		List<N> visited = new ArrayList<N>();
		Stack<N> stack = new Stack<N>();
		N next;
		stack.push( startat );
		do {
			next = stack.pop();
			visited.add( next );
			if (next==toVertex) return true;
			for(N adjacent: getOutgoingEdges(next)) {
				if( !visited.contains( adjacent ) && !stack.contains( adjacent )) stack.push( adjacent );
			}
		} while(!stack.isEmpty() );
		return false;
	}
	
}

