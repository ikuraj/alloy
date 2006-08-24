package kodviz.graph;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A Graph contains a set of Nodes and Edges. In addition, graph contains 
 * information about the Ranking of sets of Nodes (Types) or edges 
 * (relations). Nodes or edges on the same rank mean that they are laid out
 * on the same level, either vertically or horizontally.
 */
public class Graph {

	/*
	 * Rep invariant:
	 *  for all g: Graph |
	 *    all fields are not null &&
	 *      for i < nodes.length, j < edges.length:
	 *        nodes[i] instanceof Node && edges[j] instance of Edge
	 *    && for k < nodeRankings.length, nodeRankings[k] instanceof
	 *      Set | for l < nodeRankings[k].length, nodeRankings[k].get(l) 
	 *        instanceof Node
	 */
     
    
	private Set nodes;
	private Set edges;

	/**
	 * The ranking of nodes.
	 */
	private Set nodeRankings;

   
	/**
	 * Creates an empty Graph
	 */
	public Graph() {
	this.nodes = new HashSet();
	this.edges = new HashSet();
	this.nodeRankings = new HashSet();
	}

	/**
	 * Creates a new Graph object with specified nodes, edges, and ranking.
	 */
	public Graph(Set nodes, Set edges, Set nodeRankings) {
	this.nodes = nodes;
	this.edges = edges;
	this.nodeRankings = nodeRankings;
	}

	/**
	 * Returns an Iterator of Nodes in the Graph
	 */
    @SuppressWarnings("unchecked")
	public Iterator getNodes() {
	return (Collections.unmodifiableSet(nodes)).iterator();
	}

	/**
	 * Returns an Iterator of Edges in the Graph
	 */
    @SuppressWarnings("unchecked")
	public Iterator getEdges() {
	return (Collections.unmodifiableSet(edges)).iterator();
	}

	/**
	 * Returns an Iterator of Node Rankings. Each entry is a set of nodes
	 * that have the same rank.
	 */
    @SuppressWarnings("unchecked")
	public Iterator getNodeRankings() {
	return (Collections.unmodifiableSet(nodeRankings)).iterator();
	}
    
	/**
	 * Two graphs are equal if their sets of nodes, edges, and rankings 
	 * are equal.
	 */
	public boolean equals(Object o) {
	if(o == null || !(o instanceof Graph)) {
		return false;
	}
	Graph g = (Graph)o;
	if(this.nodes.equals(g.nodes) && this.edges.equals(g.edges) &&
	   this.nodeRankings.equals(g.nodeRankings)) {
		return true;
	}
	return false;
	}

	/**
	 * Creates a hashcode for this graph.
	 */
	public int hashCode() {
	return nodes.hashCode() + edges.hashCode() + nodeRankings.hashCode();
	}

	/**
	 * Creates a String representation of this graph.
	 */
	public String toString() {
	return "Nodes: " + nodes.toString() + "\nEdges: " + edges.toString()
		+ "\nNode Rankings: " + nodeRankings.toString();
	} 
}
	









