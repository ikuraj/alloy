// Prim's Algorithm for finding a minimum spanning tree (MST)
// in a graph.

open util/ordering[Step] as SO

// represents a node in the graph
sig Node {}

// represents an edge in the graph
sig Edge {
  weight: Int,
  n1, n2: Node
} {
  weight >= 0
  n1 != n2
} 

// represents one step of the algorithm
sig Step {
  marked: set Node, -- nodes marked so far (including all prev steps)
  edge: lone Edge,  -- edge selected in this step
  totalWeight: Int  -- total weight of selected edges so far
}

// returns a set nodes that are not marked in a given step
fun unmarked(s: Step): set Node {
  Node - s.marked
}

// returns all edges that connect one marked and one unmarked
// node (with respect to a given Step)
fun cuttingEdges(s: Step): set Edge {
  {e: Edge | (e.n1 in s.marked and e.n2 in unmarked[s]) or
             (e.n2 in s.marked and e.n1 in unmarked[s])}
}

// marks a given step as "unused" by fixing the values of
// its fields to some default values
pred markUnused(s: Step) {
  no s.marked
  no s.edge
  s.totalWeight = -1
}

// initial condition: 
//   - exactly one node is marked
//   - no edge is selected in this step
//   - total weight so far is 0 (since 0 edges have been selected)
pred init(s: Step) {
  one s.marked
  no s.edge
  s.totalWeight = 0
}

// final condition: all nodes are marked 
pred final(s: Step) {
  s.marked = Node
}

// transition: at every step the minimal cutting edge is selected
pred trans(s, s': Step) {
  some e: cuttingEdges[s] {
    (no e2: cuttingEdges[s] - e | e2.weight < e.weight)
    s'.marked = s.marked + e.n1 + e.n2
    s'.edge = e
    s'.totalWeight = s.totalWeight.plus[e.weight]
  }
}

// Models the execution of Prim's Algorithm. 
//
// The conceptual last step need not be the one returned 
// by the SO/last function; instead it can be any other 
// step where the final condition is satisfied.
pred PrimAlg[sLast: Step] {
  // initial condition holds for the first step
  init[SO/first]
  // final condition holds for the last step
  final[sLast]
  // transition condition holds for every two neighbouring 
  // steps between first and last
  all s: SO/prevs[sLast] |
    trans[s, SO/next[s]]
  // fix values for the steps after the last one 
  // for the sake of visualization and also to avoid
  // spurious instances
  all s: SO/nexts[sLast] {
    markUnused[s]
  }
}

// Find valid executions of Prim's algorithm 
RunPrim: run {
  some sLast: Step | 
    PrimAlg[sLast] 
} for 5


// Declarative specification for a spanning tree
// whose weight is strictly less than a given integer
// ("maxWeight")
pred SpanningTree[maxWeight: Int] {
  some edges: set Edge {
    // all nodes are covered
    edges.(n1 + n2) = Node
    // it is a tree, i.e. the number of edges is #Node - 1
    #edges = (#Node).minus[1]
    // the weight is smaller than the given max weight
    (sum e: edges | e.weight) < maxWeight
  }
}

// Find instaces of spanning trees by satisfying its  
// declarative specification (SpanningTree predicate)
RunST: run {
  #Node > 3
  SpanningTree[5]
  all s: Step | markUnused[s]
} for 5


CheckPrim: run {
  some sLast: Step | 
    PrimAlg[sLast] and SpanningTree[sLast.totalWeight]
} for 5
