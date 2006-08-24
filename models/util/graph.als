module util/graph[node]

/*
 * Utilities for some common operations and contraints
 * on graphs.
 *
 * author: Greg Dennis
 */

open util/relation as rel

// graph in undirected
pred undirected (r: node->node) {
  symmetric[r]
}
// graph has no self-loops
pred noSelfLoops(r: node->node) {
  irreflexive[r]
}

// graph is weakly connected
pred weaklyConnected(r: node->node) {
  all n1, n2: node | n1 in n2.^(r + ~r)
}
// graph is strongly connected
pred stronglyConnected(r: node->node) {
  all n1, n2: node | n1 in n2.^r
}
// graph is rooted at root
pred rootedAt(r: node->node, root: node) {
  node in root.*r
}

// graph is a ring
pred ring (r: node->node) {
  all n: node | one n.r && rootedAt[r, n]
}
// graph is a dag
pred dag (r: node->node) {
  acyclic[r, node]
}
// graph is a forest
pred forest (r: node->node) {
  dag[r]
  all n: node | lone r.n
}
// graph is a tree
pred tree (r: node->node) {
  forest[r]
  // lone root implies one root
  lone root: node | no r.node
}
// graph is a tree rooted at root
pred treeRootedAt(r: node->node, root: node) {
  forest[r]
  rootedAt[r, root]
}

// returns the roots of the graph
fun roots (r: node->node) : set node {
  node - node.^r
}
// returns the leaves of the grpah
fun leaves (r: node->node) : set node {
  node - node.^~r
}
// returns the inner nodes (non-leaves) of the graph
fun  innerNodes (r: node->node) : set node {
  node - leaves[r]
}
