module util/graph[node]

/*
 * Utilities for some common operations and contraints
 * on graphs.
 *
 * author: Greg Dennis
 */

open util/relation as _rel

// graph in undirected
pred undirected [r: node->node] {
  symmetric[r]
}

// graph has no self-loops
pred noSelfLoops[r: node->node] {
  irreflexive[r]
}

// graph is weakly connected
pred weaklyConnected[r: node->node] {
  all n1, n2: node | n1 in n2.*(r + ~r)  // Changed from ^ to * to permit singleton
}

// graph is strongly connected
pred stronglyConnected[r: node->node] {
  all n1, n2: node | n1 in n2.*r         // Changed from ^ to * to permit singleton
}

// graph is rooted at root
pred rootedAt[r: node->node, root: node] {
  node in root.*r
}

// graph is a ring
pred ring [r: node->node] {
  all n: node | one n.r && rootedAt[r, n]
}

// graph is a dag
pred dag [r: node->node] {
  acyclic[r, node]
}

// graph is a forest
pred forest [r: node->node] {
  dag[r]
  all n: node | lone r.n
}

// graph is a tree
pred tree [r: node->node] {
  forest[r]
  lone root: node | no r.root
}

// graph is a tree rooted at root
pred treeRootedAt[r: node->node, root: node] {
  forest[r]
  rootedAt[r, root]
}

// returns the roots of the graph
fun roots [r: node->node] : set node {
  node - node.^r
}

// returns the leaves of the grpah
fun leaves [r: node->node] : set node {
  node - node.^~r
}

// returns the inner nodes (non-leaves) of the graph
fun  innerNodes [r: node->node] : set node {
  node - leaves[r]
}

// following operations transparently provide all the functionality in util/relation
fun dom [r: univ->univ] : set (r.univ) { _rel/dom[r] }
fun ran [r: univ->univ] : set (univ.r) { _rel/ran[r] }
pred total [r: univ->univ, s: set univ] { _rel/total[r,s] }
pred functional [r: univ->univ, s: set univ] { _rel/functional[r,s] }
pred function [r: univ->univ, s: set univ] { _rel/function[r,s] }
pred surjective [r: univ->univ, s: set univ] { _rel/surjective[r,s] }
pred injective [r: univ->univ, s: set univ] { _rel/injective[r,s] }
pred bijective[r: univ->univ, s: set univ] { _rel/bijective[r,s] }
pred bijection[r: univ->univ, d, c: set univ] { _rel/bijection[r,d,c] }
pred reflexive [r: univ -> univ, s: set univ] { _rel/reflexive[r,s] }
pred irreflexive [r: univ -> univ] { _rel/irreflexive[r] }
pred symmetric [r: univ -> univ] { _rel/symmetric[r] }
pred antisymmetric [r: univ -> univ] { _rel/antisymmetric[r] }
pred transitive [r: univ -> univ] { _rel/transitive[r] }
pred acyclic[r: univ->univ, s: set univ] { _rel/acyclic[r,s] }
pred complete[r: univ->univ, s: univ] { _rel/complete[r,s] }
pred preorder [r: univ -> univ, s: set univ] { _rel/preorder[r,s] }
pred equivalence [r: univ->univ, s: set univ] { _rel/equivalence[r,s] }
pred partialOrder [r: univ -> univ, s: set univ] { _rel/partialOrder[r,s] }
pred totalOrder [r: univ -> univ, s: set univ] { _rel/totalOrder[r,s] }
