module examples/systems/marksweepgc

/*
 * Model of mark and sweep garbage collection.
 */

// a node in the heap
sig Node {}

sig HeapState {
  left, right : Node -> lone Node,
  marked : set Node,
  freeList : lone Node
}

pred HeapState.clearMarks(hs' : HeapState) {
  // clear marked set
  no hs'.marked
  // left and right fields are unchanged
  hs'.left = this.left
  hs'.right = this.right
}

// simulate the recursion of the mark() function using transitive closure
fun HeapState.reachable(n: Node) : set Node {
  n + n.^(this.left) + n.^(this.right)
}

pred HeapState.mark(from : Node, hs': HeapState) {
  hs'.marked = this::reachable[from]
  hs'.left = this.left
  hs'.right = this.right
}

// complete hack to simulate behavior of code to set freeList
pred HeapState.setFreeList(hs': HeapState) {
  // especially hackish
  hs'.freeList.*(hs'.left) in (Node - this.marked)
  all n: Node |
    (n !in this.marked) => {
      no hs'.right[n]
      hs'.left[n] in (hs'.freeList.*(hs'.left))
      n in hs'.freeList.*(hs'.left)
    } else {
      hs'.left[n] = this.left[n]
      hs'.right[n] = this.right[n]
    }
  hs'.marked = this.marked
}

pred HeapState.GC(root : Node, hs': HeapState) {
  some hs1, hs2: HeapState |
    this::clearMarks[hs1] && hs1::mark[root, hs2] && hs2::setFreeList[hs']
}

assert Soundness1 {
  all h, h' : HeapState, root : Node |
    h::GC[root, h'] =>
      (all live : h::reachable[root] | {
        h'.left[live] = h.left[live]
        h'.right[live] = h.right[live]
      })
}

assert Soundness2 {
  all h, h' : HeapState, root : Node |
    h::GC[root, h'] =>
      no h'::reachable[root] & h'::reachable[h'.freeList]
}

assert Completeness {
  all h, h' : HeapState, root : Node |
    h::GC[root, h'] =>
      (Node - h'::reachable[root]) in h'::reachable[h'.freeList]
}

check Soundness1 for 3 expect 0
check Soundness2 for 3 expect 0
check Completeness for 3 expect 0
