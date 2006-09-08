module main

one sig A {}
one sig B {}
one sig C {}
one sig D {}

one sig Q {
 p : (A -> B) + (C -> D)
}

assert claim {
 (A -> B) in Q.p
}

check claim for 3 expect 0
