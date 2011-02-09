open util/integer

one sig S {
  a: one Int, 
  b: one Int
} { 
  add[a, b] < 3 and a >= 0 and b >= 0
}

run {} for 1