module tests/test

open util/ordering[Int] as ord

run { first=Int[0-2-2] } for 3 int expect 1
run { first=Int[0-3] } for 3 int expect 1
run { first=Int[0-2] } for 3 int expect 1
run { first=Int[0-1] } for 3 int expect 1
run { first=Int[0]   } for 3 int expect 1
run { first=Int[1]   } for 3 int expect 1
run { first=Int[2]   } for 3 int expect 1
run { first=Int[3]   } for 3 int expect 1
