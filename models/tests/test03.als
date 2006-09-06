open util/ordering[univ] as ord

sig A {}

one sig A1,A2 extends A {}

run { first=A1  } for 2 but 2 int expect 1
run { first=Ord } for 2 but 2 int expect 1
run { first=$1  } for 2 but 2 int expect 1

