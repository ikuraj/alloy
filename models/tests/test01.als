open util/ordering[Int] as ord

run   { first=$(0-4) } for 3 int expect 1
run   { first=$(0-3) } for 3 int expect 1
run   { first=$(0-2) } for 3 int expect 1
run   { first=$(0-1) } for 3 int expect 1
run   { first=$0     } for 3 int expect 1
run   { first=$1     } for 3 int expect 1
run   { first=$2     } for 3 int expect 1
run   { first=$3     } for 3 int expect 1


