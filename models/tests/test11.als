module x

abstract sig Person {}
sig Man, Woman extends Person {}
one sig Eve extends Woman {}
one sig Adam extends Man {}

assert SubSigsMustBeDisjoint { Person-Eve != Person-Adam }

check SubSigsMustBeDisjoint for 2 expect 0
