//a trivial model whose command has no solution
module trivial

sig S {}

fact {S != S}

run {some S} expect 0
