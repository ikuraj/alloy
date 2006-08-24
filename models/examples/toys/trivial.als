//a trivial model whose command has no solution
module trivial
sig S {}
fact {S != S}
pred show () {some S}
run show
