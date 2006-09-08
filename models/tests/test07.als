// Actual bug: what should iden be?

sig S {}
run { no S && some (iden-Int->Int) } for 1 expect 0


