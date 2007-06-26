module tests/test

open tests/test10b[A,B] as pm2AB

sig A {}

sig B {}

run { some A } expect 1

pred ex[disj a,b:Int] { a=b }

run ex expect 0
