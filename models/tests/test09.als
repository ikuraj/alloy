module test01/main

one sig P extends Int {}

pred something { some P }

run something expect 1
