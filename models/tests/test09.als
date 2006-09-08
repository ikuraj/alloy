TWO BUGS:
(1) Do we really want to allow users to extend SIGINT?
(2) SIGINT's subs() status doens't get cleared between runs!

one sig P extends Int {}

pred something { some P }

run something expect 1



