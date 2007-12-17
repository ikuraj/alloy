module tests/test

open tests/test63b as _hidden
run { some f } // should complain that the name cannot be found
