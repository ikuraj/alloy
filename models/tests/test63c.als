module tests/test

open tests/test63a as _hidden
run { some _f } // should complain that the name cannot be found
