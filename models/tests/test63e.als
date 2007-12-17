module tests/test

open tests/test63a as _hidden
run { some _g } // should complain that the name cannot be found
