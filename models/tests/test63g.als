module tests/test

open tests/test63a as _hidden
run { some _hidden/_s } // should complain that the name cannot be found
