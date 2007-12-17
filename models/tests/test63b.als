module tests/test

open tests/test63a as _hidden
sig n { }
run { some s && some _hidden/s && some f } expect 1
