module tests/test

sig s { f: Int, _f: Int }
sig _s { g: Int, _g: Int }
run { some s && some f && some _f && some _s && some g && some _g } expect 1
