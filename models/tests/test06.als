NEED TO DO BETTER SKOLEM NAMING:

/*

This is how it was done in Alloy 3.
1) given a string s, split it into two strings s0 and s1 such that

s0 + s1 == s &&
all c : s1 | c in {'0'...'9'} &&
s1[0] != '0' &&
s0[s0.length-1] !in {'1'...'9'}

For example, if s = "a0001034", then s0 = "a000" and s1 == "1034"

2) if we have seen s0 before, then

let currentSuffix = max(intValue(s1),maxReturnedSuffix(s0))
maxReturnedSuffix(s0) = currentSuffix + 1
return s0 + currentSuffix

3) if we have not seen s0 before, then

maxReturnedSuffix(s0) = intValue(s1) + 1
return s

*/

