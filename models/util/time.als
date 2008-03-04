open util/ordering[Time]

sig Time { }

let dynamic[x] = x one-> Time

let dynamicSet[x] = x -> Time

let then [a, b, t, t'] {
    some x:Time | a[t,x] && b[x,t']
}

let while = while3

let while3 [cond, body, t, t'] {
    cond[t] => (some x:Time | body[t,x] && while2[cond,body,x,t']) else (t=t')
}

let while2 [cond, body, t, t'] {
    cond[t] => (some x:Time | body[t,x] && while1[cond,body,x,t']) else (t=t')
}

let while1 [cond, body, t, t'] {
    cond[t] => (some x:Time | body[t,x] && while0[cond,body,x,t']) else (t=t')
}

let while0 [cond, body, t, t'] {
    !cond[t] && t=t'
}
