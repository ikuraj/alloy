module util/sequniv

open util/integer as _ui

/*
 * A sequence utility for modeling sequences as just a
 * relation as opposed to reifying them into sequence
 * atoms like the util/sequence module does.
 *
 * Precondition: each input sequence must range over a prefix
 * of seq/Int.
 *
 * Postcondition: we guarantee the returned sequence
 * also ranges over a prefix of seq/Int.
 *
 * @author Greg Dennis
 */

/* sequence covers a prefix of seq/Int */
pred isSeq[s: Int -> univ] {
  s in seq/Int -> lone univ
  s.inds - _ui/next[s.inds] in 0
}

/* returns all the elements in this sequence */
fun elems [s: Int -> univ]: set (Int.s) { seq/Int . s }

/*
 * returns the first element in the sequence
 * (Returns the empty set if the sequence is empty)
 */
fun first [s: Int -> univ]: lone (Int.s) { s[0] }

/*
 * returns the last element in the sequence
 * (Returns the empty set if the sequence is empty)
 */
fun last [s: Int -> univ]: lone (Int.s) { s[lastIdx[s]] }

/*
 * returns the cdr of the sequence
 * (Returns the empty sequence if the sequence has 1 or fewer element)
 */
fun rest [s: Int -> univ] : s { seq/Int <: ((_ui/next).s) }

/* returns all but the last element of the sequence */
fun butlast [s: Int -> univ] : s {
  (seq/Int - lastIdx[s]) <: s
}

/* true if the sequence is empty */
pred isEmpty [s: Int -> univ] { no s }

/* true if this sequence has duplicates */
pred hasDups [s: Int -> univ] { # elems[s] < # inds[s] }

/* returns all the indices occupied by this sequence */
fun inds [s: Int -> univ]: set Int { s.univ }

/*
 * returns last index occupied by this sequence
 * (Returns the empty set if the sequence is empty)
 */
fun lastIdx [s: Int -> univ]: lone Int { _ui/max[inds[s]] }

/*
 * returns the index after the last index
 * if this sequence is empty, returns 0
 * if this sequence is full, returns empty set
 */
fun afterLastIdx [s: Int -> univ] : lone Int { _ui/min[seq/Int - inds[s]] }

/* returns first index at which given element appears or the empty set if it doesn't */
fun idxOf [s: Int -> univ, e: univ] : lone Int { _ui/min[indsOf[s, e]] }

/* returns last index at which given element appears or the empty set if it doesn't */
fun lastIdxOf [s: Int -> univ, e: univ] : lone Int { _ui/max[indsOf[s, e]] }

/* returns set of indices at which given element appears or the empty set if it doesn't */
fun indsOf [s: Int -> univ, e: univ] : set Int { s.e }

/*
 * return the result of appending e to the end of s
 * (returns s if s exhausted seq/Int)
 */
fun add [s: Int -> univ, e: univ] : s + (seq/Int->e) {
  setAt[s, afterLastIdx[s], e]
}

/*
 * returns the result of setting the value at index i in sequence to e
 * Precondition: 0 <= i < #s
 */
fun setAt [s: Int -> univ, i: Int, e: univ] : s + (seq/Int->e) {
  s ++ i -> e
}

/*
 * returns the result of inserting value e at index i
 * (if sequence was full, the original last element will be removed first)
 * Precondition: 0 <= i <= #s
 */
fun insert [s: Int -> univ, i: Int, e: univ] : s + (seq/Int->e) {
  seq/Int <: ((_ui/prevs[i] <: s) + (i->e) + _ui/prev.((_ui/nexts[i] + i) <: s))
}

/*
 * returns the result of deleting the value at index i
 * Precondition: 0 <= i < #s
 */
fun delete[s: Int -> univ, i: Int] : s {
  (_ui/prevs[i] <: s) + (_ui/next).(_ui/nexts[i] <: s)
}

/*
 * appended is the result of appending s2 to s1
 * (If the resulting sequence is too long, it will be truncated)
 */
fun append [s1, s2: Int -> univ] : s1+s2 {
  let shift = {i', i: seq/Int | int[i'] = int[i] + int[lastIdx[s1]] + 1 } |
    no s1 => s2 else (s1 + shift.s2)
}

/*
 * returns the subsequence of s between from and to, inclusive
 * Precondition: 0 <= from <= to < #s
 */
fun subseq [s: Int -> univ, from, to: Int] : s {
  let shift = {i', i: seq/Int | int[i'] = int[i] - int[from] } |
    shift.((seq/Int - _ui/nexts[to]) <: s)
}

// provides the available methods from util/integer
fun add [n1, n2: Int] : Int { _ui/add[n1,n2] }
fun sub [n1, n2: Int] : Int { _ui/sub[n1,n2] }
fun negate [n: Int] : Int { _ui/negate[n] }
pred eq [n1, n2: Int] { _ui/eq[n1,n2] }
pred gt [n1, n2: Int] { _ui/gt[n1,n2] }
pred lt [n1, n2: Int] { _ui/lt[n1,n2] }
pred gte [n1, n2: Int] { _ui/gte[n1,n2] }
pred lte [n1, n2: Int] { _ui/lte[n1,n2] }
pred zero [n: Int] { _ui/zero[n] }
pred pos  [n: Int] { _ui/pos[n] }
pred neg  [n: Int] { _ui/neg[n] }
pred nonpos [n: Int] { _ui/nonpos[n] }
pred nonneg [n: Int] { _ui/nonneg[n] }
fun signum [n: Int] : Int { _ui/signum[n] }
fun int2elem[i: Int, next: univ->univ, s: set univ] : lone s { _ui/int2elem[i,next,s] }
fun elem2int[e: univ, next: univ->univ] : lone Int { _ui/elem2int[e,next] }
fun max:one Int { _ui/max }
fun min:one Int { _ui/min }
fun next:Int->Int { _ui/next }
fun prev:Int->Int { _ui/prev }
fun max [es: set Int]: lone Int { _ui/max[es] }
fun min [es: set Int]: lone Int { _ui/min[es] }
fun prevs [e: Int]: set Int { _ui/prevs[e] }
fun nexts [e: Int]: set Int { _ui/nexts[e] }
fun larger [e1, e2: Int]: Int { _ui/larger[e1,e2] }
fun smaller [e1, e2: Int]: Int { _ui/smaller[e1,e2] }
