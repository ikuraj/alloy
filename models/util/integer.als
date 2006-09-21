module util/integer

/**
 * A collection of utility functions for using Integers in
 * Alloy. Note that the functions that return Ints, such as
 * 'add' and 'negate', may return an empty set if no such
 * Int exists for that integer value.
 *
 * If only nonnegative or positive numbers are required, use
 * the util/natural model instead.
 *
 * @author Greg Dennis
 */

/* add */
fun add (n1, n2: Int) : Int { Int [int n1 + int n2] }
/* subtract */
fun sub (n1, n2: Int) : Int { Int [int n1 - int n2] }

/* negate */
fun negate (n: Int) : Int { Int [0 - int n] }

/* equal to */
pred eq (n1, n2: Int) { int n1 = int n2 }

/* greater than */
pred gt (n1, n2: Int) { int n1 > int n2 }
/* less then */
pred lt (n1, n2: Int) { int n1 < int n2 }

/* greater than or equal */
pred gte (n1, n2: Int) { int n1 >= int n2 }
/* less than or equal */
pred lte (n1, n2: Int) { int n1 =< int n2 }

/* integer is zero */
pred zero (n: Int) { int n = 0 }

/* positive */
pred pos  (n: Int) { int n > 0 }
/* negative */
pred neg  (n: Int) { int n < 0 }

/* non-positive */
pred nonpos (n: Int) { int n =< 0 }
/* non-negative */
pred nonneg (n: Int) { int n >= 0 }

/* signum (aka sign or sgn) */
fun signum (n: Int) : Int {
  Int [pos[n] => (0 - 1) else (neg[n] => 1 else 0)]
}

/*
 * returns the ith element (zero-based) from the set s
 * in the ordering of 'next', which is a linear ordering
 * relation like that provided by util/ordering
 */
fun int2elem(i: Int, next: univ->univ, s: set univ) : lone univ {
  {e: s | #^next.e = int i }
}

/*
 * returns the index of the element (zero-based) in the
 * ordering of next, which is a linear ordering relation
 * like that provided by util/ordering
 */
fun elem2int(e: univ, next: univ->univ) : lone Int {
  Int[# ^next.e]
}
