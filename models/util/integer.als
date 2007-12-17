module util/integer

/**
 * A collection of utility functions for using Integers in Alloy.
 * Note that integer overflows are silently truncated to the current bitwidth
 * using the 2's complement arithmetic.
 */

fun add [n1, n2: Int] : Int { int n1 + int n2 }

fun sub [n1, n2: Int] : Int { Int [int n1 - int n2] }

fun mul [n1, n2: Int] : Int {
   // Note: the translator recognizes this method and provides a much more efficient translation
   let s1 = { x:Int | (n1<0 && x<0 && x>=n1) || (n1>0 && x>0 && x<=n1) } |
   let s2 = { x:Int | (n2<0 && x<0 && x>=n2) || (n2>0 && x>0 && x<=n2) } |
   #(s1->s2)
}

// Performs the division with "round to zero" semantics, except the following 3 cases
// 1) if a is 0, then it returns 0
// 2) else if b is 0, then it returns 1 if a is negative and -1 if a is positive
// 3) else if a is the smallest negative integer, and b is -1, then it returns a
fun div [a, b: Int] : Int {
   // Note: the translator recognizes this method and provides a much more efficient translation
   let sn = (((a>=0 && b>=0) || (a<0 && b<0)) => 1 else 0-1) |
   let na = (a>0 => 0-a else int[a]) |
   let nb = (b>0 => 0-b else int[b]) |
   let r = div[na-nb, nb] |
   (na=0 || na>nb) => 0 else
   nb=0 => 0-sn else
   na=nb => sn else (sn>0 => 1+r else (0-1)-r)
}

/* answer is defined to be the unique integer that satisfies "a = ((a/b)*b) + remainder" */
fun rem [a, b: Int] : Int {
   // Note: the translator recognizes this method and provides a much more efficient translation
   int[a] - (a.div[b].mul[b])
}

/* negate */
fun negate [n: Int] : Int { 0 - n }

/* equal to */
pred eq [n1, n2: Int] { int[n1] = int[n2] }

/* greater than */
pred gt [n1, n2: Int] { n1 > n2 }

/* less then */
pred lt [n1, n2: Int] { n1 < n2 }

/* greater than or equal */
pred gte [n1, n2: Int] { n1 >= n2 }

/* less than or equal */
pred lte [n1, n2: Int] { n1 <= n2 }

/* integer is zero */
pred zero [n: Int] { n = 0 }

/* positive */
pred pos  [n: Int] { n > 0 }

/* negative */
pred neg  [n: Int] { n < 0 }

/* non-positive */
pred nonpos [n: Int] { n <= 0 }

/* non-negative */
pred nonneg [n: Int] { n >= 0 }

/* signum (aka sign or sgn) */
fun signum [n: Int] : Int { n<0 => (0-1) else (n>0 => 1 else 0) }

/*
 * returns the ith element (zero-based) from the set s
 * in the ordering of 'next', which is a linear ordering
 * relation like that provided by util/ordering
 */
fun int2elem[i: Int, next: univ->univ, s: set univ] : lone s {
  {e: s | #^next.e = int i }
}

/*
 * returns the index of the element (zero-based) in the
 * ordering of next, which is a linear ordering relation
 * like that provided by util/ordering
 */
fun elem2int[e: univ, next: univ->univ] : lone Int {
  Int[#^next.e]
}

// returns the largest integer in the current bitwidth
fun max:one Int { {a:Int | a>=0 && (a+1)<0} }

// returns the smallest integer in the current bitwidth
fun min:one Int { {a:Int | a<0 && (a-1)>=0} }

// maps each integer (except max) to the integer after it
fun next:Int->Int { {a,b:Int | b>a && b=1+a} }

// maps each integer (except min) to the integer before it
fun prev:Int->Int { ~next }

// given a set of integers, return the largest element
fun max [es: set Int]: lone Int { es - es.^prev }

// given a set of integers, return the smallest element
fun min [es: set Int]: lone Int { es - es.^next }

// given an integer, return all integers prior to it
fun prevs [e: Int]: set Int { e.^prev }

// given an integer, return all integers following it
fun nexts [e: Int]: set Int { e.^next }

// returns the larger of the two integers
fun larger [e1, e2: Int]: Int { let a=int[e1], b=int[e2] | (a<b => b else a) }

// returns the smaller of the two integers
fun smaller [e1, e2: Int]: Int { let a=int[e1], b=int[e2] | (a<b => a else b) }
