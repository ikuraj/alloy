module util/ordering[elem]

/*
 * Creates a single linear ordering over the atoms in elem. It also constrains all
 * the atoms to exist that are permitted by the scope on elem. That is, if the scope
 * on a signature S is 5, opening util/ordering[S] will force S to have 5 elements
 * and create a linear ordering over those five elements. The predicates and
 * functions below provide access to properties of the linear ordering, such as
 * which element is first in the ordering, or whether a given element precedes
 * another. You cannotcreate multiple linear orderings over the same signature with
 * this model. If you that functionality, try using the util/sequence module instead.
 *
 * Technical comment:
 * An important constraint: elem must contain all atoms permitted by the scope.
 * This is to let the analyzer optimize the analysis by setting all fields of each
 * instantiation of Ord to predefined values: e.g. by setting 'last' to the highest
 * atom of elem and by setting 'next' to {<T0,T1>,<T1,T2>,...<Tn-1,Tn>}, where n is
 * the scope of elem. Without this constraint, it might not be true that Ord.last is
 * a subset of elem, and that the domain and range of Ord.next lie inside elem.
 *
 * author: Ilya Shlyakhter
 * revisions: Daniel jackson
 */

one sig Ord {
   First, Last: elem,
   Next, Prev: elem -> lone elem
}{
  // constraints that actually define the total order
  Prev = ~Next
  one First
  one Last
  no First.Prev
  no Last.Next
  (
   // either elem has exactly one atom,
   // which has no predecessor or successor...
   (one elem && no elem.Prev && no elem.Next) ||
   // or...
    (all e: elem | {
      // ...each element (except the first) has one predecessor, and...
      (e = First || one e.Prev)
      // ...each element (except the last) has one successor, and...
      (e = Last || one e.Next)
      // ...there are no cycles
      (e !in e.^Next)
    })
  )
  // all elements of elem are totally ordered
  elem in First.*Next
}

// first
fun first: elem { Ord.First }

// last
fun last: elem { Ord.Last }

// return the predecessor of e, or empty set if e is the first element
fun prev [e: elem]: lone elem { e.(Ord.Prev) }

// return the successor of e, or empty set of e is the last element
fun next [e: elem]: lone elem { e.(Ord.Next) }

// return elements prior to e in the ordering
fun prevs [e: elem]: set elem { e.^(Ord.Prev) }

// return elements following e in the ordering
fun nexts [e: elem]: set elem { e.^(Ord.Next) }

// e1 is less than e2 in the ordering
pred lt [e1, e2: elem] { e1 in prevs[e2] }

// e1 is greater than e2 in the ordering
pred gt [e1, e2: elem] { e1 in nexts[e2] }

// e1 is less than or equal to e2 in the ordering
pred lte [e1, e2: elem] { e1=e2 || lt [e1,e2] }

// e1 is greater than or equal to e2 in the ordering
pred gte [e1, e2: elem] { e1=e2 || gt [e1,e2] }

// returns the larger of the two elements in the ordering
fun larger [e1, e2: elem]: elem { lt[e1,e2] => e2 else e1 }

// returns the smaller of the two elements in the ordering
fun smaller [e1, e2: elem]: elem { lt[e1,e2] => e1 else e2 }

// returns the largest element in es
// or the empty set if es is empty
fun max [es: set elem]: lone elem { es - es.^(Ord.Prev) }

// returns the smallest element in es
// or the empty set if es is empty
fun min [es: set elem]: lone elem { es - es.^(Ord.Next) }
