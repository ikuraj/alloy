module util/ternary

/*
 * Utilities for some common operations and constraints
 * on ternary relations. The keyword 'univ' represents the
 * top-level type, which all other types implicitly extend.
 * Therefore, all the functions and predicates in this model
 * may be applied to ternary relations of any type.
 *
 * author: Greg Dennis
 */

// returns the domain of a ternary relation
fun dom (r: univ->univ->univ) : set univ { r.univ.univ }

// returns the range of a ternary relation
fun ran (r: univ->univ->univ) : set univ { univ.r[univ] }

// returns the "middle range" of a ternary relation
fun mid (r: univ->univ->univ) : set univ { univ.r.univ }

// returns the first two columns of a ternary relation
fun select12 (r: univ->univ->univ) : univ->univ {
  r.univ
}

// returns the first and last columns of a ternary relation
//   where r is of type t1->t2->t3
fun select13 (r: univ->univ->univ, t1, t2, t3: set univ) : univ->univ {
  {x: t1, z: t3 | some y: t2 | x->y->z in r}
}

// returns the last two columns of a ternary relation
fun select23 (r: univ->univ->univ) : univ->univ {
  univ.r
}

// flips the first two columns of a ternary relation
//   where r is of type t1->t2->t3
fun flip12 (r: univ->univ->univ, t1, t2, t3: set univ) : univ->univ->univ {
  {x: t2, y: t1, z: t3 | y->x->z in r}
}

// flips the first and last columns of a ternary relation
//   where r is of type t1->t2->t3
fun flip13 (r: univ->univ->univ, t1, t2, t3: set univ) : univ->univ->univ {
  {x: t3, y: t2, z: t1 | z->y->x in r}
}

// flips the last two columns of a ternary relation
//   where r is of type t1->t2->t3
fun flip23 (r: univ->univ->univ, t1, t2, t3: set univ) : univ->univ->univ {
  {x: t1, y: t3, z: t2 | x->z->y in r}
}