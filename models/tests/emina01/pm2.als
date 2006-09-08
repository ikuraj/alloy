module tests/emina01/pm2[p1,p2]

open tests/emina01/pm1[p2] as pm1p2

fun second(elem1:p1, elem2:p2) : p2 {
  pm1p2/identity[elem2]
}
