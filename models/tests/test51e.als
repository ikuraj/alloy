module tests/test51 -- example created by Emina Torlak on behalf of Mr. X

open tests/test51r as routes

------------ Errors ------------

-- An enum of errors that can occur during route expansion
abstract sig Error {}
one sig NoConnection, SyntaxError extends Error {}

------------ Expansions ------------

-- Encapsulation of one step in the process of expanding a proposed route.
sig Expansion {
  proposedRoute, expandedRoute: Route,
  currentIndex: Int,
  error: lone Error,
  next: set Expansion
}

-- Facts about expansions.
fact ExpansionFacts  {
  (all e: Expansion, e': e.next | e.expand[e']) and         -- expansions are related by the expand op

  (all e: Expansion | no next.e <=> e.start) and            -- if no predecessor, e is the initial expansion

  (all e: Expansion | no e.next <=> (some e.error or        -- if no successor, e is the last expansion
    e.currentIndex !in e.proposedRoute.inds)) and

  (all e1, e2: Expansion | e1.eq[e2] => e1 = e2)        -- expansions are cannonical
}

-- Returns the feature that is being expanded.
fun Expansion.currentFeature [] : lone Feature {
  this.proposedRoute.at[this.currentIndex]
}

-- Returns the feature that will be expanded in the next step.
fun Expansion.nextFeature [] : lone Feature {
  this.proposedRoute.at[next[this.currentIndex]]
}

-- Returns the feature that has been expanded in the previous step.
fun Expansion.prevFeature [] : lone Feature {
  this.proposedRoute.at[prev[this.currentIndex]]
}

-- True if the given expansion has the same field values as this one.
pred Expansion.eq [other: Expansion] {
  this.proposedRoute = other.proposedRoute and
  this.currentIndex = other.currentIndex and
  this.expandedRoute = other.expandedRoute and
  this.error = other.error
}

-- True if the given expansion is the first step in the process of expanding this.proposedRoute
pred Expansion.start [] {
  this.currentIndex = 0 and
  no this.expandedRoute.inds and
  this.error = (syntacticallyValidProposedRoute[this.proposedRoute] => none else SyntaxError)
}

-- True if e'  follows this in the process of expanding this.proposedRoute
pred Expansion.expand [e': Expansion] {
  no this.error and
  e'.proposedRoute = this.proposedRoute and
  e'.currentIndex =next[this.currentIndex] and
  (this.expandPlace[e'] or this.expandRoad[e'])
}

-- True if e' is obtained by expanding this.expandedRoute with the place at this.currentIndex
pred Expansion.expandPlace [e': Expansion] {
  let p = Place & (this.currentFeature) |
   this.currentFeature in Place and
   (p = this.expandedRoute.last =>
    e'.expandedRoute = this.expandedRoute else
    this.expandedRoute.addPlace[p, e'.expandedRoute] ) and
   no e'.error
}

-- True if e' is obtained by expanding this.expandedRoute with the places on the road at this.currentIndex
pred Expansion.expandRoad [e': Expansion] {
  let r = Road & (this.currentFeature) |
   this.currentFeature in Road and
   (let entry = r.closest[r.connections[this.prevFeature]], exit = r.closest[r.connections[this.nextFeature]] |
    (no entry or no exit) =>
    (e'.error = NoConnection and e'.expandedRoute = this.expandedRoute) else
    ((let start =r.closest[entry+exit], end = r.farthest[entry+exit] |
      start.place = this.expandedRoute.last =>
      this.expandedRoute.addPlaces[r, r.successor[start], end, e'.expandedRoute] else
      this.expandedRoute.addPlaces[r, start, end, e'.expandedRoute]) and
     no e'.error)
   )
}
