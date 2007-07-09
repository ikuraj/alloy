module tests/test51 -- example created by Emina Torlak on behalf of Mr. X

open tests/test51g as geography

------------ Routes ------------

-- A route is a defined by a sequence of features.
sig Route {
  features: seq Feature
}
-- Facts about routes
fact RouteFacts {
  all r1, r2: Route | r1.features = r2.features => r1 = r2  -- routes are canonical
}

-- Defines a proposed route as syntactically valid if it has at least two features,
-- and if the first and last features are places.
pred Route.syntacticallyValidProposedRoute [] {
   not lone this.features and
   this.first in Place and
   this.last in Place
}

-- Returns the set of features that comprise this route.
fun Route.elements [] : set Feature { this.features[Int] }

-- Returns the set of indices that comprise this route.
fun Route.inds [] : set Int { this.features.Feature }

-- Returns the first feature of this route.
fun Route.first [] : lone Feature { this.features.first }

-- Returns the last feature of this route.
fun Route.last [] : lone Feature { this.features.last }

-- Returns the feature at the given index.
fun Route.at [idx: Int] : lone Feature { this.features[idx] }

-- Constrains route' to have this route as its prefix and the places containing the points
-- between start and end, inclusive, as its suffix.
pred Route.addPlaces [road: Road, start, end: Point, route': Route] {

 (start = end or road.closer[start, end]) and
 route'.features = append[this.features, subseq[road.geography, road.geography.start, road.geography.end].place ]

}

-- Constraints route' to have this route and its prefix and the given place as its suffix.s
pred Route.addPlace [place: Place, route': Route] {
  route'.features = this.features.add[place]
}

-- Constraints route' to be the concatenation of this and the given route.
pred Route.concat [other: Route, route': Route] {
  route'.features = this.features.append[other.features]
}
