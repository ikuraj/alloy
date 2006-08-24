module examples/case_studies/ins

/*
 * Models an Intentional Naming System (INS), a scheme for
 * dynamic resource discovery in a dynamic environment.
 *
 * For a detailed description, see:
 *   http://sdg.lcs.mit.edu/pubs/2000/INS_ASE00.pdf
 *
 * author: Sarfraz Khurshid
 */

open util/relation as rel

sig Attribute {}
sig Value {}
sig Record {}

one sig Wildcard extends Value {}

sig AVTree {
  values: set Value,
  attributes: set Attribute,
  root: values - Wildcard,
  av: attributes one -> some (values - root),
  va: (values - Wildcard) one -> attributes
}{
  // values (and attributes) of tree are those reachable from root
  values = root.*(va.av)
}

sig Query extends AVTree {} {all a:attributes | one a.av}

sig DB extends AVTree {
  records : set Record,
  recs: (values - root) some -> records,
  lookup : Query -> (values -> records)
}{
  Wildcard !in values
}

fact AddInvariants {
  all db: DB {
    all v: db.values | no v.(db.recs) & v.^(~(db.av).~(db.va)).(db.recs)
    all a: db.attributes |
      all v1, v2: a.(db.av) |
        (no(v1&v2)) => (some rr: *((db.va).(db.av)).(db.recs) | no v1.rr & v2.rr)
    }
}

pred DB::Get(r: Record, q: Query) {
  q.values = r.~(this.recs).*(~(this.av).~(this.va))
  q.attributes = q.values.~(this.av)
  q.root = this.root
  all a : attributes| a.~(q.va) = a.~(this.va)
  all v : values | v.~(q.av) = v.~(this.av)
}

pred Conforms (db: DB, q: Query, r: Record) {
  some p: Query {
    db::Get[r, p]
    q.va in p.va
    (q.av - Attribute -> Wildcard) in p.av
  }
}

pred indSubset(db : DB, q: Query, r: set Record, v: Value) {
  all a : v.(q.va) |
    (a.(q.av) in a.(db.av) => r in (a.(q.av)).(q.(db.lookup))) &&
    (a.(q.av) = Wildcard => r in a.(db.av).*((db.va).(db.av)).(db.recs))
}

pred DB::Lookup(q : Query, found: set Record) {
  all v: Value | not v.(q.va) in v.(this.va) => no v.(q.(this.lookup))
  all v: Value | all a : v.(q.va) |
    a.(q.av) != Wildcard && not a.(q.av) in a.(this.av) => no v.(q.(this.lookup))
  all v: Value - Wildcard |
    no v.(q.va) => v.(q.(this.lookup)) = v.*((this.va).(this.av)).(this.recs)
  all v: Value |
    some v.(q.va) => indSubset[this,q,v.(q.(this.lookup)),v] &&
    (no r: Record - v.(q.(this.lookup)) | indSubset[this,q,v.(q.(this.lookup)) + r, v])
  found = this.root.(q.(this.lookup))
}

assert CorrectLookup {
  all db: DB | all q : Query | all r : Record | 
    Conforms [db,q,r] <=> db::Lookup[q, r]
}

pred DB::Add(adv: Query, r: Record, db: DB) {
  // restricted version - only advertisements with fresh attributes and values added
  no this.attributes & adv.attributes
  this.values & adv.values = this.root
  this.root = adv.root
  Wildcard !in adv.values
  r !in this.records
  db.values = this.values + adv.values
  db.attributes = this.attributes + adv.attributes
  db.root = this.root
  db.av = this.av + adv.av
  db.va = this.va + adv.va
  db.recs = this.recs + ((db.values - dom[db.va]) -> r)
}

pred Query::RemoveWildCard(q: Query) {
  q.values = this.values - Wildcard
  q.attributes = this.attributes - Wildcard.~(this.av)
  q.root = this.root
  q.av = this.av - Attribute -> Wildcard
  q.va = this.va - Value -> Wildcard.~(this.av)
}

assert MissingAttributeAsWildcard {
  all db : DB, q, q' : Query, found: set Record |
    db::Lookup[q, found] && q::RemoveWildCard[q'] => db::Lookup[q', found]
}