package edu.mit.csail.sdg.alloy4;

import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * This represents the type of an expression.
 *
 * <p/> In Alloy, each expression can be a set/relation, or a formula, or a primitive integer.
 * 
 * @author Felix Chang
 */

//===========================================================================================================
//=== "Type" = (Y/N int) and (Y/N bool) and (0 or more Relations)
//===
//=== INVARIANT: Whenever a subset of relations are identical except for 1 position,
//=== where together they comprise of all direct subsigs of an abstract sig, then we merge them.
//===
//=== IMMUTABLE
//=== COMPARISON by value.
//===========================================================================================================

public final class Type implements Iterable<Type.Rel> {

  // If it's one of these, then entries and arities are always empty.
  public static final Type EMPTY         = new Type(false,false);
  public static final Type INT           = new Type(true,false);
  public static final Type FORMULA       = new Type(false,true);
  public static final Type INTANDFORMULA = new Type(true,true);

  public final boolean isInt;
  public final boolean isBool;
  private final Set<Integer> arities; // The arities of relations contained in this Type
  private final Set<Rel> entries; // The relations contained in this Type

  public Iterator<Rel> iterator() { return entries.iterator(); }

  @Override public int hashCode() { int i=0; for(Rel x:entries) i=i+x.hashCode(); return i; }

  @Override public boolean equals(Object x) {
    if (x==this) return true;
    if (x instanceof Type) {
       Type y=(Type)x;
       return isInt==y.isInt && isBool==y.isBool && entries.equals(y.entries);
    }
    return false;
  }

  @Override public String toString() {
    boolean first=true;
    String ans="{";
    if (isInt) { if (!first) ans=ans+", "; first=false; ans=ans+"PrimitiveInteger"; }
    if (isBool) { if (!first) ans=ans+", "; first=false; ans=ans+"PrimitiveBoolean"; }
    for(Rel r:entries) { if (!first) ans=ans+", "; first=false; ans=ans+r; }
    return ans+"}";
  }

  private Type(boolean i,boolean b,Set<Rel> e,Set<Integer> a) { // Assumes the caller will give up the references to "e" and "a"
    isInt=i;
    isBool=b;
    entries=Collections.unmodifiableSet(e);
    arities=Collections.unmodifiableSet(a);
  }

  private Type(boolean i,boolean b) {
    isInt=i;
    isBool=b;
    entries=Collections.unmodifiableSet(new LinkedHashSet<Rel>());
    arities=Collections.unmodifiableSet(new LinkedHashSet<Integer>());
  }

  private static Type make(boolean i,boolean b,Set<Rel> e,Set<Integer> a) { // Assumes the caller will give up the references to "e" and "a"
    if (e.size()!=0 && a.size()!=0) return new Type(i,b,e,a);
    if (i==false) return b?FORMULA:EMPTY; else return b?INTANDFORMULA:INT;
  }

  private static Type make(Set<Rel> e,Set<Integer> a) { // Assumes the caller will give up the references to "e" and "a"
    return make(false,false,e,a);
  }

  public static Type makeBool(Type x) { // A factory method that sets "isBool" flag
    if (!x.isBool) return make(x.isInt, true, x.entries, x.arities);
    return x;
  }

  public static Type makeInt(Type x) { // A factory method that sets "isInt" flag
    if (!x.isInt) return make(true, x.isBool, x.entries, x.arities);
    return x;
  }

  public static Type make() { return EMPTY; }

  public static Type make(Rel x) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    add1(ee,aa,x);
    return make(ee,aa);
  }

  public static Type make(List<ParaSig> list,int start,int end) { return make(new Rel(list.subList(start,end))); }

  public static Type make(ParaSig x) { return make(new Rel(x)); }

  public static Type make(ParaSig x,ParaSig y) {
    List<ParaSig> list=new LinkedList<ParaSig>();
    list.add(x);
    list.add(y);
    return make(new Rel(list));
  }

  // Adds rt to this, keeping this normalized. If rt==null, we do nothing.
  private static void add1(Set<Rel> entries, Set<Integer> arities, Rel rt) {
    if (rt==null) return;
    arities.add(rt.arity());
    // If rt is a subset of a Rel in this, return. Remove all subsets of rt in this (if any).
    for(Iterator<Rel> x=entries.iterator(); x.hasNext();) {
       Rel y=x.next();
       if (rt.isSubsetOf(y)) return;
       if (y.isSubsetOf(rt)) x.remove();
    }
    // replace all ocurrences of A1->B->... + ... + An->B->... with
    // a single Rel A->B-> ... whenever A is an abstract type
    // such that A = A1 + A2 + ... + An.  Repeat this collapsing
    // procedure along each column, until the Rel rtAdd that
    // subsumes {rt} + {entries of this} is generated.  Note that
    // entries can be folded only along ONE column at a time
    // (i.e. one column will contain pieces of an abstract sig type
    // while all the other columns MUST be identical -- otherwise,
    // the sum of the entries being folded and the produced
    // Rel will not represent the same sets of tuples).
    boolean changed;
    while(true) {
      changed=false;
      for (int i = 0; i < rt.arity(); i++) {
        ParaSig bt=rt.basicTypes.get(i);
        if (bt.sup()!=null && bt.sup().abs) {
           Rel folded=fold(entries,arities,rt,i);
           if (folded!=null) {rt=folded; changed=true; i--;}
        }
      }
      if (changed==false) break;
    }
    entries.add(rt);
  }

  // PRECONDITION: a[i] extends from an abstract sig
  //
  // If {a}+this.entries contain a set of entries X1..Xn, such that
  //    (1) For each X:  X[j]==a[j] for i!=j, and X[i].super==a[i].super
  //    (2) X1[i]..Xn[i] comprise of all the direct subsignatures of an abstract parent sig
  // THEN:
  //    we removeAll(X), then return the merged result of X1..Xn
  // ELSE
  //    we return null
  //
  private static Rel fold(Set<Rel> entries, Set<Integer> arities, Rel a, int i) {
    ParaSig parent = a.basicTypes.get(i).sup();
    List<ParaSig> subs = new ArrayList<ParaSig>(parent.subs);
    List<Rel> ret = new ArrayList<Rel>();
    for (Rel b:entries)
      if (b.arity() == a.arity())
        for(int j=0; ;j++) {
          if (j>=b.basicTypes.size()) {ret.add(b); subs.remove(b.basicTypes.get(i)); break;}
          ParaSig bt1 = a.basicTypes.get(j);
          ParaSig bt2 = b.basicTypes.get(j);
          if (i==j && bt2.sup()!=parent) break;
          if (i!=j && bt2!=bt1) break;
        }
    subs.remove(a.basicTypes.get(i));
    if (!subs.isEmpty()) return null;
    entries.removeAll(ret);
    entries.remove(a);
    List<ParaSig> bTypes=new ArrayList<ParaSig>(a.basicTypes);
    bTypes.set(i,parent);
    return new Rel(bTypes);
  }

  // Returns true if this.size() == 0,
  // or all entries in this Type are NONE, or NONE->NONE, or NONE->NONE->NONE...
  public boolean hasNoTuple() {
    for(Rel r:this) if (!r.isEmpty()) return false;
    return true;
  }

  // Returns true if this.size() > 0,
  // and there exists one entry that is not NONE, or NONE->NONE, or NONE->NONE->NONE...
  public boolean hasTuple() { return !hasNoTuple(); }

  // Return the number of relation types in this
  public int size() { return entries.size(); }

  // Return true if this contains a Rel of given arity; otherwise returns false
  public boolean hasArity(int arity) { return arities.contains(arity); }

  // If all RelationType in this have the same arity, that arity is returned.
  // If this contains no RelationTypes, we return 0. Otherwise, we return -1.
  public int arity() {
    if (arities.isEmpty()) return 0;
    if (arities.size()==1) return (arities.iterator().next()).intValue();
    return -1;
  }

  // Returns true if this is a subset of that: for all a in this, there exists b in that,
  // where a is a subset of b.
  // ==> IGNORES isInt and isBool
  public boolean isSubsetOf(Type that) {
    for (Rel a:this) {
      boolean found;
      found=false;
      for (Rel b:that) if (a.isSubsetOf(b)) {found=true;break;}
      if (!found) return false;
    }
    return true;
  }

  // Returns true if exists some a in this, some b in that, where (a.arity==b.arity, and a[0]&b[0]!=empty)
  // ==> IGNORES isInt and isBool
  public boolean canOverride(Type that) {
    for (Rel a:this)
     if (that.arities.contains(a.arity()))
      for (Rel b:that)
       if (a.arity()==b.arity() && a.basicTypes.get(0).intersect(b.basicTypes.get(0)).isNonEmpty())
        return true;
    return false;
  }

  // Returns true if exists some a in this, some b in that, where a.arity==b.arity
  // ==> IGNORES isInt and isBool
  public boolean hasCommonArity(Type that) {
    for (Integer a:arities) if (that.arities.contains(a)) return true;
    return false;
  }

  // Returns: reduce{ x | x in this, or x in that }
  // ==> MERGES isInt and isBool
  public Type merge(Type ut) {
    Set<Rel> ee=new LinkedHashSet<Rel>(entries);
    Set<Integer> aa=new LinkedHashSet<Integer>(arities);
    for(Rel x:ut) add1(ee,aa,x);
    return make(isInt||ut.isInt, isBool||ut.isBool, ee, aa);
  }

  // Returns: reduce{ x | x in this, or x in that }
  // ==> DELETES isInt and isBool
  public Type union(Type ut) {
    Set<Rel> ee=new LinkedHashSet<Rel>(entries);
    Set<Integer> aa=new LinkedHashSet<Integer>(arities);
    for(Rel x:ut) add1(ee,aa,x);
    return make(ee,aa);
  }

  // Returns: reduce{ x | x=~a for some a in this }
  // ==> DELETES isInt and isBool
  public Type transpose() {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    for(Rel a:this) add1(ee, aa, a.transpose());
    return make(ee,aa);
  }

  // Returns: reduce{ x | x=a&b for some a in this, some b in that, where a.arity==b.arity }
  // ==> DELETES isInt and isBool
  public Type intersect(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    for (Rel a:this) for (Rel b:that) if (a.arity()==b.arity()) add1(ee, aa, a.intersect(b));
    return make(ee,aa);
  }

  // Returns: reduce{ x | x=a->b for some a in this, some b in that }
  // ==> DELETES isInt and isBool
  public Type product_of_anyEmptyness(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    for (Rel a:this) for (Rel b:that) add1(ee, aa, a.product(b));
    return make(ee,aa);
  }

  // Returns: reduce{ x | x=a->b for some a in this, some b in that, where a.isEmpty()==b.isEmpty() }
  // ==> DELETES isInt and isBool
  public Type product_of_sameEmptyness(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    for (Rel a:this) for (Rel b:that) if (a.isEmpty()==b.isEmpty()) add1(ee, aa, a.product(b));
    return make(ee,aa);
  }

  // Returns: reduce{ x | x=a.b for some a in this, some b in that, where a[rightmost]&b[leftmost]!=empty }
  // ==> DELETES isInt and isBool
  public Type join(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    for (Rel a:this)
      for (Rel b:that)
        if (a.arity()+b.arity()>2)
          if (!a.basicTypes.get(a.arity()-1).intersect(b.basicTypes.get(0)).isEmpty())
             add1(ee, aa, a.join(b));
    return make(ee,aa);
  }

  // Returns: reduce{ rt[0]->rt[1]->...->rt[n-1] |
  //           exist n-ary rt1 in this, exists unary rt2 in that, such that
  //           rt1[i]==rt[i] for every i, except rt[0]==rt1[0]&rt2[0]
  // ==> DELETES isInt and isBool
  public Type domainRestrict(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    if (size()>0 && that.arities.contains(1))
      for (Rel b:that)
        if (b.arity() == 1)
          for (Rel a:this)
            add1(ee, aa, a.columnRestrict(b.basicTypes.get(0), 0));
    return make(ee,aa);
  }

  // Returns: reduce{ rt[0]->rt[1]->...->rt[n-1] |
  //           exists n-ary rt1 in this, exists unary rt2 in that, such that
  //           rt1[i]==rt[i] for every i, except rt[n-1]==rt1[n-1]&rt2[0]
  // ==> DELETES isInt and isBool
  public Type rangeRestrict(Type that) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    if (size()>0 && that.arities.contains(1))
      for (Rel b:that)
        if (b.arity() == 1)
          for (Rel a:this)
            add1(ee, aa, a.columnRestrict(b.basicTypes.get(0), a.arity()-1));
    return make(ee,aa);
  }

  // Returns: a new UnionType consisting of entries in this UnionType that have the given arity
  // ==> DELETES isInt and isBool
  public Type extract(int arity) {
    Set<Rel> ee=new LinkedHashSet<Rel>();
    Set<Integer> aa=new LinkedHashSet<Integer>();
    if (arities.contains(arity)) {
      aa.add(arity);
      for(Rel rt:entries) if (rt.arity()==arity) ee.add(rt);
    }
    return make(ee,aa);
  }

  // Returns: reduce(u + u.u + u.u.u + ...) , where u = {rt | rt in this AND length(rt) == 2}
  // ==> DELETES isInt and isBool
  public Type closure() {
    Type r=extract(2), ans=r, x=r.join(r);
    while(x.hasTuple()) {
      Type oldans=ans, oldx=x;
      ans=ans.union(x); x=x.join(r);
      if (oldans.equals(ans) && oldx.equals(x)) break;
    }
    return ans;
  }

//===========================================================================================================
//=== "Rel" = BasicType x BasicType x BasicType
//===
//=== INVARIANT: If one of them is NONE, then all of them must be NONE.
//=== INVARIANT: Each one must be univ, none, Int, toplevel sig, or subsig. (ie. cannot be "subset sig")
//===
//=== IMMUTABLE.
//=== COMPARISON by value.
//===========================================================================================================

public static final class Rel {

  public final List<ParaSig> basicTypes;

  @Override public String toString() {
    String ans="";
    for(int i=0; i<basicTypes.size(); i++) { if (i!=0) ans=ans+"->"; ans=ans+basicTypes.get(i).name; }
    return ans;
  }

  @Override public int hashCode() { return basicTypes.hashCode(); }

  @Override public boolean equals(Object x) {
    if (x==this) return true;
    if (x instanceof Rel) return ((Rel)x).basicTypes.equals(basicTypes);
    return false;
  }

  // If addblock==false, that means it is already a unmodifiable copy.
  // If addblock==true, that means it is a copy, but not yet made unmodifiable.
  private Rel(boolean addblock, List<ParaSig> x) {
    if (addblock) x=Collections.unmodifiableList(x);
    basicTypes=x;
  }

  // PRECONDITION: x must not be a SUBSET sig
  public Rel(ParaSig x) {
    if (x.subset) throw new ErrorInternal(null,null,"Attempting to add a subset signature into a Type.Rel object!");
    List<ParaSig> y=new ArrayList<ParaSig>();
    y.add(x);
    basicTypes=Collections.unmodifiableList(y);
  }

  // PRECONDITION: None of them can be a SUBSET SIG. And the incoming list must have at least 1 element.
  public Rel(List<ParaSig> x) {
    if (x==null || x.size()==0) throw new ErrorInternal(null,null,"Attempting to create a 0-length Type.Rel object!");
    x=new ArrayList<ParaSig>(x);
    for(int i=0; i<x.size(); i++) {
      ParaSig y=x.get(i);
      if (y.subset) throw new ErrorInternal(null,null,"Attempting to add a subset signature into a Type.Rel object!");
      if (y.isEmpty()) {
        for(int j=0;j<x.size();j++) x.set(j, ParaSig.NONE);
        break;
      }
    }
    basicTypes=Collections.unmodifiableList(x);
  }

  // Returns the ARITY of this relation
  public int arity() { return basicTypes.size(); }

  // Returns true if this is an empty product (that is, a product of the form NONE->NONE->NONE...)
  public boolean isEmpty() { return basicTypes.get(0).isEmpty(); }

  // Returns true if THIS is a subset of THAT.
  // Specifically: If THIS==A1->A2->A3, THAT==B1->B2->B3, then it's true iff, forall i, Ai is a subtype of Bi.
  // (NOTE: If the arities don't match, we return false)
  public boolean isSubsetOf(Rel that) {
    if (arity() != that.arity()) return false;
    for(int i=arity()-1; i>=0; i--) if (!basicTypes.get(i).isSubtypeOf(that.basicTypes.get(i))) return false;
    return true;
  }

  // PRECONDITION: this.arity()==that.arity()
  // RETURNS: the intersection of two Relations.
  //
  // Specifically:
  // Given this == (A1->A2->A3->A4)
  // Given that == (B1->B2->B3->B4)
  // Return (A1&B1) -> (A2&B2) -> (A3&B3) -> (A4&B4)  if each intersection != {}
  // Return  NONE   ->   NONE  ->   NONE  ->  NONE    if at least 1 intersection is {}
  public Rel intersect(Rel that) {
    if (arity()!=that.arity()) return null;
    List<ParaSig> ans = new ArrayList<ParaSig>(arity());
    for(int i=0; i<arity(); i++) {
      ParaSig x = basicTypes.get(i).intersect(that.basicTypes.get(i));
      if (x.isEmpty()) return new Rel(false, Collections.nCopies(arity(), ParaSig.NONE));
      ans.add(x);
    }
    return new Rel(true,ans);
  }

  // PRECONDITION: this.arity + that.arity > 2
  // RETURNS: The "relational join" between this and that.
  //
  // Note: if this.RIGHTMOST and that.LEFTMOST have empty intersection, we return NONE->..->NONE.
  public Rel join(Rel that) {
     int leftSize = arity();
     int rightSize = that.arity();
     if (leftSize + rightSize <= 2) return null;
     if (basicTypes.get(leftSize-1).intersect(that.basicTypes.get(0)).isEmpty())
        return new Rel(false, Collections.nCopies(leftSize+rightSize-2,ParaSig.NONE));
     List<ParaSig> ans = new ArrayList<ParaSig>(leftSize+rightSize-2);
     if (leftSize > 1) ans.addAll(basicTypes.subList(0,leftSize-1));
     if (rightSize > 1) ans.addAll(that.basicTypes.subList(1,rightSize));
     return new Rel(true, ans);
  }

  // RETURNS: The "relational product" between this and that.
  //
  // Note: If either or both is NONE->..->NONE, then we return NONE->..->NONE.
  public Rel product(Rel that) {
    int n=arity()+that.arity();
    if (isEmpty() || that.isEmpty()) return new Rel(false, Collections.nCopies(n, ParaSig.NONE));
    List<ParaSig> ans = new ArrayList<ParaSig>(n);
    ans.addAll(basicTypes);
    ans.addAll(that.basicTypes);
    return new Rel(true, ans);
  }

  // RETURNS: The "transpose" of this.
  public Rel transpose() {
    List<ParaSig> ans = new ArrayList<ParaSig>(basicTypes);
    Collections.reverse(ans);
    return new Rel(true, ans);
  }

  // Given A1->A2->A3->A4 == this
  // Given B
  // Given i
  // If  (Ai & B)!=empty, then return  A1 -> A2 -> A3 -> A4 except the i-th entry is replaced by (Ai & B)
  // If  (Ai & B)==empty, then return NONE->NONE->NONE->NONE
  public Rel columnRestrict(ParaSig b, int i) {
    b = b.intersect(basicTypes.get(i));
    if (b.isEmpty()) return new Rel(false, Collections.nCopies(arity(), ParaSig.NONE));
    List<ParaSig> bTypes=new ArrayList<ParaSig>(basicTypes);
    bTypes.set(i,b);
    return new Rel(true, bTypes);
  }
}

//===========================================================================================================

}
