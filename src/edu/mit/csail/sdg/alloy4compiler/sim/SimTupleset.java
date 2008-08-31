/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4compiler.sim;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/** Immutable; represents a tupleset. */

public final class SimTupleset implements Iterable<SimTuple> {

    /**
     * The list of tuples.
     * <br> <b>Invariant:</b> If nonempty, it must contain only same-arity tuples.
     * <br> <b>Invariant:</b> It must not contain duplicate tuples.
     */
    private final ConstList<SimTuple> tuples;

    /** Construct a tupleset containing the given list of tuples  (Note: caller MUST make sure there are no duplicates, and all tuples are of same arity!) */
    private SimTupleset(List<SimTuple> tuples) { this.tuples = ConstList.make(tuples); }

    /** The tupleset containing no tuples. */
    public static final SimTupleset EMPTY = new SimTupleset(new TempList<SimTuple>(0).makeConst());

    /** Construct a tupleset containing the given tuple. */
    public static SimTupleset make(SimTuple tuple) {
        TempList<SimTuple> x = new TempList<SimTuple>(1);
        x.add(tuple);
        return new SimTupleset(x.makeConst());
    }

    /** Make a tupleset containing the given atom. */
    public static SimTupleset make(String atom) {
        return SimTupleset.make(SimTuple.make(atom));
    }

    /** Make a tupleset containing a deep copy of the given list of tuples (Note: caller MUST make sure there are no duplicates, and all tuples are of same arity!) */
    public static SimTupleset make(List<SimTuple> tuples) {
        return tuples.size()==0 ? EMPTY : new SimTupleset(tuples);
    }

    /** If this tupleset is empty, then return 0, else return the arity of every tuple in this tupleset. */
    public int arity() { return tuples.size()==0 ? 0 : tuples.get(0).arity(); }

    /** Returns a read-only iterator over the tuples. */
    public Iterator<SimTuple> iterator() { return tuples.iterator(); }

    /** Returns the set of all integers between min and max. */
    public static SimTupleset range(int min, int max) {
       if (min>max) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(max-min+1);
       while(true) {
           ans.add(SimTuple.make(String.valueOf(min)));
           if (min==max) break;
           min++;
       }
       return new SimTupleset(ans.makeConst());
    }

    /** Returns the "next" relation among all integers between min and max. */
    public static SimTupleset next(int min, int max) {
       if (min>=max) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(max-min);
       SimAtom MIN = SimAtom.make(String.valueOf(min));
       while(min<max) { SimAtom NEXT=SimAtom.make(String.valueOf(min+1)); ans.add(SimTuple.make(MIN, NEXT)); MIN=NEXT; min++; }
       return new SimTupleset(ans.makeConst());
    }

    /** Returns the index position if the list of tuples contains the tuple (a,b) (or return -1 if not found). */
    private static int find(TempList<SimTuple> tuples, SimAtom a, SimAtom b) {
       if (tuples.size() == 0 || tuples.get(0).arity() != 2) return -1;
       for(int i=tuples.size()-1; i >= 0; i--) {
          SimTuple x = tuples.get(i);
          if (x.head()==a && x.tail()==b) return i;
       }
       return -1;
    }

    /** Returns true if this tupleset contains the given tuple. */
    public boolean has(SimTuple that) {
        return tuples.size()>0 && tuples.get(0).arity()==that.arity() && tuples.contains(that);
    }

    /** Returns true if this tupleset is unary and contains the given atom. */
    public boolean has(SimAtom that) {
       if (tuples.size()==0 || tuples.get(0).arity()!=1) return false;
       for(int i=tuples.size()-1; i>=0; i--) if (tuples.get(i).get(0)==that) return true;
       return false;
    }

    /** {@inheritDoc} */
    @Override public String toString() {
       StringBuilder sb = null;
       for(SimTuple x: tuples) {
          if (sb==null) sb = new StringBuilder("{"); else sb.append(", ");
          x.toString(sb);
       }
       return sb==null ? "{}" : (sb.append("}").toString());
    }

    /** Returns a hashcode consistent with the equals() method. */
    @Override public int hashCode() {
        int ans = 0;
        for(SimTuple t: tuples) ans = ans + t.hashCode();
        return ans;
    }

    /** Returns true if this contains the same tuples as that. */
    @Override public boolean equals(Object that) {
        return this==that || (that instanceof SimTupleset && equals((SimTupleset)that));
    }

    /** Returns true if this contains the same tuples as that. */
    public boolean equals(SimTupleset that) {
        // since SimTupleset must not contain duplicates, and this.size()==that.size(), comparing one way is sufficient
        return tuples.size()==that.tuples.size() && in(that);
    }

    /** Returns true if this is a subset of that. */
    public boolean in(SimTupleset that) {
       int n = tuples.size();
       if (n>that.tuples.size()) return false; else if (n==0 || this==that) return true;
       if (tuples.get(0).arity() != that.tuples.get(0).arity()) return false;
       for(int i=0; i<n; i++) if (!that.has(tuples.get(i))) return false;
       return true;
    }

    /** Sum up all the integer atoms in this tupleset; (if this tupleset's arity is not 1, then we return 0) */
    public int sum() {
       if (tuples.size() == 0 || tuples.get(0).arity() != 1) return 0;
       int ans = 0;
       for(SimTuple t: tuples) ans = ans + t.get(0).toInt();
       return ans;
    }

    /**
     * Return the identity over this tupleset; (if this tupleset's arity is not 1, then we return an emptyset)
     * <br/> Note: the result's tuple order is the same as this tupleset's tuple order.
     */
    public SimTupleset iden() {
       if (tuples.size() == 0 || tuples.get(0).arity() != 1) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(tuples.size());
       for(SimTuple x: tuples) ans.add(SimTuple.make(x.head(), x.head())); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans.makeConst());
    }

    /**
     * Return the relational override of this and that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: in general, the tuples may be ordered arbitrarily in the result.
     */
    public SimTupleset override(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return that;
       if (that.tuples.size()==0 || this.tuples.get(0).arity()!=that.tuples.get(0).arity()) return this;
       TempList<SimTuple> ans = new TempList<SimTuple>(this.tuples.size());
       if (that.tuples.size()==1) { // very common case, so let's optimize it
          SimTuple y = that.tuples.get(0);
          for(SimTuple x: this.tuples) { if (x.get(0)!=y.get(0)) ans.add(x); else if (that!=null) {ans.add(y); that=null;} }
          if (that!=null) ans.add(y);
          return new SimTupleset(ans.makeConst());
       }
       for(SimTuple x: tuples) {
          for(int j=that.tuples.size()-1; ;j--) if (j<0) {ans.add(x);break;} else if (x.get(0)==that.tuples.get(j).get(0)) break;
       }
       for(SimTuple x: that.tuples) {
          ans.add(x);
       }
       return new SimTupleset(ans.makeConst());
    }

    /**
     * Return the union of this and that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: the tuples in the result will be ordered as follows:
     * first comes the tuples in "this" in original order,
     * then the tuples that are in "that" but not in "this".
     */
    public SimTupleset union(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return that;
       if (that.tuples.size()==0 || this.tuples.get(0).arity()!=that.tuples.get(0).arity()) return this;
       TempList<SimTuple> ans = null; // when null, it means we haven't found any new tuple to add yet
       for(SimTuple x: that.tuples) if (!has(x)) {
          if (ans == null) ans = new TempList<SimTuple>(tuples);
          ans.add(x);
       }
       return ans==null ? this : new SimTupleset(ans.makeConst());
    }

    /**
     * Return the union of this and that; (if this tupleset and that tuple does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: the tuples in the result will be ordered as follows:
     * first comes the tuples in "this" in original order,
     * then the new tuple (if it wasn't in this set)
     */
    public SimTupleset union(SimTuple that) {
       if (tuples.size()==0) return make(that);
       if (tuples.get(0).arity()!=that.arity() || has(that)) return this;
       TempList<SimTuple> ans = new TempList<SimTuple>(size()+1);
       for(int i=0, n=size(); i<n; i++) ans.add(tuples.get(i));
       ans.add(that);
       return new SimTupleset(ans.makeConst());
    }

    /**
     * Return this minus that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: The resulting tuples will keep their original order.
     */
    public SimTupleset difference(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return EMPTY;
       if (that.tuples.size()==0 || this.tuples.get(0).arity()!=that.tuples.get(0).arity()) return this;
       TempList<SimTuple> ans = null; // when null, it means we haven't found any old tuple to delete yet
       for(int i=0; i<tuples.size(); i++) {
          SimTuple x = tuples.get(i);
          if (that.has(x)) {
              if (ans==null) { ans = new TempList<SimTuple>(size()-1); for(int j=0; j<i; j++) ans.add(tuples.get(j)); }
          } else {
              if (ans!=null) ans.add(x);
          }
       }
       return ans==null ? this : (ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst()));
    }

    /**
     * Return this minus that; (if this tupleset and that tuple does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: The resulting tuples will keep their original order.
     */
    public SimTupleset difference(SimTuple that) {
       final int n = tuples.size();
       if (n==0) return EMPTY;
       if (tuples.get(0).arity()!=that.arity()) return this;
       for(int i=0; i<n; i++) {
          if (tuples.get(i).equals(that)) {
              if (n==1) return EMPTY;
              TempList<SimTuple> ans = new TempList<SimTuple>(n-1);
              for(int j=0; j<n; j++) if (j!=i) ans.add(tuples.get(j));
              return new SimTupleset(ans.makeConst());
          }
       }
       return this;
    }

    /**
     * Return this minus any tuple that contains the given atom.
     * <br/> Note: The resulting tuples will keep their original order.
     */
    public SimTupleset difference(SimAtom that) {
        if (tuples.size()==0) return EMPTY;
        TempList<SimTuple> ans = null; // when null, it means we haven't found any old tuple to delete yet
        for(int i=0; i<tuples.size(); i++) {
           SimTuple x = tuples.get(i);
           if (x.has(that)) {
               if (ans==null) { ans = new TempList<SimTuple>(size()-1); for(int j=0; j<i; j++) ans.add(tuples.get(j)); }
           } else {
               if (ans!=null) ans.add(x);
           }
        }
        return ans==null ? this : (ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst()));
    }

    /** Return the transpose of this tupleset; (if this tupleset's arity is not 2, we'll return an empty set instead) */
    public SimTupleset transpose() {
       if (tuples.size() == 0 || tuples.get(0).arity()!=2) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(tuples.size());
       for(SimTuple x: tuples) ans.add(SimTuple.make(x.tail(), x.head())); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans.makeConst());
    }

    /** Return the cartesian product of this and that. */
    public SimTupleset product(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(tuples.size() * that.tuples.size());
       for(SimTuple a: tuples) for(SimTuple b: that.tuples) {
          SimTuple c = a.product(b);
          ans.add(c); // We don't have to check for duplicates, because we assume every tuple in "this" has same arity, and every tuple in "that" has same arity
       }
       return new SimTupleset(ans.makeConst());
    }

    /** Return the relational join between this and that (throws ErrorType if this.arity==1 and that.arity==1) */
    public SimTupleset join(SimTupleset that) throws ErrorType {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       if (tuples.get(0).arity()==1 && that.tuples.get(0).arity()==1) throw new ErrorType("Cannot join two unary relations.");
       TempList<SimTuple> ans = new TempList<SimTuple>();
       for(SimTuple a: tuples) for(SimTuple b: that.tuples) if (a.tail()==b.head()) {
          SimTuple c = a.join(b);
          if (!ans.contains(c)) ans.add(c);
       }
       return ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst());
    }

    /** Return the intersection of this and that. */
    public SimTupleset intersect(SimTupleset that) {
       if (this==that) return this; else if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(size() < that.size() ? size() : that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) if (has(that.tuples.get(i))) ans.add(that.tuples.get(i));
       if (ans.size()==0) return EMPTY;
       if (ans.size()==this.tuples.size()) return this;
       if (ans.size()==that.tuples.size()) return that; else return new SimTupleset(ans.makeConst());
    }

    /** Return true if the intersection of this and that is nonempty. */
    public boolean intersects(SimTupleset that) {
       if (this==that) return tuples.size()>0;
       if (tuples.size()>0) for(int n=that.tuples.size(), i=0; i<n; i++) if (has(that.tuples.get(i))) return true;
       return false;
    }

    /** Returns this<:that (NOTE: if this.arity!=1, then we return the empty set) */
    public SimTupleset domain(SimTupleset that) {
       if (this.tuples.size()==0 || this.tuples.get(0).arity()!=1 || that.tuples.size()==0) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) {
          SimTuple x = that.tuples.get(i);
          SimAtom a = x.head();
          for(int j=this.tuples.size()-1; j>=0; j--) if (a==this.tuples.get(j).head()) { ans.add(x); break; }
       }
       return ans.size()==that.tuples.size() ? that : (ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst()));
    }

    /** Returns this:>that (NOTE: if that.arity!=1, then we return the empty set) */
    public SimTupleset range(SimTupleset that) {
       if (that.tuples.size()==0 || that.tuples.get(0).arity()!=1 || this.tuples.size()==0) return EMPTY;
       TempList<SimTuple> ans = new TempList<SimTuple>(this.size());
       for(int n=this.tuples.size(), i=0; i<n; i++) {
          SimTuple x = this.tuples.get(i);
          SimAtom a = x.tail();
          for(int j=that.tuples.size()-1; j>=0; j--) if (a==that.tuples.get(j).head()) { ans.add(x); break; }
       }
       return ans.size()==this.tuples.size() ? this : (ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst()));
    }

    /** Returns the closure of this tupleset (NOTE: if this.arity!=2, we will return an empty set) */
    public SimTupleset closure() {
       if (tuples.size()==0 || tuples.get(0).arity()!=2) return EMPTY;
       TempList<SimTuple> ar = new TempList<SimTuple>(tuples);
       while(true) {
          int n = ar.size();
          for(int i=0; i<n; i++) {
             SimTuple left = ar.get(i);
             if (left.head()==left.tail()) continue;      // whatever "right" is, "left.right" won't add any new tuple to the final answer
             for(int j=0; j<n; j++) if (i!=j) {           // whatever "left" is,  "left.left"  won't add any new tuple to the final answer
                SimTuple right = ar.get(j);
                if (right.head()==right.tail()) continue; // whatever "left" is,  "left.right" won't add any new tuple to the final answer
                if (left.tail()==right.head() && find(ar, left.head(), right.tail())<0) ar.add(SimTuple.make(left.head(), right.tail()));
             }
          }
          if (n == ar.size()) return ar.size()==tuples.size() ? this : new SimTupleset(ar.makeConst()); // if we went through the loop without making any change, we're done
       }
    }

    /** Return the set of tuples which begins with the given tuple (where we remove the "matching leading part") */
    public SimTupleset beginWith(SimTuple x) {
        if (tuples.size()==0 || tuples.get(0).arity()<=x.arity()) return EMPTY;
        TempList<SimTuple> ans = new TempList<SimTuple>();
        int shift = tuples.get(0).arity() - x.arity();
        again:
        for(SimTuple r: tuples) {
            for(int i=0; i<x.arity(); i++) if (r.get(i) != x.get(i)) continue again;
            ans.add(r.tail(shift));
        }
        return ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst());
    }

    /** Return the set of tuples which ends with the given tuple (where we remove the "matching trailing part") */
    public SimTupleset endWith(SimTuple x) {
        if (tuples.size()==0 || tuples.get(0).arity()<=x.arity()) return EMPTY;
        TempList<SimTuple> ans = new TempList<SimTuple>();
        int shift = tuples.get(0).arity() - x.arity();
        again:
        for(SimTuple r: tuples) {
            for(int i=0; i<x.arity(); i++) if (r.get(i+shift) != x.get(i)) continue again;
            ans.add(r.head(shift));
        }
        return ans.size()==0 ? EMPTY : new SimTupleset(ans.makeConst());
    }

    /**
     * Returns an arbitrary atom from an arbitrary tuple.
     * @throws - ErrorAPI if this tupleset is empty
     */
    public SimAtom getAtom() throws ErrorAPI {
        if (tuples.size()==0) throw new ErrorAPI("This tupleset is empty");
        return tuples.get(0).get(0);
    }

    /**
     * Returns an arbitrary tuple.
     * @throws - ErrorAPI if this tupleset is empty
     */
    public SimTuple getTuple() throws ErrorAPI {
        if (tuples.size()==0) throw new ErrorAPI("This tupleset is empty");
        return tuples.get(0);
    }

    /**
     * Returns a modifiable copy of the list of all i-th atom from all tuples in some arbitrary order (0 is first atom, 1 is second atom...)
     * @throws - ErrorAPI if this tupleset contains at least one tuple whose length is less than or equal to i
     */
    public List<SimAtom> getAllAtoms(int column) throws ErrorAPI {
        if (tuples.size()==0) return new ArrayList<SimAtom>(0);
        if (column<0 || column>=tuples.get(0).arity()) throw new ErrorAPI("This tupleset does not have an \""+column+"th\" column.");
        IdentityHashMap<SimAtom,Boolean> ans = new IdentityHashMap<SimAtom,Boolean>();
        for(SimTuple x: tuples) ans.put(x.get(column), Boolean.TRUE);
        return new ArrayList<SimAtom>(ans.keySet());
    }

    /** Return an iterator over all subset x of this where x.size<=1 */
    public Iterator<SimTupleset> loneOf() {
        return new Iterator<SimTupleset>() {
            private int i = (-1); // -1 if we haven't started yet; otherwise it is the next element to return
            public SimTupleset next() {
                if (i<0) {i++; return EMPTY;} else if (i>=size()) throw new NoSuchElementException();
                SimTupleset ans = make(tuples.get(i));
                i++;
                return ans;
            }
            public boolean hasNext() { return i < size(); }
            public void remove() { throw new UnsupportedOperationException(); }
        };
    }

    /** Return an iterator over all subset x of this where x.size==1 */
    public Iterator<SimTupleset> oneOf() {
        Iterator<SimTupleset> ans = loneOf();
        ans.next(); // here, we depend on our knowledge that loneOf() will always return the emptyset first, so we can skip it up front
        return ans;
    }

    /** Return an iterator over all subset x of this */
    public Iterator<SimTupleset> setOf() {
        return new Iterator<SimTupleset>() {
            private boolean in[] = new boolean[size()]; // indicates whether each tuple should appear in the upcoming tupleset; if null, it means no more results
            public SimTupleset next() {
                if (in==null) throw new NoSuchElementException();
                TempList<SimTuple> ans = new TempList<SimTuple>();
                for(int i=0; i<in.length; i++) if (in[i]) ans.add(tuples.get(i));
                for(int i=0; ; i++) if (i==size()) {in=null;break;} else if (!in[i]) {in[i]=true; break;} else {in[i]=false;}
                return new SimTupleset(ans.makeConst());
            }
            public boolean hasNext() { return in!=null; }
            public void remove() { throw new UnsupportedOperationException(); }
        };
    }

    /** Return an iterator over all subset x of this where x.size>=1 */
    public Iterator<SimTupleset> someOf() {
        Iterator<SimTupleset> ans = setOf();
        ans.next(); // here, we depend on our knowledge that setOf() will always return the emptyset first, so we can skip it up front
        return ans;
    }

    /** Return true if this is a total ordering over "elem", with "first" being the first element of the total order. */
    public boolean totalOrder(SimTupleset elem, SimTupleset first) throws ErrorAPI {
       if (elem.size()==0) return first.size()==0 && size()==0;
       if (elem.size()==1) return elem.arity()==1 && first.equals(elem) && size()==0;
       if (first.size()!=1 || first.arity()!=1 || elem.arity()!=1 || arity()!=2 || size()!=elem.size()-1) return false;
       SimAtom e = first.tuples.get(0).get(0);
       List<SimAtom> elems = elem.getAllAtoms(0);
       List<SimTuple> next = new ArrayList<SimTuple>(tuples);
       while(true) {
         // "e" must be in elems; remove it from elems
         for(int n=elems.size(), i=0; ; i++)
            if (i>=n) return false;
            else if (elems.get(i)==e) { elems.set(i, elems.get(n-1)); elems.remove(n-1); break; }
         // if "e" was the last element, then "next" must be empty as well
         if (elems.size()==0) return next.size()==0;
         // remove (e,e') from next and let e' be the new e
         // (if there was a cycle, we would eventually detect that since the repeated element would no longer be in "elems")
         for(int n=next.size(), i=0; ; i++)
            if (i>=n) return false;
            else if (e==next.get(i).head()) { e=next.get(i).tail(); next.set(i, next.get(n-1)); next.remove(n-1); break; }
       }
    }

    /** Returns the number of tuples in this tupleset. */
    public int size() { return tuples.size(); }
}
