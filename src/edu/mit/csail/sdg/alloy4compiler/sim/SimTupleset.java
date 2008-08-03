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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.WeakHashMap;
import edu.mit.csail.sdg.alloy4.ErrorAPI;

/** Immutable; represents a tupleset. */

public final class SimTupleset {

    /** This map is used to canonicalize String and Integer and Boolean objects; all other objects are compared via object identity instead. */
    private static final WeakHashMap<Object,WeakReference<Object>> map = new WeakHashMap<Object,WeakReference<Object>>();

    /** This method is used to canonicalize String and Integer and Boolean objects; all other objects are used as-is. */
    static Object canon(Object x) {
       if (x instanceof Boolean) return Boolean.TRUE.equals(x) ? Boolean.TRUE : Boolean.FALSE;
       if (!(x instanceof String || x instanceof Integer)) return x;
       WeakReference<Object> ans = map.get(x);
       if (ans != null) return ans.get();
       map.put(x, new WeakReference<Object>(x));
       return x;
    }

    /**
     * The list of tuples.
     * <br/> <b>Invariant:</b> Each tuple must have arity 1 or above.
     * <br/> <b>Invariant:</b> If nonempty, it must contain only same-arity tuples.
     * <br/> <b>Invariant:</b> All atoms in the tuples must be in their canonical form.
     * <br/> <b>Invariant:</b> It must not contain duplicate tuples.
     */
    private final List<Object[]> tuples;

    /** Construct a tupleset based on the given list (caller must never modify the list or its contents after this) */
    private SimTupleset(List<Object[]> tuples) { this.tuples = tuples; }

    /** Construct a tupleset containing the given tuple (caller must never modify the tuple or its contents after this) */
    private SimTupleset(Object[] tuple) { this.tuples = new ArrayList<Object[]>(1); this.tuples.add(tuple); }

    /** Make a tupleset containing the given atom */
    public static SimTupleset wrap(Object atom) { return new SimTupleset(new Object[]{canon(atom)}); }

    /** Make a tupleset containing the given list of tuples; this method will make a deep copy of the list and its tuple arrays. */
    public static SimTupleset make(List<Object[]> tuples) {
        if (tuples.size()==0) return EMPTY;
        List<Object[]> ans = new ArrayList<Object[]>(tuples.size());
        for(int i=0; i<tuples.size(); i++) {
           Object[] src = tuples.get(i);
           Object[] dst = new Object[src.length];
           for(int j=0; j<dst.length; j++) dst[j] = src[j];
           ans.add(dst);
        }
        return new SimTupleset(ans);
    }

    /** Returns the "next" relation among all integers between min and max. */
    public static SimTupleset next(int min, int max) {
       if (min>=max) return EMPTY;
       if ((max-min)<=0) throw new OutOfMemoryError(); // There are too many entries to be stored in an ArrayList
       List<Object[]> ans = new ArrayList<Object[]>(max-min);
       Object MIN = canon(min);
       while(min<max) { Object NEXT=canon(min+1); ans.add(new Object[]{MIN,NEXT}); MIN=NEXT; min++; }
       return new SimTupleset(ans);
    }

    /** The tupleset containing no tuples. */
    public static final SimTupleset EMPTY = new SimTupleset(new ArrayList<Object[]>(0));

    /** The tupleset containing the atom Boolean.TRUE */
    public static final SimTupleset TRUE = wrap(Boolean.TRUE);

    /** The tupleset containing the atom Boolean.FALSE */
    public static final SimTupleset FALSE = wrap(Boolean.FALSE);

    /**
     * Returns the index position if the list of tuples contains the tuple (a,b) (or return -1 if not found).
     * (Note: it assumes "a" and "b" have been canonicalized)
     */
    private static int find(List<Object[]> tuples, Object a, Object b) {
       if (tuples.size() == 0 || tuples.get(0).length != 2) return -1;
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x[0]==a && x[1]==b) return i;
       }
       return -1;
    }

    /**
     * Returns the index position if the list of tuples contains the given tuple (or return -1 if not found)
     * (Note: it assumes that[0...] have been canonicalized)
     */
    private static int find(List<Object[]> tuples, Object[] that) {
       if (tuples.size() == 0 || tuples.get(0).length != that.length) return -1;
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x==that) return i;
          for(int j=x.length-1; ; j--) if (j<0) return i; else if (x[j] != that[j]) break;
       }
       return -1;
    }

    /** Returns true if this tupleset contains the given tuple; NOTE: it assumes that[0..] have been canonicalized. */
    private boolean has(Object[] that)  { return find(tuples, that) >= 0; }

    /** Returns true if this contains the same tuples as that. */
    public boolean equal(SimTupleset that) {
       int n = this.tuples.size();
       if (n!=that.tuples.size()) return false; else if (n==0 || this==that) return true;
       if (this.tuples.get(0).length != that.tuples.get(0).length) return false;
       for(int i=0; i<n; i++) { Object[] x = this.tuples.get(i); if (!that.has(x)) return false; }
       return true; // since SimTupleset must not contain duplicates, and this.size()==that.size(), comparing one way is sufficient
    }

    /** Returns true if this is a subset of that. */
    public boolean in(SimTupleset that) {
       int n = this.tuples.size();
       if (n>that.tuples.size()) return false; else if (n==0 || this==that) return true;
       if (this.tuples.get(0).length != that.tuples.get(0).length) return false;
       for(int i=0; i<n; i++) { Object[] x = this.tuples.get(i); if (!that.has(x)) return false; }
       return true;
    }

    /** Return a shallow copy of this tupleset's list. */
    private List<Object[]> dup() { return new ArrayList<Object[]>(tuples); }

    /** Sum up all the integer atoms in this tupleset; (if this tupleset's arity is not 1, then we return 0) */
    public int sum() {
       if (tuples.size() == 0 || tuples.get(0).length != 1) return 0;
       int ans = 0;
       for(Object[] t: tuples) if (t[0] instanceof Integer) ans = ans + ((Integer)(t[0])).intValue();
       return ans;
    }

    /**
     * Return the identity over this tupleset; (if this tupleset's arity is not 1, then we return an emptyset)
     * <br/> Note: the result's tuple order is the same as this tupleset's tuple order.
     */
    public SimTupleset iden() {
       if (tuples.size() == 0 || tuples.get(0).length != 1) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(tuples.size());
       for(Object[] x: tuples) ans.add(new Object[]{x[0], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans);
    }

    /**
     * Return the relational override of this and that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: in general, the tuples may be ordered arbitrarily in the result.
     */
    public SimTupleset override(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return that;
       if (that.tuples.size()==0 || this.tuples.get(0).length!=that.tuples.get(0).length) return this;
       List<Object[]> ans = new ArrayList<Object[]>(this.tuples.size());
       if (that.tuples.size()==1) { // very common case, so let's optimize it
          Object[] y = that.tuples.get(0);
          for(Object[] x: this.tuples) { if (x[0]!=y[0]) ans.add(x); else if (that!=null) {ans.add(y); that=null;} }
          if (that!=null) ans.add(y);
          return new SimTupleset(ans);
       }
       for(Object[] x: this.tuples) {
          for(int j=that.tuples.size()-1; ;j--) if (j<0) {ans.add(x);break;} else if (x[0]==that.tuples.get(j)[0]) break;
       }
       for(Object[] x: that.tuples) {
          ans.add(x);
       }
       return new SimTupleset(ans);
    }

    /**
     * Return the union of this and that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: the tuples in the result will be ordered as follows:
     * first comes the tuples in "this" in original order,
     * then the tuples that are in "that" but not in "this".
     */
    public SimTupleset union(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return that;
       if (that.tuples.size()==0 || this.tuples.get(0).length!=that.tuples.get(0).length) return this;
       List<Object[]> ans = null; // when null, it means we haven't found any new tuple to add yet
       for(Object[] x: that.tuples) if (!has(x)) {
          if (ans == null) ans = dup();
          ans.add(x);
       }
       return ans==null ? this : new SimTupleset(ans);
    }

    /**
     * Return this minus that; (if this tupleset and that tupleset does not have compatible arity, then we return this tupleset as is).
     * <br/> Note: The resulting tuples will keep their original order.
     */
    public SimTupleset difference(SimTupleset that) {
       if (this.tuples.size()==0 || this==that) return EMPTY;
       if (that.tuples.size()==0 || this.tuples.get(0).length!=that.tuples.get(0).length) return this;
       List<Object[]> ans = null; // when null, it means we haven't found any old tuple to delete yet
       for(int i=0; i<tuples.size(); i++) {
          Object[] x = tuples.get(i);
          if (that.has(x)) {
              if (ans==null) { ans = new ArrayList<Object[]>(size()); for(int j=0; j<i; j++) ans.add(tuples.get(j)); }
          } else {
              if (ans!=null) ans.add(x);
          }
       }
       return ans==null ? this : new SimTupleset(ans);
    }

    /** Return the transpose of this tupleset; (if this tupleset's arity is not 2, we'll return an empty set instead) */
    public SimTupleset transpose() {
       if (tuples.size() == 0 || tuples.get(0).length!=2) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(tuples.size());
       for(Object[] x: tuples) ans.add(new Object[]{x[1], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans);
    }

    /** Return the cartesian product of this and that. */
    public SimTupleset product(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       int mul = tuples.size() * that.tuples.size();
       if (mul <= 0) throw new OutOfMemoryError(); // There are too many entries to be stored in an ArrayList
       List<Object[]> ans = new ArrayList<Object[]>(mul);
       for(Object[] a: tuples) for(Object[] b: that.tuples) {
          Object[] c = new Object[a.length + b.length];
          for(int i=0; i<a.length; i++) c[i] = a[i];
          for(int i=0; i<b.length; i++) c[i+a.length] = b[i];
          ans.add(c); // We don't have to check for duplicates, because we assume every tuple in "this" has same arity, and every tuple in "that" has same arity
       }
       return new SimTupleset(ans);
    }

    /** Return the relational join between this and that (if this.arity==1 and that.arity==1 then we return the empty set) */
    public SimTupleset join(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0 || (this.tuples.get(0).length==1 && that.tuples.get(0).length==1)) return EMPTY;
       Object[] c = null;
       List<Object[]> ans = new ArrayList<Object[]>();
       for(Object[] a: tuples) for(Object[] b: that.tuples) if (a[a.length-1] == b[0]) {
          if (c==null) c = new Object[a.length + b.length - 2]; // try to reuse the array if possible
          for(int i=0; i<a.length-1; i++) c[i] = a[i];
          for(int i=0; i<b.length-1; i++) c[i+a.length-1] = b[i+1];
          if (find(ans, c)<0) { ans.add(c); c=null; } // if we add "c" to the tupleset, we set c=null so that we don't reuse the old array
       }
       return ans.size()==0 ? EMPTY : new SimTupleset(ans);
    }

    /** Return the intersection of this and that. */
    public SimTupleset intersect(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       if (this==that) return this;
       List<Object[]> ans = new ArrayList<Object[]>(size() < that.size() ? size() : that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) if (has(that.tuples.get(i))) ans.add(that.tuples.get(i));
       if (ans.size()==0) return EMPTY;
       if (ans.size()==this.tuples.size()) return this;
       if (ans.size()==that.tuples.size()) return that; else return new SimTupleset(ans);
    }

    /** Return true if the intersection of this and that is nonempty. */
    public boolean intersects(SimTupleset that) {
       if (this==that) return tuples.size()>0;
       if (tuples.size()>0) for(int n=that.tuples.size(), i=0; i<n; i++) if (has(that.tuples.get(i))) return true;
       return false;
    }

    /** If this tupleset is empty, then return 0, else return the arity of every tuple in this tupleset. */
    public int arity() {
       if (tuples.size()==0) return 0; else return tuples.get(0).length;
    }

    /** Return true if this is a total ordering over "elem", with "first" being the first element of the total order. */
    public boolean totalOrder(SimTupleset elem, SimTupleset first) {
       if (elem.size()==0) return first.size()==0 && size()==0;
       if (elem.size()==1) return elem.arity()==1 && first.equal(elem) && size()==0;
       if (first.size()!=1 || first.arity()!=1 || elem.arity()!=1 || arity()!=2 || size()!=elem.size()-1) return false;
       Object e = first.tuples.get(0)[0];
       List<Object> elems = new ArrayList<Object>(elem.size()); for(int i=0; i<elem.size(); i++) elems.add(elem.tuples.get(i)[0]);
       List<Object[]> next = new ArrayList<Object[]>(tuples);
       while(true) {
          // "e" must be in elems; if so, remove it from elems
          for(int n=elems.size(), i=0; ; i++) if (i>=n) return false; else if (elems.get(i)==e) { elems.set(i, elems.get(n-1)); elems.remove(n-1); break; }
          // if "e" was the last element, then "next" must be empty as well
          if (elems.size()==0) return next.size()==0;
          // remove (e,e') from next and let e become e'  (if there was a cycle, we would eventually detect that since the repeated element would no longer be in "elems")
          for(int n=next.size(), i=0; ; i++) if (i>=n) return false; else if (next.get(i)[0]==e) { e=next.get(i)[1]; next.set(i, next.get(n-1)); next.remove(n-1); break; }
       }
    }

    /** Returns this<:that (NOTE: if this.arity!=1, then we return the empty set) */
    public SimTupleset domain(SimTupleset that) {
       if (this.tuples.size()==0 || this.tuples.get(0).length!=1 || that.tuples.size()==0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) {
          Object[] x = that.tuples.get(i);
          for(int j=this.tuples.size()-1; j>=0; j--) if (x[0]==this.tuples.get(j)[0]) { ans.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.size()==that.tuples.size() ? that : new SimTupleset(ans));
    }

    /** Returns this:>that (NOTE: if that.arity!=1, then we return the empty set) */
    public SimTupleset range(SimTupleset that) {
       if (that.tuples.size()==0 || that.tuples.get(0).length!=1 || this.tuples.size()==0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(this.size());
       for(int n=this.tuples.size(), i=0; i<n; i++) {
          Object[] x = this.tuples.get(i);
          for(int j=that.tuples.size()-1; j>=0; j--) if (x[x.length-1]==that.tuples.get(j)[0]) { ans.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.size()==this.tuples.size() ? this : new SimTupleset(ans));
    }

    /** Returns the closure of this tupleset (NOTE: if this.arity!=2, we will return an empty set) */
    public SimTupleset closure() {
       if (tuples.size()==0 || tuples.get(0).length!=2) return EMPTY;
       List<Object[]> ar = dup();
       while(true) {
          int n = ar.size();
          for(int i=0; i<n; i++) {
             Object[] left = ar.get(i);
             if (left[0]==left[1]) continue;      // whatever "right" is, "left.right" won't add any new tuple to the final answer
             for(int j=0; j<n; j++) if (i!=j) {   // whatever "left" is,  "left.left"  won't add any new tuple to the final answer
                Object[] right = ar.get(j);
                if (right[0]==right[1]) continue; // whatever "left" is,  "left.right" won't add any new tuple to the final answer
                if (left[1]==right[0] && find(ar, left[0], right[1])<0) ar.add(new Object[]{left[0], right[1]});
             }
          }
          if (n == ar.size()) return ar.size()==tuples.size() ? this : new SimTupleset(ar); // we went through the loop without making any change, so we're done
       }
    }

    /** Return the set of tuples which begins with the that.tuples.get(i) (where we remove the "matching leading part") */
    public SimTupleset beginWith(SimTupleset that, int i) {
        Object[] x = that.tuples.get(i);
        if (tuples.size()==0 || tuples.get(0).length<=x.length) return EMPTY;
        List<Object[]> ans = new ArrayList<Object[]>();
        for(Object[] r: tuples) for(int a=0; ; a++) {
            if (a<x.length) { if (x[a]!=r[a]) break; else continue; }
            Object newtuple[] = new Object[r.length - a];
            for(int c=0; c<newtuple.length; c++) newtuple[c] = r[c+a];
            ans.add(newtuple);
            break;
        }
        if (ans.size()==0) return EMPTY; else return new SimTupleset(ans);
    }

    /** Return the set of tuples which ends with the that.tuples.get(i) (where we remove the "matching trailing part") */
    public SimTupleset endWith(SimTupleset that, int i) {
        Object[] x = that.tuples.get(i);
        if (tuples.size()==0 || tuples.get(0).length<=x.length) return EMPTY;
        List<Object[]> ans = new ArrayList<Object[]>();
        for(Object[] r: tuples) for(int a=0, b=r.length-x.length; ; a++, b++) {
            if (a<x.length) { if (x[a]!=r[b]) break; else continue; }
            Object newtuple[] = new Object[r.length - a];
            for(int c=0; c<newtuple.length; c++) newtuple[c] = r[c];
            ans.add(newtuple);
            break;
        }
        if (ans.size()==0) return EMPTY; else return new SimTupleset(ans);
    }

    /**
     * Returns an arbitrary atom from an arbitrary tuple.
     * @throws - ErrorAPI if this tupleset is empty
     */
    public Object getAtom() throws ErrorAPI {
        if (tuples.size()==0) throw new ErrorAPI("This tupleset is empty");
        return tuples.get(0)[0];
    }

    /**
     * Returns the list of all i-th atom from all tuples in some arbitrary order (0 is first atom, 1 is second atom...)
     * @throws - ErrorAPI if this tupleset contains at least one tuple whose length is less than or equal to i
     */
    public List<Object> getAllAtoms(int column) throws ErrorAPI {
        if (tuples.size()==0) return new ArrayList<Object>(0);
        if (column<0 || column>=tuples.get(0).length) throw new ErrorAPI("This tupleset does not have an \""+column+"th\" column.");
        IdentityHashMap<Object,Object> ans = new IdentityHashMap<Object,Object>();
        for(Object[] x: tuples) ans.put(x[column], Boolean.TRUE);
        List<Object> list = new ArrayList<Object>(ans.size());
        for(Object x: ans.keySet()) list.add(x);
        return list;
    }

    /** Returns the number of tuples in this tupleset. */
    public int size() { return tuples.size(); }

    /** Return an iterator over all subset x of this where x.size<=1 */
    public Iterator<SimTupleset> loneOf() {
        return new Iterator<SimTupleset>() {
            private int i = (-1); // -1 if we haven't started yet; otherwise it is the next element to return
            public SimTupleset next() {
                if (i<0) {i++; return EMPTY;} else if (i>=size()) throw new NoSuchElementException();
                SimTupleset ans = new SimTupleset(tuples.get(i));
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
                List<Object[]> ans = new ArrayList<Object[]>();
                for(int i=0; i<in.length; i++) if (in[i]) ans.add(tuples.get(i));
                for(int i=0; ; i++) if (i==size()) {in=null;break;} else if (!in[i]) {in[i]=true; break;} else {in[i]=false;}
                return new SimTupleset(ans);
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

    /** {@inheritDoc} */
    @Override public String toString() {
       StringBuilder sb = null;
       for(Object[] x: tuples) {
          if (sb==null) sb = new StringBuilder("{ "); else sb.append(", ");
          for(int i=0; i<x.length; i++) {
             if (i>0) sb.append("->");
             sb.append(x[i]);
          }
       }
       return sb==null ? "{ }" : (sb.append(" }").toString());
    }
}
