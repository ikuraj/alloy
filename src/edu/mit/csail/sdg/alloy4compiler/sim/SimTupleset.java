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
import java.util.List;
import java.util.WeakHashMap;
import edu.mit.csail.sdg.alloy4.ErrorType;

/** Immutable; represents a tupleset. */

public final class SimTupleset {

    /** This map is used to canonicalize String and Integer and Boolean objects; all other objects are compared via object identity instead. */
    private static final WeakHashMap<Object,WeakReference<Object>> map = new WeakHashMap<Object,WeakReference<Object>>();

    /** This method is used to canonicalize String and Integer and Boolean objects; all other objects are used as-is. */
    static Object canon(Object x) {
       if (x instanceof Boolean) return Boolean.TRUE.equals(x) ? Boolean.TRUE : Boolean.FALSE;
       if (!(x instanceof String || x instanceof Integer)) return x;
       WeakReference<Object> ans = map.get(x);
       if (ans==null) map.put(x, new WeakReference<Object>(x)); else x=ans.get();
       return x;
    }

    /** The list of tuples; each tuple must have arity 1 or above; must contain only same-arity tuples; all atoms in it must be canonical; must not contain duplicate tuples; each tuple (once inserted) must NEVER be modified. */
    final ArrayList<Object[]> tuples;

    /** Construct an empty tupleset. */
    public SimTupleset() { tuples = new ArrayList<Object[]>(); }

    /** Construct an empty tupleset with the given estimated capacity.. */
    private SimTupleset(int capacity) { tuples = new ArrayList<Object[]>((capacity>0) ? capacity : 0); }

    /** Returns a tupleset containing the given atom */
    public static SimTupleset wrap(Object atom) { SimTupleset ans = new SimTupleset(1);  ans.tuples.add(new Object[]{canon(atom)});  return ans; }

    /** Returns the "next" relation among all integers between min and max. */
    public static SimTupleset next(int min, int max) {
       if (min>=max) return EMPTY;
       SimTupleset ans = new SimTupleset(max-min);
       Object MIN = canon(min);
       while(min<max) { Object ADD=canon(min+1); ans.tuples.add(new Object[]{MIN,ADD}); MIN=ADD; min++; }
       return ans;
    }

    /** Prebuilt tupleset containing no tuples. */
    public static final SimTupleset EMPTY = new SimTupleset(0);

    /** Prebuilt tupleset containing the atom Boolean.TRUE */
    public static final SimTupleset TRUE = wrap(Boolean.TRUE);

    /** Prebuilt tupleset containing the atom Boolean.FALSE */
    public static final SimTupleset FALSE = wrap(Boolean.FALSE);

    // Common operations

    /** Returns the index position if this tupleset contains the given tuple (or return -1 if not found) */
    private int find(Object[] that) {
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x.length!=that.length) return -1; // we assume if this tupleset is nonempty, it will only contain tuples of same arity
          for(int j=x.length-1; ; j--) if (j<0) return i; else if (x[j]!=that[j]) break;
       }
       return -1;
    }

    /** Returns true if this tupleset contains the given tuple. */
    public boolean contains(Object[] that) {
       return find(that) >= 0;
    }

    /** Returns true if this tupleset contains the given tuple (a, b). */
    private boolean contains(Object a, Object b) {
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x.length!=2) return false; // we assume if this tupleset is nonempty, it will only contain tuples of same arity
          if (x[0]==a && x[1]==b) return true;
       }
       return false;
    }

    /** Return a copy of this tupleset; this is private since SimTupleset objects are immutable outside of this class. */
    private SimTupleset dup() {
       SimTupleset ans = new SimTupleset(tuples.size());
       ans.tuples.addAll(tuples);
       return ans;
    }

    /** Sum up all the integer atoms in this tupleset. */
    public int sum() {
       int ans = 0;
       for(Object[] t: tuples) { if (t.length==1 && t[0] instanceof Integer) ans += ((Integer)(t[0])); }
       return ans;
    }

    /** Return the identity over this tupleset (assuming this tupleset contains only unary tuples) */
    public SimTupleset iden() throws ErrorType {
       if (tuples.size() == 0) return EMPTY;
       SimTupleset ans = new SimTupleset(tuples.size());
       for(Object[] x: tuples) ans.tuples.add(new Object[]{x[0], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return ans;
    }

    /** Returns true if this equals that. */
    public boolean eq(SimTupleset that) {
       int n = this.tuples.size();
       if (n!=that.tuples.size()) return false; else if (n==0) return true;
       if (this.tuples.get(0).length != that.tuples.get(0).length) return false;
       for(int i=0; i<n; i++) { Object[] x = this.tuples.get(i); if (!that.contains(x)) return false; }
       return true; // since SimTupleset must not contain duplicates, and this.size()==that.size(), comparing one way is sufficient
    }

    /** Returns true if this is a subset of that. */
    public boolean in(SimTupleset that) {
       int n = this.tuples.size();
       if (n>that.tuples.size()) return false; else if (n==0) return true;
       if (this.tuples.get(0).length != that.tuples.get(0).length) return false;
       for(int i=0; i<n; i++) { Object[] x = this.tuples.get(i); if (!that.contains(x)) return false; }
       return true;
    }

    /** Return the relational override of this and that. */
    public SimTupleset override(SimTupleset that) throws ErrorType {
       if (tuples.size()==0) return that; else if (that.tuples.size()==0) return this;
       SimTupleset ans = new SimTupleset();
       again:
       for(Object[] x: this.tuples) {
          for(int j=that.tuples.size()-1; j>=0; j--) if (x[0]==this.tuples.get(j)[0]) continue again;
          ans.tuples.add(x);
       }
       for(Object[] x: that.tuples) if (!ans.contains(x)) {
          ans.tuples.add(x);
       }
       return ans.tuples.size()==0 ? EMPTY : ans;
    }

    /** Return the union of this and that. */
    public SimTupleset union(SimTupleset that) throws ErrorType {
       if (this.tuples.size() < that.tuples.size()) return that.union(this);
       if (tuples.size()==0) return that; else if (that.tuples.size()==0) return this;
       SimTupleset ans = null;
       for(Object[] x: that.tuples) if (!contains(x)) {
          if (ans == null) ans = dup();
          ans.tuples.add(x);
       }
       return ans==null ? this : ans;
    }

    /** Return the difference of this and that. */
    public SimTupleset difference(SimTupleset that) throws ErrorType {
       if (tuples.size()==0) return EMPTY; else if (that.tuples.size()==0) return this;
       SimTupleset ans = null;
       for(Object[] x: that.tuples) {
          int i = (ans == null ? this.find(x) : ans.find(x));
          if (i>=0) {
             if (ans == null) ans = dup();
             ans.tuples.remove(i);
          }
       }
       return ans==null ? this : ans;
    }

    /** Return the transpose of this tupleset (assuming this tupleset contains only binary tuples) */
    public SimTupleset transpose() throws ErrorType {
       if (tuples.size() == 0) return EMPTY;
       SimTupleset ans = new SimTupleset(tuples.size());
       for(Object[] x: tuples) ans.tuples.add(new Object[]{x[1], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return ans;
    }

    /** Return the cartesian product of this and that. */
    public SimTupleset product(SimTupleset that) throws ErrorType {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       SimTupleset ans = new SimTupleset(tuples.size() * that.tuples.size()); // overflow is okay; the capacity is only advisory
       for(Object[] a: tuples) for(Object[] b: that.tuples) {
          Object[] c = new Object[a.length + b.length];
          for(int i=0; i<a.length; i++) c[i]=a[i];
          for(int i=0; i<b.length; i++) c[i+a.length]=b[i];
          ans.tuples.add(c); // We don't have to check for duplicates, because we assume every tuple in "this" has same arity, and every tuple in "that" has same arity
       }
       return ans;
    }

    /** Return the relational join between this and that (assuming this.isEmpty() or that.isEmpty() or (this.arity + that.arity > 2)) */
    public SimTupleset join(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       Object[] z = null;
       SimTupleset ans = null;
       for(Object[] x: tuples) for(Object[] y: that.tuples) if (x[x.length-1]==y[0]) {
          if (ans==null) ans = new SimTupleset();
          if (z==null) z = new Object[x.length + y.length - 2]; // try to reuse the array if possible
          for(int i=0; i<x.length-1; i++) z[i]=x[i];
          for(int i=0; i<y.length-1; i++) z[i+x.length-1]=y[i+1];
          if (!ans.contains(z)) { ans.tuples.add(z); z=null; } // if we add "z" to the tupleset, we set z=null so that we don't reuse the old array
       }
       return ans==null ? EMPTY : ans;
    }

    /** Return the intersection of this and that. */
    public SimTupleset intersect(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       SimTupleset ans = new SimTupleset(size() < that.size() ? size() : that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) if (contains(that.tuples.get(i))) ans.tuples.add(that.tuples.get(i));
       if (ans.tuples.size()==0) return EMPTY;
       if (ans.tuples.size()==this.tuples.size()) return this;
       if (ans.tuples.size()==that.tuples.size()) return that; else return ans;
    }

    /** Return true if the intersection of this and that is nonempty. */
    public boolean intersects(SimTupleset that) {
       if (tuples.size()>0) for(int n=that.tuples.size(), i=0; i<n; i++) if (contains(that.tuples.get(i))) return true;
       return false;
    }

    /** If this tupleset is empty, then return 0, else return the arity of every tuple in this tupleset. */
    public int arity() {
       if (tuples.size()==0) return 0; else return tuples.get(0).length;
    }

    /** Return true if this is a total ordering over "elem" */
    public boolean totalOrder(SimTupleset elem, SimTupleset first) throws ErrorType {
       if (elem.size()==0) return first.size()==0 && size()==0;
       if (elem.size()==1) return elem.arity()==1 && first.eq(elem) && size()==0;
       if (first.size()!=1 || first.arity()!=1 || elem.arity()!=1 || arity()!=2 || size()!=elem.size()-1) return false;
       Object e = first.tuples.get(0)[0];
       List<Object> elems = new ArrayList<Object>(elem.size()); for(int i=0; i<elem.size(); i++) elems.add(elem.tuples.get(0)[0]);
       List<Object[]> next = new ArrayList<Object[]>(tuples);
       while(true) {
          // "e" must be in elems; if so, remove it from elems
          for(int n=elems.size(), i=0; ; i++) if (i>=n) return false; else if (elems.get(i)==e) { elems.set(i, elems.get(n-1)); elems.remove(n-1); break; }
          // if "e" was the last element, then "next" must be empty as well
          if (next.size()==0 || elems.size()==0) return next.size()==0 && elems.size()==0;
          // remove (e,e') from next and let e become e'  (if there was a cycle, we would eventually detect that since the repeated element would no longer be in "elems")
          for(int n=next.size(), i=0; ; i++) if (i>=n) return false; else if (next.get(i)[0]==e) { e=next.get(i)[1]; next.set(i, next.get(n-1)); next.remove(n-1); break; }
       }
    }

    /** Returns this<:that (assuming this tupleset contains only unary tuples) */
    public SimTupleset domain(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       SimTupleset ans = new SimTupleset(that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) {
          Object[] x = that.tuples.get(i);
          for(int j=this.tuples.size()-1; j>=0; j--) if (x[0]==this.tuples.get(j)[0]) { ans.tuples.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.tuples.size()==that.tuples.size() ? that : ans);
    }

    /** Returns this:>that (assuming that tupleset contains only unary tuples) */
    public SimTupleset range(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       SimTupleset ans = new SimTupleset(this.size());
       for(int n=this.tuples.size(), i=0; i<n; i++) {
          Object[] x = this.tuples.get(i);
          for(int j=that.tuples.size()-1; j>=0; j--) if (x[x.length-1]==that.tuples.get(j)[0]) { ans.tuples.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.tuples.size()==this.tuples.size() ? this : ans);
    }

    /** Returns the closure of this tupleset (assuming this tupleset contains only binary tuples) */
    public SimTupleset closure() throws ErrorType {
       if (tuples.size()==0) return EMPTY;
       SimTupleset ans = dup();
       ArrayList<Object[]> ar = ans.tuples;
       while(true) {
          int n = ar.size();
          for(int i=0; i<n; i++) {
             Object[] left = ar.get(i);
             if (left[0]==left[1]) continue;    // whatever "right" is, "left.right" won't add any new tuple to the final answer
             for(int j=0; j<n; j++) if (i!=j) { // left[0]!=left[1], thus "ar[i].ar[i]" won't add any new tuple to the final answer
                Object[] right = ar.get(j);
                if (right[0]==right[1]) continue; // whatever "left" is, "left.right" won't add any new tuple to the final answer
                if (left[1]==right[0] && !ans.contains(left[0], right[1])) ans.tuples.add(new Object[]{left[0], right[1]});
             }
          }
          if (n == ar.size()) return ans.tuples.size()==tuples.size() ? this : ans; // we went through the loop without making any change, so we're done
       }
    }

    /** Returns the number of tuples in this tupleset. */
    public int size() { return tuples.size(); }

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
