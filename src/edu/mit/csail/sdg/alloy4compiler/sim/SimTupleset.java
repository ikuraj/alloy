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

    /** The list of tuples; each tuple must have arity 1 or above; must contain only same-arity tuples; all atoms in it must be canonical; must not contain duplicate tuples; everything (once inserted) must not be modified */
    private final List<Object[]> tuples;

    /** Construct an empty tupleset. */
    private SimTupleset() { this.tuples = new ArrayList<Object[]>(); }

    /** Construct a tupleset based on the given list (caller must never modify the list or its contents after this) */
    private SimTupleset(List<Object[]> tuples) { this.tuples = tuples; }

    /** Construct a tupleset containing the given tuple (caller must never modify the tuple or its contents after this) */
    private SimTupleset(Object[] tuple) { this.tuples = new ArrayList<Object[]>(1); this.tuples.add(tuple); }

    /** Make a tupleset containing the given atom */
    public static SimTupleset wrap(Object atom) { return new SimTupleset(new Object[]{canon(atom)}); }

    /** Make a tupleset containing the given list of tuples. */
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
       List<Object[]> ans = new ArrayList<Object[]>((max-min)>0 ? (max-min) : 16);
       Object MIN = canon(min);
       while(min<max) { Object ADD=canon(min+1); ans.add(new Object[]{MIN,ADD}); MIN=ADD; min++; }
       return new SimTupleset(ans);
    }

    /** Prebuilt tupleset containing no tuples. */
    public static final SimTupleset EMPTY = new SimTupleset();

    /** Prebuilt tupleset containing the atom Boolean.TRUE */
    public static final SimTupleset TRUE = wrap(Boolean.TRUE);

    /** Prebuilt tupleset containing the atom Boolean.FALSE */
    public static final SimTupleset FALSE = wrap(Boolean.FALSE);

    // Common operations

    /** Returns the index position if the list of tuples contains the tuple (a,b) (or return -1 if not found) */
    private static int find(List<Object[]> tuples, Object a, Object b) {
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x.length!=2) return -1; // we assume if this tupleset is nonempty, it will only contain tuples of same arity
          if (x[0]==a && x[1]==b) return i;
       }
       return -1;
    }

    /** Returns the index position if the list of tuples contains the given tuple (or return -1 if not found) */
    private static int find(List<Object[]> tuples, Object[] that) {
       for(int i=tuples.size()-1; i >= 0; i--) {
          Object[] x = tuples.get(i);
          if (x.length!=that.length) return -1; // we assume if this tupleset is nonempty, it will only contain tuples of same arity
          for(int j=x.length-1; ; j--) if (j<0) return i; else if (x[j]!=that[j]) break;
       }
       return -1;
    }

    /** Returns true if this tupleset contains the given tuple. */
    public boolean contains(Object[] that) {
       return find(tuples, that) >= 0;
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

    /** Sum up all the integer atoms in this tupleset (assuming this tupleset contains only unary tuples) */
    public int sum() {
       int ans = 0;
       for(Object[] t: tuples) if (t[0] instanceof Integer) ans += ((Integer)(t[0]));
       return ans;
    }

    /** Return a copy of this tupleset's list. */
    private List<Object[]> dup() { return new ArrayList<Object[]>(tuples); }

    /** Return the identity over this tupleset (assuming this tupleset contains only unary tuples) */
    public SimTupleset iden() throws ErrorType {
       if (tuples.size() == 0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(tuples.size());
       for(Object[] x: tuples) ans.add(new Object[]{x[0], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans);
    }

    /** Return the relational override of this and that. */
    public SimTupleset override(SimTupleset that) throws ErrorType {
       if (tuples.size()==0) return that; else if (that.tuples.size()==0) return this;
       List<Object[]> ans = new ArrayList<Object[]>();
       again:
       for(Object[] x: this.tuples) {
          for(int j=that.tuples.size()-1; j>=0; j--) if (x[0]==this.tuples.get(j)[0]) continue again;
          ans.add(x);
       }
       for(Object[] x: that.tuples) if (find(ans, x)<0) {
          ans.add(x);
       }
       return ans.size()==0 ? EMPTY : new SimTupleset(ans);
    }

    /** Return the union of this and that. */
    public SimTupleset union(SimTupleset that) throws ErrorType {
       if (this.tuples.size() < that.tuples.size()) return that.union(this);
       if (tuples.size()==0) return that; else if (that.tuples.size()==0) return this;
       List<Object[]> ans = null;
       for(Object[] x: that.tuples) if (!contains(x)) {
          if (ans == null) ans = dup();
          ans.add(x);
       }
       return ans==null ? this : new SimTupleset(ans);
    }

    /** Return the difference of this and that. */
    public SimTupleset difference(SimTupleset that) throws ErrorType {
       if (tuples.size()==0) return EMPTY; else if (that.tuples.size()==0) return this;
       List<Object[]> ans = null;
       for(Object[] x: that.tuples) {
          int i = (ans == null ? find(tuples, x) : find(ans, x));
          if (i>=0) {
             if (ans == null) ans = dup();
             ans.remove(i);
          }
       }
       return ans==null ? this : new SimTupleset(ans);
    }

    /** Return the transpose of this tupleset (assuming this tupleset contains only binary tuples) */
    public SimTupleset transpose() throws ErrorType {
       if (tuples.size() == 0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(tuples.size());
       for(Object[] x: tuples) ans.add(new Object[]{x[1], x[0]}); // since "this" has no duplicate tuples, "ans" will not have duplicate tuples either
       return new SimTupleset(ans);
    }

    /** Return the cartesian product of this and that. */
    public SimTupleset product(SimTupleset that) throws ErrorType {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       int mul = tuples.size() * that.tuples.size();
       List<Object[]> ans = new ArrayList<Object[]>(mul>0 ? mul : 16); // the capacity is only advisory
       for(Object[] a: tuples) for(Object[] b: that.tuples) {
          Object[] c = new Object[a.length + b.length];
          for(int i=0; i<a.length; i++) c[i] = a[i];
          for(int i=0; i<b.length; i++) c[i+a.length] = b[i];
          ans.add(c); // We don't have to check for duplicates, because we assume every tuple in "this" has same arity, and every tuple in "that" has same arity
       }
       return new SimTupleset(ans);
    }

    /** Return the relational join between this and that (assuming this.isEmpty() or that.isEmpty() or (this.arity + that.arity > 2)) */
    public SimTupleset join(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       Object[] z = null;
       List<Object[]> ans = null;
       for(Object[] x: tuples) for(Object[] y: that.tuples) if (x[x.length-1]==y[0]) {
          if (ans == null) ans = new ArrayList<Object[]>();
          if (z==null) z = new Object[x.length + y.length - 2]; // try to reuse the array if possible
          for(int i=0; i<x.length-1; i++) z[i]=x[i];
          for(int i=0; i<y.length-1; i++) z[i+x.length-1]=y[i+1];
          if (find(ans, z)<0) { ans.add(z); z=null; } // if we add "z" to the tupleset, we set z=null so that we don't reuse the old array
       }
       return ans==null ? EMPTY : new SimTupleset(ans);
    }

    /** Return the intersection of this and that. */
    public SimTupleset intersect(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(size() < that.size() ? size() : that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) if (contains(that.tuples.get(i))) ans.add(that.tuples.get(i));
       if (ans.size()==0) return EMPTY;
       if (ans.size()==this.tuples.size()) return this;
       if (ans.size()==that.tuples.size()) return that; else return new SimTupleset(ans);
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
       List<Object> elems = new ArrayList<Object>(elem.size()); for(int i=0; i<elem.size(); i++) elems.add(elem.tuples.get(i)[0]);
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
       List<Object[]> ans = new ArrayList<Object[]>(that.size());
       for(int n=that.tuples.size(), i=0; i<n; i++) {
          Object[] x = that.tuples.get(i);
          for(int j=this.tuples.size()-1; j>=0; j--) if (x[0]==this.tuples.get(j)[0]) { ans.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.size()==that.tuples.size() ? that : new SimTupleset(ans));
    }

    /** Returns this:>that (assuming that tupleset contains only unary tuples) */
    public SimTupleset range(SimTupleset that) {
       if (tuples.size()==0 || that.tuples.size()==0) return EMPTY;
       List<Object[]> ans = new ArrayList<Object[]>(this.size());
       for(int n=this.tuples.size(), i=0; i<n; i++) {
          Object[] x = this.tuples.get(i);
          for(int j=that.tuples.size()-1; j>=0; j--) if (x[x.length-1]==that.tuples.get(j)[0]) { ans.add(x); break; }
       }
       return ans==null ? EMPTY : (ans.size()==this.tuples.size() ? this : new SimTupleset(ans));
    }

    /** Returns the closure of this tupleset (assuming this tupleset contains only binary tuples) */
    public SimTupleset closure() throws ErrorType {
       if (tuples.size()==0) return EMPTY;
       List<Object[]> ar = dup();
       while(true) {
          int n = ar.size();
          for(int i=0; i<n; i++) {
             Object[] left = ar.get(i);
             if (left[0]==left[1]) continue;    // whatever "right" is, "left.right" won't add any new tuple to the final answer
             for(int j=0; j<n; j++) if (i!=j) { // left[0]!=left[1], thus "ar[i].ar[i]" won't add any new tuple to the final answer
                Object[] right = ar.get(j);
                if (right[0]==right[1]) continue; // whatever "left" is, "left.right" won't add any new tuple to the final answer
                if (left[1]==right[0] && find(ar, left[0], right[1])<0) ar.add(new Object[]{left[0], right[1]});
             }
          }
          if (n == ar.size()) return ar.size()==tuples.size() ? this : new SimTupleset(ar); // we went through the loop without making any change, so we're done
       }
    }

    /** Return the set of tuples which begins with the that.tuples.get(i) */
    public SimTupleset beginWith(SimTupleset that, int i) {
        Object[] x = that.tuples.get(i);
        List<Object[]> ans = new ArrayList<Object[]>();
        for(Object[] r: tuples) for(int a=0; ; a++) {
            if (a<x.length) { if (x[a]!=r[a]) break; else continue; }
            Object newtuple[] = new Object[r.length - a];
            for(int c=0; c<newtuple.length; c++) newtuple[c] = r[c + r.length - newtuple.length];
            ans.add(newtuple);
            break;
        }
        return new SimTupleset(ans);
    }

    /** Return the set of tuples which ends with the that.tuples.get(i) */
    public SimTupleset endWith(SimTupleset that, int i) {
        Object[] x = that.tuples.get(i);
        List<Object[]> ans = new ArrayList<Object[]>();
        for(Object[] r: tuples) for(int a=0, b=r.length-x.length; ; a++, b++) {
            if (a<x.length) { if (x[a]!=r[b]) break; else continue; }
            Object newtuple[] = new Object[r.length - a];
            for(int c=0; c<newtuple.length; c++) newtuple[c] = r[c];
            ans.add(newtuple);
            break;
        }
        return new SimTupleset(ans);
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

    public Iterator<SimTupleset> oneOf() {
        Iterator<SimTupleset> ans = loneOf();
        ans.next();
        return ans;
    }

    public Iterator<SimTupleset> loneOf() {
        return new Iterator<SimTupleset>() {
            private int i = (-1); // next element to dish out
            public SimTupleset next() {
                if (i<0) {i++; return SimTupleset.EMPTY;} else if (i>=size()) throw new NoSuchElementException(); else i++;
                return new SimTupleset(tuples.get(i-1));
            }
            public boolean hasNext() { return i < size(); }
            public void remove() { throw new UnsupportedOperationException(); }
        };
    }

    public Iterator<SimTupleset> someOf() {
        Iterator<SimTupleset> ans = setOf();
        ans.next();
        return ans;
    }

    public Iterator<SimTupleset> setOf() {
        return new Iterator<SimTupleset>() {
            private boolean eof = false;
            private boolean in[] = new boolean[size()]; // next element to dish out
            public SimTupleset next() {
                if (eof) throw new NoSuchElementException();
                List<Object[]> ans = new ArrayList<Object[]>();
                for(int i=0; i<size(); i++) if (in[i]) ans.add(tuples.get(i));
                for(int i=0; ; i++) if (i==size()) {eof=true;break;} else if (!in[i]) {in[i]=true; break;} else {in[i]=false;}
                return new SimTupleset(ans);
            }
            public boolean hasNext() { return !eof; }
            public void remove() { throw new UnsupportedOperationException(); }
        };
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
