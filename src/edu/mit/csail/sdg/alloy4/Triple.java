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

package edu.mit.csail.sdg.alloy4;

import java.io.Serializable;

/**
 * Immutable; stores a triple of object references; Triple.equals() compares by value.
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Triple<A,B,C> implements Serializable {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The first part of the triple. */
    public final A a;

    /** The second part of the triple. */
    public final B b;

    /** The third part of the triple. */
    public final C c;

    /** Constructs a new Triple object (a,b,c). */
    public Triple(A a, B b, C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    /** If any or all of "a", "b", "c" are String, we'll return it as-is; otherwise, we call toString() on them. */
    @Override public String toString() {
        if (a instanceof String) {
            if (b instanceof String && c instanceof String) return ((String)a)+" "+b+" "+c;
            if (b instanceof String) return ((String)a)+" "+b;
            if (c instanceof String) return ((String)a)+" "+c;
            return (String)a;
        }
        if (b instanceof String) {
            if (c instanceof String) return ((String)b)+" "+c; else return (String)b;
        }
        if (c instanceof String) {
            return ((String)c);
        }
        if (a!=null) {
            if (b!=null && c!=null) return a.toString()+" "+b+" "+c;
            if (b!=null) return a.toString()+" "+b;
            if (c!=null) return a.toString()+" "+c;
            return a.toString();
        }
        if (b!=null) {
            if (c!=null) return b.toString()+" "+c; else return b.toString();
        }
        if (c!=null) {
            return c.toString();
        }
        return "";
    }

    /** Triples (a1,b1,c1) and (a2,b2,c2) are equal iff a1.equals(a2) and b1.equals(b2) and c1.equals(c2). */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Triple)) return false;
        Triple<?,?,?> p=(Triple<?,?,?>)that;
        return (a==null ? p.a==null : a.equals(p.a))
            && (b==null ? p.b==null : b.equals(p.b))
            && (c==null ? p.c==null : c.equals(p.c));
    }

    /** Returns a hashcode based on (a==null?0:a.hashCode()), (b==null?0:b.hashCode()), and (c==null?0:c.hashCode()). */
    @Override public int hashCode() {
        int i = (a==null) ? 0 : a.hashCode();
        int j = (b==null) ? 0 : b.hashCode();
        int k = (c==null) ? 0 : c.hashCode();
        return i*173123 + j*131 + k;
    }
}
