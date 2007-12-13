/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
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
        return "<null,null,null>";
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
