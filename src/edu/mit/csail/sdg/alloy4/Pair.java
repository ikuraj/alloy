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
 * Immutable; stores a pair of object references; Pair.equals() compares by value.
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Pair<A,B> implements Serializable {

    /** This ensures the class can be serialized reliably. */
    private static final long serialVersionUID = 1L;

    /** The first half of the pair. */
    public final A a;

    /** The second half of the pair. */
    public final B b;

    /** Constructs a new Pair object (a,b). */
    public Pair(A a, B b) {
        this.a = a;
        this.b = b;
    }

    /** If either or both "a" and "b" are String, we'll return it as-is; otherwise, we call toString() on them. */
    @Override public String toString() {
        if (a instanceof String) {
            if (b instanceof String) return ((String)a)+" "+b;
            return (String)a;
        }
        if (b instanceof String) {
            return (String)b;
        }
        if (a==null) {
            return (b!=null) ? b.toString() : "<null,null>";
        } else {
            return (b!=null) ? (a.toString()+" "+b) : a.toString();
        }
    }

    /** Returns a hashcode based on (a==null?0:a.hashCode()) and (b==null?0:b.hashCode()). */
    @Override public int hashCode() {
        int i = (a==null) ? 0 : a.hashCode();
        int j = (b==null) ? 0 : b.hashCode();
        return i*173123+j;
    }

    /** Pairs (a1,b1) and (a2,b2) are equal iff a1.equals(a2) and b1.equals(b2). */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof Pair)) return false;
        Pair<?,?> p=(Pair<?,?>)that;
        return (a==null ? p.a==null : a.equals(p.a)) && (b==null ? p.b==null : b.equals(p.b));
    }
}
