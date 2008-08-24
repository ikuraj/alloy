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
import java.util.WeakHashMap;

/**
 * Immutable; represents an atom.
 *
 * <p> Outside of this class, we guarantee for any SimAtom x and y, then "x.equals(y) iff x==y".
 * <br> Even though that means "equals()" and "==" are equivalent,
 * <br> the equals() method is much slower than "==" for SimAtom,
 * <br> so you should always try to use "==" on SimAtom.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class SimAtom {

    /** This map is used to canonicalize the atoms. */
    private static final WeakHashMap<SimAtom,WeakReference<SimAtom>> map = new WeakHashMap<SimAtom,WeakReference<SimAtom>>();

    /** The String label for the atom; all distinct atoms have distinct labels. */
    private final String string;

    /** Construct a SimAtom; this constructor must only be called by make() since we want to canonicalize all SimAtom instances out there. */
    private SimAtom(String x) { this.string = x; }

    /** Construct a SimAtom for the given label, or if an existing SimAtom hasn't been garbage collected yet then return that instead. */
    public static SimAtom make(String label) {
        synchronized(map) {
            SimAtom x = new SimAtom(label);
            WeakReference<SimAtom> ans = map.get(x);
            if (ans != null) { SimAtom y = ans.get(); if (y!=null) return y; }
            map.put(x, new WeakReference<SimAtom>(x));
            return x;
        }
    }

    /**
     * If the atom starts with "-" or "0-9" then convert it into a 32-bit int (assuming that it came from a 32-bit int)
     * <p>
     * If the atom does not start with "-" or "0-9", then return 0.
     */
    public int toInt() {
        int ans=0, i=0, n=string.length();
        if (n==0) return 0;
        if (string.charAt(0)=='-') i++;
        if (string.charAt(i)<'0' || string.charAt(i)>'9') return 0;
        for(;i<n;i++) ans = ans*10 + (string.charAt(i) - '0');
        if (string.charAt(0)=='-') return (-ans); else return ans;
        // Due to Java's 2's complement arithmetic, this above will successfully
        // convert all integers ranging from Integer.MIN to Integer.MAX
    }

    /** {@inheritDoc} */
    @Override public boolean equals(Object that) {
        if (this==that) return true;
        if (!(that instanceof SimAtom)) return false; else return ((SimAtom)that).string.equals(string);
    }

    /** {@inheritDoc} */
    @Override public int hashCode() { return string.hashCode(); }

    /** {@inheritDoc} */
    @Override public String toString() { return string; }
}
