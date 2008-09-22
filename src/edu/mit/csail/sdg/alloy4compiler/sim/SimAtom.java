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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
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

    /** Preconstructed atom representing emptystring. */
    public static final SimAtom EMPTYSTRING = make("");

    /** Preconstructed atom representing 0. */
    public static final SimAtom ZERO = make("0");

    /** Preconstructed atom representing 1. */
    public static final SimAtom ONE = make("1");

    /* Read a SimAtom from the given input file by reading its unique ID (and consulting the uniqueID map)
    static SimAtom readi(DataInputStream in, HashMap<Integer,SimAtom> map) throws IOException {
        SimAtom x = map.get(in.readInt());
        if (x==null) throw new IOException("Unknown atom encountered when reading the database.");
        return x;
    }*/

    /* Save this SimAtom to the given output file by writing out its unique ID (and update the uniqueID map if needed)
    void writei(DataOutputStream out, IdentityHashMap<SimAtom,Integer> map) throws IOException {
        Integer i = map.get(this);
        if (i==null) { i=map.size(); map.put(this, i); }
        out.write(i.intValue());
    }*/

    /** Write this atom as "..". */
    void write(BufferedOutputStream out) throws IOException {
        byte array[] = string.getBytes("UTF-8");
        out.write('\"');
        for(int n=array.length, i=0; i<n; i++) {
            byte b = array[i];
            if (b=='\n') { out.write('\\'); out.write('n'); }
            else if (b=='\"') { out.write('\\'); out.write(b); }
            else if (b>0 && b<=' ') out.write(' ');
            else out.write(b);
        }
        out.write('\"');
    }

    /** Read a "..." atom assuming the leading " has already been consumed. */
    static SimAtom read(BufferedInputStream in) throws IOException {
        byte temp[] = new byte[64]; // to ensure proper detection of out-of-memory error, this number must be 2^n for some n>=0
        int n = 0;
        while(true) {
           int c = in.read();
           if (c<0) throw new IOException("Unexpected EOF");
           if (c=='\"') break;
           if (c=='\\') {
              c=in.read();
              if (c<0) throw new IOException("Unexpected EOF");
              if (c=='n') c='\n';
           }
           while (n >= temp.length) {
              byte temp2[] = new byte[temp.length * 2];
              System.arraycopy(temp, 0, temp2, 0, temp.length);
              temp = temp2;
           }
           temp[n]=(byte)c;
           n++;
        }
        return make(new String(temp, 0, n, "UTF-8"));
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

    /** Return the product of this atom and that atom. */
    public SimTuple product(SimAtom that) {
        return SimTuple.make(this, that);
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
