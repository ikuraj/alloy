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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.translator;

import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import kodkod.instance.Tuple;

/** Immutable; represents an Alloy tuple; comparison is by identity rather than by value. */

public final class A4Tuple {

    /** The Kodkod tuple. */
    private final Tuple tuple;

    /** The KodkodAtom_to_AlloyAtom rename map. */
    private final ConstMap<Object,String> atomMap;

    /** The kodkodAtom_to_MostSpecificSig map. */
    private final ConstMap<Object,PrimSig> sigMap;

    /** Construct a Tuple from the kodkod Tuple, while renaming each atom using the given atomMap. */
    A4Tuple(Tuple tuple, ConstMap<Object,String> atomMap, ConstMap<Object,PrimSig> sigMap) {
        this.tuple=tuple;
        this.atomMap=atomMap;
        this.sigMap=sigMap;
    }

    /** Returns the arity. */
    public int arity() { return tuple.arity(); }

    /** Returns the i-th atom in this Tuple. */
    public String atom(int i) { Object t=tuple.atom(i); String n=atomMap.get(t); return n==null ? t.toString() : n; }

    /** Return the most-specific-sig for the i-th atom in this Tuple. */
    public PrimSig sig(int i) { Object t=tuple.atom(i); return sigMap.get(t); }

    /** Prints a human-readable description of this Tuple. */
    @Override public String toString() {
        StringBuilder sb=new StringBuilder();
        for(int i=0; i<tuple.arity(); i++) {
            if (i>0) sb.append("->");
            sb.append(atom(i));
        }
        return sb.toString();
    }
}
