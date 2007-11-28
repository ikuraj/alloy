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

package edu.mit.csail.sdg.alloy4viz;

import java.util.Collection;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

/**
 * Immutable; represents an Alloy tuple.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AlloyTuple implements Comparable<AlloyTuple> {

    /** The unmodifiable list of atoms in this tuple. */
    private final List<AlloyAtom> atoms;

    /** Creates a new AlloyTuple containing the atoms specified by the list; atoms.size() must be 2 or above. */
    public AlloyTuple(List<AlloyAtom> atoms) {
        if (atoms==null || atoms.size()<2)
            throw new RuntimeException("An AlloyTuple object must have 2 or more atoms.");
        this.atoms = Collections.unmodifiableList(new ArrayList<AlloyAtom>(atoms));
    }

    /**
     * Project this tuple and return an unmodifiable list of remaining atoms (after removing zero or more columns)
     * @param columns - the collection of columns to remove (0 is the first column, 1 is the second column...)
     */
    public List<AlloyAtom> project(Collection<Integer> columns) {
        List<AlloyAtom> list=new ArrayList<AlloyAtom>(atoms.size());
        for(int i=0; i<atoms.size(); i++) if (!columns.contains(i)) list.add(atoms.get(i));
        if (list.size()==atoms.size()) return this.atoms; else return Collections.unmodifiableList(list);
    }

    /** Returns the arity of this AlloyTuple. */
    public int getArity() { return atoms.size(); }

    /** Returns an unmodifiable list of the AlloyAtoms in this AlloyTuple. */
    public List<AlloyAtom> getAtoms() { return atoms; }

    /** Returns the first AlloyAtom in this AlloyTuple. */
    public AlloyAtom getStart() { return atoms.get(0); }

    /** Returns the last AlloyAtom in this AlloyTuple. */
    public AlloyAtom getEnd() { return atoms.get(atoms.size()-1); }

    /** Returns a new AlloyTuple whose list of atoms is the same but in reverse. */
    public AlloyTuple reversed() {
        List<AlloyAtom> newlist = new ArrayList<AlloyAtom>(atoms.size());
        for(int i=atoms.size()-1; i>=0; i--) newlist.add(atoms.get(i));
        return new AlloyTuple(newlist);
    }

    /** Provides a human-readable description of this AlloyTuple. */
    @Override public String toString() {
        String s="<";
        for(int i=0; i<atoms.size(); i++) { if (i!=0) s=s+", "; s=s+atoms.get(i); }
        return s+">";
    }

    /**
     * Two tuples are first compared based on length; if the length is the same, we compare atom-by-atom.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyTuple that) {
        if (that==null) return 1;
        if (atoms.size() < that.atoms.size()) return -1;
        if (atoms.size() > that.atoms.size()) return 1;
        for(int i=0; i<atoms.size(); i++) {
            int result=atoms.get(i).compareTo(that.atoms.get(i));
            if (result!=0) return result;
        }
        return 0;
    }

    /** Two tuples are equal if they have the same atoms in the same order. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloyTuple)) return false;
        if (other==this) return true;
        return atoms.equals(((AlloyTuple)other).atoms);
    }

    /** Compute a hash code based on the list of atoms. */
    @Override public int hashCode() { return atoms.hashCode(); }
}
