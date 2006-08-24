/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.alloyviz;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * An AlloyTuple represents a tuple that can appear in a relation.
 */
public class AlloyTuple {

    private List _atoms;

    /**
     * Creates a new AlloyTuple containing AlloyAtoms in the order specified by
     * the list.  atoms_ must contain only AlloyAtoms.
     */
    @SuppressWarnings("unchecked")
    public AlloyTuple(List atoms_) {
	_atoms = Collections.unmodifiableList(atoms_);
    }

    /**
     * Returns the arity of this AlloyTuple.
     */
    public int getArity() {
	return _atoms.size();
    }

    /**
     * Returns an unmodifiable List of the AlloyAtoms in this AlloyTuple.
     */
    public List getAtoms() {
	return _atoms;
    }

    /**
     * Returns the first AlloyAtom in this AlloyTuple.
     */
    public AlloyAtom getStart() {
	return (AlloyAtom)_atoms.get(0);
    }

    /**
     * Returns the last AlloyAtom in this AlloyTuple.
     */
    public AlloyAtom getEnd() {
	return (AlloyAtom)_atoms.get(_atoms.size() - 1);
    }

    /**
     * Returns a new AlloyTuple whose list of atoms is the same as this
     * AlloyTuple, but in reverse order.
     */
    @SuppressWarnings("unchecked")
    public AlloyTuple reversed() {
	List atoms = new LinkedList();
	for (Iterator atomsIter = _atoms.iterator(); atomsIter.hasNext();) {
	    atoms.add(0, atomsIter.next());
	}
	return new AlloyTuple(atoms);
    }
    
    /**
     * Two tuples are equal if they have the same atoms in the same order.
     */
    public boolean equals(Object o) {
	if ((o == null) || !(o instanceof AlloyTuple)) {
	    return false;
	}
	AlloyTuple tuple = (AlloyTuple)o;
	return _atoms.equals(tuple.getAtoms());
    }

    public int hashCode() {
	return _atoms.hashCode();
    }

    public String toString() {
	return "AlloyTuple: "+getAtoms();
    }
}
