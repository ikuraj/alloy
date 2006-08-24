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

/**
 * An AlloyAtom represents an Atom in an Instance.
 */
public class AlloyAtom implements Comparable {

    private String _name;
    private AlloyType _type;
    private int _index;  // index of this atom relative to others of its type

    /**
     * Create a new AlloyAtom with the given name and type.
     *
     * IMPORTANT: the specs expects that name_ is a string that already has the
     * indexing, i.e. Man_0 and not just Man.
     */
    public AlloyAtom(String name_, AlloyType type_, int index_) {
	_name = name_;
	_type = type_;
	_index = index_;
    }

    /**
     * Return the name of this AlloyAtom.
     */
    public String getName() {
	return _name;
    }

    /**
     * Return the type of the AlloyAtom.
     */
    public AlloyType getType() {
	return _type;
    }

    /**
     * Return the index of this AlloyAtom.
     * The index is a number representing how this atom is
     * ordered relative to others of the same type.
     */
    public int getIndex() {
	return _index;
    }

    /**
     * Compares first by type, then by index.
     */
    public int compareTo(Object o) {
	AlloyAtom otherAtom = (AlloyAtom)o;
	
	int temp = getType().compareTo(otherAtom.getType());
	if (temp == 0) {
	    return _index - otherAtom.getIndex();
	}
	else {
	    return temp;
	}
    }

    /**
     * Two AlloyAtoms are equal if they have the same type and the same name and index.
     */
    public boolean equals(Object o) {
	if ((o == null) || !(o instanceof AlloyAtom)) {
	    return false;
	}
	AlloyAtom otherAtom = (AlloyAtom)o;
	return (getType().equals(otherAtom.getType()) && getName().equals(otherAtom.getName()) && getIndex()==otherAtom.getIndex());
    }

    /**
     * Returns a hash code based on the name and type and index.
     */
    public int hashCode() {
	return _type.hashCode() + 7*_name.hashCode() + _index;
    }

    /**
     * prettier toString()
     */
    public String toString() {
	return "AlloyAtom\n  Name:"+getName()+"\n  Type: "+getType()+"\n  Index: "+getIndex()+"\n";
    }
    
}
