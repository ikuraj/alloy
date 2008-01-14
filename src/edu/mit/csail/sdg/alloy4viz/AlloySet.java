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

import edu.mit.csail.sdg.alloy4.Util;

/**
 * Immutable; represents an Alloy set in an instance.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AlloySet extends AlloyNodeElement {

    /** The parent type of this AlloySet. */
    private final AlloyType type;

    /** Records whether this relation is known to be "private"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isPrivate;

    /** Constructs a new AlloySet object. */
    public AlloySet(String name, boolean isPrivate, AlloyType type) { super(name); this.type=type; this.isPrivate=isPrivate; }

    /** Returns the parent type of the AlloySet. */
    public AlloyType getType() { return type; }

    /**
     * When comparing two AlloySet objects, we first compare their names, then their types.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloySet other) {
        if (other==null) return 1;
        int n=Util.slashComparator.compare(getName(), other.getName());
        return n!=0 ? n : type.compareTo(other.type);
    }

    /**
     * When comparing two AlloySet objects, we first compare their names, then their types.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyNodeElement other) {
        if (!(other instanceof AlloySet)) return 1;
        AlloySet x=(AlloySet)other;
        int n=Util.slashComparator.compare(getName(), x.getName());
        return n!=0 ? n : type.compareTo(x.type);
    }

    /**
     * When comparing two AlloySet objects, we first compare their names, then their types.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyElement other) {
        if (other instanceof AlloyRelation) return -1;
        if (!(other instanceof AlloySet)) return 1;
        AlloySet x = (AlloySet)other;
        int n=Util.slashComparator.compare(getName(), x.getName());
        return n!=0 ? n : type.compareTo(x.type);
    }

    /** This value is used to display this type in the Visualizer's customization screen. */
    @Override public String toString() { return getName()+" :  "+getType().getName(); }

    /** Two sets are equal if they have the same name and the same type. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloySet)) return false;
        if (other==this) return true;
        AlloySet otherSet = (AlloySet)other;
        return getName().equals(otherSet.getName()) && type.equals(otherSet.type);
    }

    /** Compute a hash code based on the name and the type. */
    @Override public int hashCode() { return 5*type.hashCode() + 7*getName().hashCode(); }
}
