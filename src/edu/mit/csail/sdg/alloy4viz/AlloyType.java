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
 * Immutable; represents an Alloy toplevel signature or an Alloy subsignature.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AlloyType extends AlloyNodeElement {

    /** This caches an instance of the "univ" AlloyType, so we don't have to keep re-constructing it. */
    public static final AlloyType UNIV=new AlloyType("univ", false, false, true, false, false);

    /** This caches an instance of the "Int" AlloyType, so we don't have to keep re-constructing it. */
    public static final AlloyType INT=new AlloyType("Int", false, false, true, false, false);

    /** This caches an instance of the "seq/Int" AlloyType, so we don't have to keep re-constructing it. */
    public static final AlloyType SEQINT=new AlloyType("seq/Int", false, false, true, false, false);

    /** This caches an instance of the "set" AlloyType, so we don't have to keep re-constructing it. */
    public static final AlloyType SET=new AlloyType("set", false, false, false, false, false);

    /** Constructs an AlloyType object with that name. */
    public AlloyType(String name, boolean isOne, boolean isAbstract, boolean isBuiltin, boolean isOrdered, boolean isPrivate) {
        super(name);
        this.isOne = isOne;
        this.isAbstract = isAbstract;
        this.isBuiltin = isBuiltin;
        this.isOrdered = isOrdered;
        this.isPrivate = isPrivate;
    }

    /** Records whether this sig is known to be "one"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isOne;

    /** Records whether this sig is known to be "abstract"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isAbstract;

    /** Records whether this sig is known to be "builtin"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isBuiltin;

    /** Records whether this sig is known to be "ordered"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isOrdered;

    /** Records whether this sig is known to be "private"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isPrivate;

    /**
     * When comparing two AlloyType objects, we compare their names.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyType other) {
        if (other==null) return 1;
        return Util.slashComparator.compare(getName(), other.getName());
    }

    /**
     * When comparing two AlloyType objects, we compare their names.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyNodeElement other) {
        if (other==null) return 1;
        if (!(other instanceof AlloyType)) return -1;
        return Util.slashComparator.compare(getName(), ((AlloyType)other).getName());
    }

    /**
     * When comparing two AlloyType objects, we compare their names.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyElement other) {
        if (other==null) return 1;
        if (!(other instanceof AlloyType)) return -1;
        return Util.slashComparator.compare(getName(), ((AlloyType)other).getName());
    }

    /** This value is used to display this type in the Visualizer's customization screen. */
    @Override public String toString() { return getName(); }

    /** Two types are equal if they have the same name. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloyType)) return false;
        if (other==this) return true;
        return getName().equals(((AlloyType)other).getName());
    }

    /** Compute a hash code based on the name. */
    @Override public int hashCode() { return getName().hashCode(); }
}
