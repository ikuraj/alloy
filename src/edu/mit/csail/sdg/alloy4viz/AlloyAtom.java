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

/**
 * Immutable; represents an Alloy atom in an instance.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AlloyAtom implements Comparable<AlloyAtom> {

    /** The original name of this atom from the original Kodkod or other analysis. */
    private final String originalName;

    /** The most specific AlloyType that this atom belongs to. */
    private final AlloyType type;

    /**
     * The index is a number that differentiates atoms of the same AlloyType;
     * one special convention: (this atom is the only atom with this type) iff (index==Integer.MAX_VALUE)
     */
    private final int index;

    /** Create a new AlloyAtom with the given type and index. */
    public AlloyAtom(AlloyType type, int index) {
        this.type=type; this.index=index; this.originalName=type.getName()+"."+index;
    }

    /** Create a new AlloyAtom with the given type, index, and label. */
    public AlloyAtom(AlloyType type, int index, String originalName) {
        this.type=type; this.index=index; this.originalName=originalName;
    }

    /** Return a label for this atom as recommended by a theme (theme can be null if there's no theme to consult). */
    public String getVizName(VizState theme, boolean numberAtoms) {
        if (theme!=null) {
            if (theme.useOriginalName()) return originalName;
            if (index==Integer.MAX_VALUE && type.getName().equals("Int") && theme.label(type).length()==0) {
                // Special handling for Meta Model. (Only meta model could have index==MAX_VALUE)
                return "Int";
            }
            if (index==Integer.MAX_VALUE && type.getName().equals("seq/Int") && theme.label(type).length()==0) {
                // Special handling for Meta Model. (Only meta model could have index==MIN_VALUE)
                return "seq/Int";
            }
            if (index==Integer.MAX_VALUE || !numberAtoms) return theme.label(type); else return theme.label(type)+index;
        }
        if (type.getName().equals("Int")) return ""+index; // Special override to display integers better
        if (type.getName().equals("seq/Int")) return ""+index; // Special override to display integers better
        if (index==Integer.MAX_VALUE || !numberAtoms) return type.getName(); else return type.getName()+index;
    }

    /** Return the type of the AlloyAtom. */
    public AlloyType getType() { return type; }

    /** Provides a human-readable label for debugging purpose. */
    @Override public String toString() { return getVizName(null,true); }

    /**
     * Compare first by type, then by index, then by the original names.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     *
     * <p> As a special cosmetic enhancement:
     * if we're comparing integer atoms, we want to ignore the difference between seqInt and Int.
     */
    public int compareTo(AlloyAtom otherAtom) {
        if (otherAtom==null) return 1;
        AlloyType at = type;           if (at.equals(AlloyType.SEQINT)) at=AlloyType.INT;
        AlloyType bt = otherAtom.type; if (bt.equals(AlloyType.SEQINT)) bt=AlloyType.INT;
        // This renaming is necessary in order to make sure the comparison is transitive.
        // For example, assuming seq/Int comprises 0..3, then we want atom0 < atom5,
        // even though atom0's TYPENAME > atom5's TYPENAME.
        // On the other hand, if you have an atom X with type X, then we want to make sure X>5 just like X>0
        // (even though lexically, the type name "X" < the type name "seq/Int"
        if (at.equals(AlloyType.INT) && bt.equals(AlloyType.INT))
           return (index < otherAtom.index) ? -1 : ((index > otherAtom.index) ? 1 : 0);
        int result=at.compareTo(bt);
        if (result!=0) return result;
        // We don't want to use the "return (index-otherAtom.index);" trick,
        // especially since singleton sets will have index of Integer.MAX_VALUE.
        if (index != otherAtom.index) return (index < otherAtom.index)?-1:1;
        return originalName.compareTo(otherAtom.originalName);
    }

    /** Two AlloyAtoms are equal if they have the same type, same index, and same original name. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloyAtom)) return false;
        if (other==this) return true;
        AlloyAtom otherAtom = (AlloyAtom)other;
        return index==otherAtom.index && type.equals(otherAtom.type) && originalName.equals(otherAtom.originalName);
    }

    /** Returns a hash code based on the type and index. */
    @Override public int hashCode() { return 7*type.hashCode()+5*index+17*originalName.hashCode(); }
}
