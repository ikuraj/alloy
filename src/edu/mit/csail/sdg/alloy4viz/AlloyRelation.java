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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * Immutable; represents an Alloy relation of 2 or higher arity.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class AlloyRelation extends AlloyElement {

    /** The unmodifiable list of types. */
    private final List<AlloyType> types;

    /** Records whether this relation is known to be "private"; NOTE: this value is NOT USED during equals() comparison. */
    public final boolean isPrivate;

    /** Constructs a new AlloyRelation with that name and that list of types; types.size() must be 2 or above. */
    public AlloyRelation(String name, boolean isPrivate, List<AlloyType> types) {
        super(name);
        if (types==null || types.size()<2)
            throw new RuntimeException("An AlloyRelation object must have 2 or more types.");
        this.types = Collections.unmodifiableList(new ArrayList<AlloyType>(types));
        this.isPrivate = isPrivate;
    }

    /**
     * Project this relation and return an unmodifiable list of remaining types (after removing zero or more columns)
     * @param columns - the collection of columns to remove (0 is the first column, 1 is the second column...)
     */
    public List<AlloyType> project(Collection<Integer> columns) {
        List<AlloyType> list=new ArrayList<AlloyType>(types.size());
        for(int i=0; i<types.size(); i++) if (!columns.contains(i)) list.add(types.get(i));
        if (list.size()==types.size()) return this.types; else return Collections.unmodifiableList(list);
    }

    /** Returns an unmodifiable list of AlloyTypes representing the relation's type. */
    public List<AlloyType> getTypes() { return types; }

    /** Returns the arity of the relation. */
    public int getArity() { return types.size(); }

    /**
     * When comparing two AlloyRelation objects, we first compare the name, then the arity, then the types.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyRelation other) {
        if (other==null) return 1;
        // First compare the names.
        int n=Util.slashComparator.compare(getName(), other.getName()); if (n!=0) return n;
        // Now compare the arity of the two relations
        int arity = types.size();
        if (arity!=other.types.size()) return (arity<other.types.size())?-1:1;
        // Finally, compare each AlloyType
        for(int i=0; i<arity; i++) {
            n=types.get(i).compareTo(other.types.get(i));
            if (n!=0) return n;
        }
        return 0;
    }

    /**
     * When comparing two AlloyRelation objects, we first compare the name, then the arity, then the types.
     * <br> We guarantee x.equals(y) iff x.compareTo(y)==0
     */
    public int compareTo(AlloyElement other) {
        if (!(other instanceof AlloyRelation)) return 1;
        return compareTo((AlloyRelation)other);
    }

    /** This value is used to display this type in the Visualizer's customization screen. */
    @Override public String toString() {
        String answer="<html>";
        boolean first=true;
        for(AlloyType type: getTypes()) {
            if (first) {first=false; answer=answer+getName()+" :  ";} else answer=answer+" -&gt; ";
            answer=answer+type.getName();
        }
        return answer+"</html>";
    }

    /** Two relations are equal if they have the same name, and the same list of types. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloyRelation)) return false;
        if (other==this) return true;
        AlloyRelation otherRelation = (AlloyRelation)other;
        return getName().equals(otherRelation.getName()) && types.equals(otherRelation.types);
    }

    /** Computes a hash code based on the name and the list of types. */
    @Override public int hashCode() { return 5*getName().hashCode() + 7*types.hashCode(); }
}
