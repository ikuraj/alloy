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

package edu.mit.csail.sdg.alloy4viz;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

/**
 * Immutable; represents a set of types to be projected, plus the exact atom chosen for each type to be projected.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class AlloyProjection {

    /**
     * Its keySet is the set of types to be projected; each type is associated with the atom chosen to be projected.
     *
     * <p> Its keySet is guaranteed to be sorted.
     *
     * <p> For each type t in the keyset, map.get(t) is an AlloyAtom
     * (indicating the atom in t that we chose to project over).
     *
     * <p> Note: there's no way for this class to confirm that a chosen atom is really
     * in that type (since the atom may be in a subtype, and thus the atom.type() won't be exactly the same as t).
     * Thus, classes that use AlloyProjection objects need to do their own sanity check.
     */
    private final Map<AlloyType,AlloyAtom> map;

    /**
     * Constructs a new AlloyProjection object based on the set of types to be projected and the exact atoms chosen.
     * @param map - this map describes the set of types to be projected and the exact atoms chosen to be projected
     *
     * <p> For each type t in map.keySet(),
     * <br> map.get(t) is an AlloyAtom (indicating the atom in t that we chose to project over).
     *
     * <p> Note: there's no way for this class to confirm that a chosen atom is really
     * in that type (since the atom may be in a subtype, and thus the atom.type() won't be exactly the same).
     * Thus, classes that use AlloyProjection objects need to do their own sanity check.
     */
    public AlloyProjection(Map<AlloyType,AlloyAtom> map) {
        Map<AlloyType,AlloyAtom> mymap = new TreeMap<AlloyType,AlloyAtom>();
        for(Map.Entry<AlloyType,AlloyAtom> e:map.entrySet()) {
            if (e.getKey()!=null && e.getValue()!=null)
                mymap.put(e.getKey(), e.getValue());
        }
        this.map=Collections.unmodifiableMap(mymap);
    }

    /** Constructs an empty AlloyProjection object, with an empty projection list. */
    public AlloyProjection() {
        this.map=Collections.unmodifiableMap(new TreeMap<AlloyType,AlloyAtom>());
    }

    /** Return the sorted unmodifiable collection of types we are projecting. */
    public Collection<AlloyType> getProjectedTypes() { return map.keySet(); }

    /**
     * Return the atom chosen for that type;
     * returns null if that type is not projected.
     */
    public AlloyAtom getProjectedAtom(AlloyType type) { return map.get(type); }

    /** Returns a human readable dump of this object. */
    @Override public String toString() {
        boolean first=true;
        String ans="Projection[";
        for(Map.Entry<AlloyType,AlloyAtom> e:map.entrySet()) {
            if (first) first=false; else ans=ans+", ";
            ans=ans+e.getKey().getName()+":"+e.getValue().getVizName(null,true);
        }
        return ans+"]";
    }

    /** AlloyProjections are equal if they are projecting over the same types, each type with the same chosen value. */
    @Override public boolean equals(Object other) {
        if (!(other instanceof AlloyProjection)) return false;
        if (other==this) return true;
        return map.equals(((AlloyProjection)other).map);
    }

    /** Computes a hashcode based on the types and the atoms chosen for each type. */
    @Override public int hashCode() { return map.hashCode(); }
}
