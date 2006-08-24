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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class stores a type structure of a model, with information about
 * subtyping, etc.
 */
public class TypeStructure {

    /** hierarch maps an AlloyType to its AlloyType supertype, or to null if no supertype exists  */
    Map _hierarchy;

    public TypeStructure() {
        _hierarchy = new HashMap();
    }

    @SuppressWarnings("unchecked")
    public TypeStructure copy() {
    	TypeStructure ts = new TypeStructure();
    	ts._hierarchy = new HashMap(this._hierarchy);
    	return ts;    	 
    }

    /**
     * Adds the type to the type structure as a basic type.  If the type
     * already exists, does nothing.
     */
    @SuppressWarnings("unchecked")
    public void addType(AlloyType t) {
        if (!_hierarchy.containsKey(t)) {
            _hierarchy.put(t, null);
        }
    }

    /**
     * Adds a type as the super type of another.  The types should not
     * be the same.  If any of the two do not already exist in the
     * type structure, it will be automatically added.  If the subtype
     * already had a supertype specified, that relationship will be
     * overwritten.  Caution: do not specify circular relationships.
     */
    @SuppressWarnings("unchecked")
    public void addSuper(AlloyType superType, AlloyType subType) {
        if (!_hierarchy.containsKey(superType)) {
            _hierarchy.put(superType, null);
        }
        _hierarchy.put(subType, superType);
    }

    /**
     * Gets a sorted (ascending) unmodifiable view of all types
     */
    @SuppressWarnings("unchecked")
    public List getTypes() {
        List temp = new ArrayList(_hierarchy.keySet());
        Collections.sort(temp);
        return Collections.unmodifiableList(temp);
    }

    /**
     * Gets the supertype of a type.  If the type is a basic type, null
     * is returned.
     */
    public AlloyType getSuper(AlloyType sub) {
        return (AlloyType)_hierarchy.get(sub);
    }

    /**
     * Returns a Set of types that are subtypes of the given type.  This works
     * recursively, so if the subtypes themselves have subtypes, they too are
     * included.  If the given type is a leaf or null, returns an empty Set.
     */
    @SuppressWarnings("unchecked")
    public Set getSubTypes(AlloyType type) {
        Set subtypes = new HashSet();
        if (type != null) {
            for (Iterator types = _hierarchy.keySet().iterator(); types.hasNext();) {
                AlloyType subType = (AlloyType)types.next();
                if (type.equals(getSuper(subType))) {
                    subtypes.add(subType);
                    subtypes.addAll(getSubTypes(subType));
                }
            }
        }
        return subtypes;
    }
    
    /**
     * Returns true if this type structure contains t
     */
    public boolean containsType(AlloyType type) {
        if (type==null) {
            return false;        
        } else {
            return _hierarchy.containsKey(type);
        }
        
    }

    /**
     * Two type structures are equal if they have the same types and information
     * about supertypes.
     */
    public boolean equals(Object o) {
        if ((o == null) || !(o instanceof TypeStructure)) {
            return false;
        }
        return _hierarchy.equals(((TypeStructure)o)._hierarchy);
    }

    /**
     * Returns a hash code
     */
    public int hashCode() {
        return _hierarchy.hashCode();
    }

    /**
     * Returns a String representation.
     */
    public String toString() {
        return _hierarchy.toString();
    }
}
