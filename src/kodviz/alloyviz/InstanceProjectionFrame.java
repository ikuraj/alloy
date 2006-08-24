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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * An instance-specific version of ProjectionFrame that keeps track of current
 * projected atoms of each type.  The differentiation between the two is needed
 * because ProjectionFrame is to be part of the non-instance or even model dependent
 * View and serialized.  Therefore, we must not keep the notion of current AlloyAtoms
 * in it.
 */
public class InstanceProjectionFrame {

    // _typesToAtoms maps AlloyTypes to AlloyAtoms of that type.    
    private Map _typesToAtoms;

    /**
     * Create a new ProjectionFrame in which no types are projected.
     */
    public InstanceProjectionFrame() {
        _typesToAtoms = new HashMap();
    }

    /**
     * project on a certain type with a specific currently projected atom of that
     * type.  If a mapping already exists, it'll be overwritten.     
     */
    @SuppressWarnings("unchecked")
    public void projectOn(AlloyType type, AlloyAtom atom) {
        _typesToAtoms.put(type, atom);
    }

    /**
     * sets the current atom (essentially the same as projectOn.  kept here
     * for compatibility purposes).
     */
    @SuppressWarnings("unchecked")
    public void setCurrentAtom(AlloyType type, AlloyAtom atom) {
        _typesToAtoms.put(type, atom);
    }

    /**
     * Stop projecting on the specified type.  If the specified type is not
     * being projected on, no change is made.  Info about the currently
     * projected atom of this type is lost.
     *
     * @param type The type on which to stop projecting.
     */
    public void deproject(AlloyType type) {
        _typesToAtoms.remove(type);
    }

    /**
     * Gets the current atom for a type.  If none is stored (or the type is not
     * being projected), returns null.
     */
    public AlloyAtom getCurrentAtom(AlloyType type) {
        return (AlloyAtom)_typesToAtoms.get(type);
    }

    /**
     * Returns true if the specified type is being projected on, false
     * otherwise.
     *
     * @param type The type to check for projection.
     */
    public boolean isProjected(AlloyType type) {
        return _typesToAtoms.containsKey(type);
    }

    /**
     * Returns an unmodifiable Set of all types currently being projected on.
     */
    @SuppressWarnings("unchecked")
    public Set getProjectedTypes() {
        return Collections.unmodifiableSet(_typesToAtoms.keySet());
    }

    /**
     * Returns a non-instance specific ProjectionFrame based on the projected
     * types of this InstanceProjectionFrame.
     */
    public ProjectionFrame deInstantiate() {
        return new ProjectionFrame(getProjectedTypes());
    }

    /**
     * Returns true if there are no types currently being projected on
     * in this projectionFrame.  False otherwise.  
     */
    public boolean isEmpty() {
        return _typesToAtoms.isEmpty();
    }

    /**
     * Returns a String representation of this ProjectionFrame.
     */
    public String toString() {
        StringBuffer projections = new StringBuffer();
        for (Iterator i = getProjectedTypes().iterator(); i.hasNext();) {
            projections.append(i.next().toString() + "\n");
        }
        return "InstanceProjectionFrame: projecting on\n" + projections.toString();
    }
}
