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

package kodviz.util;

import java.util.Comparator;
import java.util.SortedMap;
import java.util.SortedSet;

/**
 * Assigns each allocated object a unique ID.
 * IDs are assigned in order of allocation,
 * so the relative order of IDs matches the temporal
 * order of object allocation.  This distinguishes
 * IDs assigned here from object references,
 * whose numerical order is not related to order
 * of allocation.
 * <p>
 * The ID is used as the comparison
 * key of the object.  This can be used to ensure
 * determinism by using {@link SortedSet}s and
 * {@link SortedMap}s to store the objects.
 * <p>
 * The ID can also be used for debugging purposes,
 * since it depends only on the input and not on
 * where in memory the objects are placed.  The ID
 * of a given object is returned by {@link #getObjID}.
 * Note that the IDs start at {@link Long#MIN_VALUE},
 * so they're likely to be negative -- this is not an error.
 */
public class ObjID implements Comparable {
    /** Next object ID. */
    private static long _nextObjID = Long.MIN_VALUE;

    /** This object's ID */
    private long _objID = _nextObjID++;
    
    /**
     * Get the next object ID.  Can be used to trace how many
     * objects have been allocated.
     */
    public static long getNextObjID() { return _nextObjID; }

    /**
     * Get the ID of this object (useful for debugging purposes).
     * Note that the IDs start at {@link Long#MIN_VALUE},
     * so they're likely to be negative -- this is not an error.
     */
    public long getObjID() { return _objID - Long.MIN_VALUE; }

    /** Compare the objects by comparing their IDs. */
    public int compareTo(Object obj_) {
	long thatID = ((ObjID)obj_)._objID;
	if (_objID < thatID) return -1;
	if (_objID > thatID) return +1;
	return 0;
    }

    /** Clone the object, but allocate a unique new ID to the cloned copy. */
    protected Object clone() throws CloneNotSupportedException {
	ObjID result = (ObjID)super.clone();
	result._objID = _nextObjID++;
	return result;
    }

    /** A {@link Comparator} that sorts objects in increasing order by their ObjID */
    public static class ObjIDComparator implements Comparator {
	public int compare(Object a_, Object b_) {
	    long id1 = ((ObjID)a_).getObjID();
	    long id2 = ((ObjID)b_).getObjID();
	    if (id1 < id2) return -1;
	    if (id1 > id2) return +1;
	    return 0;
	}
    }

    public static final ObjIDComparator sObjIDComparator = new ObjIDComparator();
}
