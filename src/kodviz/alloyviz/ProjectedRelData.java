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
import java.util.List;

/*
 * Rep Invariant:
 *
 * _newItem must always contain either an AlloySet or an AlloyRelation.
 * every entry in _removedIndices must be an Integer.
 */

/**
 * ProjectedRelData keeps track of what happends to the AlloyRelations when a
 * Model is projected.  Specifically, it holds an AlloyRelation or AlloySet, and
 * a List of the indices removed from the original AlloyRelation to generate
 * this new item.
 */
class ProjectedRelData {
    
    private Object _newItem;
    private List _removedIndices;

    /*
     * Create a new ProjectedRelData with the specified AlloyRelation and List
     * of removed indices.
     */
    ProjectedRelData(Object newItem_, List removedIndices_) {
	_newItem = newItem_;
	_removedIndices = removedIndices_;
    }

    /*
     * Get the AlloyRelation or AlloySet for this ProjectedRelData.
     */
    Object getNewValue() {
	return _newItem;
    }

    /*
     * Get the List of indices removed from the original AlloyRelation to create
     * the new AlloyRelation or AlloySet.  Each item in this List is an Integer.
     */
    @SuppressWarnings("unchecked")
    List getRemovedIndices() {
	return Collections.unmodifiableList(_removedIndices);
    }
}
