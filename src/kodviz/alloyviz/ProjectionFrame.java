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

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * ProjectionFrame is a data structure that holds the information about which
 * types are projected within a model.
 */
public class ProjectionFrame implements Serializable {

    // REP INVARIANT:

	private static final long serialVersionUID = 1L;

	// _projectedTypes keeps track of the projected types.
    private Set _projectedTypes;

    /**
     * Create a new ProjectionFrame in which no types are projected.
     */
    public ProjectionFrame() {
        _projectedTypes = new HashSet();
    }

    /**
     * Creates a new ProjectionFrame with types as the projected types.
     */
    @SuppressWarnings("unchecked")
    public ProjectionFrame(Set types) {
        _projectedTypes = new HashSet(types);
    }

    @SuppressWarnings("unchecked")
    public ProjectionFrame copy() {
        ProjectionFrame pf = new ProjectionFrame();
        pf._projectedTypes = new HashSet(_projectedTypes);
        return pf;
    }

    /**
     * Project on the specified type.
     *
     * @param type The type on which to project.     
     */
    @SuppressWarnings("unchecked")
    public void projectOn(AlloyType type) {
        _projectedTypes.add(type);
    }

    /**
     * Stop projecting on the specified type.  If the specified type is not
     * being projected on, no change is made.
     *
     * @param type The type on which to stop projecting.
     */
    public void deproject(AlloyType type) {
        _projectedTypes.remove(type);
    }

    /**
     * Returns true if the specified type is being projected on, false
     * otherwise.
     *
     * @param type The type to check for projection.
     */
    public boolean isProjected(AlloyType type) {
        return _projectedTypes.contains(type);
    }

    /**
     * Returns an unmodifiable Set of all types currently being projected on.
     */
    @SuppressWarnings("unchecked")
    public Set getProjectedTypes() {
        return Collections.unmodifiableSet(_projectedTypes);
    }

    /**
     * Returns true if there are no types currently being projected on
     * in this projectionFrame.  False otherwise.  
     */
    public boolean isEmpty() {
        return _projectedTypes.isEmpty();
    }

    /**
     * Clears the projected types in this Projection Frame.
     */
    public void clear() {
        _projectedTypes.clear();
    }

    /**
     * Returns an instance-specific InstanceProjectionFrame based on this
     * ProjectionFrame and a passed in instance.  Matching is attempted
     * so that only types existing in the model of the instance will
     * be kept.  The current atom is initiallized to be the first one
     * of each type after the sort.
     */
    @SuppressWarnings("unchecked")
    public InstanceProjectionFrame instantiate(VizInstance instance) {
        InstanceProjectionFrame ipf = new InstanceProjectionFrame();
        Model model = instance.getModel();
        List modelTypes = model.getTypes();
        for (Iterator types = _projectedTypes.iterator(); types.hasNext();) {
            AlloyType type = (AlloyType)types.next();
            // only project on the type if it's in the model.
            if (modelTypes.contains(type)) {
            	//System.out.println(type);
                //System.out.println(instance.getAtomsOfType(type));
                // NASTY BUG--set of atoms could be empty and thus there'd be no minimum!
                Set atoms = instance.getAtomsOfType(type);
                if (!atoms.isEmpty()) {
					ipf.projectOn(type, (AlloyAtom)Collections.min(instance.getAtomsOfType(type)));
                }
                else {
                	ipf.projectOn(type, null);                
                }
            }
        }
        return ipf;
    }

    /**
     * Returns a String representation of this ProjectionFrame.
     */
    public String toString() {
        StringBuffer projections = new StringBuffer();
        for (Iterator i = getProjectedTypes().iterator(); i.hasNext();) {
            projections.append(i.next().toString() + "\n");
        }
        return "ProjectionFrame: projecting on\n" + projections.toString();
    }

}
