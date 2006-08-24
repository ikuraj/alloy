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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * ModelProjector takes care of figuring out the proper Model to use when
 * projecting.
 */
class ModelProjector {

    private Model _unprojectedModel;
    private Model _projectedModel;
    private ProjectionFrame _frame;
    //private ProjectedRelData _projectedRelData;

    private Map _oldRelsToProjectedRelData;
    private Map _projCustEltsToOrig;

    /*
     * Create a new ModelProjector for the given Model and ProjectionFrame.
     */
    ModelProjector(Model unprojectedModel_, ProjectionFrame frame_) {
        _unprojectedModel = unprojectedModel_;
        _frame = frame_;

        _oldRelsToProjectedRelData = new HashMap();
        _projCustEltsToOrig = new HashMap();
        _projectedModel = createProjectedModel();
    }

    /*
     * Returns the projected Model created by this ModelProjector.
     */
    Model getProjectedModel() {
        return _projectedModel;
    }

    /*
     * Returns the Map of old Relations to ProjectedRelData.
     */
    Map getProjectedRelData() {
        return _oldRelsToProjectedRelData;
    }

    /*
     * Creates a projected Model based on the unprojected Model and the
     * ProjectionFrame.
     */
    @SuppressWarnings("unchecked")
    private Model createProjectedModel() {
        List modules = new LinkedList();
        
        Set remainingTypesInModel = new HashSet();        

        for (Iterator modulesIter = _unprojectedModel.getModules().iterator(); modulesIter.hasNext();) {
            AlloyModule module = (AlloyModule)modulesIter.next();
            Set types = new HashSet(module.getTypes());
            List relations = new LinkedList();
            List sets = new LinkedList(module.getSets());
            
            Set removedTypes = new HashSet();
            // Get rid of all projected types, as well as their subtypes, but keep
            // track of which types were removed.
            for (Iterator typesIter = _frame.getProjectedTypes().iterator(); typesIter.hasNext();) {
                AlloyType type = (AlloyType)typesIter.next();
                types.remove(type);
                removedTypes.add(type);
                Set subtypes = _unprojectedModel.getSubsForType(type);
                types.removeAll(subtypes);
                removedTypes.addAll(subtypes);
            }
            
            remainingTypesInModel.addAll(types);            

            // Now take care of relations.
            // For each relation, get the List of its types...
            for (Iterator relsIter = module.getRelations().iterator(); relsIter.hasNext();) {
                AlloyRelation rel = (AlloyRelation)relsIter.next();
                List relTypes = new LinkedList(rel.getTypes());
                List indices = new ArrayList(relTypes.size());
                int currentIndex = 0;
                // Go through each of these types in order.  If it is a removed
                // type, remove it and keep track of its index.
                for (Iterator relTypesIter = relTypes.iterator(); relTypesIter.hasNext();) {
                    AlloyType relType = (AlloyType)relTypesIter.next();
                    if (removedTypes.contains(relType)) {
                        relTypesIter.remove();
                        indices.add(new Integer(currentIndex));
                    }
                    currentIndex++;
                }

                // If the relation still contains at least two types, it becomes
                // a new relation
                if (relTypes.size() > 1) {
                    AlloyRelation newRel = new AlloyRelation(rel.getName(), relTypes);
                    if (rel.isCustom()) {
                    	newRel.setCustom(true);
                        _projCustEltsToOrig.put(newRel,rel);
                    }
                    relations.add(newRel);
                    _oldRelsToProjectedRelData.put(rel, new ProjectedRelData(newRel, indices));                    
                }
                // If it contains only one type, it becomes a new set.
                else if (relTypes.size() == 1) {
                    AlloySet newSet = new AlloySet(rel.getName(), (AlloyType)relTypes.get(0));
                    if (rel.isCustom()) {
                    	newSet.setCustom(true);                                  
                        _projCustEltsToOrig.put(newSet,rel);          	
                    }
                    sets.add(newSet);
                    _oldRelsToProjectedRelData.put(rel, new ProjectedRelData(newSet, indices));
                }
            }

            // Finally, do sets.
            // Go through the sets and remove any whose type was removed.
            for (Iterator setsIter = sets.iterator(); setsIter.hasNext();) {
                AlloySet set = (AlloySet)setsIter.next();
                if (removedTypes.contains(set.getType())) {
                    setsIter.remove();
                }
                else {
                    if (set.isCustom() && _projCustEltsToOrig.get(set)==null) {
                        _projCustEltsToOrig.put(set,set);
                    }
                }
            }

            modules.add(new AlloyModule(module.getName(), new ArrayList(types), sets, relations));
        }
        
        TypeStructure ts = getTypeStructure(remainingTypesInModel);
        return new Model(modules, ts, _unprojectedModel.getName());
    }
    
    /**
     * Returns a map of projected (if applicable) versions of custvar elts to
     * their original forms.  sets that disappeared after projection don't 
     * have a mapping (and hence return null for get())
     */
    public Map getCustEltConversionMap() {
        return _projCustEltsToOrig;
    }

    /*
     * Create a TypeStructure given a list of types remaining after projection.
     */
    private TypeStructure getTypeStructure(Set types) {
        TypeStructure ts = new TypeStructure();
        for (Iterator typesIter = types.iterator(); typesIter.hasNext();) {
            AlloyType type = (AlloyType)typesIter.next();
            AlloyType superType = _unprojectedModel.getSuperForType(type);
            if (superType != null) {
                ts.addSuper(superType, type);
            }
            else {
                ts.addType(type);
            }
        }
        return ts;
    }

}
