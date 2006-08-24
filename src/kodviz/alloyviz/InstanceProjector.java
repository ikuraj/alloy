package kodviz.alloyviz;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodviz.util.Dbg;


/**
 * The InstanceProjector creates a new instance with the specified
 * types projected (i.e., these types in relations are filtered out).
 * Note that if a projected type has subtypes, then those subtypes are
 * also filtered out.
 */
public class InstanceProjector { // jbaek -- changed to public

    /*
     * The Instance being projected on
     */
    private VizInstance oldInstance;

    /**
     * The instance with the projected types filtered out.
     */
    private VizInstance newInstance;

    /**
     * A Mapping of old relations to ProjectedRelData.
     */
    private Map projectedRelData;

    /**
     * The InstanceProjectinFrame of all types being projected.
     */
    private InstanceProjectionFrame ipf;

    /**
     * Creates a new InstanceProjector with the specified instance and
     * mappings. typesToIndices should contain the AlloyTypes being
     * projected mapped to the current index or state. projectedData
     * contains a mapping of old AlloyRelations to their corresponding
     * ProjectedRelData.
     */
    public InstanceProjector(VizInstance instance_, InstanceProjectionFrame ipf_) {
        this.oldInstance = instance_;
        this.ipf = ipf_;

        // first create a new model so we can create a new instance
        ModelProjector modelProjector =
            new ModelProjector(oldInstance.getModel(), ipf.deInstantiate());

        // create new Instance with new model
        this.newInstance = new VizInstance(modelProjector.getProjectedModel());
        this.projectedRelData = modelProjector.getProjectedRelData();

        createNewInstance();
    }

    /**
     * Added new atom set and relation set to new instance.
     */
    private void createNewInstance() {
        AtomFilter atomFilter = new AtomFilter();
        atomFilter.updateAtoms();
        RelationFilter relationFilter = new RelationFilter();
        relationFilter.updateRelations();
    }

    /**
     * Returns the old, unprojected instance.
     */
    public VizInstance getUnprojectedInstance() {
        return oldInstance;
    }

    /**
     * Returns the current InstanceProjectionFrame.
     */
    public InstanceProjectionFrame getProjectionFrame() {
        return ipf;
    }

    /**
     * Returns the new Instance based on the specified types that are
     * projected.  
     */
    public VizInstance getProjectedInstance() {
        return newInstance;
    }

    /**
     * AtomFilter uses the ProjectionFrame to filter out types in the
     * old instance that do not exist in the new Model (in the new
     * Instance). Note that if any types have subtypes in the
     * ProjectionFrame, the subtypes no longer exist in the Model, and
     * therefore atoms of those types do not exist in the new
     * Instance.
     */
    private class AtomFilter {
        // the constructor doesn't really do anything
        private AtomFilter() {
        }

        /**
         * Filters out the atoms of types that no longer exist in the model.
         */
        private void updateAtoms() {
            List modelTypes = newInstance.getModel().getTypes();
            List alloyAtoms = oldInstance.getAllAtoms();

            Iterator atomsIterator = alloyAtoms.iterator();
            while (atomsIterator.hasNext()) {
                AlloyAtom nextAtom = (AlloyAtom)atomsIterator.next();
                //System.out.println(nextAtom);
                if (modelTypes.contains(nextAtom.getType())) {
                    //System.out.println("yes");
                    // Get the Set of sets for this atom  
                    Set alloySets = oldInstance.getSetsForAtom(nextAtom);
                    //System.out.println(alloySets);
                    newInstance.addAtom(nextAtom);
                    // There are sets to add the atom to
                    if (!alloySets.isEmpty()) {
                        Iterator setIterator = alloySets.iterator();
                        while (setIterator.hasNext()) {
                            AlloySet nextSet = (AlloySet)setIterator.next();
                            newInstance.addAtomToSet(nextAtom, nextSet);
                        }
                    }
                }
            }
            //System.out.println(oldInstance);
        }
    }

    /**
     * this inner class rocks your world.
     * Filters out tuples of each relation.
     */
    private class RelationFilter {
        

        @SuppressWarnings("unchecked")
        void updateRelations() {
            /*
             * Strategy:
             * 1. Go through each relation in the instance.
             * 2. Look up in projectedRelData the relation/set this relation
             *    transforms into because of projection
             * 3. For each tuple in each changed relation, use the removed
             *    indices data to remove atoms of the corresponding columns
             *    from the tuple.  If it is found that none of the atoms
             *    in the to-remove columns of a certain tuple are the ones
             *    currently projected, then this whole relation is removed.	     *
             * 4. For each new tuple, make sure all the atoms kept are
             *    in the instance.  If not, add them and preserve original sets.
             *
             * note: all changes are applied directly on newInstance.
             */

            for (Iterator relIter = oldInstance.getAllRelations().iterator(); relIter.hasNext();) {
                AlloyRelation rel = (AlloyRelation)relIter.next();
                ProjectedRelData prd = (ProjectedRelData)projectedRelData.get(rel);
                if (prd == null) {
                    // relation got lost in the abyss after being totally taken apart
                } else {
                    Object newRelOrSet = prd.getNewValue();
                    if (newRelOrSet instanceof AlloyRelation) {
                        AlloyRelation newRel = (AlloyRelation)newRelOrSet;

                        if (newRel.equals(rel)) {
                            // do nothing.  Projection didn't affect this relation at all.

                            // the problem is: these relations could have been on a supertype of a
                            // type we're projecting, in which case we'd like to show the atom.
                            // unfortunately, the only way to make sure the atoms stay there is to
                            // very slowly iterate through each and every atom and see.  Perhaps a
                            // set of already checked elements can prove to be efficient.

                            tupleLabel:                                
                            for (Iterator tupleIter =
                                oldInstance.getTuplesInRelation(rel).iterator();
                                tupleIter.hasNext();
                                ) {

                                AlloyTuple tup = (AlloyTuple)tupleIter.next();

                                for (Iterator atomIter = tup.getAtoms().iterator();
                                    atomIter.hasNext();
                                    ) {
                                    AlloyAtom temp = (AlloyAtom)atomIter.next();
                                    if (!newInstance.getAllAtoms().contains(temp)) {
                                        //System.out.println(newInstance.getAllAtoms().toString());
                                        
                                        Dbg.chk(temp);
                                        
                                        // skip this tuple if we reach the boundary case with univ relations...
                                        if (!newInstance.getModel().containsType(temp.getType())) {
                                            //System.out.println("skipping tuple " + tup + "because of atom : "+temp);
                                            continue tupleLabel;
                                        }
                                        
                                        newInstance.addAtom(temp);                                        

                                        List remainingTypes = newInstance.getModel().getTypes();
                                        // also makes sure that any sets the atom used to be in are accounted for.
                                        // however if that set was eliminated (because it is of a projected type), then
                                        // we don't try to add it back in...
                                        for (Iterator setIter =
                                            oldInstance.getSetsForAtom(temp).iterator();
                                            setIter.hasNext();
                                            ) {
                                            AlloySet set = (AlloySet)setIter.next();
                                            
                                            // don't add the set if adding this atom required type fixing.
                                            if (remainingTypes.contains(set.getType())) {
                                                newInstance.addAtomToSet(temp, set);
                                            }
                                        }
                                    }
                                }
                                newInstance.addTupleToRelation(tup, newRel);
                            }
                        } else {
                            
                            tupleLabel:
                            for (Iterator tupleIter =
                                oldInstance.getTuplesInRelation(rel).iterator();
                                tupleIter.hasNext();
                                ) {

                                // to get to this else clause, the relation must have been changed by 
                                // projection.  so, we can begin by assuming that removeEntireTuple 
                                // is false, and then only resetting it to true if all of the tuple's 
                                // projected indicies match the currently selected atoms.
                                
                                boolean removeEntireTuple = false;
                                AlloyTuple tuple = (AlloyTuple)tupleIter.next();
                                // create a new List so that I can remove stuff
                                List atoms = new ArrayList(tuple.getAtoms());
                                // also create a collection of atoms to be removed so that they
                                // can be taken out at once after the loop.
                                Set atomsToBeRemoved = new HashSet();

                                for (Iterator removedIndices = prd.getRemovedIndices().iterator();
                                    removedIndices.hasNext();
                                    ) {
                                    int curIndex = ((Integer)removedIndices.next()).intValue();
                                    AlloyAtom atom = (AlloyAtom)atoms.get(curIndex);
                                    atomsToBeRemoved.add(atom);
                                    if (!atom
                                        .equals(
                                            ipf.getCurrentAtom(
                                                (AlloyType)rel.getTypes().get(curIndex)))) {
                                        // this tuple should not be shown because at least one projected atom 
                                        // does not match the current one
                                        removeEntireTuple = true;
                                    }
                                }

                                if (removeEntireTuple) {
                                    // there were no atoms in this tuple that were currently selected.  Thus, the
                                    // whole tuple is not visualized and nothing is done.
                                } else {
                                    // this tuple will be shown.  We must remove any atoms that need removing.
                                    atoms.removeAll(atomsToBeRemoved);

                                    // goes through all the atoms that are kept and makes sure they are in the new Instance
                                    // this is necessary because some subtypes that were under a supertype column (that's
                                    // not projected) in the relation could have been removed, but we now want to show them.
                                    for (Iterator atomIter = atoms.iterator();
                                        atomIter.hasNext();
                                        ) {
                                        AlloyAtom temp = (AlloyAtom)atomIter.next();
                                        if (!newInstance.getAllAtoms().contains(temp)) {
                                            
                                            // skip this tuple if we reach the boundary case with univ relations...
                                            if (!newInstance.getModel().containsType(temp.getType())) {
                                                //System.out.println("skipping tuple " + tuple + "because of atom : "+temp);
                                                continue tupleLabel;
                                            }
                                            
                                            newInstance.addAtom(temp);                                            
                                            
                                            // also makes sure that any sets the atom used to be are accounted for.
                                            for (Iterator setIter =
                                                oldInstance.getSetsForAtom(temp).iterator();
                                                setIter.hasNext();
                                                ) {
                                                AlloySet set = (AlloySet)setIter.next();                                                                                                
                                                newInstance.addAtomToSet(
                                                    temp,
                                                    set);                                                
                                            }
                                        }
                                    }

                                    AlloyTuple newTuple = new AlloyTuple(atoms);
                                    newInstance.addTupleToRelation(newTuple, newRel);
                                }
                            }

                        }
                    } else {
                        // not a relation, it must be a set
                        AlloySet newSet = (AlloySet)newRelOrSet;
                                                                       
                        tupleLabel:
                        for (Iterator tupleIter = oldInstance.getTuplesInRelation(rel).iterator();
                            tupleIter.hasNext();
                            ) {
                            
                            // to get to this else clause, the relation must have been changed by 
                            // projection (from rel to set).  so, we can begin by assuming that removeEntireTuple 
                            // is false, and then only resetting it to true if all of the tuple's 
                            // projected indicies match the currently selected atoms.
                            
                            boolean removeEntireTuple = false;
                            
                            AlloyTuple tuple = (AlloyTuple)tupleIter.next();
                            // create a new List so that I can remove stuff
                            List atoms = new ArrayList(tuple.getAtoms());
                            // also create a collection of atoms to be removed so that they
                            // can be taken out at once after the loop.
                            Set atomsToBeRemoved = new HashSet();

                            for (Iterator removedIndices = prd.getRemovedIndices().iterator();
                                removedIndices.hasNext();
                                ) {
                                int curIndex = ((Integer)removedIndices.next()).intValue();
                                AlloyAtom atom = (AlloyAtom)atoms.get(curIndex);
                                atomsToBeRemoved.add(atom);
                                if (!atom
                                    .equals(
                                        ipf.getCurrentAtom(
                                            (AlloyType)rel.getTypes().get(curIndex)))) {
                                    // this tuple should not be shown because at least one projected atom 
                                    // does not match the current one
                                    removeEntireTuple = true;
                                }
                            }

                            if (removeEntireTuple) {
                                // there were no atoms in this tuple that were currently selected.  Thus, the
                                // whole tuple is not visualized and nothing is done.
                            } else {
                                // this tuple will be shown.  We must remove any atoms that need removing.
                                atoms.removeAll(atomsToBeRemoved);

                                Dbg.chk(atoms.size()==1,"this is a set so there should only be one atom left");
                                    // this should never happen.  Assertion error maybe?
                                    AlloyAtom remainingAtom = (AlloyAtom)atoms.get(0);
                                    if (!newInstance.getAllAtoms().contains(remainingAtom)) {
                                        
                                        // skip this tuple if we reach the boundary case with univ relations...
                                        if (!newInstance.getModel().containsType(remainingAtom.getType())) {
                                            //System.out.println("skipping tuple " + tuple + "because of atom : "+remainingAtom);
                                            continue tupleLabel;
                                        }                                        
                                        newInstance.addAtom(remainingAtom);
                                        
                                        // also makes sure that any sets the atom used to be are accounted for.
                                        for (Iterator setIter =
                                            oldInstance.getSetsForAtom(remainingAtom).iterator();
                                            setIter.hasNext();
                                            ) {                                            
                                            AlloySet set = (AlloySet)setIter.next(); 
                                            
                                            newInstance.addAtomToSet(
                                                remainingAtom,
                                                set);
                                            
                                        }
                                    }
                                    newInstance.addAtomToSet(remainingAtom, newSet);
                               
                            }
                        }
                    }
                }
            }
        }
    }
}
