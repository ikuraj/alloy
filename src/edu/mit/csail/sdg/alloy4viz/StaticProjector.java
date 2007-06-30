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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This utility class performs projection of AlloyModel and AlloyInstance.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class StaticProjector {

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private StaticProjector() { }

    /**
     * Given an unprojected model, project it over the given collection of AlloyType(s).
     * @param unprojectedModel - the original unprojected model
     * @param typesToBeProjected - the collection of types to project over
     */
    public static AlloyModel project(AlloyModel unprojectedModel, Collection<AlloyType> typesToBeProjected) {
        return project(unprojectedModel, typesToBeProjected, null);
    }

    /**
     * Given an unprojected model, project it over the given collection of AlloyType(s).
     * @param unprojectedModel - the original unprojected model
     * @param typesToBeProjected - the collection of types to project over
     * @param data - if nonnull, this method will record into this map how the relations are changed by the projection
     * <p>
     * For every relation R that gets altered to become a new AlloySet or a new AlloyRelation,
     * data.get(R) will give us a list of integers indicating the columns deleted from R due to the projection.
     * (For example, if an original relation A->B->C->D becomes B->D,
     * then the list of integers will be (0,2) indicating the first and third columns were removed).
     * <p>
     * If a relation R remains unchanged during the projection, then data.get(R) will return an empty list.
     * <p>
     * If a relation R is totally deleted, due to the projection, then R won't be in data.keySet().
     */
    private static AlloyModel project(
            AlloyModel unprojectedModel,
            Collection<AlloyType> typesToBeProjected,
            Map<AlloyRelation,List<Integer>> data) {
        Set<AlloyType> types = new LinkedHashSet<AlloyType>(unprojectedModel.getTypes());
        List<AlloySet> sets = new ArrayList<AlloySet>(unprojectedModel.getSets());
        List<AlloyRelation> relations = new ArrayList<AlloyRelation>();
        // Get rid of all projected types, as well as their subtypes.
        for (AlloyType type: typesToBeProjected) {
            types.remove(type);
            types.removeAll(unprojectedModel.getSubTypes(type));
        }
        types.add(AlloyType.UNIV); // "univ" has to be a type
        // Now go over the relations...
        for (AlloyRelation rel: unprojectedModel.getRelations()) {
            List<AlloyType> relTypes = new ArrayList<AlloyType>(rel.getTypes());
            List<Integer> indices = new ArrayList<Integer>();
            int currentIndex = 0;
            // For each type in a relation, if it is a removed type, remove it and keep track of its index.
            for (Iterator<AlloyType> relTypesIter = relTypes.iterator(); relTypesIter.hasNext();) {
                if (!types.contains(relTypesIter.next())) { relTypesIter.remove(); indices.add(currentIndex); }
                currentIndex++;
            }
            // If the relation still contains at least two types, it becomes a new relation
            if (relTypes.size() > 1) {
                relations.add(new AlloyRelation(rel.getName(), relTypes));
                if (data!=null) data.put(rel, indices);
            }
            // If it contains only one type, it becomes a new set.
            else if (relTypes.size() == 1) {
                sets.add(new AlloySet(rel.getName(), relTypes.get(0)));
                if (data!=null) data.put(rel, indices);
            }
        }
        // Finally, go through the sets and remove any whose type was removed.
        for (Iterator<AlloySet> setsIter = sets.iterator(); setsIter.hasNext();) {
            AlloySet set = setsIter.next();
            if (!types.contains(set.getType())) setsIter.remove();
        }
        return new AlloyModel(types, sets, relations, unprojectedModel);
    }

    /**
     * Project an instance over the given list of types (and their associated chosen atom).
     * @param oldInstance - the original unprojected instance
     * @param projection - the list of types to be projected and their associated chosen atoms
     *
     * <p> For each type t in projection.getProjectedTypes:
     *
     * <p> (1) If t doesn't exist in the instance, then we will simply ignore t.
     *
     * <p> (2) Otherwise, if t has one or more atoms in the original instance,
     * <br> then projection.getProjectedAtom(t) must be one of the atoms (indicating the chosen atom for that type)
     * <br> else projection.getProjectedAtom(t) must be null.
     * <br> If rule (2) is violated, then some tuples may not show up in the return value.
     */
    public static AlloyInstance project(AlloyInstance oldInstance, AlloyProjection projection) {
        Map<AlloyRelation,List<Integer>> data=new LinkedHashMap<AlloyRelation,List<Integer>>();
        Map<AlloyAtom,Set<AlloySet>> atom2sets = new LinkedHashMap<AlloyAtom,Set<AlloySet>>();
        Map<AlloyRelation,Set<AlloyTuple>> rel2tuples = new LinkedHashMap<AlloyRelation,Set<AlloyTuple>>();
        AlloyModel newModel = project(oldInstance.model, projection.getProjectedTypes(), data);
        // First put all the atoms from the old instance into the new one
        for(AlloyAtom atom:oldInstance.getAllAtoms()) {
            atom2sets.put(atom, new LinkedHashSet<AlloySet>(oldInstance.atom2sets(atom)));
        }
        // Now, decide what tuples to generate
        for(AlloyRelation r:oldInstance.model.getRelations()) {
            List<Integer> list=data.get(r);
            if (list==null) continue; // This means that relation was deleted entirely
            tupleLabel:
                for(AlloyTuple oldTuple:oldInstance.relation2tuples(r)) {
                    for (Integer i:list) {
                        // If an atom in the original tuple should be projected, but it doesn't match the
                        // chosen atom for that type, then this tuple must not be included in the new instance
                        AlloyAtom a=oldTuple.getAtoms().get(i);
                        AlloyType bt=r.getTypes().get(i);
                        bt=oldInstance.model.getTopmostSuperType(bt);
                        if (!a.equals(projection.getProjectedAtom(bt))) continue tupleLabel;
                    }
                    List<AlloyAtom> newTuple=oldTuple.project(list);
                    List<AlloyType> newObj=r.project(list);
                    if (newObj.size()>1 && newTuple.size()>1) {
                        AlloyRelation r2=new AlloyRelation(r.getName(), newObj);
                        Set<AlloyTuple> answer=rel2tuples.get(r2);
                        if (answer==null) rel2tuples.put(r2, answer=new LinkedHashSet<AlloyTuple>());
                        answer.add(new AlloyTuple(newTuple));
                    } else if (newObj.size()==1 && newTuple.size()==1) {
                        AlloyAtom a=newTuple.get(0);
                        Set<AlloySet> answer=atom2sets.get(a);
                        if (answer==null) atom2sets.put(a, answer=new LinkedHashSet<AlloySet>());
                        answer.add(new AlloySet(r.getName(), newObj.get(0)));
                    }
                }
        }
        // Here, we don't have to explicitly filter out "illegal" atoms/tuples/...
        // (that is, atoms that belong to types that no longer exist, etc).
        // That's because AlloyInstance's constructor must do the check too, so there's no point in doing that twice.
        return new AlloyInstance(oldInstance.filename, oldInstance.commandname
                , oldInstance.kodkod_input, oldInstance.kodkod_output, newModel, atom2sets, rel2tuples, oldInstance.isMetamodel);
    }
}
