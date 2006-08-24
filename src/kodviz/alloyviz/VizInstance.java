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
import java.util.TreeMap;

import kodviz.util.Dbg;


/**
 * Instance is a class that represents an Instance of a model that can then be
 * displayed in the visualization.
 */
public class VizInstance {

    //
    // MEMBER VARIABLES
    //

    /*
     * Rep Invariant:
     *
     * Every key in _atomsToSets is an AlloyAtom
     * Every value in _atomsToSets is a Set of AlloySets.
     *
     * Every key in _relsToTuples is an AlloyRelation
     * Every value in _relsToTuples is a Set of AlloyTuples.
     */

    /**
     * The Model that this Instance is an instance of.
     */
    private Model _model;

    /**
     * A mapping of all of the AlloyAtoms in this Instance's Model to the
     * AlloySets that each AlloyAtom is a member of.
     */
    private Map _atomsToSets;

    /**
     * A mapping of all of the AlloyRelations in this Instance's Model to the
     * AlloyTuples in those AlloyRelations.
     */
    private Map _relsToTuples;

    /**
     * A mapping of every AlloyType in this Instance to the AlloyAtoms of that
     * AlloyType.
     */
    private Map _typesToAtoms;

    /**
     *  A mapping of every AlloySet in this Instance to the AlloyAtoms that are
     *  members of that AlloySet.  This could be extracted form _atomsToSets,
     *  but is included for more efficient lookup.
     */
    private Map _setsToAtoms;

    //
    // CONSTRUCTORS
    //

    /**
     * Create a new Instance with no AlloyAtoms based on the specified Model.
     */
    @SuppressWarnings("unchecked")
    public VizInstance(Model model_) {

        _model = model_;
        _atomsToSets = new TreeMap();
        _relsToTuples = new HashMap();
        _typesToAtoms = new HashMap();
        _setsToAtoms = new HashMap();

        for (Iterator sets = _model.getSets().iterator(); sets.hasNext();) {
            _setsToAtoms.put(sets.next(), new HashSet());
        }
        for (Iterator types = _model.getTypes().iterator(); types.hasNext();) {
            _typesToAtoms.put(types.next(), new HashSet());
        }
        for (Iterator rels = _model.getRelations().iterator(); rels.hasNext();) {
            _relsToTuples.put(rels.next(), new HashSet());
        }
        //repCheck();
    }

    //
    // ACCESSORS
    //

    /**
     * Returns the Model on which this Instance is based.
     */
    public Model getModel() {
        return _model;
    }

    /**
     * Returns an unmodifiable List of AlloyAtoms in this Instance.
     */
    @SuppressWarnings("unchecked")
    public List getAllAtoms() {
        List value = new ArrayList(_atomsToSets.keySet());
        Collections.sort(value);
        return Collections.unmodifiableList(value);
    }

    /**
     * Returns an unmodifiable List of AlloyRelations in this Instance.
     */
    public List getAllRelations() {
        return _model.getRelations();
    }

    /**
     * Returns an unmodifiable List of AlloyTypes in this Instance.
     */
    public List getAllTypes() {
        return _model.getTypes();
    }

    /**
     * Returns an unmodifiable List of AlloySets in this Instance.
     */
    public List getAllSets() {
        return _model.getSets();
    }

    /**
     * Returns an unmodifiable Set of AlloySets that the specified AlloyAtom is
     * a member of.
     */
    @SuppressWarnings("unchecked")
    public Set getSetsForAtom(AlloyAtom atom) {
    	Dbg.chk((Set)_atomsToSets.get(atom));
        return Collections.unmodifiableSet((Set)_atomsToSets.get(atom));
    }

    /**
     * Returns an unmodifiable Set of AlloyTuples in the specified AlloyRelation.
     */
    @SuppressWarnings("unchecked")
    public Set getTuplesInRelation(AlloyRelation rel) {
        return Collections.unmodifiableSet((Set)_relsToTuples.get(rel));
    }

    /**
     * Returns an unmodifiable Set of AlloyAtoms of the specified AlloyType in
     * this Instance.
     */
    @SuppressWarnings("unchecked")
    public Set getAtomsOfType(AlloyType type) {
        return Collections.unmodifiableSet((Set)_typesToAtoms.get(type));
    }

    /**
     * Returns an unmodifiable Set of AlloyAtoms in the specified AlloySet in
     * this Instance.
     */
    @SuppressWarnings("unchecked")
    public Set getAtomsInSet(AlloySet set) {
        return Collections.unmodifiableSet((Set)_setsToAtoms.get(set));
    }

    /**
     * Returns the super type of the specified AlloyType.  Returns null if the
     * specified type has no super type in this Instance's Model.
     */
    public AlloyType getSuperForType(AlloyType type) {
        return _model.getSuperForType(type);
    }

    /**
     * Determines whether or not an atom exists in this instance.
     */
    public boolean containsAtom(AlloyAtom atom) {
        return _atomsToSets.containsKey(atom);
    }

    //
    // MUTATORS
    //

	public void setModel(Model model_) {
		_model = model_;
	}
    /**
     * Adds a new AlloyAtom to this Instance, associated with no AlloySets.  If
     * there is already a mapping from the AlloyAtom in this Instance, the set
     * settings are erased!
     */
    @SuppressWarnings("unchecked")
    public void addAtom(AlloyAtom atom) {
        //repCheck();
        //System.out.println(atom);	        
        Dbg.chk(atom, "null atom");        
        _atomsToSets.put(atom, new HashSet());
        AlloyType type = atom.getType();
        
        // there was a bug when you had a relation r: A->univ, and you project on A, while 
        // the instance has a tuple {A1, A2}.  In this case, we want to show A2 on the 
        // cartoon slide for A1 even though A was projected.  However, this used to 
        // cause the assertion Dbg.chk(typeSet) to fail here because no set exists for 
        // type A in the new model.
        //
        // (this type of special case is the reason we only allow projecting on "top-level"
        // types, but the introduction of univ made those types not truly "top-level")

        if (_model.containsType(type)) {
            while (type != null) {

                Set typeSet = (Set) _typesToAtoms.get(type);
                // if the subtype was in the model, the supertype should be, too.
                Dbg.chk(typeSet);
                typeSet.add(atom);

                type = _model.getSuperForType(type);
            }
        } else {
            Dbg.fail("instance does not contain the type for this atom");

        }
                
        //repCheck();
    }
    

    /**
     * Associates the given AlloyAtom with the given AlloySet in this Instance.
     * If the specified AlloyAtom has not been added to this Instance, throws a
     * NullPointerException.  If the specified AlloyAtom is already associated
     * with the AlloySet, no change is made.
     */
    @SuppressWarnings("unchecked")
    public void addAtomToSet(AlloyAtom atom, AlloySet set) {
        //repCheck();
        Set atomSet = (Set)_setsToAtoms.get(set);
        Dbg.chk(atomSet,
         "You cannot add atom " + atom + " to set " + set + "because that set is not in the Model.");        
        atomSet.add(atom);

        Set sets = (Set)_atomsToSets.get(atom);
        sets.add(set);
        //repCheck();
    }

	/**
	 * addSet and removeSet not recommended for non-evaluation use
	 */
    @SuppressWarnings("unchecked")
    public void addSet(AlloySet set, String moduleName) { // jbaek - changed from protected to public
        _setsToAtoms.put(set, new HashSet());
        _model.getModuleByName(moduleName).addSet(set);
    }

    /**
     * normally you wouldn't remove a set unless you're using 
     * custom variables w/ the evaluator
     * 
     * Clears all mappings and removes set from model
     */
    void removeSet(AlloySet set, String moduleName) {
        Set atomSet = (Set)_setsToAtoms.get(set);
        Dbg.chk(atomSet,"set does not exist");        
        for (Iterator i = atomSet.iterator(); i.hasNext();) {
            AlloyAtom atom = (AlloyAtom)i.next();
            Set sets = (Set)_atomsToSets.get(atom);
            sets.remove(set);
        }
        _setsToAtoms.remove(set);

        _model.getModuleByName(moduleName).removeSet(set);

    }

    /**
     * Associates the given AlloyTuple with the give AlloyRelation in this
     * Instance.  If the specified AlloyRelation has not been added to this
     * Instance, throws a NullPointerException.  If the specified AlloyTuple is
     * already associated with the AlloyRelation, no change is made.
     */
    @SuppressWarnings("unchecked")
    public void addTupleToRelation(AlloyTuple tuple, AlloyRelation rel) {
        //repCheck();
        Set tuples = (Set)_relsToTuples.get(rel);
        Dbg.chk(tuples,
                "You cannot add tuple "
                    + tuple
                    + " to relation "
                    + rel
                    + " because that relation is not in the Model.");
                    
        tuples.add(tuple);
        //repCheck();
    }

	/**
	 * addRelation and removeRelation not recommended for use outside of
	 * evaluation stuff
	 */
    @SuppressWarnings("unchecked")
    public void addRelation(AlloyRelation rel, String moduleName) { // jbaek - changed from protected to public
        _relsToTuples.put(rel,new HashSet());
        _model.getModuleByName(moduleName).addRelation(rel);
    }

    void removeRelation(AlloyRelation rel, String moduleName) {
        Set tuples = (Set)_relsToTuples.get(rel);
        Dbg.chk(tuples, "relation does not exist");        
        _relsToTuples.remove(rel);
        _model.getModuleByName(moduleName).removeRelation(rel);
    }
    
    /**
     * toString(), yay!
     */
    public String toString() {
        //repCheck();
        StringBuffer ret = new StringBuffer();
        ret.append("Instance\n  Model: " + getModel());
        ret.append("\n  Atoms: " + getAllAtoms());
        ret.append("\n  Tuples: ");
        for (Iterator rels = _model.getRelations().iterator(); rels.hasNext();) {
            AlloyRelation rel = (AlloyRelation)rels.next();
            ret.append("\n  Tuples for " + rel + "\n" + getTuplesInRelation(rel));
        }
        for (Iterator sets = _model.getSets().iterator(); sets.hasNext();) {
            AlloySet set = (AlloySet)sets.next();
            ret.append("\n Atoms for " + set + "\n" + getAtomsInSet(set));
        }
        return ret.toString() + "\n" + _atomsToSets.toString() + _setsToAtoms.toString();
    }

    /*private boolean repCheck() {
        //quadratic time stuff....ooooh (used to debug)
          for (Iterator atoms = _atomsToSets.keySet().iterator(); atoms.hasNext();) {
            AlloyAtom atom = (AlloyAtom)atoms.next();
            //System.out.println(atom);
            for (Iterator sets = ((Set)_atomsToSets.get(atom)).iterator(); sets.hasNext();) {
        	AlloySet set = (AlloySet)sets.next();
        	//System.out.println("\t"+set);
        	Set temp = (Set)_setsToAtoms.get(set);
        	assertTrue(temp!=null);
        	assertTrue(temp.contains(atom));
            }		
        }
        for (Iterator sets = _setsToAtoms.keySet().iterator(); sets.hasNext();) {
            AlloySet set = (AlloySet)sets.next();
            //System.out.println(atom);
            for (Iterator atoms = ((Set)_setsToAtoms.get(set)).iterator(); atoms.hasNext();) {
        	AlloyAtom atom = (AlloyAtom)atoms.next();
        	//System.out.println("\t"+set);
        	Set temp = (Set)_atomsToSets.get(atom);
        	assertTrue(temp!=null);
        	assertTrue(temp.contains(set));
            }		
            }
        return true;
    }*/

}
