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
import java.util.List;

/**
 * This class is used to represent a module of an Alloy model.  It will
 * provide means to extract the type hierarchy of the model, the sets of the
 * model, and the relations of the model.
 */
public class AlloyModule {

    String _name;

    List _types, _sets, _rels;

    //TypeStructure _typeStruct;

    /**
     * Creates an AlloyModule from lists of AlloyType's, AlloySet's, and AlloyRelation's     
     */
    @SuppressWarnings("unchecked")
    public AlloyModule(String moduleName_, List types_, List sets_, List rels_) {
        _name = moduleName_;
        _types = new ArrayList(types_);
        _sets = new ArrayList(sets_);
        //_typeStruct = typeStruct_;
        _rels = new ArrayList(rels_);
        
        Collections.sort(_types);
        Collections.sort(_sets);
        Collections.sort(_rels);
    }

    @SuppressWarnings("unchecked")
    public AlloyModule copy() {
        return new AlloyModule(_name, new ArrayList(_types), new ArrayList(_sets), new ArrayList(_rels));
    }

    public String getName() {
        return _name;
    }

    @SuppressWarnings("unchecked")
    public List getSets() {
        return Collections.unmodifiableList(_sets);
    }

    @SuppressWarnings("unchecked")
    public List getTypes() {
        return Collections.unmodifiableList(_types);
    }
    
    public boolean containsType(AlloyType t) {
        return _types.contains(t);
    }

    /*
    public AlloyType getSuperForType(AlloyType t) {
        return (AlloyType)_typeStruct.getSuper(t);
    }*/

    @SuppressWarnings("unchecked")
    public List getRelations() {
        return Collections.unmodifiableList(_rels);
    }

    /*public Set getSubsForType(AlloyType t) {
        return _typeStruct.getSubTypes(t);
    }*/

    public boolean equals(Object o) {
        if ((o == null) || !(o instanceof AlloyModule)) {
            return false;
        }
        AlloyModule mod = (AlloyModule)o;
        return (
            getName().equals(mod.getName())
                && getSets().equals(mod.getSets())
                && getRelations().equals(mod.getRelations()));
                //&& _typeStruct.equals(mod._typeStruct));
    }

    public int hashCode() {
        return _name.hashCode() + _sets.hashCode() + _rels.hashCode();
    }

    public String toString() {
        return "Module "
            + _name
            + ":\n"
            + "Types:\n"
            + this.getTypes()
            + "\n\n\nSets:\n"
            + _sets.toString()
            + "\n\n\nRelations:\n"
            + _rels.toString();
    }

    // forced to make class mutable to handle evaluation stuff...
    @SuppressWarnings("unchecked")
    public void addSet(AlloySet set) {
        if (!_sets.contains(set)) {
            _sets.add(set);
            Collections.sort(_sets);
        }
    }
    
    public void removeSet(AlloySet set) {
    	_sets.remove(set);
    }

    @SuppressWarnings("unchecked")
    public void addRelation(AlloyRelation rel) {
        if (!_rels.contains(rel)) {
            _rels.add(rel);
            Collections.sort(_rels);
        }
    }
    
    public void removeRelation(AlloyRelation rel) {
    	_rels.remove(rel);    
    }
    
}
