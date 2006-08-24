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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class is used to represent an Alloy model.
 */
public class Model {

    List _modules;
    Map _namesToModules;
    String _name;
    TypeStructure _ts;

    /**
     * use a List of AlloyModule's
     */
    public Model(List modules_, TypeStructure ts_, String name_) {
        _modules = modules_;
        _name = name_;
        _namesToModules = new HashMap();
        _ts = ts_;
        generateModuleMappings();
    }
	
    @SuppressWarnings("unchecked")
	public Model copy() {
		List newModules = new ArrayList();
		for (Iterator i=_modules.iterator();i.hasNext();) {
			newModules.add(((AlloyModule)i.next()).copy());
		}
		return new Model(newModules, _ts.copy(), _name);		
	}
	
    // kind of makes this type not immutable anymore...but is a quick
    // and reasonable solution
    public void setName(String name) {
        _name = name;
    }

    public String getName() {
        return _name;
    }

    @SuppressWarnings("unchecked")
    private void generateModuleMappings() {
        for (Iterator mods = _modules.iterator(); mods.hasNext();) {
            AlloyModule mod = (AlloyModule)mods.next();
            _namesToModules.put(mod.getName(), mod);
        }
    }

    public AlloyModule getModuleByName(String name) {
        return (AlloyModule)_namesToModules.get(name);
    }

    @SuppressWarnings("unchecked")
    public List getModules() {
        return Collections.unmodifiableList(_modules);
    }

    // NOTE:
    // the return values for the following three methods are NOT sorted
    //
    
    /**
     * Returns null if type has no supertype
     */
    public AlloyType getSuperForType(AlloyType type) {
        return _ts.getSuper(type);
    }    
    
    /**
     * Returns true if model contains type
     */
    public boolean containsType(AlloyType type) {
        return _ts.containsType(type);
    }
    
    public AlloyModule getModuleOfType(AlloyType type) {
        for (Iterator iter = _modules.iterator(); iter.hasNext();) {
            AlloyModule module = (AlloyModule) iter.next();
            if (module.containsType(type)) {
                return module;
            }
        }
        
        return null;
    }

    @SuppressWarnings("unchecked")
    public List getTypes() {
        List temp = new ArrayList();
        for (Iterator iter = _modules.iterator(); iter.hasNext();) {
            temp.addAll(((AlloyModule)iter.next()).getTypes());
        }
        return Collections.unmodifiableList(temp);
    }

    @SuppressWarnings("unchecked")
    public List getSets() {
        List temp = new ArrayList();
        for (Iterator iter = _modules.iterator(); iter.hasNext();) {
            temp.addAll(((AlloyModule)iter.next()).getSets());
        }
        return Collections.unmodifiableList(temp);
    }

    /**
     * returns null if this type does not exist in the model
     * returns an empty set if this is a leaf
     * returns subtypes of subtypes as well, etc.
     */
    public Set getSubsForType(AlloyType t) {
        return _ts.getSubTypes(t);
    }

    @SuppressWarnings("unchecked")
    public List getRelations() {
        List temp = new ArrayList();
        for (Iterator iter = _modules.iterator(); iter.hasNext();) {
            temp.addAll(((AlloyModule)iter.next()).getRelations());
        }
        return Collections.unmodifiableList(temp);
    }

    public boolean equals(Object o) {
        if ((o == null) || !(o instanceof Model)) {
            return false;
        }
        return getModules().equals(((Model)o).getModules());
    }

    public int hashCode() {
        return _modules.hashCode();
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();

        for (Iterator modulesIter = _modules.iterator(); modulesIter.hasNext();) {
            AlloyModule mod = (AlloyModule)modulesIter.next();
            sb.append(mod.toString());
        }
        return sb.toString();
    }
}
