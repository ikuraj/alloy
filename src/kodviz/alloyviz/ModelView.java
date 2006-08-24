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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * ModelView is a data structure that holds the model-specific visualization
 * information for a View.
 */
public class ModelView implements Serializable {

	private static final long serialVersionUID = 1L;
	private ProjectionFrame _projectionFrame;
    private VizMap _vizMap;
    private Map _modulesToCustVars;

    /**
     * Create a new ModelView in which no types are being projected and no
     * model-specific information is being stored.
     */
    public ModelView() {
        _projectionFrame = new ProjectionFrame();
        _vizMap = new VizMap();
        _modulesToCustVars = new HashMap();
    }

    @SuppressWarnings("unchecked")
    public ModelView copy() {
        ModelView mv = new ModelView();
        mv._projectionFrame = this._projectionFrame.copy();
        mv._vizMap = this._vizMap.copy();
        mv._modulesToCustVars = new HashMap();
        for (Iterator i=_modulesToCustVars.keySet().iterator(); i.hasNext();) {
        	String mod = (String)i.next();
        	Set vars = (Set)_modulesToCustVars.get(mod);
        	if (vars!=null) {
        		Set varsCopy = new HashSet();
        		for (Iterator j=vars.iterator();j.hasNext();) {
        			varsCopy.add(((CustVar)j.next()).copy());        			        			        			
        		}
        		mv._modulesToCustVars.put(mod, varsCopy);        		
        	}        	
        }
        return mv;
    }

    /**
     * Returns the ProjectionFrame in use by this ModelView.
     */
    public ProjectionFrame getProjectionFrame() {
        return _projectionFrame;
    }

    /**
     * Returns the VizMap in use by this ModelView.
     */
    public VizMap getVizMap() {
        return _vizMap;
    }
    
    /**
     * Returns an unmodifiable view of the set of custom variables
     * for a given module (indicate by name).  Returns null if 
     * no such set can be found     
     */
    @SuppressWarnings("unchecked")
    public Set getCustVars(String moduleName) {
		Set vars = (Set)_modulesToCustVars.get(moduleName);
		if (vars!=null) {
			return Collections.unmodifiableSet(vars);
		}
		else return null;    	    
    }
    
    /**
     * Adds a custom variable to a specific module (choose by name)          
     */
    @SuppressWarnings("unchecked")
    public void addCustVar(CustVar newVar) {
    	Set vars = (Set)_modulesToCustVars.get(newVar.getModuleName());
    	if (vars==null) {    	
    		vars = new HashSet();
    		_modulesToCustVars.put(newVar.getModuleName(), vars);
    	}
    	vars.add(newVar);    	
    }
    
    public void removeCustVar(CustVar newVar) {
		Set vars = (Set)_modulesToCustVars.get(newVar.getModuleName());
		vars.remove(newVar);
    }
    
    public void reset() {
    	_projectionFrame.clear();
    	_vizMap.reset();
    	_modulesToCustVars.clear();
    }

    /**
     * Returns a String representation of this ModelView.
     */
    public String toString() {
        return "ModelView:\n" + _projectionFrame.toString() + "\n" + _vizMap.toString();
    }
}
