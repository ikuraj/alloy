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

/**
 * Stores info about a custom (i.e. defined via evaluator) 
 * variable in the viz.  CustVar's are associated with a 
 * module name, as they will appear under that module's tab
 */
public class CustVar implements Serializable {

	private static final long serialVersionUID = 1L;

	private String _name, _expr, _moduleName;

    public CustVar(String name_, String expr_, String moduleName_) {
        _name = name_;
        _expr = expr_;
        _moduleName = moduleName_;
    }
    
    public CustVar copy() {
    	return new CustVar(_name, _expr, _moduleName);
    }

    public String getName() {
        return _name;
    }

    public String getExpr() {
        return _expr;
    }

    public String getModuleName() {
        return _moduleName;
    }

    public boolean equals(Object o) {
        if (o == null || !(o instanceof CustVar)) {
            return false;
        }

        if (_name.equals(((CustVar)o)._name)
            && _expr.equals(((CustVar)o)._expr)
            && _moduleName.equals(((CustVar)o)._moduleName)) {
            return true;
        }
        else {
            return false;
        }

    }
    public int hashCode() {
        return _name.hashCode() + _expr.hashCode() + _moduleName.hashCode();
    }

}
