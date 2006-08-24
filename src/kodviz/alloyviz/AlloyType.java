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

public class AlloyType implements AlloyNodeElement, Serializable {

	private static final long serialVersionUID = 1L;

	String _name;

    private boolean _isCustom;

    public AlloyType(String name_) {
        _name = name_;
        _isCustom = false;
    }

    public String getName() {
        return _name;
    }

    public boolean isCustom() {
        return _isCustom;
    }

    public void setCustom(boolean f) {
        //_isCustom = f;  a type is never custom
    }

    public int compareTo(Object o) {
        AlloyType temp = (AlloyType)o;
        return this._name.compareTo(temp._name);
    }

    public boolean equals(Object o) {
        if (o == null || !(o instanceof AlloyType)) {
            return false;
        }
        else {
            AlloyType t = (AlloyType)o;
            return (_name.equals(t._name));
        }
    }

    public int hashCode() {
        return _name.hashCode();
    }

    public String toString() {
        return "AlloyType: " + _name;
    }

}
