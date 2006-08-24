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

public class AlloySet implements AlloyNodeElement, Serializable {

	private static final long serialVersionUID = 1L;
	String _name;
    AlloyType _type;
    boolean _isCustom;

    public AlloySet(String name_, AlloyType type_) {
        _name = name_;
        _type = type_;
        _isCustom = false;
    }

    public String getName() {
        return _name;
    }

    public AlloyType getType() {
        return _type;
    }

    public boolean isCustom() {
        return _isCustom;
    }

    public void setCustom(boolean f) {
        _isCustom = f;
    }

    public int compareTo(Object o) {
        AlloySet s = (AlloySet)o;
        int temp = _name.compareTo(s._name);
        if (temp == 0) {
            return _type.compareTo(s._type);
        }
        else {
            return temp;
        }
    }

    public boolean equals(Object o) {
        if (o == null || !(o instanceof AlloySet)) {
            return false;
        }
        else {
            AlloySet s = (AlloySet)o;
            return ((_name.equals(s._name)) && (_type.equals(s._type)));
        }
    }

    public int hashCode() {
        return _type.hashCode() + _name.hashCode();
    }

    public String toString() {
        return "AlloySet: " + _name + "\nType: " + _type.toString();
    }

}
