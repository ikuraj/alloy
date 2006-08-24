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
import java.util.Iterator;
import java.util.List;

public class AlloyRelation implements Comparable, Serializable {

	private static final long serialVersionUID = 1L;
	String _name;
    List _types; // list of AlloyType's
    boolean _isCustom;

    public AlloyRelation(String name_, List types_) {
        _name = name_;
        _types = types_;
        _isCustom = false;
    }

    public String getName() {
        return _name;
    }

	/**
	 * defaults to false	 
	 */
    public boolean isCustom() {
        return _isCustom;
    }

    public void setCustom(boolean f) {
        _isCustom = f;
    }

    @SuppressWarnings("unchecked")
    public List getTypes() {
        return Collections.unmodifiableList(_types);
    }

    public int getArity() {
        return _types.size();
    }

    public int compareTo(Object o) {
        AlloyRelation r = (AlloyRelation)o;
        int temp = _name.compareTo(r._name);
        if (temp == 0) {
            // this could potentially break....but you shouldn't have relations
            // with the same name anyhow...
            Iterator thisTypes = _types.iterator();
            Iterator rTypes = r._types.iterator();
            /*
             * Okay, basically what this says is, if the names are equal, you
             * start comparing elements of the lists, in order from start to
             * finish (these elements are all AlloyTypes).  At the first pair of
             * different elements, you return the value of calling compareTo on
             * those elements.  If one of the lists is a sublist of the other
             * (i.e. one of the iterators runs out before you find a pair that
             * doesn't match), the longer list is declared "higher."  Finally,
             * if both lists contain the same elements in the same order, you
             * return 0.
             */
            for (; thisTypes.hasNext();) {
                if (rTypes.hasNext()) {
                    int val = ((AlloyType)thisTypes.next()).compareTo(((AlloyType)rTypes.next()));
                    if (val != 0) {
                        return val;
                    }
                }
                else {
                    return 1;
                }
            }
            if (rTypes.hasNext()) {
                return -1;
            }
            else {
                return 0;
            }
        }
        else {
            return temp;
        }
    }

    public boolean equals(Object o) {
        if (o == null || !(o instanceof AlloyRelation)) {
            return false;
        }
        else {
            AlloyRelation r = (AlloyRelation)o;
            return (_name.equals(r._name) && _types.equals(r._types));
        }
    }

    public int hashCode() {
        return _name.hashCode() + _types.hashCode();
    }

    public String toString() {
        return "AlloyRelation: " + _name + "->" + _types.toString();
    }
}
