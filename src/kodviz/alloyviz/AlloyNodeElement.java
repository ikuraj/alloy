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

/**
 * AlloyNodeElement is an interface that specifies the functionality needed for
 * a key that can be mapped to a NodeViz in the VizMap.
 */
public interface AlloyNodeElement extends Comparable {

    /**
     * Return the name of this AlloyNodeElement.
     */
    public String getName();

    /**
     * Indicates whether or not this AlloyNodeElement was created 
     * customarily (e.g. with the evaluator).  Defaults to false     
     */
    public boolean isCustom();

    /**
     * Sets the value to be returned by isCustom               
     */
    public void setCustom(boolean f);

    /**
     * Compare this AlloyNodeElement to the specified Object.
     */
    public int compareTo(Object o);

    /**
     * Tests whether this AlloyNodeElement is equal to the specified Object.
     */
    public boolean equals(Object o);

    /**
     * Returns a hash code for this AlloyNodeElement.
     */
    public int hashCode();

}
