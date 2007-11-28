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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

/**
 * Immutable; it's the abstract superclass extended by AlloyType, AlloySet, and AlloyRelation.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public abstract class AlloyElement implements Comparable<AlloyElement> {

    /** The name of the element. */
    private final String name;

    /** Returns the name of this element. */
    public String getName() { return name; }

    /** Constructs a new AlloyElement with that name. */
    AlloyElement(String name) { this.name=name; }
}
