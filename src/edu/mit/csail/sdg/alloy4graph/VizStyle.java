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

package edu.mit.csail.sdg.alloy4graph;

/** Immutable; enumerates the possible line styles (SOLID, DASHED, DOTTED...) */

public enum VizStyle {

    /** Solid line. */
    SOLID("Solid", "solid"),

    /** Dashed line. */
    DASHED("Dashed", "dashed"),

    /** Dotted line. */
    DOTTED("Dotted", "dotted"),

    /** Bold line. */
    BOLD("Bold", "bold");

    /** The brief description of this line style. */
    private final String longName;

    /** Constructs a VizStyle with the given long name and short name. */
    private VizStyle(String longName, String shortName) { this.longName=longName; }

    /** Returns a brief description of this line style. */
    @Override public String toString() { return longName; }
}
