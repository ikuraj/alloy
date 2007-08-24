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
 * Immutable; this defines the set of possible edge directions.
 *
 * <p><b>Thread Safety:</b>  Safe (since everything is constructed statically)
 */

public final class DotDirection extends DotAttribute{

    /**
     * Constructs a new DotDirection object.
     * @param dotText - the text to write into the .dot file
     */
    private DotDirection(String dotText) { super(dotText, dotText, null); }

    /** Going forward. */     public static final DotDirection FORWARD = new DotDirection("forward");
    /** Going backwards. */   public static final DotDirection BACK = new DotDirection("back");
    /** Going both ways. */   public static final DotDirection BOTH = new DotDirection("both");
}
