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

package edu.mit.csail.sdg.alloy4;

/**
 * Mutable; this class holds a mutable integer.
 *
 * <p><b>Thread Safety:</b> Safe
 */

public final class IntegerBox {

    /** The integer value. */
    private int value;

    /** Constructs an IntegerBox with the given initial value. */
    public IntegerBox(int initialValue) {
        value=initialValue;
    }

    /** Changes the current value. */
    public synchronized void set(int newValue) {
        value=newValue;
    }

    /** Returns the current value. */
    public synchronized int get() {
        return value;
    }
}
