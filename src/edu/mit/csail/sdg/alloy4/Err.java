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
 * Immutable; this is the abstract super class of the various possible errors.
 *
 * <p><b>Invariant</b>:       pos!=null && msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public abstract class Err extends Exception {

    /** This stores the filename/line/row information (Pos.UNKNOWN if unknown). */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     * @param ex - if nonnull, it will be recorded as the cause of this exception
     */
    Err(Pos pos, String msg, Throwable ex) {
        super((msg==null ? "" : msg), ex);
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
        this.msg = (msg==null ? "" : msg);
    }
}
