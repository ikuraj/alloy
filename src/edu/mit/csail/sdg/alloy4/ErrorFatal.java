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
 * Immutable; represents a fatal error.
 *
 * <p><b>Invariant:</b>       pos!=null && msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public final class ErrorFatal extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new fatal error.
     * @param msg - the actual error message
     */
    public ErrorFatal(String msg) {
        super(null, msg, null);
    }

    /**
     * Constructs a new fatal error.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public ErrorFatal(Pos pos, String msg) {
        super(pos, msg, null);
    }

    /**
     * Two Err objects are equal if the type, position, and message are the same; the "Throwable other" field is not considered.
     */
    @Override public boolean equals(Object other) {
        if (this==other) return true;
        if (!(other instanceof ErrorFatal)) return false;
        Err that = (Err) other;
        return pos.equals(that.pos) && msg.equals(that.msg);
    }

    /**
     * Returns a hash code consistent with equals()
     */
    @Override public int hashCode() {
        return msg.hashCode();
    }

    /** Returns a textual description of the error. */
    @Override public String toString() {
        if (pos==Pos.UNKNOWN) {
            return "Fatal error:\n"+msg;
        }
        if (pos.filename.length()>0) {
            return "Fatal error in "+pos.filename+" at line "+pos.y+" column "+pos.x+":\n"+msg;
        }
        return "Fatal error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }

    /**
     * Constructs a new fatal error with the additional stacktrace entries from "ex" inserted
     * @param msg - the actual error message
     * @param ex - the underlying exception
     */
    public ErrorFatal(String msg, Throwable ex) {
        super(null,msg,ex);
    }
}
