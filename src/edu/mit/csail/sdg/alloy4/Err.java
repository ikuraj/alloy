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
 * <p><b>Invariant</b>:       pos!=null and msg!=null
 *
 * <p><b>Thread Safety:</b>   Safe (since objects of this class are immutable).
 */

public abstract class Err extends Exception {

    /** This stores the filename/line/row information (Pos.UNKNOWN if unknown). */
    public final Pos pos;

    /** The actual error message. */
    public final String msg;

    /** The additional stack trace information. */
    private final StackTraceElement[] trace;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     * @param trace - if nonnull, it is a list of additional StackTraceElement(s) to add into this Err object's StackTrace
     */
    public Err(Pos pos, String msg, StackTraceElement[] trace) {
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
        this.msg = (msg==null ? "" : msg);
        if (trace==null || trace.length==0) {
            this.trace=null;
        } else {
            this.trace = new StackTraceElement[trace.length];
            for(int i=0; i<this.trace.length; i++) this.trace[i]=trace[i];
        }
    }

    /** Retrieves the complete stack trace as a String */
    public final String getTotalTrace() {
        StringBuilder sb=new StringBuilder();
        if (trace!=null) for(StackTraceElement st: trace) { sb.append(st.toString()); sb.append("\n"); }
        for(StackTraceElement st: getStackTrace()) { sb.append(st.toString()); sb.append("\n"); }
        return sb.toString();
    }

    /** Returns a textual description of the error. */
    @Override public final String getMessage() {
        return toString();
    }
}
