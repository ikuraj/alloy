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

    /** The additional stack trace information. */
    private final Throwable other;

    /**
     * Constructs a new Err object.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     * @param ex - if nonnull, its stack trace will be merged with this exception's stack trace
     */
    public Err(Pos pos, String msg, Throwable ex) {
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
        this.msg = (msg==null ? "" : msg);
        this.other = ex;
    }

    /** Returns a copy of the stack trace. */
    @Override public StackTraceElement[] getStackTrace() {
        if (other==null) return super.getStackTrace();
        StackTraceElement[] that = other.getStackTrace();
        StackTraceElement[] here = super.getStackTrace();
        int same=0;
        if (here.length>0 && that.length>0) {
          while(same<here.length && same<that.length) {
            if (!here[here.length-same-1].equals(that[that.length-same-1])) break;
            same++;
          }
        }
        StackTraceElement[] ans = new StackTraceElement[that.length + here.length - same];
        int j=0;
        for(int i=0; i<that.length-same; i++, j++) ans[j]=that[i];
        for(int i=0; i<here.length; i++, j++) ans[j]=here[i];
        return ans;
    }

    /** Returns the stack trace as a String. */
    public final String getTotalTrace() {
        final StringBuilder sb=new StringBuilder();
        for(StackTraceElement st: getStackTrace()) { sb.append(st.toString()); sb.append("\n"); }
        return sb.toString();
    }

    /** Returns a textual description of the error. */
    @Override public final String getMessage() {
        return toString();
    }
}
