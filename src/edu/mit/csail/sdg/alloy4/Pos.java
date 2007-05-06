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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; stores the filename and line/column position.
 *
 * <p> <b>Invariant:</b>     filename!=null && x>0 && y>0
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Pos {

    /** The filename (it can be an empty string if unknown) */
    public final String filename;

    /** The starting column position (from 1..) */
    public final int x;

    /** The starting row position (from 1..) */
    public final int y;

    /** The ending column position (from 1..) */
    public final int x2;

    /** The ending row position (from 1..) */
    public final int y2;

    /** The default "unknown" location. */
    public static final Pos UNKNOWN=new Pos("",1,1);

    /**
     * Constructs a new Pos object.
     * @param filename - the filename (it can be an empty string if unknown)
     * @param x - the column position (from 1..)
     * @param y - the row position (from 1..)
     */
    public Pos(String filename, int x, int y) {
        this.filename=(filename==null?"":filename);
        this.x=(x>0?x:1);
        this.y=(y>0?y:1);
        this.x2=this.x;
        this.y2=this.y;
    }

    /**
     * Constructs a new Pos object.
     * @param filename - the filename (it can be an empty string if unknown)
     * @param x - the starting column position (from 1..)
     * @param y - the starting row position (from 1..)
     * @param x2 - the ending column position (from 1..)
     * @param y2 - the ending row position (from 1..)
     */
    public Pos(String filename, int x, int y, int x2, int y2) {
        this.filename=(filename==null?"":filename);
        this.x=(x>0?x:1);
        this.y=(y>0?y:1);
        if (y2<(this.y)) {
            y2=this.y;
        }
        if (y2==this.y) {
            if (x2<(this.x)) {
                x2=this.x;
            }
        } else {
            if (x2<1) {
                x2=1;
            }
        }
        this.x2=x2;
        this.y2=y2;
    }

    /**
     * Return a new position that merges this and that
     * @param that - the other position object
     */
    public Pos merge(Pos that) {
        if (this==UNKNOWN) {
            return that;
        }
        if (that==UNKNOWN) {
            return this;
        }
        int x=this.x, y=this.y, x2=that.x2, y2=that.y2;
        if (that.y<y || (that.y==y && that.x<x)) {
            x=that.x;
            y=that.y;
        }
        if (this.y2>y2 || (this.y2==y2 && this.x2>x2)) {
            x2=this.x2;
            y2=this.y2;
        }
        return new Pos(filename, x, y, x2, y2);
    }

    /**
     * Return a new position that merges every Pos object in the array.
     * <p> (If pos[] contains null entries, those null entries are ignored.)
     * <p> (If pos[] contains only null entries, or if the array is empty, or if pos==null, we return Pos.UNKNOWN)
     */
    public static Pos merge(Pos... pos) {
        Pos ans=null;
        if (pos!=null) {
            for(int i=0; i<pos.length; i++) {
                if (pos[i]!=null && pos[i]!=UNKNOWN) {
                   ans = (ans==null) ? pos[i] : ans.merge(pos[i]);
                }
            }
        }
        return ans==null ? UNKNOWN : ans;
    }

    /** Returns a String representation of this position value. */
    @Override public String toString() {
        if (filename.length()==0) {
            return "line "+y+", column "+x;
        }
        return "line "+y+", column "+x+", filename="+filename;
    }
}
