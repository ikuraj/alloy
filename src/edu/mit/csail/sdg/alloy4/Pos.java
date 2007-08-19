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

import java.io.Serializable;

/**
 * Immutable; stores the filename and line/column position.
 *
 * <p> <b>Invariant:</b>     filename!=null && x>0 && y>0 && ((y2>y && x2>0) || (y2==y && x2>=x))
 *
 * <p><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Pos implements Serializable {

    /** To make sure the serialization form is stable. */
    private static final long serialVersionUID = 1L;

    /** The filename (it can be an empty string if unknown) */
    public final String filename;

    /** Additional annotation about this position (if can be null if there are no additional annotation) */
    public transient final Object comment;

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
        this.comment=null;
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
    public Pos(String filename, int x, int y, int x2, int y2) { this(filename,x,y,x2,y2,null); }

    /**
     * Constructs a new Pos object.
     * @param filename - the filename (it can be an empty string if unknown)
     * @param x - the starting column position (from 1..)
     * @param y - the starting row position (from 1..)
     * @param x2 - the ending column position (from 1..)
     * @param y2 - the ending row position (from 1..)
     * @param comment - the comment
     */
    private Pos(String filename, int x, int y, int x2, int y2, Object comment) {
        this.comment=comment;
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
     * Return a new position that is identical to the old position, except the comment is changed.
     * @param newComment - the new comment value
     */
    public Pos addComment(Object newComment) {
        return new Pos(filename, x, y, x2, y2, newComment);
    }

    /**
     * Return a new position that merges this and that
     * @param that - the other position object
     */
    public Pos merge(Pos that) {
        if (this==UNKNOWN || this==that) {
            return that;
        }
        if (that==null || that==UNKNOWN) {
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
        return new Pos(filename, x, y, x2, y2, (this.comment!=null ? this.comment : that.comment));
    }

    /**
     * Two Pos objects are equal if the filename x y x2 y2 are the same; the "Object comment" field is not considered.
     */
    @Override public boolean equals(Object other) {
        if (this==other) return true;
        if (!(other instanceof Pos)) return false;
        Pos that = (Pos) other;
        return x==that.x && y==that.y && x2==that.x2 && y2==that.y2 && filename.equals(that.filename);
    }

    /**
     * Returns a hash code consistent with equals()
     */
    @Override public int hashCode() {
        return x*111 + y*171 + x2*1731 + y2*2117;
    }

    /** Returns a short String representation of this position value. */
    public String toShortString() {
        String f=filename;
        int a=f.lastIndexOf('/'), b=f.lastIndexOf('\\');
        if (a<b) a=b;
        if (a>=0) f=f.substring(a+1);
        if (f.length()==0) return "line "+y+", column "+x;
        return "line "+y+", column "+x+", filename="+f;
    }

    /** Returns a String representation of this position value. */
    @Override public String toString() {
        String comment=(this.comment==null?"":(this.comment.toString()));
        if (filename.length()==0) return "line "+y+", column "+x+(comment.length()==0?"":(" ["+comment+"]"));
        return "line "+y+", column "+x+", filename="+filename+(comment.length()==0?"":(" ["+comment+"]"));
    }
}
