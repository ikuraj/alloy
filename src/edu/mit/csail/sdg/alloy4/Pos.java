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
        if (y2<(this.y)) y2=this.y;
        if (y2==this.y) { if (x2<(this.x)) x2=this.x; } else { if (x2<1) x2=1; }
        this.x2=x2;
        this.y2=y2;
    }

    /**
     * Return a new position that spans from the start of (this.x, this.y), up to and including (other.x2, other.y2)
     * @param other - the other position object
     */
    public Pos upto(Pos other) {
        int x=this.x, y=this.y, x2=other.x2, y2=other.y2;
        if (other.y<y || (other.y==y && other.x<x)) { x=other.x; y=other.y; }
        if (this.y2>y2 || (this.y2==y2 && this.x2>x2)) { x2=this.x2; y2=this.y2; }
        return new Pos(filename, x, y, x2, y2);
    }

    /** Returns a String representation of this position value. */
    @Override public String toString() {
        if (filename.length()==0) return "(line "+y+", column "+x+")";
        return "(line "+y+", column "+x+", filename="+filename+")";
    }
}
