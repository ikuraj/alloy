package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; stores the filename and line/column position.
 *
 * <p/> <b>Invariant:</b>     filename!=null && x>0 && y>0
 *
 * <p/><b>Thread Safety:</b>  Safe (since objects of this class are immutable).
 */

public final class Pos {

    /** The filename (it can be an empty string if unknown) */
    public final String filename;

    /** The column position (from 1..) */
    public final int x;

    /** The row position (from 1..) */
    public final int y;

    /** The column position (from 1..) */
    public final int x2;

    /** The row position (from 1..) */
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
}
