package edu.mit.csail.sdg.alloy4graph;

import java.awt.BasicStroke;
import static java.awt.BasicStroke.CAP_ROUND;
import static java.awt.BasicStroke.JOIN_ROUND;
import java.awt.Graphics2D;

/** Immutable; enumerates the possible line styles (SOLID, DASHED, DOTTED...) */

public enum VizStyle {

    /** Solid line. */
    SOLID("Solid", "solid"),

    /** Dashed line. */
    DASHED("Dashed", "dashed"),

    /** Dotted line. */
    DOTTED("Dotted", "dotted"),

    /** Bold line. */
    BOLD("Bold", "bold");

    /** The brief description of this line style. */
    private final String longName;

    /** Constructs a VizStyle with the given long name and short name. */
    private VizStyle(String longName, String shortName) { this.longName=longName; }

    /** The pattern for dotted line. */
    private static float[] dot = new float[]{1f,3f};

    /** The pattern for dashed line. */
    private static float[] dashed = new float[]{6f,3f};

    /**
     * Modifies the given Graphics2D object to use the line style representing by this object.
     * <p> NOTE: as a special guarantee, if gr2d==null, then this method returns immediately without doing anything.
     * <p> NOTE: just like the typical AWT and Swing methods, this method can be called only by the AWT event dispatching thread.
     */
    void set(Graphics2D gr2d, double scale) {
       if (gr2d==null) return;
       BasicStroke bs;
       switch(this) {
          case BOLD:   bs=new BasicStroke(scale>1 ? (float)(2.5d/scale) : 2.5f); break;
          case DOTTED: bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f, CAP_ROUND, JOIN_ROUND, 15f, dot, 0f); break;
          case DASHED: bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f, CAP_ROUND, JOIN_ROUND, 15f, dashed, 5f); break;
          default:     bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f);
       }
       gr2d.setStroke(bs);
    }

    /** Returns a brief description of this line style. */
    @Override public String toString() { return longName; }
}
