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
