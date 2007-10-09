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

import static java.awt.BasicStroke.CAP_ROUND;
import static java.awt.BasicStroke.JOIN_ROUND;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;

/**
 * This class abstracts the drawing operations so that we can (potentially)
 * draw the graph using different frameworks; eg. Java2D, asPNG, asPDF...
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class Artist {

    /** The corresponding Graphics2D object. */
    private Graphics2D gr;

    /** Construct an artist that acts as a wrapper around the given Graphics2D object. */
    public Artist(Graphics2D gr) { this.gr=gr; }

    /** Draws the outline of the given shape. */
    public void draw(Shape x) { gr.draw(x); }

    /** Fills the given shape. */
    public void fill(Shape x) { gr.fill(x); }

    /** Shifts the coordinate space by the given amount. */
    public void translate(int x, int y) { gr.translate(x,y); }

    /** Changes the current color. */
    public void setColor(Color c) { gr.setColor(c); }

    /** Changes the current font. */
    public void setFont(Font fn) { gr.setFont(fn); }

    /** Fills a circle of the given radius, centered at (0,0) */
    public void fillCircle(int radius) { gr.fillArc(-radius, -radius, radius*2, radius*2, 0, 360); }

    /** Draws a circle of the given radius, centered at (0,0) */
    public void drawCircle(int radius) { gr.drawArc(-radius, -radius, radius*2, radius*2, 0, 360); }

    /** Draws a line from (x1,y1) to (x2,y2) */
    public void drawLine(int x1, int y1, int x2, int y2) { gr.drawLine(x1,y1,x2,y2); }

    /** Draws the given string at (x,y) */
    public void drawString(String text, int x, int y) { gr.drawString(text,x,y); }

    /**
     * Modifies the given Graphics2D object to use the line style representing by this object.
     * <p> NOTE: as a special guarantee, if gr2d==null, then this method returns immediately without doing anything.
     * <p> NOTE: just like the typical AWT and Swing methods, this method can be called only by the AWT event dispatching thread.
     */
    public void set(VizStyle style, double scale) {
        BasicStroke bs;
        switch(style) {
          case BOLD:   bs=new BasicStroke(scale>1 ? (float)(2.5d/scale) : 2.5f); break;
          case DOTTED: bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f, CAP_ROUND, JOIN_ROUND, 15f, dot, 0f); break;
          case DASHED: bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f, CAP_ROUND, JOIN_ROUND, 15f, dashed, 5f); break;
          default:     bs=new BasicStroke(scale>1 ? (float)(1.0d/scale) : 1f);
        }
        gr.setStroke(bs);
    }

    /** The pattern for dotted line. */
    private static float[] dot = new float[]{1f,3f};

    /** The pattern for dashed line. */
    private static float[] dashed = new float[]{6f,3f};
}
