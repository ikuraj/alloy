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

import static java.awt.Color.BLACK;
import java.awt.Color;
import java.awt.Graphics2D;

/**
 * Mutable; represents a graphical edge.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizEdge extends DiGraph.DiEdge {

    /** Whether to draw an arrow head on the "from" node; default is false. */
    private boolean ahead = false;

    /** Whether to draw an arrow head on the "to" node; default is true. */
    private boolean bhead = true;

    /** The color of the edge; default is black; never null. */
    private Color color = BLACK;

    /** The line-style of the edge; default is SOLID; never null. */
    private VizStyle style = VizStyle.SOLID;

    /** The edge weight; always between 1 and 100 inclusively. */
    private int weight = 1;

    /** The actual path corresponding to this edge; null if we have not assigned the path yet. */
    private VizPath path = null;

    /** Returns the edge weight (which is always between 1 and 100 inclusively). */
    public int weight() { return weight; }

    /** Returns the line style; never null. */
    public VizStyle style() { return style; }

    /** Returns the line color; never null. */
    public Color color() { return color; }

    /** Returns true if we will draw an arrow head on the "from" node. */
    public boolean ahead() { return ahead; }

    /** Returns true if we will draw an arrow head on the "to" node. */
    public boolean bhead() { return bhead; }

    /** Sets the edge weight between 1 and 100. */
    public VizEdge set(int weightBetween1And100) {
        if (weightBetween1And100>=1 && weightBetween1And100<=100) weight=weightBetween1And100;
        return this;
    }

    /** Sets whether we will draw an arrow head on the "from" node, and whether we will draw an arrow head on the "to" node. */
    public VizEdge set(boolean from, boolean to) {
        this.ahead=from;
        this.bhead=to;
        return this;
    }

    /** Sets the line style. */
    public VizEdge set(VizStyle style) {
        if (style!=null) this.style=style;
        return this;
    }

    /** Sets the line color. */
    public VizEdge set(Color color) {
        if (color!=null) this.color=color;
        return this;
    }

    /** Returns a String representing this edge. */
    @Override public String toString() { return "Edge " + a() + (ahead?"<--":"---") + (bhead?"-->":"---") + b(); }

    /** Construct an edge from "from" to "to" with the given arrow head settings, then add the edge to the graph. */
    public VizEdge(VizNode from, VizNode to, boolean drawArrowHeadOnFrom, boolean drawArrowHeadOnTo, VizStyle style, Color color) {
       super(from, to); // The parent's constructor will add the edge A->B to the graph
       this.ahead=drawArrowHeadOnFrom;
       this.bhead=drawArrowHeadOnTo;
       if (style!=null) this.style=style;
       if (color!=null) this.color=color;
    }

    /** Construct an edge from "from" to "to" with the default arrow head settings, then add the edge to the graph. */
    public VizEdge(VizNode from, VizNode to) {
       this(from, to, false, true, null, null);
    }

    /** Reset the path as a straightline from the center of the "from" node to the center of the "to" node. */
    void resetPath() {
        VizNode a=a(), b=b();
        this.path=new VizPath(a.x(), a.y()-a.getUp()+a.getHeight()/2, b.x(), b.y()-b.getUp()+b.getHeight()/2);
    }

    /** Returns the current path; if the path was not yet assigned, it returns a straight line from "from" node to "to" node. */
    VizPath path() {
        if (path==null) resetPath();
        return path;
    }

    /** Add the given (x,y) point into the path at the i-th position in the path (where i counts from 0...) */
    void pathAdd(int i, double x, double y) {
        if (path==null) resetPath();
        path.add(i,x,y);
    }

    /** Returns true iff the edge intersects the given point (px,py), given the current zoom scale. */
    public boolean intersects(double px, double py, double scale) {
        if (a() == b()) return false;
        double fudge=10/scale; // we enlarge (px,py) into a square of size (fudge*2) x (fudge*2) when testing for intersection
        return path.intersectsVertical(px, py-fudge, py+fudge, null)>=0 || path.intersectsHorizontal(px-fudge, px+fudge, py);
    }

    /** Assuming this edge's coordinates have been assigned, and given the current zoom scale, draw the edge. */
    public void draw(Graphics2D gr, double scale, Object highlight) {
       if (a()==b()) return;
       gr.setColor(color);
       style.set(gr, scale);
       // Concatenate this path and its connected segments into a single VizPath object
       VizPath p=null;
       VizEdge e=this;
       while(e.a().shape()==null) e=e.a().inEdges().get(0); // Let e points to the first segment of this chain of connected segments
       while(true) {
          if (e==highlight) { gr.setColor(Color.RED); VizStyle.BOLD.set(gr,scale); } // If this segment is highlighted, change color!
          p = (p==null) ? e.path : new VizPath(p, e.path);
          if (e.b().shape()!=null) break;
          e = e.b().outEdges().get(0);
       }
       // Now draw the combined VizPath
       p.draw(gr);
       VizStyle.SOLID.set(gr,scale);
    }

    /** Assuming this edge's coordinates have been assigned, and given the current zoom scale, draw the arrow heads if any. */
    public void drawArrowhead(Graphics2D gr, double scale, Object highlight) {
       // Check to see if this edge is highlighted or not
       if (a()==b()) return;
       VizEdge e=this;
       while(e.a().shape()==null) e=e.a().inEdges().get(0); // Let e points to the first segment of this chain of connected segments
       while(true) {
          if (e==highlight) { gr.setColor(Color.RED); VizStyle.BOLD.set(gr,scale); break; }
          if (e.b().shape()!=null) { gr.setColor(color); style.set(gr,scale); break; }
          e = e.b().outEdges().get(0);
       }
       int n = path.getPoints();
       if (ahead && a().shape()!=null) {
          gr.fillOval((int)(path.getX(0)  -5), (int)(path.getY(0)  -5), 10, 10);
       }
       if (bhead && b().shape()!=null) {
          gr.fillOval((int)(path.getX(n-1)-5), (int)(path.getY(n-1)-5), 10, 10);
       }
       /*
       final double fan = Math.toRadians(18); // the angle to fan out the arrow head
       int tip = (fm.getMaxAscent()+fm.getMaxDescent());
       if (ahead && a.shape!=null) {
          double t=Math.PI+Math.atan2(a.y-b.y, a.x-b.x);
          double gx1 = a.x + tip*Math.cos(t-fan), gy1 = a.y + tip*Math.sin(t-fan);
          double gx2 = a.x + tip*Math.cos(t+fan), gy2 = a.y + tip*Math.sin(t+fan);
          GeneralPath gp=new GeneralPath(); gp.moveTo((float)gx1,(float)gy1); gp.lineTo(a.x, a.y); gp.lineTo((float)gx2, (float)gy2); gp.closePath(); gr.fill(gp);
       }
       if (bhead && b.shape!=null) {
          double t=Math.PI+Math.atan2(b.y-a.y, b.x-a.x);
          double gx1 = b.x + tip*Math.cos(t-fan), gy1 = b.y + tip*Math.sin(t-fan);
          double gx2 = b.x + tip*Math.cos(t+fan), gy2 = b.y + tip*Math.sin(t+fan);
          GeneralPath gp=new GeneralPath(); gp.moveTo((float)gx1,(float)gy1); gp.lineTo(b.x, b.y); gp.lineTo((float)gx2, (float)gy2); gp.closePath(); gr.fill(gp);
       }
       */
    }
}
