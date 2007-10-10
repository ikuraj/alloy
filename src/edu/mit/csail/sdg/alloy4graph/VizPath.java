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

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;
import java.util.ArrayList;
import java.util.List;

/**
 * Mutable; represents a connected path.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizPath {

    /** The list of points. */
    private List<Point2D.Double> points = new ArrayList<Point2D.Double>();

    /** Constructs a new path consisting of (x1,y1) to (x2,y2) */
    public VizPath(double x1, double y1, double x2, double y2) {
       points.add(new Point2D.Double(x1,y1));
       points.add(new Point2D.Double(x2,y2));
    }

    /** Constructs a new path by joining a and b, assuming a's last point is at the same (X,Y) position as b's first point. */
    public VizPath(VizPath a, VizPath b) {
       points.addAll(a.points);
       points.remove(points.size()-1);
       points.addAll(b.points);
    }

    /** Returns the number of points in this path. */
    public int getPoints() { return points.size(); }

    /** Returns the i-th point's x coordinate (where i counts from 0...) */
    public double getX(int index) { return points.get(index).x; }

    /** Returns the i-th point's y coordinate (where i counts from 0...) */
    public double getY(int index) { return points.get(index).y; }

    /** Helper method that returns true if (x2>=x>=x1 or x1>=x>=x2) */
    private static boolean in(double x1, double x, double x2) {
       return (x1<=x && x<=x2) || (x2<=x && x<=x1);
    }

    /** Move the i-th point to the given coordinate (x,y) where i must be between 0 and points.size()-1 inclusively. */
    public void move(int i, double x, double y) {
       Point2D.Double pt=points.get(i);
       pt.x=x;
       pt.y=y;
    }

    /** Insert a new point into the list at the i-th position where i must be between 0 and points.size() inclusively. */
    public void add(int i, double x, double y) {
       points.add(i, new Point2D.Double(x,y));
    }

    /**
     * If the path intersects the horizontal line segment (x1,y)..(x2,y), then return true.
     *
     * <p> NOTE: this method may fail to find an intersection if the path is too horizontal at the intersection point.
     */
    public boolean intersectsHorizontal(double x1, double x2, double y) {
       if (!(x1<x2)) { double tmp=x1; x1=x2; x2=tmp; }
       Point2D.Double a=null;
       int i=(-1);
       for(Point2D.Double b:points) {
          i++; // so now, points.get(i)==b
          if (a==null) {a=b; continue;}
          if (!in(a.y, y, b.y)) continue;
          double m = (b.y-a.y)/(b.x-a.x);
          double intersect = (y-a.y)/m + a.x;
          if (in(x1, intersect, x2)) return true;
          a=b;
       }
       return false;
    }

    /**
     * If the path intersects the vertical line segment (x,y1)..(x,y2),
     * then return the index position corresponding to where the new point can be added.
     * else return -1.
     *
     * <p> NOTE: this method may fail to find an intersection if the path is too vertical at the intersection point.
     * <p> NOTE: if ans!=null, we will store the point of intersection into it.
     */
    public int intersectsVertical(double x, double y1, double y2, Point2D.Double ans) {
       if (!(y1<y2)) { double tmp=y1; y1=y2; y2=tmp; }
       Point2D.Double a=null;
       int i=(-1);
       for(Point2D.Double b:points) {
          i++; // so now, points.get(i)==b
          if (a==null) {a=b; continue;}
          if (!in(a.x, x, b.x)) continue;
          double m = (b.y-a.y)/(b.x-a.x);
          double intersect = (x-a.x)*m + a.y;
          if (in(y1, intersect, y2)) { if (ans!=null) { ans.x=x; ans.y=intersect; } return i; }
          a=b;
       }
       return -1;
    }

    /** Draws the path; we will attempt to round the corner a bit so that the turns are not too sharp. */
    public void draw(Artist gr) {
       Point2D.Double a=points.get(0);
       for(int i=1; i<points.size(); i++) {
          double gap;
          Point2D.Double b=points.get(i);
          Point2D.Double c=(i+1<points.size() ? points.get(i+1) : null);
          if (c!=null && b.y-a.y>8 && c.y-b.y>8) gap=8;
             else if (c!=null && b.y-a.y>4 && c.y-b.y>4) gap=4;
             else { gr.draw(new Line2D.Double(a.x, a.y, b.x, b.y), false); a=b; continue; }
          double m1 = (b.y-a.y)/(b.x-a.x), i1 = ((b.y-gap)-a.y)/m1 + a.x;
          double m2 = (c.y-b.y)/(c.x-b.x), i2 = ((b.y+gap)-b.y)/m2 + b.x;
          gr.draw(new Line2D.Double(a.x, a.y, i1, b.y-gap), false);
          a=new Point2D.Double(i2, b.y+gap);
          gr.draw(new QuadCurve2D.Double(i1, b.y-gap, b.x, b.y, a.x, a.y), false);
       }
    }
}
