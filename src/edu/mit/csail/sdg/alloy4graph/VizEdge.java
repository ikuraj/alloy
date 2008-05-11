/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4graph;

import java.awt.Color;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import static java.awt.Color.BLACK;
import static java.lang.StrictMath.PI;
import static java.lang.StrictMath.sin;
import static java.lang.StrictMath.cos;
import static java.lang.StrictMath.atan2;
import static java.lang.StrictMath.toRadians;
import static edu.mit.csail.sdg.alloy4graph.Artist.getBounds;
import static edu.mit.csail.sdg.alloy4graph.VizGraph.selfLoopA;
import static edu.mit.csail.sdg.alloy4graph.VizGraph.selfLoopGL;
import static edu.mit.csail.sdg.alloy4graph.VizGraph.selfLoopGR;

/**
 * Mutable; represents a graphical edge.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final strictfp class VizEdge extends DiGraph.DiEdge {

    // =============================== cached for performance efficiency ===================================

    /** The maximum ascent and descent. We deliberately do NOT make this field "static" because only AWT thread can call Artist. */
    private final int ad = Artist.getMaxAscentAndDescent();

    // =============================== per-edge settings ===================================================

    /** a user-provided annotation that will be associated with this edge (can be null) */
    public final Object uuid;

    /** The label; can be an empty string if there is no label; NOTE: label is only shown if the start node is not a dummy node. */
    private String label = "";

    /** The location and size of the label box (if it's been calculated) */
    private AvailableSpace.Box labelbox = new AvailableSpace.Box();

    /** The group that this edge belongs to. */
    final Object group;

    /** Return the X coordinate of the top-left corner of the label box. */
    public int getLabelX() { return labelbox.x; }

    /** Return the Y coordinate of the top-left corner of the label box. */
    public int getLabelY() { return labelbox.y; }

    /** Return the width of the label box. */
    public int getLabelW() { return labelbox.w; }

    /** Return the height of the label box. */
    public int getLabelH() { return labelbox.h; }

    /** Whether to draw an arrow head on the "from" node; default is false. */
    private boolean ahead = false;

    /** Whether to draw an arrow head on the "to" node; default is true. */
    private boolean bhead = true;

    /** The color of the edge; default is black; never null. */
    private Color color = BLACK;

    /** The line-style of the edge; default is SOLID; never null. */
    private VizStyle style = VizStyle.SOLID;

    /** The edge weight; always between 1 and 10000 inclusively. */
    private int weight = 1;

    /** The actual path corresponding to this edge; null if we have not assigned the path yet. */
    private VizCurve path = null;

    /** Returns the group that this VizEdge belongs to. */
    public Object group() { return group; }

    /** Returns the edge weight (which is always between 1 and 10000 inclusively). */
    public int weight() { return weight; }

    /** Returns the line style; never null. */
    public VizStyle style() { return style; }

    /** Returns the line color; never null. */
    public Color color() { return color; }

    /** Returns true if we will draw an arrow head on the "from" node. */
    public boolean ahead() { return ahead; }

    /** Returns true if we will draw an arrow head on the "to" node. */
    public boolean bhead() { return bhead; }

    /** Returns the label on this edge. */
    public String label() { return label; }

    /** Sets the edge weight between 1 and 10000. */
    public VizEdge set(int weightBetween1And10000) {
        if (weightBetween1And10000<1) weightBetween1And10000=1;
        if (weightBetween1And10000>10000) weightBetween1And10000=10000;
        weight=weightBetween1And10000;
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
    @Override public String toString() {
        return "Edge " + a() + (ahead?"<--":"---") + label + (bhead?"-->":"---") + b();
    }

    /** Construct an edge from "from" to "to" with the given arrow head settings, then add the edge to the graph. */
    public VizEdge(VizNode from, VizNode to, Object uuid, String label, boolean drawArrowHeadOnFrom, boolean drawArrowHeadOnTo, VizStyle style, Color color, Object group) {
       super(from, to); // The parent's constructor will add the edge A->B to the graph
       if (group instanceof VizNode) throw new IllegalArgumentException("group cannot be a VizNode");
       if (group instanceof VizEdge) throw new IllegalArgumentException("group cannot be a VizEdge");
       this.uuid = uuid;
       this.group = (group==null) ? (new Object()) : group;
       this.label=label;
       this.ahead=drawArrowHeadOnFrom;
       this.bhead=drawArrowHeadOnTo;
       if (style!=null) this.style=style;
       if (color!=null) this.color=color;
       if (label.length()>0) {
           Rectangle2D box = getBounds(false, label);
           labelbox.x = 0;
           labelbox.y = 0;
           labelbox.w = (int) box.getWidth();
           labelbox.h = (int) box.getHeight();
       }
    }

    /** Construct an edge from "from" to "to" with the default arrow head settings, then add the edge to the graph. */
    public VizEdge(VizNode from, VizNode to, Object uuid, String label, Object group) {
       this(from, to, uuid, label, false, true, null, null, group);
    }

    /** Reset the path as a straightline from the center of the "from" node to the center of the "to" node. */
    void resetPath() {
        VizNode a=a(), b=b();
        double ax=a.x(), ay=a.y();
        if (a==b) {
           double w=0;
           for(int n=a.selfEdges().size(), i=0; i<n; i++) {
               if (i==0) w = a.getWidth()/2 + selfLoopA;
               else w = w + getBounds(false, a.selfEdges().get(i-1).label()).getWidth() + selfLoopGL + selfLoopGR;
               VizEdge e = a.selfEdges().get(i);
               if (e!=this) continue;
               double h=a.getHeight()/2D*0.7D, k=0.55238D, wa=(a.getWidth()/2.0D), wb=w-wa;
               e.path = new VizCurve(ax, ay);
               e.path.cubicTo(ax, ay-k*h, ax+wa-k*wa, ay-h, ax+wa, ay-h);
               e.path.cubicTo(ax+wa+k*wb, ay-h, ax+wa+wb, ay-k*h, ax+wa+wb, ay);
               e.path.cubicTo(ax+wa+wb, ay+k*h, ax+wa+k*wb, ay+h, ax+wa, ay+h);
               e.path.cubicTo(ax+wa-k*wa, ay+h, ax, ay+k*h, ax, ay);
               e.labelbox.x = (int) (ax + w + selfLoopGL);
               e.labelbox.y = (int) (ay - getBounds(false, e.label()).getHeight()/2);
               break;
           }
        } else {
           int i=0, n=0;
           for(VizEdge e:a.outEdges()) {
               if (e==this) i=n;
               if (e.b()==b) n++;
           }
           double cx=b.x(), cy=b.y(), bx=(ax+cx)/2, by=(ay+cy)/2;
           path=new VizCurve(ax, ay);
           if (n>1 && (n&1)==1) {
               if (i<n/2) bx=bx-(n/2-i)*10; else if (i>n/2) bx=bx+(i-n/2)*10;
               path.lineTo(bx, by);
               path.lineTo(cx, cy);
           } else if (n>1) {
               if (i<n/2) bx=bx-(n/2-i)*10+5; else bx=bx+(i-n/2)*10+5;
               path.lineTo(bx, by);
               path.lineTo(cx, cy);
           } else {
               path.lineTo(cx, cy);
           }
        }
    }

    /** Given that this edge is already well-laidout, this method moves the label hoping to avoid/minimize overlap. */
    void repositionLabel(AvailableSpace sp) {
        if (label.length()==0 || a()==b()) return;
        final int gap = style==VizStyle.BOLD ? 4 : 2; // If the line is bold, we need to shift the label to the right a little bit
        boolean failed = false;
        VizCurve p = path;
        for(VizNode a=a(); a.shape()==null;) { VizEdge e=a.inEdges().get(0); a=e.a(); p=e.path().join(p); }
        for(VizNode b=b(); b.shape()==null;) { VizEdge e=b.outEdges().get(0); b=e.b(); p=p.join(e.path()); }
        for(double t=0.5D; ; t=t+0.05D) {
            if (t>=1D) { failed=true; t=0.7D; }
            double x1 = p.getX(t), y = p.getY(t), x2 = p.getXatY(y+labelbox.h, t, 1D, x1);
            int x = (int) (x1<x2 ? x2+gap : x1+gap);
            if (failed || sp.ok(x, (int)y, labelbox.w, labelbox.h)) { sp.add(labelbox.x=x, labelbox.y=(int)y, labelbox.w, labelbox.h); return; }
            double t2=1D-t;
            x1 = p.getX(t2); y = p.getY(t2); x2 = p.getXatY(y+labelbox.h, t2, 1D, x1);
            x = (int) (x1<x2 ? x2+gap : x1+gap);
            if (sp.ok(x, (int)y, labelbox.w, labelbox.h)) { sp.add(labelbox.x=x, labelbox.y=(int)y, labelbox.w, labelbox.h); return; }
        }
    }

    /** Returns the current path; if the path was not yet assigned, it returns a straight line from "from" node to "to" node. */
    VizCurve path() {
        if (path==null) resetPath();
        return path;
    }

    /**
     * Assuming this edge's coordinates have been assigned, and given the current zoom scale, draw the edge.
     */
    public void draw(Artist gr, double scale, VizEdge highEdge, Object highGroup) {
       final int top = a().graph.getTop(), left = a().graph.getLeft();
       gr.translate(-left, -top);
       if (highEdge==this) { gr.setColor(color); gr.set(VizStyle.BOLD, scale); }
          else if ((highEdge==null && highGroup==null) || highGroup==group) { gr.setColor(color); gr.set(style, scale); }
          else { gr.setColor(Color.LIGHT_GRAY); gr.set(style, scale); }
       if (a()==b()) {
          gr.draw(path);
       } else {
          // Concatenate this path and its connected segments into a single VizPath object, then draw it
          VizCurve p=null;
          VizEdge e=this;
          while(e.a().shape()==null) e=e.a().inEdges().get(0); // Let e be the first segment of this chain of connected segments
          while(true) {
             p = (p==null) ? e.path : p.join(e.path);
             if (e.b().shape()!=null) break;
             e = e.b().outEdges().get(0);
          }
          gr.drawSmoothly(p);
       }
       gr.set(VizStyle.SOLID, scale);
       gr.translate(left, top);
       if (highEdge==null && highGroup==null && label.length()>0) drawLabel(gr, color, null);
       drawArrowhead(gr, scale, highEdge, highGroup);
    }

    /** Draw the edge label using the given color. */
    void drawLabel(Artist gr, Color color, Color erase) {
        if (label.length()>0) {
            final int top = a().graph.getTop(), left = a().graph.getLeft();
            gr.translate(-left, -top);
            if (erase!=null && a()!=b()) {
                Rectangle2D.Double rect = new Rectangle2D.Double(labelbox.x, labelbox.y, labelbox.w, labelbox.h);
                gr.setColor(erase);
                gr.draw(rect, true);
            }
            gr.setColor(color);
            gr.drawString(label, labelbox.x, labelbox.y+Artist.getMaxAscent());
            gr.translate(left, top);
            return;
        }
    }

    /** Positions the arrow heads of the given edge properly. */
    void layout_arrowHead() {
        VizCurve c=path();
        if (ahead() && a().shape()!=null) {
            double in=0D, out=1D;
            while(StrictMath.abs(out-in)>0.0001D) {
                double t=(in+out)/2;
                if (a().contains(c.getX(t), c.getY(t))) in=t; else out=t;
            }
            c.chopStart(in);
        }
        if (bhead() && b().shape()!=null) {
            double in=1D, out=(a()==b() ? 0.5D : 0D);
            while(StrictMath.abs(out-in)>0.0001D) {
                double t=(in+out)/2;
                if (b().contains(c.getX(t), c.getY(t))) in=t; else out=t;
            }
            c.chopEnd(in);
        }
    }

    /** Assuming this edge's coordinates have been assigned, and given the current zoom scale, draw the arrow heads if any. */
    private void drawArrowhead(Artist gr, double scale, VizEdge highEdge, Object highGroup) {
       final double tipLength = ad * 0.6D;
       final int top = a().graph.getTop(), left = a().graph.getLeft();
       // Check to see if this edge is highlighted or not
       double fan = (style==VizStyle.BOLD ? bigFan : smallFan);
       if (highEdge==this) { fan=bigFan; gr.setColor(color); gr.set(VizStyle.BOLD, scale); }
          else if ((highEdge==null && highGroup==null) || highGroup==group) { gr.setColor(color); gr.set(style, scale); }
          else { gr.setColor(Color.LIGHT_GRAY); gr.set(style, scale); }
       for(VizEdge e=this; ;e=e.b().outEdges().get(0)) {
          if ((e.ahead && e.a().shape()!=null) || (e.bhead && e.b().shape()!=null)) {
             VizCurve cv=e.path();
             if (e.ahead && e.a().shape()!=null) {
                CubicCurve2D.Double bez = cv.list.get(0);
                double ax = bez.x1, ay = bez.y1, bx = bez.ctrlx1, by = bez.ctrly1;
                double t = PI + atan2(ay-by, ax-bx);
                double gx1 = ax + tipLength*cos(t-fan), gy1 = ay + tipLength*sin(t-fan);
                double gx2 = ax + tipLength*cos(t+fan), gy2 = ay + tipLength*sin(t+fan);
                GeneralPath gp=new GeneralPath();
                gp.moveTo((float)(gx1-left), (float)(gy1-top)); gp.lineTo((float)(ax-left), (float)(ay-top));
                gp.lineTo((float)(gx2-left), (float)(gy2-top)); gp.closePath(); gr.draw(gp,true);
             }
             if (e.bhead && e.b().shape()!=null) {
                CubicCurve2D.Double bez = cv.list.get(cv.list.size()-1);
                double bx = bez.x2, by = bez.y2, ax = bez.ctrlx2, ay = bez.ctrly2;
                double t = PI + atan2(by-ay, bx-ax);
                double gx1 = bx + tipLength*cos(t-fan), gy1 = by + tipLength*sin(t-fan);
                double gx2 = bx + tipLength*cos(t+fan), gy2 = by + tipLength*sin(t+fan);
                GeneralPath gp=new GeneralPath();
                gp.moveTo((float)(gx1-left), (float)(gy1-top)); gp.lineTo((float)(bx-left), (float)(by-top));
                gp.lineTo((float)(gx2-left), (float)(gy2-top)); gp.closePath(); gr.draw(gp,true);
             }
          }
          if (e.b().shape()!=null) break;
       }
    }

    /** The angle (in radian) to fan out the arrow head, if the line is not bold. */
    private final double smallFan = toRadians(16);

    /** The angle (in radian) to fan out the arrow head, if the line is bold. */
    private final double bigFan = toRadians(32);
}
