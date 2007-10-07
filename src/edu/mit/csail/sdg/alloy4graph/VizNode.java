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
import static java.awt.Color.WHITE;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

/**
 * Mutable; represents a graphical node.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizNode extends DiGraph.DiNode {

    // =============================== adjustable options ==================================================

    /** This determines the minimum width of a dummy node. */
    private static final int dummyWidth = 30;

    /** This determines the minimum height of a dummy node. */
    private static final int dummyHeight = 10;

    /** This determines the minimum amount of padding added above, left, right, and below the text label. */
    private static final int labelPadding = 5;

    // =============================== cached for performance efficiency ===================================

    /** Caches the value of sqrt(3.0). The extra digits in the definition will be truncated by the Java compiler. */
    private static final double sqrt3 = 1.7320508075688772935274463415058723669428052538103806280558D;

    /** Caches the value of sin(36 degree). The extra digits in the definition will be truncated by the Java compiler. */
    private static final double sin36 = 0.5877852522924731291687059546390727685976524376431459910723D;

    /** Caches the value of cos(36 degree). The extra digits in the definition will be truncated by the Java compiler. */
    private static final double cos36 = 0.8090169943749474241022934171828190588601545899028814310677D;

    /** Caches the value of cos(18 degree). The extra digits in the definition will be truncated by the Java compiler. */
    private static final double cos18 = 0.9510565162951535721164393333793821434056986341257502224473D;

    /** Caches the value of tan(18 degree). The extra digits in the definition will be truncated by the Java compiler. */
    private static final double tan18 = 0.3249196962329063261558714122151344649549034715214751003078D;

    /** If nonnull, it caches an empty bitmap. */
    private static BufferedImage cachedImage;

    /** If nonnull, it caches the Graphics2D object associated with the current font size and font boldness. */
    private static Graphics2D cachedGraphics;

    /** If nonnull, it caches the FontMetrics object associated with the current font size and font boldness. */
    private static FontMetrics cachedFontMetrics;

    /** If nonnull, it caches the Font object associated with the current font size and font boldness. */
    private static Font cachedFont;

    /** If cachedFont!=null, this is its font size. */
    private static int cachedFontSize;

    /** If cachedFont!=null, this is its font boldness. */
    private static boolean cachedFontBoldness;

    /** Updates cached{Image,Graphics,Metrics,Font} based on the given font size and font boldness. */
    private static void updateCache(int fontSize, boolean fontBoldness) {
       if (cachedFont==null || cachedFontMetrics==null || fontSize!=cachedFontSize || fontBoldness!=cachedFontBoldness) {
          if (cachedImage==null) cachedImage=new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
          if (cachedGraphics==null) cachedGraphics=(Graphics2D)(cachedImage.getGraphics());
          if (fontSize<8) fontSize=8;
          if (fontSize>72) fontSize=72;
          cachedFont = new Font("Lucida Grande", ((cachedFontBoldness=fontBoldness) ? Font.BOLD : Font.PLAIN), cachedFontSize=fontSize);
          cachedGraphics.setFont(cachedFont);
          cachedFontMetrics=cachedGraphics.getFontMetrics();
       }
    }

    // =============================== per-node settings ==================================================

    /** The X coordinate of the center of the text labels. */
    private int textX = 0;

    /** The Y coordinate of the center of the text labels. */
    private int textY = 0;

    /**
     * The font boldness.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private boolean fontBold = false;

    /**
     * The font size.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private int fontSize = 12;

    /**
     * The node labels; if null or empty, then the node has no labels.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private List<String> labels = null;

    /**
     * The node color; never null.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private Color color = WHITE;

    /**
     * The line style; never null.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private VizStyle style = VizStyle.SOLID;

    /**
     * The node shape; if null, then the node is a dummy node.
     * <p> When this value changes, we automatically invalidate the previously computed bounds information.
     */
    private VizShape shape = VizShape.BOX;

    /** Returns the X coordinate of the center of the text labels. */
    public int x() { return textX; }

    /** Returns the Y coordinate of the center of the text labels. */
    public int y() { return textY; }

    /** Changes the X coordinate of the center of the text labels. */
    public void setX(int x) { textX=x;}

    /** Changes the Y coordinate of the center of the text labels. */
    public void setY(int y) { textY=y; }

    /** Returns the node shape (or null if the node is a dummy node). */
    public VizShape shape() { return shape; }

    /** Add the given label to the start of the labels, then invalidate the computed bounds. */
    public VizNode addBefore(String label) {
        if (label==null || label.length()==0) return this;
        if (labels==null) labels=new ArrayList<String>();
        labels.add(0,label);
        up=(-1);
        return this;
    }

    /** Add the given label after the existing labels, then invalidate the computed bounds. */
    public VizNode addAfter(String label) {
        if (label==null || label.length()==0) return this;
        if (labels==null) labels=new ArrayList<String>();
        labels.add(label);
        up=(-1);
        return this;
    }

    /** Changes the node color, then invalidate the computed bounds. */
    public VizNode set(Color color) {
        if (color!=null && this.color!=color) { this.color=color; up=(-1); }
        return this;
    }

    /** Changes the node shape (where null means change the node into a dummy node), then invalidate the computed bounds. */
    public VizNode set(VizShape shape) {
        if (this.shape!=shape) { this.shape=shape; up=(-1); }
        return this;
    }

    /** Changes the line style, then invalidate the computed bounds. */
    public VizNode set(VizStyle style) {
        if (style!=null && this.style!=style) { this.style=style; up=(-1); }
        return this;
    }

    /** Changes the font boldness, then invalidate the computed bounds. */
    public VizNode setFontBoldness(boolean bold) {
        if (this.fontBold!=bold) { this.fontBold=bold; up=(-1); }
        return this;
    }

    /** Changes the font size, then invalidate the computed bounds. */
    public VizNode setFontSize(int fontSize)   {
       if (fontSize<8) fontSize=8; else if (fontSize>72) fontSize=72;
       if (this.fontSize!=fontSize) { this.fontSize=fontSize; up=(-1); }
       return this;
    }

    /** Create a new node with the given list of labels, then add it to the given graph. */
    public VizNode(DiGraph graph, String... labels) {
        super(graph); // The parent's constructor will add this node to the graph automatically
        if (labels==null || labels.length==0) return;
        this.labels = new ArrayList<String>(labels.length);
        for(int i=0; i<labels.length; i++) this.labels.add(labels[i]);
    }

    /** Returns a brief summary of this node. */
    @Override public String toString() { return (labels!=null && labels.size()>0) ? labels.get(0).trim() : ""; }

    // ============================ these are computed by calcBounds() =========================================

    /** If (up>=0), this is the distance from the center of the text label to the top edge. */
    private int up=(-1);

    /** If (up>=0), this is the distance from the center of the text label to the bottom edge. */
    private int down;

    /** If (up>=0), this is the distance from the center of the text label to the left edge. */
    private int side;

    /** If (up>=0), this is the width of the text label. */
    private int width;

    /** If (up>=0), this is the height of the text label. */
    private int height;

    /** If (up>=0 and shape!=null), this is the bounding polygon. */
    private Shape poly;

    /** If (up>=0 and shape!=null and poly2!=null), then poly2 will also be drawn during the draw() method. */
    private Shape poly2;

    /** If (up>=0 and shape!=null and poly3!=null), then poly3 will also be drawn during the draw() method. */
    private Shape poly3;

    /** Returns the distance from the center of the text label to the top edge. */
    public int getUp() { if (up<0) calcBounds(); return up; }

    /** Returns the distance from the center of the text label to the bottom edge. */
    public int getDown() { if (up<0) calcBounds(); return down; }

    /** Returns the node height. */
    public int getHeight()  { if (up<0) calcBounds(); return up+down; }

    /** Returns the node width. */
    public int getWidth()  { if (up<0) calcBounds(); return side+side; }

    /** Returns the amount of space we need to reserve on the right hand side for the self edges (0 if this has no self edges now) */
    public int getReserved() {
        int n=selfEdges().size();
        if (n==0) return 0; else return VizEdge.selfLoopMinWidth + VizEdge.selfLoopXGap*(n-1);
    }

    /** Returns true if the given point intersects this node or not. */
    public boolean intersects(double x, double y) {
        if (shape==null) return false;
        if (up<0) calcBounds();
        return poly.contains(x-textX, y-textY);
    }

    /**
     * Find the point of intersection between this node and a given ray, and store the point of intersection into ans.
     * <p> The ray starts from this node's center, and goes through the point (rx,ry) given as arguments.
     * <p> Note: this method may find the wrong point of intersection if the ray is too horizontal.
     */
    public void intersectsNonhorizontalRay(double rx, double ry, Point2D.Double ans) {
       if (shape==null) { ans.x=textX; ans.y=textY; return; }
       if (up<0) calcBounds();
       // Shift the input argument to the center of this node
       rx=rx-textX; ry=ry-textY;
       double slope=rx/ry, step=(ry<0 ? -1 : 1);
       // Use the radius to directly compute the intersection, if the shape is CIRCLE, M_CIRCLE, or DOUBLE_CIRCLE
       if (shape==VizShape.CIRCLE || shape==VizShape.M_CIRCLE || shape==VizShape.DOUBLE_CIRCLE) {
          int hw=width/2, hh=height/2;
          int radius = ((int) (Math.sqrt( hw*((double)hw) + ((double)hh)*hh ))) + 2;
          if (shape==VizShape.DOUBLE_CIRCLE) radius=radius+5;
          // x^2+y^2=radius^2  and x=y*slope, thus (1+slope^2)(y^2)=radius^2
          ry=Math.sqrt((radius*radius)/(1+slope*slope)); if (step<0) ry=(-ry);
          ans.x=ry*slope + textX;
          ans.y=ry + textY;
          return;
       }
       // Check for intersection
       for(ry=0;;ry=ry+step) {
          rx=ry*slope;
          if (poly.contains(rx, ry)) continue;
          ans.x=rx+textX;
          ans.y=ry+textY;
          return;
       }
    }

    /**
     * Find the point of intersection between this node and a given ray, and store the point of intersection into ans.
     * <p> The ray starts from this node's center, and goes through the point (rx,ry) given as arguments.
     * <p> Note: this method may find the wrong point of intersection if the ray is too vertical.
     */
    public void intersectsNonverticalRay(double rx, double ry, Point2D.Double ans) {
       if (shape==null) { ans.x=textX; ans.y=textY; return; }
       if (up<0) calcBounds();
       // Shift the input argument to the center of this node
       rx=rx-textX; ry=ry-textY;
       double slope=ry/rx, step=(rx<0 ? -1 : 1);
       // Use the radius to directly compute the intersection, if the shape is CIRCLE, M_CIRCLE, or DOUBLE_CIRCLE
       if (shape==VizShape.CIRCLE || shape==VizShape.M_CIRCLE || shape==VizShape.DOUBLE_CIRCLE) {
          int hw=width/2, hh=height/2;
          int radius = ((int) (Math.sqrt( hw*((double)hw) + ((double)hh)*hh ))) + 2;
          if (shape==VizShape.DOUBLE_CIRCLE) radius=radius+5;
          // x^2+y^2=radius^2  and y=x*slope, thus (1+slope^2)(x^2)=radius^2
          rx=Math.sqrt((radius*radius)/(1+slope*slope)); if (step<0) rx=(-rx);
          ans.y=rx*slope + textY;
          ans.x=rx + textX;
          return;
       }
       // Check for intersection
       for(rx=0;;rx=rx+step) {
          ry=rx*slope;
          if (poly.contains(rx, ry)) continue;
          ans.x=rx+textX;
          ans.y=ry+textY;
          return;
       }
    }

    /** Return the horizontal point of intersection of this node with a horizontal ray at height y going from this.x() rightward. */
    public double intersectsAtHeight(double y) {
       if (shape==null) return 0;
       if (up<0) calcBounds();
       y=y-textY;
       double x;
       for(x=0;;x=x+1) if (!poly.contains(x,y)) return x+textX;
    }

    //===================================================================================================

    /** Calculate this node's bounds. */
    public void calcBounds() {
       width=2*labelPadding; if (width<dummyWidth) side=dummyWidth/2;
       height=width;         if (height<dummyHeight) down=(up=dummyHeight/2);
       poly=(poly2=(poly3=null));
       if (shape==null) return;
       Polygon poly=new Polygon();
       updateCache(fontSize, fontBold);
       final Graphics2D gr = cachedGraphics;
       final FontMetrics fm = cachedFontMetrics;
       final int ad = fm.getMaxAscent() + fm.getMaxDescent();
       if (labels!=null) for(int i=0; i<labels.size(); i++) {
          String t = labels.get(i);
          Rectangle2D rect = fm.getStringBounds(t, gr);
          int ww = ((int)(rect.getWidth())) + 1; // Round it up
          if (width<ww) width=ww;
          height=height+ad;
       }
       int hw=((width+1)/2)+labelPadding;  if (hw<ad/2) hw=ad/2; width=hw*2; side=hw;
       int hh=((height+1)/2)+labelPadding; if (hh<ad/2) hh=ad/2; height=hh*2; up=(down=hh);
       switch(shape) {
          case HEXAGON:
             side += ad;
             poly.addPoint(-hw-ad, 0); poly.addPoint(-hw, -hh); poly.addPoint(hw, -hh);
             poly.addPoint(hw+ad, 0); poly.addPoint(hw, hh); poly.addPoint(-hw, hh);
             break;
          case HOUSE:
             up += ad;
             poly.addPoint(-hw,-hh); poly.addPoint(0,-hh-ad); poly.addPoint(hw,-hh); poly.addPoint(hw,hh); poly.addPoint(-hw,hh);
             break;
          case INV_HOUSE:
             down += ad;
             poly.addPoint(-hw,-hh); poly.addPoint(hw,-hh); poly.addPoint(hw,hh); poly.addPoint(0,hh+ad); poly.addPoint(-hw,hh);
             break;
          case TRAPEZOID:
             side += ad;
             poly.addPoint(-hw,-hh); poly.addPoint(hw,-hh); poly.addPoint(hw+ad,hh); poly.addPoint(-hw-ad,hh);
             break;
          case INV_TRAPEZOID:
             side += ad;
             poly.addPoint(-hw-ad, -hh); poly.addPoint(hw+ad, -hh); poly.addPoint(hw, hh); poly.addPoint(-hw, hh);
             break;
          case PARALLELOGRAM:
             side += ad;
             poly.addPoint(-hw, -hh); poly.addPoint(hw+ad, -hh); poly.addPoint(hw, hh); poly.addPoint(-hw-ad, hh);
             break;
          case TRIANGLE:
          case INV_TRIANGLE: {
             int dx = (int) (height/sqrt3); dx=dx+1; if (dx<5) dx=5;
             int dy = (int) (hw*sqrt3);     dy=dy+1; if (dy<5) dy=5;
             if (shape==VizShape.TRIANGLE) {
                side += dx; up += dy;
                poly.addPoint(0, -hh-dy); poly.addPoint(hw+dx, hh); poly.addPoint(-hw-dx, hh);
             } else {
                side += dx; down += dy;
                poly.addPoint(-hw-dx, -hh); poly.addPoint(hw+dx, -hh); poly.addPoint(0, hh+dy);
             }
             break;
          }
          case M_DIAMOND:
          case DIAMOND:
             if (shape==VizShape.M_DIAMOND) {
                if (hw<10) { hw=10; side=10; width=20; }
                if (hh<10) { hh=10; up=10; down=10; height=20; }
             }
             up += hw; down += hw; side += hh;
             poly.addPoint(-hw-hh, 0); poly.addPoint(0, -hh-hw); poly.addPoint(hw+hh, 0); poly.addPoint(0, hh+hw);
             break;
          case M_SQUARE:
             if (hh<hw) hh=hw; else hw=hh;
             if (hh<6) { hh=6; hw=6; }
             this.width=hw*2;  this.side=hw;
             this.height=hh*2; this.up=hh; this.down=hh;
             side += 4; up +=4; down += 4;
             poly.addPoint(-hw-4,-hh-4); poly.addPoint(hw+4,-hh-4); poly.addPoint(hw+4,hh+4); poly.addPoint(-hw-4,hh+4);
             break;
          case OCTAGON:
          case DOUBLE_OCTAGON:
          case TRIPLE_OCTAGON: {
             int dx=(width)/3, dy=ad;
             up += dy;
             down += dy;
             poly.addPoint(-hw, -hh); poly.addPoint(-hw+dx, -hh-dy); poly.addPoint(hw-dx, -hh-dy); poly.addPoint(hw, -hh);
             poly.addPoint(hw, hh); poly.addPoint(hw-dx, hh+dy); poly.addPoint(-hw+dx, hh+dy); poly.addPoint(-hw, hh);
             if (shape==VizShape.OCTAGON) break;
             double c=Math.sqrt(dx*dx+dy*dy), a=(dx*dy)/c, k=((a+5)*dy)/dx, r=Math.sqrt((a+5)*(a+5)+k*k)-dy;
             double dx1=((r-5)*dx)/dy, dy1=-(((dx+5D)*dy)/dx-dy-r);
             int x1=(int)(Math.round(dx1)), y1=(int)(Math.round(dy1));
             up+=5; down+=5; side+=5;
             poly2=poly; poly=new Polygon();
             poly.addPoint(-hw-5, -hh-y1); poly.addPoint(-hw+dx-x1, -hh-dy-5); poly.addPoint(hw-dx+x1, -hh-dy-5); poly.addPoint(hw+5, -hh-y1);
             poly.addPoint(hw+5, hh+y1); poly.addPoint(hw-dx+x1, hh+dy+5); poly.addPoint(-hw+dx-x1, hh+dy+5); poly.addPoint(-hw-5, hh+y1);
             if (shape==VizShape.DOUBLE_OCTAGON) break;
             up+=5; down+=5; side+=5;
             poly3=poly; poly=new Polygon(); x1=(int)(Math.round(dx1*2)); y1=(int)(Math.round(dy1*2));
             poly.addPoint(-hw-10, -hh-y1); poly.addPoint(-hw+dx-x1, -hh-dy-10); poly.addPoint(hw-dx+x1, -hh-dy-10); poly.addPoint(hw+10, -hh-y1);
             poly.addPoint(hw+10, hh+y1); poly.addPoint(hw-dx+x1, hh+dy+10); poly.addPoint(-hw+dx-x1, hh+dy+10); poly.addPoint(-hw-10, hh+y1);
             break;
          }
          case M_CIRCLE:
          case CIRCLE:
          case DOUBLE_CIRCLE: {
             int radius = ((int) (Math.sqrt( hw*((double)hw) + ((double)hh)*hh ))) + 2;
             if (shape==VizShape.DOUBLE_CIRCLE) radius=radius+5;
             int L = ((int) (radius / cos18))+2, a = (int) (L * sin36), b = (int) (L * cos36), c = (int) (radius * tan18);
             poly.addPoint(-L,0); poly.addPoint(-b,a); poly.addPoint(-c,L); poly.addPoint(c,L); poly.addPoint(b,a);
             poly.addPoint(L,0); poly.addPoint(b,-a); poly.addPoint(c,-L); poly.addPoint(-c,-L); poly.addPoint(-b,-a);
             up=L; down=L; side=L;
             break;
          }
          case EGG:
          case ELLIPSE: {
             int pad = ad/2;
             side+=pad;
             up+=pad;
             down+=pad;
             int d = (shape==VizShape.ELLIPSE) ? 0 : (ad/2);
             GeneralPath path=new GeneralPath();
             path.moveTo(-side,d);
             path.quadTo(-side,-up,0,-up); path.quadTo(side,-up,side,d); path.quadTo(side,up,0,up); path.quadTo(-side,up,-side,d);
             path.closePath();
             this.poly=path;
             return; // We must return, since otherwise "this.poly" will be overwritten by the local variable "poly"
          }
          default: { // BOX
             if (shape!=VizShape.BOX) { int d=ad/2; hw=hw+d; side=hw; hh=hh+d; up=hh; down=hh; }
             poly.addPoint(-hw,-hh); poly.addPoint(hw,-hh); poly.addPoint(hw,hh); poly.addPoint(-hw,hh);
          }
       }
       this.poly=poly;
    }

    /** Assuming calcBounds() have been called, and (x,y) have been set, then this draws the node. */
    public void draw(Graphics2D gr, double scale, Object highlight) {
       if (shape==null) return;
       if (up<0) calcBounds();
       style.set(gr, scale);
       updateCache(fontSize, fontBold);
       gr.setFont(cachedFont);
       final int ad = cachedFontMetrics.getMaxAscent() + cachedFontMetrics.getMaxDescent();
       gr.translate(textX, textY);
       if (this==highlight) gr.setColor(Color.RED); else gr.setColor(color);
       if (shape==VizShape.CIRCLE || shape==VizShape.M_CIRCLE || shape==VizShape.DOUBLE_CIRCLE) {
          int hw=width/2, hh=height/2;
          int radius = ((int) (Math.sqrt( hw*((double)hw) + ((double)hh)*hh ))) + 2;
          if (shape==VizShape.DOUBLE_CIRCLE) radius=radius+5;
          gr.fillArc(-radius, -radius, radius*2, radius*2, 0, 360);
          gr.setColor(BLACK);
          gr.drawArc(-radius, -radius, radius*2, radius*2, 0, 360);
          if (style==VizStyle.DOTTED || style==VizStyle.DASHED) VizStyle.SOLID.set(gr, scale);
          if (shape==VizShape.M_CIRCLE && 10*radius>=25 && radius>5) {
             int d = (int) Math.sqrt((double)(10*radius-25));
             if (d>0) { gr.drawLine(-d,-radius+5,d,-radius+5); gr.drawLine(-d,radius-5,d,radius-5); }
          }
          if (shape==VizShape.DOUBLE_CIRCLE) {
             int r = radius-5; gr.drawArc(-r, -r, r*2, r*2, 0, 360);
          }
       } else {
          gr.fill(poly);
          gr.setColor(BLACK);
          gr.draw(poly);
          if (poly2!=null) gr.draw(poly2);
          if (poly3!=null) gr.draw(poly3);
          if (style==VizStyle.DOTTED || style==VizStyle.DASHED) VizStyle.SOLID.set(gr, scale);
          if (shape==VizShape.M_DIAMOND) {
             gr.drawLine(-side+8, -8, -side+8, 8); gr.drawLine(-8, -side+8, 8, -side+8);
             gr.drawLine(side-8, -8, side-8, 8); gr.drawLine(-8, side-8, 8, side-8);
          }
          if (shape==VizShape.M_SQUARE) {
             gr.drawLine(-side, -side+8, -side+8, -side); gr.drawLine(side, -side+8, side-8, -side);
             gr.drawLine(-side, side-8, -side+8, side); gr.drawLine(side, side-8, side-8, side);
          }
       }
       VizStyle.SOLID.set(gr, scale);
       int clr = (color.getRGB() & 0xFFFFFF);
       gr.setColor((clr==0x000000 || clr==0xff0000 || clr==0x0000ff) ? Color.WHITE : Color.BLACK);
       if (labels!=null && labels.size()>0) {
          int x=(-width/2), y=(-labels.size()*ad/2);
          for(int i=0; i<labels.size(); i++) {
             String t = labels.get(i);
             int w = ((int) (cachedFontMetrics.getStringBounds(t, gr).getWidth())) + 1; // Round it up
             if (width>w) w=(width-w)/2; else w=0;
             gr.drawString(t, x+w, y+cachedFontMetrics.getMaxAscent());
             y=y+ad;
          }
       }
       gr.translate(-textX, -textY);
    }
}
