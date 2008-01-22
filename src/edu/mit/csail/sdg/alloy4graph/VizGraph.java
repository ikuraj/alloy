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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.awt.Color;
import java.awt.geom.Line2D;
import java.awt.geom.RoundRectangle2D;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Rational;
import edu.mit.csail.sdg.alloy4.Util;
import static edu.mit.csail.sdg.alloy4graph.Artist.getBounds;

/**
 * Mutable; represents a graph.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final strictfp class VizGraph extends DiGraph {

    //================================ adjustable options ========================================================================//

    /** Minimum horizontal distance between adjacent nodes. */
    static final int xJump=30;

    /** Minimum vertical distance between adjacent layers. */
    static final int yJump=60;

    /** The horizontal distance between the first self-loop and the node itself. */
    static final int selfLoopA=40;

    /** The horizontal padding to put on the left side of a self-loop's edge label. */
    static final int selfLoopGL=2;

    /** The horizontal padding to put on the right side of a self-loop's edge label. */
    static final int selfLoopGR=20;

    /** The maximum ascent and descent. We deliberately do NOT make this field "static" because only AWT thread can call Artist. */
    private final int ad = Artist.getMaxAscentAndDescent();

    /** The left edge. */
    private int left=0;

    /** The top edge. */
    private int top=0;

    /** The bottom edge. */
    private int bottom=0;

    /** The total width of the graph; this value is computed by layout(). */
    private int totalWidth=0;

    /** The total height of the graph; this value is computed by layout(). */
    private int totalHeight=0;

    /** The height of each layer. */
    int[] layerPH=null;

    /** Assuming layout() has been called, this returns the left edge. */
    public int getLeft() { return left; }

    /** Assuming layout() has been called, this returns the top edge. */
    public int getTop() { return top; }

    /** Assuming layout() has been called, this returns the total width. */
    public int getTotalWidth() { return totalWidth; }

    /** Assuming layout() has been called, this returns the total height. */
    public int getTotalHeight() { return totalHeight; }

    //=============================== constructors and additional fields =========================================================//

    /** A list of legends; each legend is an Object with the associated text label and color. */
    private final SortedMap<Comparable<?>,Pair<String,Color>> legends = new TreeMap<Comparable<?>,Pair<String,Color>>();

    /**
     * Add a legend with the given object and the associated text label; if color==null, that means we will
     * still add this legend into the list of legends, but this legend will be hidden.
     */
    public void addLegend(Comparable<?> object, String label, Color color) { legends.put(object, new Pair<String,Color>(label,color)); }

    /** The default magnification. */
    private double defaultScale = 1;

    /** Retrieves the default magnification. */
    public double getDefaultScale() { return defaultScale; }

    /** Constructs an empty VizGraph object. */
    public VizGraph(double defaultScale) { this.defaultScale = defaultScale; }

    //============================================================================================================================//

    /** Assign a total order on the nodes. */
    private void layout_assignOrder() {
       // This is an implementation of the GR algorithm described by Peter Eades, Xuemin Lin, and William F. Smyth
       // in "A Fast & Effective Heuristic for the Feedback Arc Set Problem"
       // in Information Processing Letters, Volume 47, Number 6, Pages 319-323, 1993
       final int num = nodes.size();
       if ((Integer.MAX_VALUE-1)/2 < num) throw new OutOfMemoryError();
       // Now, allocate 2n+1 bins labeled -n .. n
       // Note: inside this method, whenever we see #in and #out, we ignore repeated edges.
       // Note: since Java ArrayList always start at 0, we'll index it by adding "n" to it.
       final List<List<VizNode>> bins = new ArrayList<List<VizNode>>(2*num+1);
       for(int i=0; i<2*num+1; i++) bins.add(new LinkedList<VizNode>());
       // For each N, figure out its in-neighbors and out-neighbors, then put it in the correct bin
       ArrayList<LinkedList<VizNode>> grIN=new ArrayList<LinkedList<VizNode>>(num);
       ArrayList<LinkedList<VizNode>> grOUT=new ArrayList<LinkedList<VizNode>>(num);
       int[] grBIN=new int[num];
       for(VizNode n:nodes) {
          int ni=n.pos();
          LinkedList<VizNode> in=new LinkedList<VizNode>(), out=new LinkedList<VizNode>();
          for(VizEdge e:n.inEdges()) { VizNode a=e.a(); if (!in.contains(a)) in.add(a); }
          for(VizEdge e:n.outEdges()) { VizNode b=e.b(); if (!out.contains(b)) out.add(b); }
          grIN.add(in);
          grOUT.add(out);
          grBIN[ni] = (out.size()==0) ? 0 : (in.size()==0 ? (2*num) : (out.size()-in.size()+num));
          bins.get(grBIN[ni]).add(n);
          // bin[0]     = { v | #out=0 }
          // bin[n + d] = { v | d=#out-#in and #out!=0 and #in!=0 } for -n < d < n
          // bin[n + n] = { v | #in=0 and #out>0 }
       }
       // Main loop
       final LinkedList<VizNode> s1=new LinkedList<VizNode>(), s2=new LinkedList<VizNode>();
       while(true) {
          VizNode x=null;
          if (!bins.get(0).isEmpty()) {
             // If a sink exists, take a sink X and prepend X to S2
             x = bins.get(0).remove(bins.get(0).size()-1);
             s1.add(x);
          } else for(int j=2*num; j>0; j--) {
             // Otherwise, let x be a source if one exists, or a node with the highest #out-#in. Then append X to S1.
             List<VizNode> bin=bins.get(j);
             int sz=bin.size();
             if (sz>0) { x=bin.remove(sz-1); s2.addFirst(x); break; }
          }
          if (x==null) break; // This means we're done; else, delete X from its bin, and move each of X's neighbor into their new bin
          bins.get(grBIN[x.pos()]).remove(x);
          for(VizNode n:grIN.get(x.pos()))  grOUT.get(n.pos()).remove(x);
          for(VizNode n:grOUT.get(x.pos())) grIN.get(n.pos()).remove(x);
          for(VizNode n:Util.fastJoin(grIN.get(x.pos()), grOUT.get(x.pos()))) {
              int ni=n.pos(), out=grOUT.get(ni).size(), in=grIN.get(ni).size();
              int b=(out==0)?0:(in==0?(2*num):(out-in+num));
              if (grBIN[ni]!=b) { bins.get(grBIN[ni]).remove(n); grBIN[ni]=b; bins.get(b).add(n); }
          }
       }
       sortNodes(Util.fastJoin(s1,s2));
    }

    //============================================================================================================================//

    /** Reverses all backward edges. */
    private void layout_reverseAllBackwardEdges() {
       for(VizEdge e:edges) if (e.a().pos() < e.b().pos()) e.set(e.bhead(), e.ahead()).reverse();
    }

    //============================================================================================================================//

    /** Assign the nodes into one or more layers, then return the number of layers. */
    private int layout_assignNodesIntoLayers() {
       // Here, for each node X, I compute its maximum length to a sink; if X is a sink, its length to sink is 0.
       final int n = nodes.size();
       int[] len=new int[n];
       for(VizNode x:nodes) {
           // Since we ensured that arrows only ever go from a node with bigger pos() to a node with smaller pos(),
           // we can compute the "len" array in O(n) time by visiting each node IN THE SORTED ORDER
           int max=0;
           for(VizEdge e:x.outEdges()) {
               VizNode y=e.b();
               int yLen=len[y.pos()]+1;
               if (max<yLen) max=yLen;
           }
           len[x.pos()]=max;
       }
       // Now, we simply do the simplest thing: assign each node to the layer corresponding to its max-length-to-sink.
       for(VizNode x:nodes) x.setLayer(len[x.pos()]);
       // Now, apply a simple trick: whenever every one of X's incoming edge is more than one layer above, then move X up
       while(true) {
         boolean changed=false;
         for(VizNode x:nodes) if (x.inEdges().size()>0) {
            int closestLayer=layers()+1;
            for(VizEdge e:x.inEdges()) {
               int y=e.a().layer();
               if (closestLayer>y) closestLayer=y;
            }
            if (closestLayer-1>x.layer()) { x.setLayer(closestLayer-1); changed=true; }
         }
         if (!changed) break;
       }
       // All done!
       return layers();
    }

    //============================================================================================================================//

    /** Add dummy nodes so that each edge only goes between adjacent layers. */
    private void layout_addDummyNodes() {
       for(final VizEdge edge:new ArrayList<VizEdge>(edges)) {
          VizEdge e=edge;
          VizNode a=e.a(), b=e.b();
          while(a.layer() - b.layer() > 1) {
             VizNode tmp = a;
             a = new VizNode(a.graph, e.uuid).set((VizShape)null);
             a.setLayer(tmp.layer()-1);
             // now we have three nodes in the vertical order of "tmp", "a", then "b"
             e.changeB(a);                                                                   // let old edge go from "tmp" to "a"
             e=new VizEdge(a, b, e.uuid, "", e.ahead(), e.bhead(), e.style(), e.color(), e.group()); // let new edge go from "a" to "b"
          }
       }
    }

    //============================================================================================================================//

    /** Within each layer, decide the order of the nodes. */
    private void layout_decideTheOrderPerLayer() {
       // This uses the original Barycenter heuristic
       final IdentityHashMap<VizNode,Object> map=new IdentityHashMap<VizNode,Object>();
       final Rational[] bc=new Rational[nodes.size()+1];
       int i=1; for(VizNode n:layer(0)) { bc[n.pos()]=Rational.make(i); i++; }
       for(int layer=0; layer<layers()-1; layer++) {
          for(VizNode n:layer(layer+1)) {
             map.clear();
             int count=0;
             Rational sum=Rational.ZERO;
             for(VizEdge e:n.outEdges()) {
                VizNode nn=e.b();
                if (map.put(nn,nn)==null) { count++; sum=sum.add(bc[nn.pos()]); }
             }
             bc[n.pos()] = (count==0 ? Rational.ZERO : (sum.div(count)));
          }
          sortLayer(layer+1, new Comparator<VizNode>() {
             public int compare(VizNode o1, VizNode o2) {
                // If the two nodes have the same barycenter, we use their ordering that was established during layout_assignOrder()
                if (o1==o2) return 0;
                int n=bc[o1.pos()].cmp(bc[o2.pos()]); if (n!=0) return n; else if (o1.pos()<o2.pos()) return -1; else return 1;
             }
          });
          int j=1; for(VizNode n:layer(layer+1)) { bc[n.pos()]=Rational.make(j); j++; }
       }
    }

    //============================================================================================================================//

    /** Decide the exact X position of each component. */
    private void layout_computeX(List<VizNode> nodes) {
       // This implementation uses the iterative approach described in the paper "Layout of Bayesian Networks"
       // by Kim Marriott, Peter Moulder, Lucas Hope, and Charles Twardy
       final int n = nodes.size();
       if (n==0) return;
       final Block[] block = new Block[n+1];
       block[0] = new Block(); // The sentinel block
       for(int i=1; i<=n; i++) {
          Block b = new Block(nodes.get(i-1), i);
          block[i] = b;
          while(block[b.first-1].posn + block[b.first-1].width > b.posn) {
             b = new Block(block[b.first-1], b);
             block[b.last] = b;
             block[b.first] = b;
          }
       }
       int i=1;
       while(true) {
          Block b = block[i];
          double tmp = b.posn + (nodes.get(b.first-1).getWidth() + nodes.get(b.first-1).getReserved() + xJump)/2D;
          nodes.get(i-1).setX((int)tmp);
          for(i=i+1; i<=b.last; i++) {
             VizNode v1 = nodes.get(i-1);
             VizNode v2 = nodes.get(i-2);
             v1.setX(v2.x() + xsep(v1,v2));
          }
          i=b.last+1;
          if (i>n) break;
       }
    }

    /** This computes the xsep() value as described in the paper. */
    private static int xsep(VizNode a, VizNode b) {
        return (a.getWidth() + a.getReserved() + b.getWidth() + b.getReserved())/2 + xJump;
    }

    /** This computes the des() value as described in the paper. */
    private static double des(VizNode n) {
       // The desired location of V = ("sum e:in(V) | wt(e) * phi(start of e)" + "sum e:out(V) | wt(e) * phi(end of e)") / wt(v)
       int wt = wt(n);
       if (wt==0) return 0; // This means it has no "in" edges and no "out" edges
       double ans=0;
       for(VizEdge e:n.inEdges())  ans += ((double)e.weight()) * e.a().x();
       for(VizEdge e:n.outEdges()) ans += ((double)e.weight()) * e.b().x();
       return ans/wt;
    }

    /** This computes the wt() value as described in the paper. */
    private static int wt(VizNode n) {
       // Weight of a node is the sum of the weights of its in edges and out edges
       int ans=0;
       for(VizEdge e:n.inEdges()) ans += e.weight();
       for(VizEdge e:n.outEdges()) ans += e.weight();
       return ans;
    }

    /** This corresponds to the Block structure described in the paper. */
    private static final class Block {
       /** These fields are described in the paper. */
       private int first, last, weight;
       /** These fields are described in the paper. */
       private double width, posn, wposn;
       /** This constructs a regular block. */
       public Block(VizNode v, int i) {
          first=i; last=i; width=v.getWidth()+v.getReserved()+xJump; posn=des(v)-(width/2); weight=wt(v); wposn=weight*posn;
       }
       /** This merges the two existing blocks into a new block. */
       public Block(Block a, Block b) {
          first=a.first; last=b.last; width=a.width+b.width;
          wposn=a.wposn+b.wposn-a.width*b.weight;
          weight=a.weight+b.weight;
          posn=wposn/weight;
       }
       /** This constructs a sentinel block. */
       public Block() {
          first=0; last=0; width=0; posn=Double.NEGATIVE_INFINITY; weight=0; wposn=weight*posn;
       }
    }

    //============================================================================================================================//

    /** For each edge coming out of this layer of nodes, add bends to it if it currently overlaps some nodes inappropriately. */
    private void checkUpperCollision(List<VizNode> top) {
        final int room=2; // This is how much we need to stay clear of a node's boundary
        for(int i=0; i<top.size(); i++) {
            VizNode a=top.get(i); double left=a.x()-a.getWidth()/2, right=a.x()-a.getWidth()/2;
            for(VizEdge e:a.outEdges()) {
               VizNode b=e.b();
               if (b.x()>=right) for(int j=i+1; j<top.size(); j++) { // This edge goes from top-left to bottom-right
                  VizNode c=top.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cleft=c.x()-c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  e.path().bendDown(cleft, ctop-room, cbottom+room, 3);
               }
               else if (b.x()<=left) for(int j=i-1; j>=0; j--) { // This edge goes from top-right to bottom-left
                  VizNode c=top.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cright=c.x()+c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  e.path().bendDown(cright, ctop-room, cbottom+room, 3);
               }
            }
         }
    }

    //============================================================================================================================//

    /** For each edge going into this layer of nodes, add bends to it if it currently overlaps some nodes inappropriately. */
    private void checkLowerCollision(List<VizNode> bottom) {
        final int room=2; // This is how much we need to stay clear of a node's boundary
        for(int i=0; i<bottom.size(); i++) {
            VizNode b=bottom.get(i); double left=b.x()-b.getWidth()/2, right=b.x()-b.getWidth()/2;
            for(VizEdge e:b.inEdges()) {
               VizNode a=e.a();
               if (a.x()<=left) for(int j=i-1; j>=0; j--) { // This edge goes from top-left to bottom-right
                  VizNode c=bottom.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cright=c.x()+c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  e.path().bendUp(cright, ctop-room, cbottom+room, 3);
               }
               else if (a.x()>=right) for(int j=i+1; j<bottom.size(); j++) { // This edge goes from top-right to bottom-left
                  VizNode c=bottom.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cleft=c.x()-c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  e.path().bendUp(cleft, ctop-room, cbottom+room, 3);
               }
            }
         }
    }

    //============================================================================================================================//

    /** Perform the layout. */
    public void layout() {

        // The rest of the code below assumes at least one node, so we return right away if nodes.size()==0
        if (nodes.size()==0) return;

        // Calculate each node's width and height
        for(VizNode n:nodes) n.calcBounds();

        // Layout the nodes
        layout_assignOrder();
        layout_reverseAllBackwardEdges();
        final int layers = layout_assignNodesIntoLayers();
        layout_addDummyNodes();
        layout_decideTheOrderPerLayer();

        // For each layer, this array stores the height of its tallest node
        layerPH = new int[layers];

        // figure out the Y position of each layer, and also give each component an initial X position
        for(int layer=layers-1; layer>=0; layer--) {
           int x=5; // So that we're not touching the left-edge of the window
           int h=0;
           for(VizNode n: layer(layer)) {
              int nHeight = n.getHeight(), nWidth = n.getWidth();
              n.setX(x + nWidth/2);
              if (h < nHeight) h = nHeight;
              x = x + nWidth + n.getReserved() + 20;
           }
           layerPH[layer] = h;
        }

        // If there are more than one layer, then iteratively refine the X position of each component 3 times; 4 is a good number
        if (layers>1) {
           // It's important to NOT DO THIS when layers==0, because without edges the nodes will overlap each other into the center
           for(int i=0; i<3; i++) for(int layer=0; layer<layers; layer++) layout_computeX(layer(layer));
        }

        // Calculate each node's y; we start at y==5 so that we're not touching the top-edge of the window
        int py=5;
        for(int layer=layers-1; layer>=0; layer--) {
           final int ph = layerPH[layer];
           for(VizNode n:layer(layer)) n.setY(py + ph/2);
           py = py + ph + yJump;
        }

        relayout_edges(true);

        // Since we're doing layout for the first time, we need to explicitly set top and bottom, since
        // otherwise "recalc_bound" will merely "extend top and bottom" as needed.
        recalc_bound(true);
    }

    /** Re-establish top/left/width/height. */
    public void recalc_bound(boolean fresh) {
        if (nodes.size()==0) { top=0; bottom=10; totalHeight=10; left=0; totalWidth=10; return; }
        if (fresh) { top=nodes.get(0).y()-nodes.get(0).getHeight()/2-5; bottom=nodes.get(0).y()+nodes.get(0).getHeight()/2+5; }
        // Find the leftmost and rightmost pixel
        int minX = nodes.get(0).x() - nodes.get(0).getWidth()/2 - 5;
        int maxX = nodes.get(0).x() + nodes.get(0).getWidth()/2 + nodes.get(0).getReserved() + 5;
        for(VizNode n:nodes) {
            int min = n.x() - n.getWidth()/2                   - 5; if (minX>min) minX=min;
            int max = n.x() + n.getWidth()/2 + n.getReserved() + 5; if (maxX<max) maxX=max;
        }
        for(VizEdge e:edges) if (e.getLabelW()>0 && e.getLabelH()>0) {
            int x1=e.getLabelX(), x2=x1+e.getLabelW()-1;
            if (minX>x1) minX=x1;
            if (maxX<x2) maxX=x2;
        }
        left=minX-20;
        totalWidth=maxX-minX+20;
        // Find the topmost and bottommost pixel
        for(int layer=layers()-1; layer>=0; layer--) {
           for(VizNode n:layer(layer)) {
              int ytop=n.y()-n.getHeight()/2-5;    if (top>ytop) top=ytop;
              int ybottom=n.y()+n.getHeight()/2+5; if (bottom<ybottom) bottom=ybottom;
           }
        }
        totalHeight=bottom-top;
        int widestLegend=0, legendHeight=30;
        for(Map.Entry<Comparable<?>,Pair<String,Color>> e: legends.entrySet()) {
            if (e.getValue().b==null) continue; // that means this legend is not visible
            int widthOfLegend = (int) getBounds(true, e.getValue().a).getWidth();
            if (widestLegend < widthOfLegend) widestLegend = widthOfLegend;
            legendHeight += ad;
        }
        if (widestLegend>0) {
            left -= (widestLegend+10);
            totalWidth += (widestLegend*2+10);
            if (totalHeight<legendHeight) { bottom=bottom+(legendHeight-totalHeight); totalHeight=legendHeight; }
        }
    }

    /** Returns true if a direct line between a and b will not intersect any other node. */
    private boolean free(VizNode a, VizNode b) {
        if (a.layer() > b.layer()) { VizNode tmp=a; a=b; b=tmp; }
        Line2D.Double line = new Line2D.Double(a.x(), a.y(), b.x(), b.y());
        for(VizNode n:nodes) if (n!=a && n!=b && a.layer()<n.layer() && n.layer()<b.layer() && n.shape()!=null) {
            if (line.intersects(n.getBoundingBox(10,10))) return false;
        }
        return true;
    }

    /**
     * Assuming everything was laid out already, but at least one node just moved, then this re-layouts ALL edges.
     */
    void relayout_edges(boolean straighten) {
        // Move pairs of virtual nodes to straighten the lines if possible
        if (straighten) for(int i=0; i<5; i++) for(VizNode n:nodes) if (n.shape()==null) {
            VizEdge e1=n.inEdges().get(0), e2=n.outEdges().get(0);
            if (!free(e1.a(), e2.b())) continue;
            double slope = (e2.b().x()-e1.a().x()) / ((double)(e2.b().y()-e1.a().y()));
            double xx = (n.y()-e1.a().y())*slope + e1.a().x();
            n.setX((int)xx);
        }
        // Move the virtual nodes between endpoints to straighten the lines if possible
        if (straighten) for(VizEdge e:edges) if (e.a().shape()!=null && e.b().shape()==null) {
            VizNode a=e.a(), b;
            for(VizEdge ee=e;;) {
                b=ee.b();
                if (b.shape()!=null) break;
                ee=b.outEdges().get(0);
            }
            if (!free(a,b)) continue;
            double slope = (b.x()-a.x()) / ((double)(b.y()-a.y()));
            for(VizEdge ee=e;;) {
                b=ee.b();
                if (b.shape()!=null) break;
                double xx = (b.y()-a.y())*slope + a.x();
                b.setX((int)xx);
                ee=b.outEdges().get(0);
            }
        }
        // Now restore the invariant that nodes in each layer is ordered by x
        if (straighten) for(int i=0; i<layers(); i++) {
            sortLayer(i, new Comparator<VizNode>() {
                public int compare(VizNode o1, VizNode o2) {
                    if (o1.x()<o2.x()) return -1; else if (o1.x()>o2.x()) return 1;
                    return 0;
                }
            });
            // Ensure that nodes are not bunched up together horizontally.
            List<VizNode> layer=new ArrayList<VizNode>(layer(i));
            for(int j=layer.size()/2; j>=0 && j<layer.size()-1; j++) {
                VizNode a=layer.get(j), b=layer.get(j+1);
                int ax = a.shape()==null ? a.x() : (a.x()+a.getWidth()/2+a.getReserved());
                int bx = b.shape()==null ? b.x() : (b.x()-b.getWidth()/2);
                if (bx<=ax || bx-ax<5) b.setX(ax+5+b.getWidth()/2);
            }
            for(int j=layer.size()/2; j>0 && j<layer.size(); j--) {
                VizNode a=layer.get(j-1), b=layer.get(j);
                int ax = a.shape()==null ? a.x() : (a.x()+a.getWidth()/2+a.getReserved());
                int bx = b.shape()==null ? b.x() : (b.x()-b.getWidth()/2);
                if (bx<=ax || bx-ax<5) a.setX(bx-5-a.getWidth()/2-a.getReserved());
            }
        }
        // Now layout the edges, initially as straight lines
        for(VizEdge e:edges) e.resetPath();
        // Now, scan layer-by-layer to find edges that intersect nodes improperly, and bend them accordingly
        for(int layer=layers()-1; layer>0; layer--) {
           List<VizNode> top=layer(layer), bottom=layer(layer-1);
           checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
        }
        // Now, for each edge, adjust its arrowhead and label.
        AvailableSpace sp = new AvailableSpace();
        for(VizNode n:nodes) if (n.shape()!=null) sp.add(n.x()-n.getWidth()/2, n.y()-n.getHeight()/2, n.getWidth()+n.getReserved(), n.getHeight());
        for(VizEdge e:edges) { e.layout_arrowHead(); e.repositionLabel(sp); }
    }

    /**
     * Assuming everything was laid out already, and that the nodes in layer[i] just moved horizontally,
     * this re-layouts the edges going to and from layer i.
     */
    public void relayout_edges(int i) {
        if (nodes.size()==0) return; // The rest of the code assumes there is at least one node
        for(VizNode n:layer(i)) for(VizEdge e:n.selfEdges()) { e.resetPath(); e.layout_arrowHead(); }
        if (i>0) {
            List<VizNode> top=layer(i), bottom=layer(i-1);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) e.resetPath();
            checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
        }
        if (i<layers()-1) {
            List<VizNode> top=layer(i+1), bottom=layer(i);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) e.resetPath();
            checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
        }
        // Now, for each edge, adjust its arrowhead and label.
        AvailableSpace sp = new AvailableSpace();
        for(VizNode n:nodes) if (n.shape()!=null) sp.add(n.x()-n.getWidth()/2, n.y()-n.getHeight()/2, n.getWidth()+n.getReserved(), n.getHeight());
        for(VizEdge e:edges) { e.layout_arrowHead(); e.repositionLabel(sp); }
    }

    //============================================================================================================================//

    /** Locates the node or edge at the given (X,Y) location. */
    Object alloyFind(double scale, int mouseX, int mouseY) {
       int h=getTop()+10-ad;
       double x=mouseX/scale+getLeft(), y=mouseY/scale+getTop();
       for(Map.Entry<Comparable<?>,Pair<String,Color>> e:legends.entrySet()) {
           if (e.getValue().b==null) continue;
           h=h+ad;
           if (y<h || y>=h+ad) continue;
           int w = (int) getBounds(true, e.getValue().a).getWidth();
           if (x>=getLeft()+10 && x<=getLeft()+10+w) return e.getKey();
       }
       for(VizNode n:nodes) {
           if (n.shape()==null && Math.abs(n.x()-x)<10 && Math.abs(n.y()-y)<10) return n;
           if (n.contains(x,y)) return n;
       }
       for(VizEdge e:edges) {
           if (e.a()!=e.b()) {
              double dx;
              dx=e.path().getXatY(y, 0, 1, Double.NaN); if (!Double.isNaN(dx) && StrictMath.abs(x-dx)<12/scale) return e;
           } else {
              double dx;
              dx=e.path().getXatY(y, 0.25, 0.75, Double.NaN); if (!Double.isNaN(dx) && StrictMath.abs(x-dx)<12/scale) return e;
              dx=e.path().getXatY(y, 0,    0.25, Double.NaN); if (!Double.isNaN(dx) && StrictMath.abs(x-dx)<12/scale) return e;
              dx=e.path().getXatY(y, 0.75, 1,    Double.NaN); if (!Double.isNaN(dx) && StrictMath.abs(x-dx)<12/scale) return e;
           }
       }
       return null;
    }

    /** Assuming layout has been performed, this draws the graph with the given magnification scale. */
    public void draw(Artist gr, double scale, Object highlight, boolean showLegends) {
        if (nodes.size()==0) return; // The rest of this procedure assumes there is at least one node
        Object group=null;
        VizNode highFirstNode=null, highLastNode=null;
        VizEdge highFirstEdge=null, highLastEdge=null;
        if (highlight instanceof VizEdge) {
          highFirstEdge=(VizEdge)highlight;
          highLastEdge=highFirstEdge;
          group=highFirstEdge.group;
          while(highFirstEdge.a().shape()==null) highFirstEdge=highFirstEdge.a().inEdges().get(0);
          while(highLastEdge.b().shape()==null) highLastEdge=highLastEdge.b().outEdges().get(0);
          highFirstNode=highFirstEdge.a();
          highLastNode=highLastEdge.b();
        } else if (!(highlight instanceof VizNode) && highlight!=null) {
          group=highlight;
        }
        // Since drawing an edge will automatically draw all segments if they're connected via dummy nodes,
        // we must make sure we only draw out edges from non-dummy-nodes
        int maxAscent = Artist.getMaxAscent();
        for(VizNode n:nodes) if (n.shape()!=null) {
          for(VizEdge e:n.outEdges())  if (e.group!=group) e.draw(gr, scale, highFirstEdge, group);
          for(VizEdge e:n.selfEdges()) if (e.group!=group) e.draw(gr, scale, highFirstEdge, group);
        }
        if (group!=null) {
          for(VizNode n:nodes) if (n.shape()!=null) {
            for(VizEdge e:n.outEdges())  if (e.group==group && e!=highFirstEdge) e.draw(gr, scale, highFirstEdge, group);
            for(VizEdge e:n.selfEdges()) if (e.group==group && e!=highFirstEdge) e.draw(gr, scale, highFirstEdge, group);
          }
          if (highFirstEdge!=null) highFirstEdge.draw(gr, scale, highFirstEdge, group);
        }
        for(VizNode n:nodes) if (highFirstNode!=n && highLastNode!=n) n.draw(gr, scale, n==highlight);
        if (highFirstNode!=null) highFirstNode.draw(gr, scale, true);
        if (highLastNode!=null && highLastNode!=highFirstNode) highLastNode.draw(gr, scale, true);
        if (highFirstEdge!=null) highFirstEdge.drawLabel(gr, highFirstEdge.color(), new Color(255,255,255,160));
        // show legends?
        if (!showLegends || legends.size()==0) return;
        boolean groupFound=false;
        int y=0, maxWidth=0;
        for(Map.Entry<Comparable<?>,Pair<String,Color>> e:legends.entrySet()) {
            if (e.getValue().b==null) continue;
            if (group!=null && e.getKey()==group) groupFound=true;
            int w = (int) getBounds(true, e.getValue().a).getWidth();
            if (maxWidth<w) maxWidth=w;
            y = y + ad;
        }
        if (y==0) return; // This means no legends need to be drawn
        gr.setColor(Color.GRAY);
        gr.draw(new RoundRectangle2D.Double(5, 5, maxWidth+10, y+10, 5, 5), false);
        y=10;
        for(Map.Entry<Comparable<?>,Pair<String,Color>> e:legends.entrySet()) {
            Color color = e.getValue().b;
            if (color==null) continue;
            gr.setFont((groupFound && e.getKey()==group) || !groupFound);
            gr.setColor((!groupFound || e.getKey()==group) ? color : Color.GRAY);
            gr.drawString(e.getValue().a, 8, y+maxAscent);
            y = y + ad;
        }
    }
}
