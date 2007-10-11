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

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Rational;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * Mutable; represents a graph.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizGraph extends DiGraph {

    //================================ adjustable options ========================================================================//

    /** Minimum horizontal distance between adjacent nodes. */
    static final int xJump=15;

    /** Minimum vertical distance between adjacent layers. */
    static final int yJump=30;

    /** The left edge. */
    int left=0;

    /** The top edge. */
    int top=0;

    /** The bottom edge. */
    int bottom=0;

    /** The total width of the graph; this value is computed by layout(). */
    int totalWidth=0;

    /** The total height of the graph; this value is computed by layout(). */
    int totalHeight=0;

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

    //=============================== constructors and other methods =============================================================//

    /** Constructs an empty VizGraph object. */
    public VizGraph() { }

    //============================================================================================================================//

    /** Convert multiedges into unique edges by inserting dummy nodes. */
    private void layout_remoteMultiEdges() {
       for(VizNode n:new ArrayList<VizNode>(nodes)) {
          List<VizEdge> outs=n.outEdges();
          for(int i=0; i<outs.size(); i++) {
             VizEdge e1=outs.get(i);
             VizNode n1=e1.b();
             boolean multiedge=false;
             for(int j=i+1; j<outs.size(); j++) {
                VizEdge e2=outs.get(j);
                VizNode n2=e2.b();
                if (n1==n2) {
                   n2=new VizNode(this).set((VizShape)null);
                   e2.changeB(n2);
                   new VizEdge(n2, n1, e2.ahead(), e2.bhead(), e2.style(), e2.color());
                   multiedge=true;
                }
             }
             if (multiedge) {
                VizNode n2=new VizNode(this).set((VizShape)null);
                e1.changeB(n2);
                new VizEdge(n2, n1, e1.ahead(), e1.bhead(), e1.style(), e1.color());
             }
          }
       }
    }

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
       for(final VizEdge e:new ArrayList<VizEdge>(edges)) if (e.a()!=e.b()) { // Skip the self loops
          int n=(e.a().layer()-e.b().layer());
          if (n>1) {
             VizNode last=e.a();
             for(int i=1; i<n; i++) {
                VizNode next=new VizNode(last.graph).set((VizShape)null);
                next.setLayer(last.layer()-1);
                new VizEdge(last, next, e.ahead(), e.bhead(), e.style(), e.color());
                last=next;
             }
             e.changeA(last);
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
                  int intersect=e.path().intersectsVertical(cleft, ctop-room, cbottom+room, null);
                  if (intersect>=0) e.pathAdd(intersect, cleft, cbottom+5);
               }
               else if (b.x()<=left) for(int j=i-1; j>=0; j--) { // This edge goes from top-right to bottom-left
                  VizNode c=top.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cright=c.x()+c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  int intersect=e.path().intersectsVertical(cright, ctop-room, cbottom+room, null);
                  if (intersect>=0) e.pathAdd(intersect, cright, cbottom+5);
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
                  int intersect=e.path().intersectsVertical(cright, ctop-room, cbottom+room, null);
                  if (intersect>=0) e.pathAdd(intersect, cright, ctop-5);
               }
               else if (a.x()>=right) for(int j=i+1; j<bottom.size(); j++) { // This edge goes from top-right to bottom-left
                  VizNode c=bottom.get(j);
                  if (c.shape()==null) continue; // You can intersect thru a dummy node
                  double ctop=c.y()-c.getHeight()/2, cleft=c.x()-c.getWidth()/2, cbottom=c.y()+c.getHeight()/2;
                  int intersect=e.path().intersectsVertical(cleft, ctop-room, cbottom+room, null);
                  if (intersect>=0) e.pathAdd(intersect, cleft, ctop-5);
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

        // If a node has too many self-edges, we add blank labels to the node's label until there is a reasonable vertical gap.
        for(VizNode n:nodes) {
           int num=n.selfEdges().size();
           while(true) {
             double y=n.getHeight()/(1D+2*num);
             if (!(y<VizEdge.selfLoopYGap)) break;
             n.addBefore(" ");
             n.addAfter(" ");
             n.calcBounds();
           }
        }

        // Layout the nodes
        layout_remoteMultiEdges();
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

        relayout_edges();

        // Since we're doing layout for the first time, we need to explicitly set top and bottom, since
        // otherwise "recalc_bound" will merely "extend top and bottom" as needed.
        recalc_bound(true);
    }

    /** Re-establish top/left/width/height. */
    public void recalc_bound(boolean fresh) {
        if (nodes.size()==0) { top=0; bottom=10; totalHeight=10; left=0; totalWidth=10; return; }
        if (fresh) { top=nodes.get(0).y()-nodes.get(0).getHeight()/2-5; bottom=nodes.get(0).y()+nodes.get(0).getHeight()/2+5; }
        // Find the leftmost and rightmost pixel
        int minX = (int) (nodes.get(0).x() - nodes.get(0).getWidth()/2 - 5);
        int maxX = (int) (nodes.get(0).x() + nodes.get(0).getWidth()/2 + 5);
        for(VizNode n:nodes) {
            int min = (int) (n.x() - n.getWidth()/2                   - 5); if (minX>min) minX=min;
            int max = (int) (n.x() + n.getWidth()/2 + n.getReserved() + 5); if (maxX<max) maxX=max;
        }
        left=minX;
        totalWidth=maxX-minX;
        // Find the topmost and bottommost pixel
        for(int layer=layers()-1; layer>=0; layer--) {
           for(VizNode n:layer(layer)) {
              int ytop=n.y()-n.getHeight()/2-5;    if (top>ytop) top=ytop;
              int ybottom=n.y()+n.getHeight()/2+5; if (bottom<ybottom) bottom=ybottom;
           }
        }
        totalHeight=bottom-top;
    }

    /**
     * Assuming everything was laid out already, but at least one node just moved, then this re-layouts ALL edges.
     */
    public void relayout_edges() {
        // Now layout the edges, initially as straight lines
        for(VizEdge e:edges) e.resetPath();
        // Now, scan layer-by-layer to find edges that intersect nodes improperly, and bend them accordingly
        for(int layer=layers()-1; layer>0; layer--) {
           List<VizNode> top=layer(layer), bottom=layer(layer-1);
           checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
        }
        // Now, for each edge that has an arrow head, move it to the right place
        for(VizEdge e:edges) layout_arrowHead(e);
    }

    /**
     * Assuming everything was laid out already, and that the nodes in layer[i] just moved horizontally,
     * this re-layouts the edges going to and from layer i.
     */
    public void relayout_edges(int i) {
        if (nodes.size()==0) return; // The rest of the code assumes there is at least one node
        for(VizNode n:layer(i)) for(VizEdge e:n.selfEdges()) { e.resetPath(); layout_arrowHead(e); }
        if (i>0) {
            List<VizNode> top=layer(i), bottom=layer(i-1);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) e.resetPath();
            checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) layout_arrowHead(e);
        }
        if (i<layers()-1) {
            List<VizNode> top=layer(i+1), bottom=layer(i);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) e.resetPath();
            checkUpperCollision(top); checkLowerCollision(bottom); checkUpperCollision(top);
            for(VizNode n:top) for(VizEdge e:n.outEdges()) layout_arrowHead(e);
        }
    }

    /** Positions the arrow heads of the given edge properly. */
    private void layout_arrowHead(VizEdge e) {
        VizPath p=e.path();
        if (e.a()==e.b()) {
           if (e.ahead() && e.a().shape()!=null) {
              double y=p.getY(0), x=e.a().intersectsAtHeight(y);
              p.move(0, x, y);
           }
           if (e.bhead() && e.b().shape()!=null) {
              double y=p.getY(p.getPoints()-1), x=e.a().intersectsAtHeight(y);
              p.move(p.getPoints()-1, x, y);
           }
        } else {
           if (e.ahead() && e.a().shape()!=null) {
              Point2D.Double ans = new Point2D.Double();
              e.a().intersectsNonhorizontalRay(p.getX(1), p.getY(1), ans);
              p.move(0, ans.x, ans.y);
           }
           if (e.bhead() && e.b().shape()!=null) {
              Point2D.Double ans = new Point2D.Double();
              e.b().intersectsNonhorizontalRay(p.getX(p.getPoints()-2), p.getY(p.getPoints()-2), ans);
              p.move(p.getPoints()-1, ans.x, ans.y);
           }
        }
    }

    //============================================================================================================================//

    /** Assuming layout has been performed, this drwas the graph with the given magnification scale. */
    public void draw(Artist gr, double scale, Object selected, Object highlight) {
        if (nodes.size()==0) return; // The rest of this procedure assumes there is at least one node
        if (highlight==null) highlight=selected;
        VizEdge highFirstEdge=null, highLastEdge=null;
        if (highlight instanceof VizEdge) {
           highFirstEdge=(VizEdge)highlight;
           while(highFirstEdge.a().shape()==null) highFirstEdge=highFirstEdge.a().inEdges().get(0);
           highLastEdge=(VizEdge)highlight;
           while(highLastEdge.b().shape()==null) highLastEdge=highLastEdge.b().outEdges().get(0);
        }
        // Since drawing an edge will automatically draw all segments if they're connected via dummy nodes,
        // we must make sure we only draw out edges from non-dummy-nodes
        for(VizNode n:nodes) if (n.shape()!=null) {
           for(VizEdge e:n.outEdges())  if (e!=highFirstEdge) e.draw(gr, scale, false);
           for(VizEdge e:n.selfEdges()) if (e!=highFirstEdge) e.draw(gr, scale, false);
        }
        if (highFirstEdge!=null) highFirstEdge.draw(gr, scale, true);
        for(VizNode n:nodes) n.draw(gr, scale, n==highlight);
        double tip = Artist.getMaxAscentAndDescent(nodes.get(0).fontSize(), false)*0.6D;
        for(VizEdge e:edges) if (e!=highFirstEdge && e!=highLastEdge) e.drawArrowhead(gr, scale, false, tip);
        if (highFirstEdge!=null) highFirstEdge.drawArrowhead(gr, scale, true, tip);
        if (highLastEdge!=null && highLastEdge!=highFirstEdge) highLastEdge.drawArrowhead(gr, scale, true, tip);
    }
}
