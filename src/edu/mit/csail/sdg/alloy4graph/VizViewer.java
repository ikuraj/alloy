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

import static java.awt.Color.WHITE;
import static java.awt.Color.BLACK;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.OurPNGWriter;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * This class displays the graph.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizViewer extends JPanel {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /** The graph that we are displaying. */
    private final VizGraph graph;

    /** The current amount of zoom. */
    private double scale = 1d;

    /** The currently hovered VizNode or VizEdge, or null if there is none. */
    private Object highlight = null;

    /** The currently selected VizNode or VizEdge, or null if there is none. */
    private Object selected = null;

    /** The right-click context menu associated with this JPanel. */
    private final JPopupMenu pop = new JPopupMenu();

    /** This allows users to attach a String object to this JPanel. */
    private String annotation = "";

    /** Locates the node or edge at the given (X,Y) location. */
    private Object do_find(int mouseX, int mouseY) {
       double x=mouseX/scale+graph.left, y=mouseY/scale+graph.top;
       for(VizNode n:graph.nodes) {
           if (n.shape()==null && Math.abs(n.x()-x)<10 && Math.abs(n.y()-y)<10) return n;
           if (n.intersects(x,y)) return n;
       }
       for(VizEdge e:graph.edges) {
           if (e.intersects(x,y,scale)) return e;
       }
       return null;
    }

    private int oldMouseX=0, oldMouseY=0, oldX=0, oldY=0;

    /** Repaint this component. */
    private void do_repaint() {
        Container c=getParent();
        while(c!=null) { if (c instanceof JViewport) break; else c=c.getParent(); }
        setSize((int)(graph.totalWidth*scale), (int)(graph.totalHeight*scale));
        if (c!=null) { c.invalidate(); c.repaint(); c.validate(); } else { invalidate(); repaint(); validate(); }
    }

    /** Construct a VizViewer that displays the given graph. */
    public VizViewer(final VizGraph graph) {
        setOpaque(true);
        setBackground(WHITE);
        setBorder(new EmptyBorder(0,0,0,0));
        this.graph = graph;
        graph.layout();
        final JMenuItem zoomIn = new JMenuItem("Zoom In");
        final JMenuItem zoomOut = new JMenuItem("Zoom Out");
        final JMenuItem zoom100 = new JMenuItem("Zoom to 100%");
        final JMenuItem zoomToFit = new JMenuItem("Zoom to Fit");
        final JMenuItem print = new JMenuItem("Export to PNG");
        pop.add(zoomIn);
        pop.add(zoomOut);
        pop.add(zoom100);
        pop.add(zoomToFit);
        pop.add(print);
        ActionListener act = new ActionListener() {
           public void actionPerformed(ActionEvent e) {
              Container c=getParent();
              while(c!=null) { if (c instanceof JViewport) break; else c=c.getParent(); }
              if (e.getSource() == print) do_saveAsPNG();
              if (e.getSource() == zoom100) scale=1d;
              if (e.getSource() == zoomIn) { scale=scale*1.33d; if (!(scale<500d)) scale=500d; }
              if (e.getSource() == zoomOut) { scale=scale/1.33d; if (!(scale>0.1d)) scale=0.1d; }
              if (e.getSource() == zoomToFit) {
                 if (c==null) return;
                 int w=c.getWidth()-15, h=c.getHeight()-15; // 15 gives a comfortable round-off margin
                 if (w<=0 || h<=0) return;
                 double scale1 = ((double)w)/graph.totalWidth, scale2 = ((double)h)/graph.totalHeight;
                 if (scale1<scale2) scale=scale1; else scale=scale2;
              }
              do_repaint();
           }
        };
        zoomIn.addActionListener(act);
        zoomOut.addActionListener(act);
        zoom100.addActionListener(act);
        zoomToFit.addActionListener(act);
        print.addActionListener(act);
        addMouseMotionListener(new MouseMotionAdapter() {
           @Override public void mouseMoved(MouseEvent ev) {
              if (pop.isVisible()) return;
              Object obj=do_find(ev.getX(), ev.getY());
              if (highlight!=obj) { highlight=obj; do_repaint(); }
           }
           @Override public void mouseDragged(MouseEvent ev) {
              if (selected instanceof VizNode) {
                 int newX=(int)(oldX+(ev.getX()-oldMouseX)/scale);
                 int newY=(int)(oldY+(ev.getY()-oldMouseY)/scale);
                 VizNode n=(VizNode)selected;
                 if (n.x()!=newX || n.y()!=newY) {
                    n.tweak(newX,newY);
                    do_repaint();
                    scrollRectToVisible(new Rectangle(
                      (int)((newX-graph.left)*scale)-n.getWidth()/2-5,
                      (int)((newY-graph.top)*scale)-n.getHeight()/2-5,
                      n.getWidth()+n.getReserved()+10, n.getHeight()+10
                    ));
                 }
              }
           }
        });
        addMouseListener(new MouseAdapter() {
           @Override public void mouseReleased(MouseEvent ev) {
               Object obj=do_find(ev.getX(), ev.getY());
               if (selected!=null || highlight!=obj) { graph.recalc_bound(true); selected=null; highlight=obj; do_repaint(); }
           }
           @Override public void mousePressed(MouseEvent ev) {
               if (ev.getButton()==MouseEvent.BUTTON3) {
                  Object x=do_find(ev.getX(), ev.getY());
                  if (selected!=x || highlight!=null) { selected=x; highlight=null; do_repaint(); }
                  pop.show(VizViewer.this, ev.getX(), ev.getY());
               } else if (ev.getButton()==MouseEvent.BUTTON1 && ev.isControlDown()) {
                  // This lets Ctrl+LeftClick bring up the popup menu, just like RightClick,
                  // since many Mac mouses do not have a right button.
                  Object x=do_find(ev.getX(), ev.getY());
                  if (selected!=x || highlight!=null) { selected=x; highlight=null; do_repaint(); }
                  pop.show(VizViewer.this, ev.getX(), ev.getY());
               } else if (ev.getButton()==MouseEvent.BUTTON1 && !ev.isControlDown() && !ev.isShiftDown() && !ev.isAltDown()) {
                  Object x=do_find(oldMouseX=ev.getX(), oldMouseY=ev.getY());
                  if (x instanceof VizNode) { oldX=((VizNode)x).x(); oldY=((VizNode)x).y(); }
                  if (selected!=x || highlight!=null) { selected=x; highlight=null; do_repaint(); }
               }
           }
           @Override public void mouseExited(MouseEvent ev) {
               if (highlight!=null) { highlight=null; do_repaint(); }
           }
        });
    }

    /** Retrieves the annotation associated with this object; "" if no annotation has been set yet. */
    public String do_getAnnotation() { return annotation; }

    /** Changes the annotation associated with this object. */
    public void do_setAnnotation(String newAnnotation) { this.annotation=newAnnotation; }

    /**
     * This color is used as the background for a JTextField that contains bad data.
     * <p> Note: we intentionally choose to make it an instance field rather than a static field,
     * since we want to make sure we only instantiate it from the AWT Event Dispatching thread.
     */
    private final Color badColor = new Color(255,200,200);

    /** This synchronized field stores the most recent DPI value. */
    private static double oldDPI=300;

    /** True if we are currently in the middle of a DocumentListener already. */
    private boolean recursive=false;

    /** This updates the three input boxes and the three accompanying text labels, then return the width in pixels. */
    private int do_refresh
    (int who, double ratio, JTextField w1, JLabel w2, JTextField h1, JLabel h2, JTextField d1, JLabel d2, JLabel msg) {
       if (recursive) return 0;
       try {
          recursive=true;
          w1.setBackground(WHITE); h1.setBackground(WHITE); d1.setBackground(WHITE);
          boolean bad=false;
          double w; try { w=Double.parseDouble(w1.getText()); } catch(NumberFormatException ex) { w=0; }
          double h; try { h=Double.parseDouble(h1.getText()); } catch(NumberFormatException ex) { h=0; }
          double d; try { d=Double.parseDouble(d1.getText()); } catch(NumberFormatException ex) { d=0; }
          if (who==1) { h=((int)(w*100/ratio))/100D; h1.setText(""+h); } // Maintains aspect ratio
          if (who==2) { w=((int)(h*100*ratio))/100D; w1.setText(""+w); } // Maintains aspect ratio
          if (!(d>=0.01) || !(d<=10000)) {
              bad=true;
              d1.setBackground(badColor);
              msg.setText("DPI must be between 0.01 and 10000");
          }
          if (!(h>=0.01) || !(h<=10000)) {
              bad=true;
              h1.setBackground(badColor);
              msg.setText("Height must be between 0.01 and 10000");
              if (who==1) h1.setText("");
          }
          if (!(w>=0.01) || !(w<=10000)) {
              bad=true;
              w1.setBackground(badColor);
              msg.setText("Width must be between 0.01 and 10000");
              if (who==2) w1.setText("");
          }
          if (bad) { w2.setText(" inches"); h2.setText(" inches"); return 0; } else msg.setText(" ");
          w2.setText(" inches ("+(int)(w*d)+" pixels)");
          h2.setText(" inches ("+(int)(h*d)+" pixels)");
          return (int)(w*d);
       } finally {
          recursive=false;
       }
    }

    /** Export the current drawing as a PNG file by asking the user for the filename and the image resolution. */
    public void do_saveAsPNG() {
       // Find the enclosing JFrame if such a JFrame exists
       JFrame me=null;
       for(Container c=getParent(); c!=null; c=c.getParent()) if (c instanceof JFrame) { me=(JFrame)c; break; }
       // Figure out the initial width, height, and DPI that we might want to suggest to the user
       final double ratio=((double)(graph.totalWidth))/graph.totalHeight;
       double dpi, iw=8.5D, ih=((int)(iw*100/ratio))/100D;    // First set the width to be 8.5inch and compute height accordingly
       if (ih>11D) { ih=11D; iw=((int)(ih*100*ratio))/100D; } // If too tall, then set height=11inch, and compute width accordingly
       synchronized(VizViewer.class) { dpi=oldDPI; }
       // Prepare the dialog box
       final JLabel msg=new JLabel(" ");
       msg.setForeground(Color.RED);
       final JLabel w=new JLabel("Width: "+((int)(graph.totalWidth*scale))+" pixels");
       final JLabel h=new JLabel("Height: "+((int)(graph.totalHeight*scale))+" pixels");
       final JTextField w1=new JTextField(""+iw); final JLabel w0=new JLabel("Width: "), w2=new JLabel();
       final JTextField h1=new JTextField(""+ih); final JLabel h0=new JLabel("Height: "), h2=new JLabel();
       final JTextField d1=new JTextField(""+(int)dpi); final JLabel d0=new JLabel("Resolution: "), d2=new JLabel(" dots per inch");
       Dimension dim = new Dimension(100,20);
       w1.setMaximumSize(dim); w1.setPreferredSize(dim);
       h1.setMaximumSize(dim); h1.setPreferredSize(dim);
       d1.setMaximumSize(dim); d1.setPreferredSize(dim);
       do_refresh(0,ratio,w1,w2,h1,h2,d1,d2,msg);
       w1.setEnabled(false);
       w1.getDocument().addDocumentListener(new DocumentListener() {
          public void changedUpdate(DocumentEvent e) { do_refresh(1,ratio,w1,w2,h1,h2,d1,d2,msg); }
          public void insertUpdate(DocumentEvent e) { changedUpdate(null); }
          public void removeUpdate(DocumentEvent e) { changedUpdate(null); }
       });
       h1.setEnabled(false);
       h1.getDocument().addDocumentListener(new DocumentListener() {
          public void changedUpdate(DocumentEvent e) { do_refresh(2,ratio,w1,w2,h1,h2,d1,d2,msg); }
          public void insertUpdate(DocumentEvent e) { changedUpdate(null); }
          public void removeUpdate(DocumentEvent e) { changedUpdate(null); }
       });
       d1.setEnabled(false);
       d1.getDocument().addDocumentListener(new DocumentListener() {
          public void changedUpdate(DocumentEvent e) { do_refresh(3,ratio,w1,w2,h1,h2,d1,d2,msg); }
          public void insertUpdate(DocumentEvent e) { changedUpdate(null); }
          public void removeUpdate(DocumentEvent e) { changedUpdate(null); }
       });
       final JRadioButton b1 = new JRadioButton("Using the window's current magnification:", true);
       final JRadioButton b2 = new JRadioButton("Using a specific width, height, and resolution:", false);
       b1.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
             if (b2.isSelected()) b2.setSelected(false);
             if (!b1.isSelected()) b1.setSelected(true);
             w1.setEnabled(false); h1.setEnabled(false); d1.setEnabled(false); msg.setText(" ");
             w1.setBackground(WHITE); h1.setBackground(WHITE); d1.setBackground(WHITE);
          }
       });
       b2.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
             if (b1.isSelected()) b1.setSelected(false);
             if (!b2.isSelected()) b2.setSelected(true);
             w1.setEnabled(true); h1.setEnabled(true); d1.setEnabled(true);
             do_refresh(1,ratio,w1,w2,h1,h2,d1,d2,msg);
          }
       });
       // Ask whether the user wants to change the width, height, and DPI
       double myScale;
       while(true) {
          if (!OurDialog.getInput(me, "Export as PNG", new Object[]{
             b1, OurUtil.makeH(20, w, null), OurUtil.makeH(20, h, null), " ",
             b2, OurUtil.makeH(20, w0, w1, w2, null),
             OurUtil.makeH(20, h0, h1, h2, null),
             OurUtil.makeH(20, d0, d1, d2, null),
             OurUtil.makeH(20, msg, null)
          })) return;
          // Let's validate the values
          if (b2.isSelected()) {
             int widthInPixel=do_refresh(3,ratio,w1,w2,h1,h2,d1,d2,msg);
             String err = msg.getText().trim();
             if (err.length()>0) continue;
             dpi=Integer.parseInt(d1.getText());
             myScale=((double)widthInPixel)/graph.totalWidth;
             int heightInPixel=(int)(graph.totalHeight*myScale);
             if (widthInPixel>4000 || heightInPixel>4000)
                if (!OurDialog.yesno(me, "The image dimension ("+widthInPixel+"x"+heightInPixel+") is very large. Are you sure?"))
                   continue;
          } else {
             dpi=300;
             myScale=scale;
          }
          break;
       }
       // Ask the user for a filename
       File filename = OurDialog.askFile(me, false, null, ".png", "PNG file");
       if (filename==null) return;
       if (filename.exists() && !OurDialog.askOverwrite(me, filename.getAbsolutePath())) return;
       // Attempt to write the PNG file
       try {
          System.gc(); // Try to avoid possible premature out-of-memory exceptions
          do_saveAsPNG(filename.getAbsolutePath(), myScale, dpi, dpi);
          synchronized(VizViewer.class) { oldDPI=dpi; }
          Util.setCurrentDirectory(filename.getParentFile());
       } catch(IOException ex) {
          OurDialog.alert(me, "An error has occured in writing the PNG file:\n"+ex, "Error");
       } catch(OutOfMemoryError ex) {
          System.gc();
          OurDialog.alert(me, "Insufficient memory to export a file of that size; please reduce the DPI and try again.", "Error");
       }
    }

    /** Export the current drawing as a PNG file with the given file name and image resolution. */
    public void do_saveAsPNG(String filename, double scale, double dpiX, double dpiY) throws IOException, OutOfMemoryError {
       int width = (int) (graph.totalWidth*scale);   if (width<10) width=10;
       int height = (int) (graph.totalHeight*scale); if (height<10) height=10;
       BufferedImage bf = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
       Graphics2D gr = (Graphics2D) (bf.getGraphics());
       gr.setColor(WHITE);
       gr.fillRect(0, 0, width, height);
       gr.setColor(BLACK);
       gr.scale(scale,scale);
       gr.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
       graph.draw(new Artist(gr), scale, null, null);
       OurPNGWriter.writePNG(bf, filename, dpiX, dpiY);
    }

    /** Show the popup menu at location (x,y) */
    public void do_popup(Component c, int x, int y) {
       pop.show(c,x,y);
    }

    /** {@inheritDoc} */
    @Override public Dimension getPreferredSize() {
        return new Dimension((int)(graph.totalWidth*scale), (int)(graph.totalHeight*scale));
    }

    /** {@inheritDoc} */
    @Override public void paintComponent(final Graphics gr) {
        super.paintComponent(gr);
        Graphics2D g2 = (Graphics2D)gr;
        AffineTransform oldAF = (AffineTransform) (g2.getTransform().clone());
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.scale(scale, scale);
        Object sel=(selected!=null ? selected : highlight);
        VizNode c=null;
        if (selected instanceof VizNode && ((VizNode)selected).shape()==null) { c=(VizNode)selected; sel=c.inEdges().get(0); }
        if (highlight instanceof VizNode && ((VizNode)highlight).shape()==null) { c=(VizNode)highlight; sel=c.inEdges().get(0); }
        graph.draw(new Artist(g2), scale, sel, null);
        if (c!=null) { gr.setColor(Color.RED); gr.fillArc(c.x()-5-graph.left, c.y()-5-graph.top, 10, 10, 0, 360); }
        g2.setTransform(oldAF);
    }
}
