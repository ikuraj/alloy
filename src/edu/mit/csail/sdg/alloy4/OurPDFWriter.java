package edu.mit.csail.sdg.alloy4;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import edu.mit.csail.sdg.alloy4graph.Artist;
import edu.mit.csail.sdg.alloy4graph.VizEdge;
import edu.mit.csail.sdg.alloy4graph.VizGraph;
import edu.mit.csail.sdg.alloy4graph.VizNode;
import edu.mit.csail.sdg.alloy4graph.VizShape;
import edu.mit.csail.sdg.alloy4graph.VizStyle;
import edu.mit.csail.sdg.alloy4graph.VizViewer;

/**
 * Graphical convenience methods for producing PDF files.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurPDFWriter {

    /** This is the file we're writing. */
    private RandomAccessFile out;

    /** This stores the exact offset of each PDF object we've written. */
    private List<Long> offset=new ArrayList<Long>();

    /** This stores the ID of the "Font" PDF Object. */
    private long fontID;

    /** This stores the ID of the "Content" PDF Object. */
    private long contentID;

    /** This stores the ID of the "ContentSize" PDF Object. */
    private long contentSizeID;

    /** This stores the ID of the "Page" PDF Object. */
    private long pageID;

    /** This stores the ID of the "Pages" PDF Object. */
    private long pagesID;

    /** This stores the ID of the "Catalog" PDF Object. */
    private long catalogID;

    /** This stores the exact offset of the start of the content stream. */
    private long startOfContent;

    /** The user-specified DPI. */
    private long dpi;

    /** The width (in terms of dots). */
    private long width;

    /** The height (in terms of dots). */
    private long height;

    /** Nonnull if an IOException has occurred. */
    private IOException err=null;

    /**
     * Write the PDF header into the file, then begins a Contents stream; (if the file already exists, it will be overwritten).
     * @throws IllegalArgumentException if dpi is less than 72 or is greater than 3000
     * @throws IOException if an error occurred in opening or writing to the file
     */
    public OurPDFWriter(int dpi, String filename) throws IOException {
        // Initialize various data structures
        if (dpi<72 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 72 and 3000");
        this.dpi=dpi;
        width=dpi*8+(dpi/2);
        height=dpi*11;
        offset.clear();
        offset.add(0L); // ID 0 does not exist
        out=new RandomAccessFile(filename, "rw");
        out.setLength(0);
        // Write %PDF-1.3
        out.write(new byte[]{'%', 'P', 'D', 'F', '-', '1', '.', '3', 10, '%', -127, 10, 10});
        // Write FONT
        String fontType="Type1", fontFamily="Helvetica", fontEncoding="WinAnsiEncoding";
        fontID=offset.size();
        offset.add(out.getFilePointer());
        write(""+fontID+" 0 obj\n<<\n/Type /Font\n/Subtype /"+fontType
        +"\n/BaseFont /"+fontFamily+"\n/Encoding /"+fontEncoding+"\n>>\nendobj\n\n");
        // Opens the CONTENT object
        contentID=offset.size();
        offset.add(out.getFilePointer());
        contentSizeID=offset.size();
        write(""+contentID+" 0 obj\n<< /Length "+contentSizeID+" 0 R>>\nstream\n");
        startOfContent=out.getFilePointer();
        write("q\n1 J\n1 j\n[] 0 d\n1 w\n1 0 0 -1 0 "+height+" cm\n");
    }

    /** Writes the given String into the current Contents stream object. */
    public void write(String x) {
        if (err!=null) return;
        try { out.write(x.getBytes("UTF-8")); } catch(IOException ex) { err=ex; }
    }

    /**
     * Finishes writing the PDF file, then flushes and closes the file.
     * @throws IOException if an error occurred in writing or closing the file
     */
    public void close() throws IOException {
        if (err!=null) throw err;
        // Closes the CONTENT object
        long len = out.getFilePointer() - startOfContent;
        write("endstream\nendobj\n\n");
        offset.add(out.getFilePointer());
        write(""+contentSizeID+" 0 obj\n"+len+"\nendobj\n\n");
        // Write PAGE and PAGES
        pageID=offset.size();
        offset.add(out.getFilePointer());
        pagesID=offset.size();
        write(""+pageID+" 0 obj\n<<\n/Type /Page\n/Parent "+pagesID+" 0 R\n/Contents "+contentID+" 0 R\n>>\nendobj\n\n");
        offset.add(out.getFilePointer());
        write("" + pagesID + " 0 obj\n<<\n" + "/Type /Pages\n" + "/Count 1\n" + "/Kids ["+pageID+" 0 R]\n"
        + "/MediaBox [0 0 "+width+" "+height+"]\n" + "/Resources\n" + "<<\n"
        + "/Font <<\n/F1 "+fontID+" 0 R >>\n>>\n>>\nendobj\n\n");
        // Write CATALOG
        catalogID=offset.size();
        offset.add(out.getFilePointer());
        write(""+catalogID+" 0 obj\n" + "<<\n" + "/Type /Catalog\n" + "/Pages " + pagesID + " 0 R\n" + ">>\n" + "endobj\n\n");
        // Write XREF
        long xref = out.getFilePointer();
        write("xref\n" + "0 " + offset.size() + "\n");
        for(int i=0; i<offset.size(); i++) {
            long off = offset.get(i);
            String a = ""+off;
            while(a.length()<10) a="0"+a;
            if (i==0) write(a+" 65535 f\r\n"); else write(a+" 00000 n\r\n");
        }
        // Writer TRAILER
        write("trailer\n" + "<<\n" + "/Size " + offset.size() + "\n" + "/Root " + catalogID + " 0 R\n" + ">>\n" + "startxref\n" + xref + "\n" + "%%EOF\n");
        // Close the file
        out.close();
    }

    /*
     * Here is a quick summary of various PDF Graphics operations
     * ==========================================================
     *
     * $x $y m                 --> begins a new path at the given coordinate
     * $x $y l                 --> add the segment (LASTx,LASTy)..($x,$y) to the current path
     * $cx $cy $x $y v         --> add the bezier curve (LASTx,LASTy)..(LASTx,LASTy)..($cx,$cy)..($x,$y) to the current path
     * $cx $cy $x $y y         --> add the bezier curve (LASTx,LASTy)....($cx,$cy).....($x,$y)...($x,$y) to the current path
     * $ax $ay $bx $by $x $y c --> add the bezier curve (LASTx,LASTy)....($ax,$ay)....($bx,$by)..($x,$y) to the current path
     * h                       --> close the current path by adding a straightline segment from the current point to the start of the path
     * $x $y $w $h re          --> append a rectangle to the current path as a complete subpath with lower-left corner at $x $y
     *
     * PATH S    --> draw that path
     * PATH f    --> fill that path
     * PATH B    --> fill then draw the path
     *
     * q                      --> saves the current graphics state
     * 1 J                    --> sets the round cap
     * 1 j                    --> sets the round joint
     * [] 0 d                 --> sets the dash pattern as SOLID
     * [4 6] 0 d              --> sets the dash pattern as 4 UNITS ON then 6 UNITS OFF
     * 5 w                    --> sets the line width as 5 UNITS
     * $a $b $c $d $e $f cm   --> appends the given matrix                [1 0 0 1 dx dy] is translate to dx dy
     * $R $G $B RG            --> sets the stroke color (where 0 <= $R <= 1, etc)
     * $R $G $B rg            --> sets the fill   color (where 0 <= $R <= 1, etc)
     * Q                      --> restores the current graphics state
     */

    /** Temporary test driver used to test this module. */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() { OurPDFWriter.run(); }
        });
    }

    /** Helper method used by the test driver. */
    private static void run() {
        final VizGraph gr = new VizGraph();
        VizNode n0 = new VizNode(gr, "N0").set(VizShape.CIRCLE).set(Color.RED);
        VizNode n1 = new VizNode(gr, "N1").set(VizShape.BOX).set(Color.LIGHT_GRAY).setFontBoldness(true);
        VizNode n2 = new VizNode(gr, "N2").set(VizShape.BOX).set(Color.YELLOW);
        VizNode n3 = new VizNode(gr, "N3").set(VizShape.HOUSE).set(Color.GREEN);
        new VizNode(gr, "N4").set(VizShape.INV_HOUSE).set(Color.GRAY);
        VizNode a=null, b=null;
        int k=0;
        for(int jj=1; jj<=1; jj++) for(VizShape s:VizShape.values()) {
           String n=s.toString();
           b=new VizNode(gr,n).set(s).set(Color.YELLOW).set(VizStyle.SOLID);
           //new VizEdge(b,b); new VizEdge(b,b); new VizEdge(b,b); new VizEdge(b,b);
           //new VizEdge(b,b); new VizEdge(b,b); new VizEdge(b,b); new VizEdge(b,b);
           if (a==null) a=b; else if (k<4) { new VizEdge(a,b); a=b; k++; } else { k=0; a=b; }
        }
        new VizEdge(n0, n1).set(Color.RED);
        new VizEdge(n0, n1).set(Color.GREEN);
        new VizEdge(n0, n2).set(VizStyle.BOLD);
        new VizEdge(n0, n3).set(VizStyle.DOTTED);
        new VizEdge(n2, n1).set(VizStyle.DASHED);
        new VizEdge(n3, n1);
        new VizEdge(n0, n0);
        new VizEdge(n2, n2);

        final JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new BorderLayout());
        final JPanel panel = new VizViewer(gr);
        final JScrollPane scr=new JScrollPane(panel);
        scr.getHorizontalScrollBar().setUnitIncrement(20);
        scr.getVerticalScrollBar().setUnitIncrement(20);
        scr.setBorder(new EmptyBorder(0,0,0,0));
        scr.setFocusable(true);
        scr.requestFocusInWindow();
        frame.add(scr, BorderLayout.CENTER);
        frame.pack();
        frame.setLocation(600,0);
        frame.setSize(615,650);
        frame.setVisible(true);

        try {
        OurPDFWriter x = new OurPDFWriter(300, "/tmp/z.pdf");
        Artist art = new Artist(x);
        double scale1 = ((double)(x.width-x.dpi))  / gr.getTotalWidth();  // We leave 0.5 inch on the left and right
        double scale2 = ((double)(x.height-x.dpi)) / gr.getTotalHeight(); // We leave 0.5 inch on the left and right
        if (scale1<scale2) scale2=scale1;
        x.write("q\n");
        x.write(""+scale2+" 0 0 "+scale2+" "+(x.dpi/2)+" "+(x.dpi/2)+" cm\n");
        gr.draw(art, scale2, null, null);
        x.write("Q\n");
        x.close();
        Runtime.getRuntime().exec("xpdf -geometry 570x700+0+0 -z page /tmp/z.pdf");
        } catch(IOException ex) { }
    }
}
