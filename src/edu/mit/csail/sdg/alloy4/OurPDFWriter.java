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

package edu.mit.csail.sdg.alloy4;

import java.awt.Color;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.Line2D;
import java.awt.geom.PathIterator;
import java.awt.geom.QuadCurve2D;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Graphical convenience methods for producing PDF files.
 *
 * <p> This implementation explicitly generates a very simple 8.5 inch by 11 inch one-page PDF
 * which consists of a single stream of graphical operations.
 * Hopefully this class will no longer be needed in the future,
 * once Java comes with better PDF support.
 */

public final strictfp class OurPDFWriter {

    /** Nonnull if a file is currently open for us to write into. */
    private OutputStream out;

    /** Nonnull if an IOException has occurred. */
    private IOException err = null;

    /** This maps each PDF Object ID to its exact offset in the file. */
    private final long[] offset = new long[7];

    /** This is the ID of the "Font" PDF Object. */
    private static final int fontID = 1;

    /** This is the ID of the "Content" PDF Object. */
    private static final int contentID = 2;

    /** This is the ID of the "ContentSize" PDF Object. */
    private static final int contentSizeID = 3;

    /** This is the ID of the "Page" PDF Object. */
    private static final int pageID = 4;

    /** This is the ID of the "Pages" PDF Object. */
    private static final int pagesID = 5;

    /** This is the ID of the "Catalog" PDF Object. */
    private static final int catalogID = 6;

    /** The default font type. */
    private static final String fontType = "Type1";

    /** The default font family. */
    private static final String fontFamily = "Helvetica";

    /** The default font encoding. */
    private static final String fontEncoding = "WinAnsiEncoding";

    /** This stores the exact offset of the start of the content stream. */
    private final long startOfContent;

    /** The width (in terms of dots). */
    private final long width;

    /** The height (in terms of dots). */
    private final long height;

    /** The number of bytes written into the file thus far. */
    private long now = 0;

    /**
     * Write the PDF header into the file, then begins a Contents stream; (if the file already exists, it will be overwritten).
     * @throws IllegalArgumentException if dpi is less than 50 or is greater than 3000
     * @throws IOException if an error occurred in opening or writing to the file
     */
    public OurPDFWriter(String filename, int dpi, double scale) throws IOException {
        if (dpi<50 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 50 and 3000");
        width = dpi*8L + (dpi/2L); // "8.5 inch"
        height = dpi*11L;          // "11 inch"
        out = new BufferedOutputStream(new FileOutputStream(filename));
        try {
           // Write %PDF-1.3, followed by a non-ASCII comment to force the PDF into binary mode
           out.write(new byte[]{'%', 'P', 'D', 'F', '-', '1', '.', '3', 10, '%', -127, 10, 10});
           now = 13;
           // Write out the default font choices
           offset[fontID] = now;
           write(fontID + " 0 obj\n<<\n/Type /Font\n/Subtype /" + fontType + "\n/BaseFont /" + fontFamily + "\n/Encoding /" + fontEncoding + "\n>>\n" + "endobj\n\n");
           // Opens the CONTENT object
           offset[contentID] = now;
           write(contentID + " 0 obj\n<< /Length " + contentSizeID + " 0 R>>\n" + "stream\n");
           startOfContent = now;
           // Write the default settings, and add a default transformation that flips (0,0) into the top-left corner of the page, then scale the page
           write("q\n" + "1 J\n" + "1 j\n" + "[] 0 d\n" + "1 w\n" + "1 0 0 -1 0 ").writes(height).write("cm\n");
           writes(scale).write("0 0 ").writes(scale).writes(dpi/2d).writes(dpi/2d).write("cm\n");
        } catch(IOException ex) {
           Util.close(out); // open files are a scarce resource, so try to close it at all cost
           throw ex;
        }
    }

    /**
     * Writes the String into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    private OurPDFWriter write(String string) {
        if (err==null && out!=null) {
            try { byte[] data=string.getBytes("UTF-8"); now=now+data.length; out.write(data); } catch(IOException ex) { err=ex; }
        }
        return this;
    }

    /**
     * Writes the number into the current Contents stream object, write a space character, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    private OurPDFWriter writes(long x)  { return write(Long.toString(x)).write(" "); }

    /**
     * Writes the number into the current Contents stream object, write a space character, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    private OurPDFWriter writes(double x) {
        // These extreme values shouldn't happen, but we want to protect against them
        if (Double.isNaN(x)) return write("0 ");
        if (x==Double.POSITIVE_INFINITY) return write("32767 ");  // this is the maximum requirement stated in PDF Spec 1.3
        if (x==Double.NEGATIVE_INFINITY) return write("-32767 "); // this is the minimum requirement stated in PDF Spec 1.3
        // Now, regular doubles... we only want up to 6 digits after the decimal point
        long num = (long)(x * 1000000.0);
        if (num>32767000000L) return write("32767 "); else if (num<(-32767000000L)) return write("-32767 ");
        String sign="", str=Long.toString(num);
        if (str.charAt(0)=='-') { str=str.substring(1); sign="-"; }
        while(str.length()<6) str="0"+str;
        return write(sign).write(str.substring(0, str.length()-6)).write(".").write(str.substring(str.length()-6)).write(" ");
    }

    /**
     * Changes the color for subsequent graphical drawing.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter setColor(Color color) {
        int rgb = color.getRGB(), r = (rgb>>16)&0xFF, g = (rgb>>8)&0xFF, b = (rgb&0xFF);
        writes(r/255d).writes(g/255d).writes(b/255d).write("RG\n");
        writes(r/255d).writes(g/255d).writes(b/255d).write("rg\n");
        return this;
    }

    /**
     * Changes the line style to be bold.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter setBoldLine()  { return write("2 w [] 0 d\n"); }

    /**
     * Changes the line style to be dotted.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter setDottedLine()  { return write("1 w [1 3] 0 d\n"); }

    /**
     * Changes the line style to be dashed.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter setDashedLine()  { return write("1 w [6 3] 0 d\n"); }

    /**
     * Changes the line style to be normal.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter setNormalLine()  { return write("1 w [] 0 d\n"); }

    /**
     * Shifts the coordinate space by the given amount.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter shiftCoordinateSpace(int x, int y)  { return write("1 0 0 1 ").writes(x).writes(y).write("cm\n"); }

    /**
     * Draws a circle of the given radius, centered at (0,0).
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter drawCircle(int radius, boolean fillOrNot) {
        double k = (0.55238d*radius); // Approximate a circle using 4 cubic bezier curves
        writes( radius).write("0 m ");
        writes( radius).writes(      k).writes(      k).writes( radius).write("0 ")    .writes( radius).write("c ");
        writes(     -k).writes( radius).writes(-radius).writes(      k).writes(-radius).write("0 c ");
        writes(-radius).writes(     -k).writes(     -k).writes(-radius).write("0 ")    .writes(-radius).write("c ");
        writes(      k).writes(-radius).writes( radius).writes(     -k).writes(radius) .write(fillOrNot ? "0 c f\n" : "0 c S\n");
        return this;
    }

    /**
     * Draws a line from (x1,y1) to (x2,y2).
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter drawLine(int x1, int y1, int x2, int y2)  { return writes(x1).writes(y1).write("m ").writes(x2).writes(y2).write("l S\n"); }

    /**
     * Draws a shape.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter drawShape(Shape shape, boolean fillOrNot) {
        if (shape instanceof Line2D.Double) {
            Line2D.Double obj = (Line2D.Double)shape;
            writes(obj.x1).writes(obj.y1).write("m ").writes(obj.x2).writes(obj.y2).write(fillOrNot ? "l f\n" : "l S\n");
        } else if (shape instanceof QuadCurve2D.Double) {
            // Convert the quadratic bezier curve into a cubic bezier curve
            QuadCurve2D.Double obj = (QuadCurve2D.Double)shape;
            double px = obj.x1 + (obj.ctrlx - obj.x1)*2d/3d, qx = px + (obj.x2 - obj.x1)/3d;
            double py = obj.y1 + (obj.ctrly - obj.y1)*2d/3d, qy = py + (obj.y2 - obj.y1)/3d;
            writes(obj.x1).writes(obj.y1).write("m ").writes(px).writes(py).writes(qx).writes(qy).writes(obj.x2).writes(obj.y2).write(fillOrNot ? "c f\n" : "c S\n");
        } else if (shape instanceof Polygon) {
            Polygon obj = (Polygon)shape;
            for(int i=0; i<obj.npoints; i++) writes(obj.xpoints[i]).writes(obj.ypoints[i]).write(i==0 ? "m\n" : "l\n");
            write(fillOrNot ? "h f\n" : "h S\n");
        } else {
            double[] pt = new double[6];
            double moveX=0, moveY=0, nowX=0, nowY=0;
            for(PathIterator it=shape.getPathIterator(null); !it.isDone(); it.next()) {
               switch(it.currentSegment(pt)) {
                 case PathIterator.SEG_MOVETO:
                    nowX=moveX=pt[0]; nowY=moveY=pt[1];
                    writes(nowX).writes(nowY).write("m\n");
                    break;
                 case PathIterator.SEG_CLOSE:
                    nowX=moveX; nowY=moveY; write("h\n");
                    break;
                 case PathIterator.SEG_LINETO:
                    nowX=pt[0]; nowY=pt[1];
                    writes(nowX).writes(nowY).write("l\n");
                    break;
                 case PathIterator.SEG_QUADTO:
                    double px = nowX + (pt[0] - nowX)*2d/3d, qx = px + (pt[2] - nowX)/3d;
                    double py = nowY + (pt[1] - nowY)*2d/3d, qy = py + (pt[3] - nowY)/3d;
                    nowX=pt[2]; nowY=pt[3];
                    writes(px).writes(py).writes(qx).writes(qy).writes(nowX).writes(nowY).write("c\n");
                    break;
                 case PathIterator.SEG_CUBICTO:
                    nowX=pt[4]; nowY=pt[5];
                    writes(pt[0]).writes(pt[1]).writes(pt[2]).writes(pt[3]).writes(nowX).writes(nowY).write("c\n");
                    break;
               }
            }
            write(fillOrNot ? "f\n" : "S\n");
        }
        return this;
    }

    /*
     *  START_OF_FILE ideally should consist of the following 13 characters
     *  ===================================================================
     *
     *  "%PDF-1.3" 10 "%" -127 10 10
     *
     *  Now comes one or more objects.
     *  One simple single-page arrangement is to have exactly 6 objects in this order: FONT, CONTENT, CONTENTSIZE, PAGE, PAGES, and CATALOG.
     *
     *  Font Object
     *  ===========
     *
     *  1 0 obj\n                    // "1" is because this is the first object
     *  <<\n
     *  /Type /Font\n
     *  /Subtype /Type1\n
     *  /BaseFont /Helvetica\n
     *  /Encoding /WinAnsiEncoding\n
     *  >>\n
     *  endobj\n\n
     *
     *  Content Object
     *  ==============
     *
     *  2 0 obj\n                    // "2" is because this is the second object
     *  << /Length 3 0 R>>\n         // "3" is because we will store the streamLength into object3
     *  stream\n
     *  $streamContent               // $streamContent is a stream of zero or more graphical operators such as drawLine or drawCircle
     *  endstream\n
     *  endobj\n\n
     *
     *  Here is a quick summary of various PDF Graphics operations
     *  ==========================================================
     *
     *  $x $y m                 --> begins a new path at the given coordinate
     *  $x $y l                 --> add the segment (LASTx,LASTy)..($x,$y) to the current path
     *  $cx $cy $x $y v         --> add the bezier curve (LASTx,LASTy)..(LASTx,LASTy)..($cx,$cy)..($x,$y) to the current path
     *  $cx $cy $x $y y         --> add the bezier curve (LASTx,LASTy)....($cx,$cy).....($x,$y)...($x,$y) to the current path
     *  $ax $ay $bx $by $x $y c --> add the bezier curve (LASTx,LASTy)....($ax,$ay)....($bx,$by)..($x,$y) to the current path
     *  h                       --> close the current subpath by straightline segment from current point to the start of this subpath
     *  $x $y $w $h re          --> append a rectangle to the current path as a complete subpath with lower-left corner at $x $y
     *
     *  S                       --> assuming we've just described a path, draw the path
     *  f                       --> assuming we've just described a path, fill the path
     *  B                       --> assuming we've just described a path, fill then draw the path
     *
     *  q                       --> saves the current graphics state
     *  1 J                     --> sets the round cap
     *  1 j                     --> sets the round joint
     *  [] 0 d                  --> sets the dash pattern as SOLID
     *  [4 6] 0 d               --> sets the dash pattern as 4 UNITS ON then 6 UNITS OFF
     *  5 w                     --> sets the line width as 5 UNITS
     *  $a $b $c $d $e $f cm    --> appends the given matrix; for example, [1 0 0 1 dx dy] means "translation to dx dy"
     *  $R $G $B RG             --> sets the stroke color (where 0 <= $R <= 1, etc)
     *  $R $G $B rg             --> sets the fill   color (where 0 <= $R <= 1, etc)
     *  Q                       --> restores the current graphics state
     *
     *  ContentSize Object
     *  ==================
     *
     *  3 0 obj\n                    // "3" is because this is the third object
     *  $len\n                       // $len is the number of bytes inside "$streamContent" above
     *  endobj\n\n
     *
     *  Page Object
     *  ===========
     *
     *  4 0 obj\n                    // "4" is because this is the fourth object
     *  <<\n
     *  /Type /Page\n
     *  /Parent 5 0 R\n              // "5" is because the Pages object is object5
     *  /Contents 2 0 R\n            // "2" is because the Content object is object2
     *  >>\n
     *  endobj\n\n
     *
     *  Pages Object
     *  ============
     *
     *  5 0 obj\n                    // "5" is because this is the fifth object
     *  <<\n
     *  /Type /Pages\n
     *  /Count 1\n                                 // "1" is because we have assumed only a single page
     *  /Kids [4 0 R]\n                            // "4" is because the only page we have is object4
     *  /MediaBox [0 0 $w $h]\n                    // $w is 8.5*DPI and $h is 11*DPI (since we are assuming 8.5in x 11in page)
     *  /Resources << /Font << /F1 1 0 R >> >>\n   // "1" is because our only font is object1
     *  >>\n
     *  endobj\n\n
     *
     *  Catalog Object
     *  ==============
     *
     *  6 0 obj\n                    // "6" is because this is the sixth object
     *  <<\n
     *  /Type /Catalog\n
     *  /Pages 5 0 R\n               // "5" is because the PAGES object is object5
     *  >>\n
     *  endobj\n\n
     *
     *  END_OF_FILE format (assuming we have obj1 obj2 obj3 obj4 obj5 obj6 where obj6 is the "PDF Catalog")
     *  ===================================================================================================
     *
     *  xref\n
     *  0 7\n                   // 7 is because it's the number of objects plus 1
     *  0000000000 65535 f\r\n
     *  $offset1 00000 n\r\n    // $offset1 is the byte offset of the start of obj1, left-padded-with-zero until you get exactly 10 digits
     *  $offset2 00000 n\r\n    // $offset2 is the byte offset of the start of obj2, left-padded-with-zero until you get exactly 10 digits
     *  $offset3 00000 n\r\n    // $offset3 is the byte offset of the start of obj3, left-padded-with-zero until you get exactly 10 digits
     *  $offset4 00000 n\r\n    // $offset4 is the byte offset of the start of obj4, left-padded-with-zero until you get exactly 10 digits
     *  $offset5 00000 n\r\n    // $offset5 is the byte offset of the start of obj5, left-padded-with-zero until you get exactly 10 digits
     *  $offset6 00000 n\r\n    // $offset6 is the byte offset of the start of obj6, left-padded-with-zero until you get exactly 10 digits
     *  trailer\n
     *  <<\n
     *  /Size 7\n               // 7 is because it's the number of objects plus 1
     *  /Root 6 0 R\n           // 6 is because it's the Catalog Object's object ID
     *  >>\n
     *  startxref\n
     *  $xref\n                 // $xref is the byte offset of the start of this entire "xref" paragraph
     *  %%EOF\n
     */

    /**
     * Closes the content stream, write the PDF end-of-file, then flushes and closes the file.
     * @throws IOException if an error occurred in writing or closing the file
     */
    public void close() throws IOException {
        if (err==null && out!=null) try {
           // Closes the CONTENT object
           final long len = now - startOfContent;
           write("endstream\n" + "endobj\n\n");
           // Write CONTENTSIZE object
           offset[contentSizeID] = now;
           write(contentSizeID + " 0 obj\n").write(len + "\n" + "endobj\n\n");
           // Write PAGE object
           offset[pageID] = now;
           write(pageID + " 0 obj\n<<\n/Type /Page\n/Parent " + pagesID + " 0 R\n/Contents " + contentID + " 0 R\n>>\n" + "endobj\n\n");
           // Write PAGES object
           offset[pagesID] = now;
           write(pagesID + " 0 obj\n<<\n/Type /Pages\n/Count 1\n/Kids [" + pageID + " 0 R]\n");
           write("/MediaBox [0 0 ").writes(width).write(height + "]\n/Resources << /Font << /F1 " + fontID + " 0 R >> >>\n>>\n" + "endobj\n\n");
           // Write CATALOG object
           offset[catalogID] = now;
           write(catalogID + " 0 obj\n<<\n/Type /Catalog\n/Pages " + pagesID + " 0 R\n>>\n" + "endobj\n\n");
           // Write XREF
           final long xref = now;
           write("xref\n0 " + offset.length + "\n");
           for(int i=0; i<offset.length; i++) {
              String a = Long.toString(offset[i]);
              while(a.length()<10) a = "0" + a; // must be exactly 10 characters long
              if (i==0) write(a).write(" 65535 f\r\n"); else write(a).write(" 00000 n\r\n");
           }
           // Write TRAILER
           write("trailer\n<<\n/Size " + offset.length + "\n/Root " + catalogID + " 0 R\n>>\n" + "startxref\n").write(xref + "\n%%EOF\n");
           out.flush();
           out.close();
           out = null;
        } catch(IOException ex) {
           err = ex;
        }
        // Close the file, since open files are a scarce system resource
        Util.close(out);
        out = null;
        if (err!=null) throw err; // If any errors occurred during writing or closing the file, then throw the exception
    }
}
