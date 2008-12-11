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
import java.io.IOException;
import java.io.RandomAccessFile;

/** Graphical convenience methods for producing PDF files.
 *
 * <p> This implementation explicitly generates a very simple 8.5 inch by 11 inch one-page PDF consisting of graphical operations.
 * Hopefully this class will no longer be needed in the future once Java comes with better PDF support.
 */

public final strictfp class OurPDFWriter {

   /** The page width (in terms of dots). */
   private final long width;

   /** The page height (in terms of dots). */
   private final long height;

   /** Latest color (-1 if none has been explicitly set so far) */
   private int color = -1;

   /** Latest line style (0=normal, 1=bold, 2=dotted, 3=dashed) */
   private int line = 0;

   /** The buffer that will store the list of graphical operations issued so far. */
   private final ByteBuffer buf = new ByteBuffer();

   /** Begin a blank PDF file with the given dots-per-inch and the given scale.
    * @throws IllegalArgumentException if dpi is less than 50 or is greater than 3000
    */
   public OurPDFWriter(int dpi, double scale) {
      if (dpi<50 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 50 and 3000");
      width = dpi*8 + (dpi/2); // "8.5 inches"
      height = dpi*11;         // "11 inches"
      // Write the default settings, and add a default transformation that flips (0, 0) into the top-left corner of the page, then scale the page
      buf.write("q\n" + "1 J\n" + "1 j\n" + "[] 0 d\n" + "1 w\n" + "1 0 0 -1 0 ").write(height).write("cm\n");
      buf.write(scale).write("0 0 ").write(scale).write(dpi/2.0).write(dpi/2.0).write("cm\n");
   }

   /** Changes the color for subsequent graphical drawing. */
   public OurPDFWriter setColor(Color color) {
      int rgb = color.getRGB() & 0xFFFFFF, r = (rgb>>16)&0xFF, g = (rgb>>8)&0xFF, b = (rgb&0xFF);
      if (this.color == rgb) return this; // no need to change
      buf.write(r/255.0).write(g/255.0).write(b/255.0).write("RG\n");
      buf.write(r/255.0).write(g/255.0).write(b/255.0).write("rg\n");
      this.color = rgb;
      return this;
   }

   /** Changes the line style to be normal. */
   public OurPDFWriter setNormalLine()  { if (line!=0) buf.write("1 w [] 0 d\n"); line=0; return this; }

   /** Changes the line style to be bold. */
   public OurPDFWriter setBoldLine()  { if (line!=1) buf.write("2 w [] 0 d\n"); line=1; return this; }

   /** Changes the line style to be dotted. */
   public OurPDFWriter setDottedLine()  { if (line!=2) buf.write("1 w [1 3] 0 d\n"); line=2; return this; }

   /** Changes the line style to be dashed. */
   public OurPDFWriter setDashedLine()  { if (line!=3) buf.write("1 w [6 3] 0 d\n"); line=3; return this; }

   /** Shifts the coordinate space by the given amount. */
   public OurPDFWriter shiftCoordinateSpace(int x, int y)  { buf.write("1 0 0 1 ").write(x).write(y).write("cm\n"); return this; }

   /** Draws a circle of the given radius, centered at (0, 0). */
   public OurPDFWriter drawCircle(int radius, boolean fillOrNot) {
      double k = (0.55238 * radius); // Approximate a circle using 4 cubic bezier curves
      buf.write( radius).write("0 m ");
      buf.write( radius).write(      k).write(      k).write( radius).write("0 ")   .write( radius).write("c ");
      buf.write(     -k).write( radius).write(-radius).write(      k).write(-radius).write("0 c ");
      buf.write(-radius).write(     -k).write(     -k).write(-radius).write("0 ")   .write(-radius).write("c ");
      buf.write(      k).write(-radius).write( radius).write(     -k).write(radius) .write(fillOrNot ? "0 c f\n" : "0 c S\n");
      return this;
   }

   /** Draws a line from (x1, y1) to (x2, y2). */
   public OurPDFWriter drawLine(int x1, int y1, int x2, int y2) {
      buf.write(x1).write(y1).write("m ").write(x2).write(y2).write("l S\n");
      return this;
   }

   /** Draws a shape. */
   public OurPDFWriter drawShape(Shape shape, boolean fillOrNot) {
      if (shape instanceof Line2D.Double) {
         Line2D.Double obj = (Line2D.Double)shape;
         buf.write(obj.x1).write(obj.y1).write("m ").write(obj.x2).write(obj.y2).write("l ");
      } else if (shape instanceof QuadCurve2D.Double) { // Convert quadratic bezier into cubic bezier using de Casteljau algorithm
         QuadCurve2D.Double obj = (QuadCurve2D.Double)shape;
         double px = obj.x1 + (obj.ctrlx - obj.x1)*(2.0/3.0), qx = px + (obj.x2 - obj.x1)/3.0;
         double py = obj.y1 + (obj.ctrly - obj.y1)*(2.0/3.0), qy = py + (obj.y2 - obj.y1)/3.0;
         buf.write(obj.x1).write(obj.y1).write("m ").write(px).write(py).write(qx).write(qy).write(obj.x2).write(obj.y2).write("c ");
      } else if (shape instanceof Polygon) {
         Polygon obj = (Polygon)shape;
         for(int i=0; i<obj.npoints; i++) buf.write(obj.xpoints[i]).write(obj.ypoints[i]).write(i==0 ? "m\n" : "l\n");
         buf.write("h ");
      } else {
         double[] pt = new double[6];
         double moveX = 0, moveY = 0, nowX = 0, nowY = 0;
         for(PathIterator it = shape.getPathIterator(null); !it.isDone(); it.next()) switch(it.currentSegment(pt)) {
            case PathIterator.SEG_MOVETO:
               nowX = moveX = pt[0]; nowY = moveY = pt[1];
               buf.write(nowX).write(nowY).write("m\n");
               break;
            case PathIterator.SEG_CLOSE:
               nowX = moveX; nowY = moveY;
               buf.write("h\n");
               break;
            case PathIterator.SEG_LINETO:
               nowX = pt[0]; nowY = pt[1];
               buf.write(nowX).write(nowY).write("l\n");
               break;
            case PathIterator.SEG_QUADTO: // Convert quadratic bezier into cubic bezier using de Casteljau algorithm
               double px = nowX + (pt[0] - nowX)*(2.0/3.0), qx = px + (pt[2] - nowX)/3.0;
               double py = nowY + (pt[1] - nowY)*(2.0/3.0), qy = py + (pt[3] - nowY)/3.0;
               nowX = pt[2]; nowY = pt[3];
               buf.write(px).write(py).write(qx).write(qy).write(nowX).write(nowY).write("c\n");
               break;
            case PathIterator.SEG_CUBICTO:
               nowX = pt[4]; nowY = pt[5];
               buf.write(pt[0]).write(pt[1]).write(pt[2]).write(pt[3]).write(nowX).write(nowY).write("c\n");
               break;
         }
      }
      buf.write(fillOrNot ? "f\n" : "S\n");
      return this;
   }

   /*  PDF File Structure Summary:
    *  ===========================
    *
    *  START_OF_FILE ideally should consist of the following 13 bytes
    *  ==============================================================
    *
    *  "%PDF-1.3" 10 "%" -127 10 10
    *
    *  Now comes one or more objects.
    *  One simple single-page arrangement is to have exactly 5 objects in this order: FONT, CONTENT, PAGE, PAGES, and CATALOG.
    *
    *  Font Object
    *  ===========
    *
    *  1 0 obj\n         // "1" is because this is the first object
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
    *  2 0 obj\n                                    // "2" is because this is the second object
    *  << /Length 12345 /Filter /FlateDecode >>\n   // Suppose the $streamContent occupies 12345 bytes exactly, and is compressed by FLATE
    *  stream\r\n
    *  $streamContent                               // $streamContent is a stream of zero or more graphical operations
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
    *  Page Object
    *  ===========
    *
    *  3 0 obj\n                    // "3" is because this is the third object
    *  <<\n
    *  /Type /Page\n
    *  /Parent 4 0 R\n              // "4" is because the Pages object is object5
    *  /Contents 2 0 R\n            // "2" is because the Content object is object2
    *  >>\n
    *  endobj\n\n
    *
    *  Pages Object
    *  ============
    *
    *  4 0 obj\n                    // "4" is because this is the fourth object
    *  <<\n
    *  /Type /Pages\n
    *  /Count 1\n                                 // "1" is because we have assumed only a single page
    *  /Kids [3 0 R]\n                            // "3" is because the only page we have is object3
    *  /MediaBox [0 0 $w $h]\n                    // $w is 8.5*DPI and $h is 11*DPI (since we are assuming 8.5in x 11in page)
    *  /Resources << /Font << /F1 1 0 R >> >>\n   // "1" is because our only font is object #1
    *  >>\n
    *  endobj\n\n
    *
    *  Catalog Object
    *  ==============
    *
    *  5 0 obj\n                    // "5" is because this is the fifth object
    *  <<\n
    *  /Type /Catalog\n
    *  /Pages 4 0 R\n               // "4" is because the PAGES object is object4
    *  >>\n
    *  endobj\n\n
    *
    *  END_OF_FILE format (assuming we have obj1 obj2 obj3 obj4 obj5 where obj5 is the "PDF Catalog")
    *  ==============================================================================================
    *
    *  xref\n
    *  0 6\n                   // 6 is because it's the number of objects plus 1
    *  0000000000 65535 f\r\n
    *  $offset1 00000 n\r\n    // $offset1 is the byte offset of the start of obj1, left-padded-with-zero until you get exactly 10 digits
    *  $offset2 00000 n\r\n    // $offset2 is the byte offset of the start of obj2, left-padded-with-zero until you get exactly 10 digits
    *  $offset3 00000 n\r\n    // $offset3 is the byte offset of the start of obj3, left-padded-with-zero until you get exactly 10 digits
    *  $offset4 00000 n\r\n    // $offset4 is the byte offset of the start of obj4, left-padded-with-zero until you get exactly 10 digits
    *  $offset5 00000 n\r\n    // $offset5 is the byte offset of the start of obj5, left-padded-with-zero until you get exactly 10 digits
    *  trailer\n
    *  <<\n
    *  /Size 6\n               // 6 is because it's the number of objects plus 1
    *  /Root 5 0 R\n           // 5 is because it's the Catalog Object's object ID
    *  >>\n
    *  startxref\n
    *  $xref\n                 // $xref is the byte offset of the start of this entire "xref" paragraph
    *  %%EOF\n
    */

   /** Helper method that writes the given String to the output file, then return the number of bytes written. */
   private static int out(RandomAccessFile file, String string) throws IOException {
      byte[] array = string.getBytes("UTF-8");
      file.write(array);
      return array.length;
   }

   /** Close this PDF object and write it to the given output file (which is overwritten if it already exists) */
   public void close(String filename) throws IOException {
      RandomAccessFile out = null;
      try {
         String space = "                    "; // reserve 20 bytes for the file size, which is far far more than enough
         final int fontID = 1, contentID = 2, pageID = 3, pagesID = 4, catalogID = 5;
         final String fontType = "Type1", fontFamily = "Helvetica", fontEncoding = "WinAnsiEncoding";
         final long[] offset = new long[6];
         // Write %PDF-1.3, followed by a non-ASCII comment to force the PDF into binary mode
         out = new RandomAccessFile(filename, "rw");
         out.setLength(0);
         byte[] head = new byte[]{'%', 'P', 'D', 'F', '-', '1', '.', '3', 10, '%', -127, 10, 10};
         out.write(head);
         long now = head.length;
         // Font
         offset[1] = now;
         now += out(out, fontID + " 0 obj\n<<\n/Type /Font\n/Subtype /" + fontType + "\n/BaseFont /" + fontFamily + "\n/Encoding /" + fontEncoding + "\n>>\n" + "endobj\n\n");
         // Content
         offset[2] = now;
         now += out(out, contentID + " 0 obj\n<< /Length " + space + " /Filter /FlateDecode >>\n" + "stream\r\n");
         long ct = buf.dumpFlate(out);
         now += ct + out(out, "endstream\n" + "endobj\n\n");
         // Page
         offset[3] = now;
         now += out(out, pageID + " 0 obj\n<<\n/Type /Page\n/Parent " + pagesID + " 0 R\n/Contents " + contentID + " 0 R\n>>\n" + "endobj\n\n");
         // Pages
         offset[4] = now;
         now += out(out, pagesID + " 0 obj\n<<\n/Type /Pages\n/Count 1\n/Kids [" + pageID + " 0 R]\n" + "/MediaBox [0 0 " + width + " " + height + "]\n/Resources << /Font << /F1 " + fontID + " 0 R >> >>\n>>\n" + "endobj\n\n");
         // Catalog
         offset[5] = now;
         now += out(out, catalogID + " 0 obj\n<<\n/Type /Catalog\n/Pages " + pagesID + " 0 R\n>>\n" + "endobj\n\n");
         // Xref
         String xr = "xref\n0 " + offset.length + "\n";
         for(int i = 0; i < offset.length; i++) {
            String txt = Long.toString(offset[i]);
            while(txt.length() < 10) txt = "0" + txt; // must be exactly 10 characters long
            if (i==0) xr = xr + txt + " 65535 f\r\n"; else xr = xr + txt + " 00000 n\r\n";
         }
         // Trailer
         xr = xr + "trailer\n<<\n/Size " + offset.length + "\n/Root " + catalogID + " 0 R\n>>\n" + "startxref\n" + now + "\n%%EOF\n";
         out(out, xr);
         out.seek(offset[2]);
         out(out, contentID + " 0 obj\n<< /Length " + ct); // move the file pointer back so we can write out the real Content Size
         out.close();
         out = null;
      } finally {
         Util.close(out); // try to close the file at all cost
      }
   }
}
