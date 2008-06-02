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

import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * Graphical convenience methods for producing PDF files.
 *
 * <p> This implementation explicitly generates a very simple 8.5 inch by 11 inch one-page PDF
 * which consists of a single stream of graphical operations.
 * Hopefully this class will no longer be needed in the future,
 * once Java comes with better PDF support.
 *
 * <p><b>Thread Safety:</b> unsafe.
 */

public final strictfp class OurPDFWriter {

    /** This is the file we're writing. */
    private RandomAccessFile out;

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

    /** Nonnull if an IOException has occurred. */
    private IOException err = null;

    /**
     * Write the PDF header into the file, then begins a Contents stream; (if the file already exists, it will be overwritten).
     * @throws IllegalArgumentException if dpi is less than 50 or is greater than 3000
     * @throws IOException if an error occurred in opening or writing to the file
     */
    public OurPDFWriter(int dpi, String filename) throws IOException {
        if (dpi<50 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 50 and 3000");
        width = dpi*8L + (dpi/2L); // "8.5 inch"
        height = dpi*11L;          // "11 inch"
        out = new RandomAccessFile(filename, "rw");
        try {
           // truncate the file if it already exists
           out.setLength(0);
           // Write %PDF-1.3, followed by a non-ASCII comment to force the PDF into binary mode
           out.write(new byte[]{'%', 'P', 'D', 'F', '-', '1', '.', '3', 10, '%', -127, 10, 10});
           // Write out the default font choices
           offset[fontID] = out.getFilePointer();
           write(fontID).write(" 0 obj\n<<\n/Type /Font\n/Subtype /" + fontType + "\n/BaseFont /" + fontFamily + "\n/Encoding /" + fontEncoding + "\n>>\nendobj\n\n");
           // Opens the CONTENT object
           offset[contentID] = out.getFilePointer();
           write(contentID).write(" 0 obj\n<< /Length ").write(contentSizeID).write(" 0 R>>\nstream\n");
           startOfContent = out.getFilePointer();
           // Write the default settings, and add a default transformation that flips (0,0) into the top-left corner of the page
           write("q\n1 J\n1 j\n[] 0 d\n1 w\n1 0 0 -1 0 ").write(height).write(" cm\n");
        } catch(IOException ex) {
           Util.close(out); // open files are a scarce resource, so try to close it at all cost
           throw ex;
        }
    }

    /**
     * Writes the String into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter write(String string) {
        if (err!=null || out==null) return this;
        try { out.write(string.getBytes("UTF-8")); } catch(IOException ex) { err=ex; }
        return this;
    }

    /**
     * Writes a blank space into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter writespace() { return write(" "); }

    /**
     * Writes the number into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter write(long x) { return write(Long.toString(x)); }

    /**
     * Writes the number into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter write(double x) {
        // These extreme values shouldn't happen, but we want to protect against them
        if (Double.isNaN(x)) return write("0");
        if (x==Double.POSITIVE_INFINITY) return write("65535");
        if (x==Double.NEGATIVE_INFINITY) return write("-65536");
        // Now, regular doubles... we only want up to 6 digits after the decimal point
        String sign="", str=Long.toString((long)(x*1000000d));
        if (str.charAt(0)=='-') { str=str.substring(1); sign="-"; }
        while(str.length()<6) str="0"+str;
        return write(sign + str.substring(0, str.length()-6) + "." + str.substring(str.length()-6));
    }

    /**
     * Closes the content stream, write the PDF end-of-file, then flushes and closes the file.
     * @throws IOException if an error occurred in writing or closing the file
     */
    public void close() throws IOException {
        if (err==null && out!=null) try {
           // Closes the CONTENT object
           final long len = out.getFilePointer() - startOfContent;
           write("endstream\n" + "endobj\n\n");
           offset[contentSizeID] = out.getFilePointer();
           write(contentSizeID).write(" 0 obj\n").write(len).write("\n" + "endobj\n\n");
           // Write PAGE and PAGES
           offset[pageID] = out.getFilePointer();
           write(pageID).write(" 0 obj\n<<\n/Type /Page\n/Parent ").write(pagesID);
           write(" 0 R\n/Contents ").write(contentID).write(" 0 R\n>>\n" + "endobj\n\n");
           offset[pagesID] = out.getFilePointer();
           write(pagesID).write(" 0 obj\n<<\n/Type /Pages\n/Count 1\n/Kids [").write(pageID).write(" 0 R]\n");
           write("/MediaBox [0 0 ").write(width).writespace().write(height).write("]\n/Resources\n<<\n/Font <<\n/F1 ");
           write(fontID).write(" 0 R >>\n>>\n>>\n" + "endobj\n\n");
           // Write CATALOG
           offset[catalogID] = out.getFilePointer();
           write(catalogID).write(" 0 obj\n<<\n/Type /Catalog\n/Pages ").write(pagesID).write(" 0 R\n>>\n" + "endobj\n\n");
           // Write XREF
           final long xref = out.getFilePointer();
           write("xref\n0 ").write(offset.length).write("\n");
           for(int i=0; i<offset.length; i++) {
              long off = offset[i];
              String a = "" + off;
              while(a.length()<10) a = "0" + a; // must be exactly 10 characters long
              if (i==0) write(a).write(" 65535 f\r\n"); else write(a).write(" 00000 n\r\n");
           }
           // Write TRAILER
           write("trailer\n<<\n/Size ").write(offset.length).write("\n/Root ").write(catalogID).write(" 0 R\n>>\n" + "startxref\n").write(xref).write("\n%%EOF\n");
        } catch(IOException ex) {
           err=ex;
        }
        // Close the file at all cost, since open files are a scarce system resource
        try { if (out!=null) out.close(); } catch(IOException ex) { if (err==null) err=ex; }
        out = null;
        // If any errors occurred during writing or flushing or closing the file, then re-throw the exception
        if (err!=null) throw err;
    }
}
