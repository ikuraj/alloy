package edu.mit.csail.sdg.alloy4;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;
import java.util.ArrayList;

/**
 * Graphical convenience methods for producing PDF files.
 *
 * <p> This implementation explicitly generates a very simple 1-page PDF
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
    private List<Long> offset = new ArrayList<Long>();

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

    /** The width (in terms of dots). */
    private long width;

    /** The height (in terms of dots). */
    private long height;

    /** Nonnull if an IOException has occurred. */
    private IOException err = null;

    /**
     * Write the PDF header into the file, then begins a Contents stream; (if the file already exists, it will be overwritten).
     * @throws IllegalArgumentException if dpi is less than 50 or is greater than 3000
     * @throws IOException if an error occurred in opening or writing to the file
     */
    public OurPDFWriter(int dpi, String filename) throws IOException {
        // Initialize various data structures
        if (dpi<50 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 50 and 3000");
        width = dpi*8 + (dpi/2);
        height = dpi*11;
        offset.clear();
        offset.add(0L); // this reserves a spot in the array for ID 0 (which is NOT used in a PDF file)
        out = new RandomAccessFile(filename, "rw");
        try {
          // truncate the file if it already exists
          out.setLength(0);
          // Write %PDF-1.3, followed by a non-ASCII comment to force the PDF into binary mode
          out.write(new byte[]{'%', 'P', 'D', 'F', '-', '1', '.', '3', 10, '%', -127, 10, 10});
          // Choose Helvetica as the default font
          String fontType="Type1", fontFamily="Helvetica", fontEncoding="WinAnsiEncoding";
          fontID=offset.size();
          offset.add(out.getFilePointer());
          write(fontID).write(" 0 obj\n<<\n/Type /Font\n/Subtype /").write(fontType);
          write("\n/BaseFont /").write(fontFamily).write("\n/Encoding /").write(fontEncoding).write("\n>>\nendobj\n\n");
          // Opens the CONTENT object
          contentID=offset.size();
          offset.add(out.getFilePointer());
          contentSizeID=offset.size();
          write(contentID).write(" 0 obj\n<< /Length ").write(contentSizeID).write(" 0 R>>\nstream\n");
          startOfContent=out.getFilePointer();
          // Write the default settings, and add a default transformation that flips (0,0) into the top-left corner of the page
          write("q\n1 J\n1 j\n[] 0 d\n1 w\n1 0 0 -1 0 ").write(height).write(" cm\n");
        } catch(IOException ex) {
          try { out.close(); } catch(IOException ex2) { } // open files are a scarce resource, so try to close it at all cost
          throw ex;
        }
    }

    /**
     * Writes the String into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter write(String theString) {
        if (err==null && out!=null) try {
            out.write(theString.getBytes("UTF-8"));
        } catch(IOException ex) {
            err=ex;
        }
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
    public OurPDFWriter write(long x) { return write(""+x); }

    /**
     * Writes the number into the current Contents stream object, then return the OurPDFWriter object itself as the return value.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public OurPDFWriter write(double x) {
        // These extreme values shouldn't happen, but we want to protect against them
        if (Double.isNaN(x)) return write("0 ");
        if (x==Double.POSITIVE_INFINITY) return write("65535");
        if (x==Double.NEGATIVE_INFINITY) return write("-65535");
        // Now, regular doubles... we only want up to 6 digits after the decimal point
        String str = ""+((long)(x*1000000d));
        String sign = "";
        if (str.charAt(0)=='-') { str=str.substring(1); sign="-"; }
        while(str.length()<6) str="0"+str;
        str = sign + str.substring(0, str.length()-6) + "." + str.substring(str.length()-6);
        return write(str);
    }

    /**
     * Closes the content stream, write the PDF end-of-file, then flushes and closes the file.
     * @throws IOException if an error occurred in writing or closing the file
     */
    public void close() throws IOException {
        if (err==null && out!=null) try {
           // Closes the CONTENT object
           long len = out.getFilePointer() - startOfContent;
           write("endstream\n" + "endobj\n\n");
           offset.add(out.getFilePointer());
           write(contentSizeID).write(" 0 obj\n").write(len).write("\n" + "endobj\n\n");
           // Write PAGE and PAGES
           pageID = offset.size();
           offset.add(out.getFilePointer());
           pagesID = offset.size();
           write(pageID).write(" 0 obj\n<<\n/Type /Page\n/Parent ").write(pagesID);
           write(" 0 R\n/Contents ").write(contentID).write(" 0 R\n>>\n" + "endobj\n\n");
           offset.add(out.getFilePointer());
           write(pagesID).write(" 0 obj\n<<\n/Type /Pages\n/Count 1\n/Kids [").write(pageID).write(" 0 R]\n");
           write("/MediaBox [0 0 ").write(width).writespace().write(height).write("]\n/Resources\n<<\n/Font <<\n/F1 ");
           write(fontID).write(" 0 R >>\n>>\n>>\n" + "endobj\n\n");
           // Write CATALOG
           catalogID=offset.size();
           offset.add(out.getFilePointer());
           write(catalogID).write(" 0 obj\n<<\n/Type /Catalog\n/Pages ").write(pagesID).write(" 0 R\n>>\n" + "endobj\n\n");
           // Write XREF
           long xref = out.getFilePointer();
           write("xref\n0 ").write(offset.size()).write("\n");
           for(int i=0; i<offset.size(); i++) {
              long off = offset.get(i);
              String a = "" + off;
              while(a.length()<10) a="0"+a; // must be exactly 10 characters long
              if (i==0) write(a).write(" 65535 f\r\n"); else write(a).write(" 00000 n\r\n");
           }
           // Write TRAILER
           write("trailer\n<<\n/Size ").write(offset.size()).write("\n/Root ").write(catalogID).write(" 0 R\n>>\n");
           write("startxref\n").write(xref).write("\n%%EOF\n");
        } catch(IOException ex) {
           err=ex;
        }
        // Close the file at all cost, since open files are a scarce system resource
        try { if (out!=null) out.close(); } catch(IOException ex) { if (err==null) err=ex; }
        out=null;
        // If any errors occurred during writing or flushing or closing the file, then re-throw the exception
        if (err!=null) throw err;
    }
}
