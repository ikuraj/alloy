package edu.mit.csail.sdg.alloy4;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;

/**
 * Graphical convenience methods for producing PDF files.
 *
 * <p> This implementation explicitly generates a very simple 1-page PDF
 * which consists of a single stream of graphical operations.
 * Hopefully this class will no longer be needed in the future,
 * once Java comes with better PDF support.
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

    /** The width (in terms of dots). */
    private long width;

    /** The height (in terms of dots). */
    private long height;

    /** Nonnull if an IOException has occurred. */
    private IOException err=null;

    /**
     * Write the PDF header into the file, then begins a Contents stream; (if the file already exists, it will be overwritten).
     * @throws IllegalArgumentException if dpi is less than 50 or is greater than 3000
     * @throws IOException if an error occurred in opening or writing to the file
     */
    public OurPDFWriter(int dpi, String filename) throws IOException {
        // Initialize various data structures
        if (dpi<50 || dpi>3000) throw new IllegalArgumentException("The DPI must be between 50 and 3000");
        width=dpi*8+(dpi/2);
        height=dpi*11;
        offset.clear();
        offset.add(0L); // ID 0 does not exist
        out=new RandomAccessFile(filename, "rw");
        try {
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
        } catch(IOException ex) {
          try { out.close(); } catch(IOException ex2) { } // open files are a scarce resource, so try to close it at all cost
          throw ex;
        }
    }

    /**
     * Writes the given String into the current Contents stream object.
     * <p> Note: IO errors are recorded and delayed until you call close() on this OurPDFWriter object.
     */
    public void write(String x) {
        if (err==null && out!=null) try {
            out.write(x.getBytes("UTF-8"));
        } catch(IOException ex) {
            err=ex;
        }
    }

    /**
     * Finishes writing the PDF file, then flushes and closes the file.
     * @throws IOException if an error occurred in writing or closing the file
     */
    public void close() throws IOException {
        if (err==null && out!=null) try {
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
        } catch(IOException ex) {
          err=ex;
        }
        // Close the file at all cost, since open files are a scarce system resource
        try { if (out!=null) out.close(); } catch(IOException ex) { if (err==null) err=ex; }
        out=null;
        if (err!=null) throw err;
    }
}
