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
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;

/** Mutable; implements a growable array of bytes.
 *
 * For writing large amount of data, ByteBuffer is more efficient than Java's ByteArrayOutputStream
 * because the ByteArrayOutputStream will keep resizing and copying contents every time the array needs to grow.
 */

public final class ByteBuffer {

   /** The size per chunk. */
   private static final int SIZE = 65536;

   /** The list of chunks allocated so far; every chunk is always exactly of size SIZE. */
   private final List<byte[]> list = new ArrayList<byte[]>();

   /** The number of bytes stored in the latest chunk; every chunk before that is always fully filled. */
   private int n = 0;

   /** Construct an empty byte buffer. */
   public ByteBuffer() { }

   /** Write the given byte into this byte buffer. */
   private ByteBuffer w(int b) {
      if (n==SIZE || list.size()==0) { list.add(new byte[SIZE]); n=0; }
      byte[] array = list.get(list.size()-1);
      array[n] = (byte)b;
      n++;
      return this;
   }

   /** Write the given array of bytes into this byte buffer. */
   public ByteBuffer write(int[] b) {
      if (b!=null) for(int i=0; i<b.length; i++) w(b[i]);
      return this;
   }

   /** Write the given String into this byte buffer (by converting the String into its UTF-8 representation if necessary) */
   public ByteBuffer write(String string) {
      if (string.length() == 0) return this;
      byte[] b;
      try { b = string.getBytes("UTF-8"); } catch(UnsupportedEncodingException ex) { return this; } // exception not possible
      for(int i=0; i<b.length; i++) w(b[i]);
      return this;
   }

   /** Write the given number using decimal representation into this byte buffer, followed by a space. */
   public ByteBuffer write(long x)  {
      return write(Long.toString(x)).write(" ");
   }

   /** Write the given number using floating point representation into this byte buffer (truncated to the range -32767..+32767), followed by a space. */
   public strictfp ByteBuffer write(double x) {
      // These extreme values shouldn't happen, but we want to protect against them
      x = x * 1000000;
      if (Double.isNaN(x))             return write("0 ");
      if (x==Double.POSITIVE_INFINITY) return write("32767 ");  // this is the maximum requirement stated in PDF Spec 1.3
      if (x==Double.NEGATIVE_INFINITY) return write("-32767 "); // this is the minimum requirement stated in PDF Spec 1.3
      // Now, regular doubles... let's allow up to 6 digits after the decimal point
      long num = (long)x;
      if (num>32767000000L) return write("32767 "); else if (num<(-32767000000L)) return write("-32767 ");
      String sign = "";
      String str = Long.toString(num);
      if (str.charAt(0)=='-') { str = str.substring(1); sign = "-"; }
      while(str.length()<6) str = "0" + str;
      return write(sign).write(str.substring(0, str.length()-6)).write(".").write(str.substring(str.length()-6)).write(" ");
   }

   /** Write the current ByteBuffer content into the given output file using the Flate compression algorithm (see RFC1951), then return the number of bytes written. */
   public long dumpFlate(RandomAccessFile os) throws IOException {
      Deflater zip = new Deflater();
      zip.setLevel(Deflater.BEST_COMPRESSION);
      byte[] output = new byte[4096];
      int i = 0;    // when negative, that means we have told the Deflater that no more input would be coming
      long sum = 0; // the number of bytes written out so far
      while(true) {
         if (i >= 0 && zip.needsInput()) {
            if (i < list.size()) { zip.setInput(list.get(i), 0, i==list.size()-1 ? this.n : SIZE); i++; }
            if (i == list.size()) { i=(-1); zip.finish(); }
         }
         if (i < 0 && zip.finished()) break;
         int count = zip.deflate(output);
         if (count > 0) { sum = sum + count; os.write(output, 0, count); }
      }
      return sum;
   }

   /** Write the current ByteBuffer content into the given output file as-is, then return the number of bytes written. */
   public long dump(RandomAccessFile os) throws IOException {
      if (list.size() == 0) return 0;
      for(int i=0, n=list.size()-1; i<n; i++) os.write(list.get(i));
      os.write(list.get(list.size()-1), 0, n);
      return ((long)(list.size()-1)) * SIZE + n;
   }
}
