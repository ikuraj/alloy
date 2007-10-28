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

package edu.mit.csail.sdg.alloy4;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import javax.imageio.ImageIO;

/**
 * Graphical convenience methods for producing PNG files.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class OurPNGWriter {

    /** The buffer size; must be at least 21 since we need to have enough prefetch to write out the pHYs chunk into the file. */
    private static final int BUFSIZE=4096;

    /** The constructor is private, since this utility class never needs to be instantiated. */
    private OurPNGWriter() { }

    /** Writes the image as a PNG file with the given horizontal and vertical dots-per-inch. */
    public static void writePNG(BufferedImage image, String filename, double dpiX, double dpiY) throws IOException {
        ImageIO.write(image, "PNG", new File(filename));
        setDPI(filename, dpiX, dpiY);
    }

    /** Modifies the given PNG file to have the given horizontal and vertical dots-per-inch. */
    private static void setDPI(String filename, double dpiX, double dpiY) throws IOException {
       RandomAccessFile f = null;
       try {
          f = new RandomAccessFile(filename, "rw");
          byte[] buf1=new byte[BUFSIZE], buf2=new byte[BUFSIZE];
          int fill=0;
          long total=f.length(), rOffset=8, wOffset;
          // Jump to the appropriate place for inserting the pHYs chunk, then insert it
          while(true) {
             if (rOffset>=total) throw new IOException("PNG is missing the IDAT chunk.");
             f.seek(rOffset);
             int a1=f.read(), a2=f.read(), a3=f.read(), a4=f.read(), b1=f.read(), b2=f.read(), b3=f.read(), b4=f.read();
             if (a1<0 || a2<0 || a3<0 || a4<0 || b1<0 || b2<0 || b3<0 || b4<0 || (b1=='I' && b2=='E' && b3=='N' && b4=='D'))
                throw new IOException("PNG is missing the IDAT chunk.");
             int jump=(a1<<24)|(a2<<16)|(a3<<8)|a4;
             if (jump<0 || (total-rOffset)-12<jump) throw new IOException("PNG chunk size exceeds the rest of the file.");
             if ((b1=='I' && b2=='D' && b3=='A' && b4=='T') || (b1=='p' && b2=='H' && b3=='Y' && b4=='s')) {
                wOffset=rOffset;
                if (b1=='p') rOffset=(rOffset+12)+jump; // We can skip over the original pHYs chunk, since we're writing our own
                f.seek(rOffset);
                fill=fill(f,buf1);
                rOffset=rOffset+fill;
                // At this point, buf1 either contains the ENTIRE REMAINING FILE, or buf1 contains at least BUFSIZE bytes
                f.seek(wOffset);
                writeDPI(f, dpiX, dpiY);
                wOffset=wOffset+(12+9);
                break;
             }
             rOffset=(rOffset+12)+jump;
          }
          // Now, shift everything else in the file down by 21 bytes
          while(true) {
             // At this point, buf1 either contains the ENTIRE REMAINING FILE, or buf1 contains at least BUFSIZE bytes
             f.seek(rOffset);
             int fill2=fill(f,buf2);
             if (total-rOffset<fill2) fill2=(int)(total-rOffset); // We must not read beyond the original end of file
             rOffset += fill2;
             // At this point, buf1+buf2 either contains the ENTIRE REMAING FILE, or buf2 contains at least BUFSIZE bytes
             if (fill>0) { f.seek(wOffset); f.write(buf1,0,fill); wOffset+=fill; }
             byte[] tmp=buf1; buf1=buf2; buf2=tmp;
             fill=fill2;
             if (fill==0) break;
          }
          // Now, end the file at the current file length
          f.setLength(wOffset);
          f.close();
          f=null;
       } finally {
          if (f!=null) { try { f.close(); } catch(IOException ex) { } }
       }
    }

    /** Read as much as possible until we get bufsize bytes or EOF; return the number of bytes successfully read into the buffer. */
    private static int fill(RandomAccessFile f, byte[] buf) throws IOException {
        int n=0;
        while(n<BUFSIZE) {
            int i=f.read(buf, n, BUFSIZE-n);
            if (i<0) return n;
            n=n+i;
        }
        return n;
    }

    /** Write the "pHYs" chunk into the PNG file with the given horizontal and vertical dots-per-inch. */
    private static void writeDPI(RandomAccessFile f, double dpiX, double dpiY) throws IOException {
        int dpmX = (int) (dpiX/2.54d*100d); // Translate dots-per-inch into dots-per-meter
        int dpmY = (int) (dpiY/2.54d*100d); // Translate dots-per-inch into dots-per-meter
        int crc = 0xFFFFFFFF, b;
        f.write(0); f.write(0); f.write(0); f.write(9);
        b='p';              crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b='H';              crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b='Y';              crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b='s';              crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmX>>>24)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmX>>>16)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmX>>> 8)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmX     )&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmY>>>24)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmY>>>16)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmY>>> 8)&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=(dpmY     )&0xFF; crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        b=1;                crc=table[(crc ^ b) & 0xff] ^ (crc >>> 8); f.write(b);
        crc = crc ^ 0xFFFFFFFF;
        f.write((crc>>>24)&0xFF); f.write((crc>>>16)&0xFF); f.write((crc>>>8)&0xFF); f.write(crc&0xFF);
    }

    /** This precomputed table makes it faster to calculate CRC; this is based on the suggestion in the PNG specification. */
    private static final int[] table = new int[] {
      0,1996959894,-301047508,-1727442502,124634137,1886057615,-379345611,-1637575261,249268274
      ,2044508324,-522852066,-1747789432,162941995,2125561021,-407360249,-1866523247,498536548,1789927666
      ,-205950648,-2067906082,450548861,1843258603,-187386543,-2083289657,325883990,1684777152,-43845254
      ,-1973040660,335633487,1661365465,-99664541,-1928851979,997073096,1281953886,-715111964,-1570279054
      ,1006888145,1258607687,-770865667,-1526024853,901097722,1119000684,-608450090,-1396901568,853044451
      ,1172266101,-589951537,-1412350631,651767980,1373503546,-925412992,-1076862698,565507253,1454621731
      ,-809855591,-1195530993,671266974,1594198024,-972236366,-1324619484,795835527,1483230225,-1050600021
      ,-1234817731,1994146192,31158534,-1731059524,-271249366,1907459465,112637215,-1614814043,-390540237
      ,2013776290,251722036,-1777751922,-519137256,2137656763,141376813,-1855689577,-429695999,1802195444
      ,476864866,-2056965928,-228458418,1812370925,453092731,-2113342271,-183516073,1706088902,314042704
      ,-1950435094,-54949764,1658658271,366619977,-1932296973,-69972891,1303535960,984961486,-1547960204
      ,-725929758,1256170817,1037604311,-1529756563,-740887301,1131014506,879679996,-1385723834,-631195440
      ,1141124467,855842277,-1442165665,-586318647,1342533948,654459306,-1106571248,-921952122,1466479909
      ,544179635,-1184443383,-832445281,1591671054,702138776,-1328506846,-942167884,1504918807,783551873
      ,-1212326853,-1061524307,-306674912,-1698712650,62317068,1957810842,-355121351,-1647151185,81470997
      ,1943803523,-480048366,-1805370492,225274430,2053790376,-468791541,-1828061283,167816743,2097651377
      ,-267414716,-2029476910,503444072,1762050814,-144550051,-2140837941,426522225,1852507879,-19653770
      ,-1982649376,282753626,1742555852,-105259153,-1900089351,397917763,1622183637,-690576408,-1580100738
      ,953729732,1340076626,-776247311,-1497606297,1068828381,1219638859,-670225446,-1358292148,906185462
      ,1090812512,-547295293,-1469587627,829329135,1181335161,-882789492,-1134132454,628085408,1382605366
      ,-871598187,-1156888829,570562233,1426400815,-977650754,-1296233688,733239954,1555261956,-1026031705
      ,-1244606671,752459403,1541320221,-1687895376,-328994266,1969922972,40735498,-1677130071,-351390145
      ,1913087877,83908371,-1782625662,-491226604,2075208622,213261112,-1831694693,-438977011,2094854071
      ,198958881,-2032938284,-237706686,1759359992,534414190,-2118248755,-155638181,1873836001,414664567
      ,-2012718362,-15766928,1711684554,285281116,-1889165569,-127750551,1634467795,376229701,-1609899400
      ,-686959890,1308918612,956543938,-1486412191,-799009033,1231636301,1047427035,-1362007478,-640263460
      ,1088359270,936918000,-1447252397,-558129467,1202900863,817233897,-1111625188,-893730166,1404277552
      ,615818150,-1160759803,-841546093,1423857449,601450431,-1285129682,-1000256840,1567103746,711928724
      ,-1274298825,-1022587231,1510334235,755167117
    };
}
