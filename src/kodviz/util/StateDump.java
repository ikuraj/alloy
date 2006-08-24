/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * Reads state dumps written out by {@link Dbg#computeStateDump}, and attempts to
 * reconstruct the environment in the dump: put the files in the right places,
 * set the parameters, etc.  Because of the decentralized nature in which
 * the dump is generated, right now this is based on heuristics -- not all
 * parts of the environment will be correctly reconstructed.  What we need is for
 * each part of the system to make explicit its notion of state, and to
 * provide routines for both writing its state to a dump file and
 * reconstructing its state from the dump.
 * <p>
 * The main purpose of this code is to simplify reproducing bugs found by users.
 */
public class StateDump {
    /**
     * Read one state dump from the given stream, and reconstructs its environment
     * under the given subdirectory.
     */
    public static void read(InputStream is_, PrintWriter os_) throws IOException {
    LineNumberReader r = new LineNumberReader(new InputStreamReader(is_));
    String dashes = "------------------------------------------------------------";
    boolean lastLineDashes = false;
    boolean readingFile = false;
    while (true) {
        String line = r.readLine();
        if (line == null) break;
        if (!readingFile && lastLineDashes && line.startsWith("File: "))
        readingFile = true;
        else {
        if (readingFile && (line.equals("#System properties") || line.startsWith("File: ")))
            readingFile = false;
        else
                  if (readingFile)
                os_.println(line);
        }


        lastLineDashes = line.equals(dashes);
    }
    }

    /**
     * Test the state dump reading code.
     */
    public static void main(String[] args_) {
        try {
          String dumpFileName = args_[0];
          String outFileName = args_[1];
          InputStream inStream = new FileInputStream(dumpFileName);
          OutputStream outStream = new FileOutputStream(outFileName);
          PrintWriter w = new PrintWriter(outStream);
          //System.out.println("Reading state dump from " + dumpFileName);
          read(inStream, w);
          inStream.close();
          w.close();
        } catch (Exception e_) {
            e_.printStackTrace();
        }
    }

}  // class StateDump

