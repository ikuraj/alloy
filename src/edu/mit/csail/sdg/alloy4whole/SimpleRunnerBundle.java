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

package edu.mit.csail.sdg.alloy4whole;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;

/** This helper method is used by SimpleGUI. */

final class SimpleRunnerBundle implements Serializable {

    /** Ensures the serialized form is consistent. */
    private static final long serialVersionUID = 1L;

    public final A4Options options;
    public final Map<String,String> cache;
    public final int index;
    public final int verbosity;
    public final boolean warningNonFatal;

    public SimpleRunnerBundle
    (A4Options options, Map<String,String> cache, int index, int verbosity, boolean warningNonFatal) {
        this.options = options;
        this.cache = cache;
        this.index = index;
        this.verbosity = verbosity;
        this.warningNonFatal = warningNonFatal;
    }

    public void write(String filename) throws Exception {
        OutputStream fs=null;
        ObjectOutputStream os=null;
        try {
            fs=new FileOutputStream(filename);
            os=new ObjectOutputStream(fs);
            os.writeObject(this);
        } finally {
            Util.close(os);
            Util.close(fs);
        }
    }

    public static SimpleRunnerBundle read(String filename) {
        try {
            InputStream is = new FileInputStream(filename);
            ObjectInputStream ois = new ObjectInputStream(is);
            SimpleRunnerBundle ans = (SimpleRunnerBundle) (ois.readObject());
            return new SimpleRunnerBundle(ans.options, new LinkedHashMap<String,String>(ans.cache), ans.index, ans.verbosity, ans.warningNonFatal);
        } catch(Throwable ex) {
            return null;
        }
    }
}
