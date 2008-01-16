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
