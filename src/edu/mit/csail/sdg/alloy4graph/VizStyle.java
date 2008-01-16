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

package edu.mit.csail.sdg.alloy4graph;

/**
 * Immutable; enumerates the possible line styles (SOLID, DASHED, DOTTED...)
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public enum VizStyle {

    /** Solid line. */
    SOLID("Solid", "solid"),

    /** Dashed line. */
    DASHED("Dashed", "dashed"),

    /** Dotted line. */
    DOTTED("Dotted", "dotted"),

    /** Bold line. */
    BOLD("Bold", "bold");

    /** The brief description of this line style. */
    private final String longName;

    /** Constructs a VizStyle with the given long name and short name. */
    private VizStyle(String longName, String shortName) { this.longName=longName; }

    /** Returns a brief description of this line style. */
    @Override public String toString() { return longName; }
}
