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

package edu.mit.csail.sdg.alloy4compiler.ast;

import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * This interface represents a node that can be browsed in the graphical parse tree displayer.
 */

public abstract class Browsable {

    /** Returns a Pos object representing the position of this Expr. */
    public Pos pos() { return Pos.UNKNOWN; }

    /** Returns a Pos object representing the entire span of this Expr and all its subexpressions. */
    public Pos span() { return pos(); }

    /** Returns the description to show for this node. */
    public abstract String getDescription();

    /** Returns a list of subnodes for this node. */
    public abstract List<? extends Browsable> getSubnodes();

    /** Construct a Browsable node with the given description and the given single subnode. */
    public static Browsable make(final Pos pos, final Pos span, final String description, Browsable subnode) {
        return make(pos, span, description, Util.asList(subnode));
    }

    /** Construct a Browsable node with the given description and the given single subnode. */
    public static Browsable make(final String description, Browsable subnode) {
        return make(Pos.UNKNOWN, Pos.UNKNOWN, description, Util.asList(subnode));
    }

    /** Construct a Browsable node with the given description and the given 0 or more subnodes. */
    public static Browsable make(final String description, final List<? extends Browsable> subnodes) {
        return make(Pos.UNKNOWN, Pos.UNKNOWN, description, subnodes);
    }

    /** Construct a Browsable node with the given description and the given 0 or more subnodes. */
    public static Browsable make(final Pos pos, final Pos span, final String description, final List<? extends Browsable> subnodes) {
        final ConstList<? extends Browsable> constlist = ConstList.make(subnodes);
        return new Browsable() {
            @Override public Pos pos() { return pos; }
            @Override public Pos span() { return span; }
            @Override public String getDescription() { return description; }
            @Override public List<? extends Browsable> getSubnodes() { return constlist; }
        };
    }
}
