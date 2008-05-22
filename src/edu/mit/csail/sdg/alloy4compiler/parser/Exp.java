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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; this is the super class of all untypechecked AST nodes. */

abstract class Exp {

    /** The filename, line, and column position in the original Alloy model file (cannot be null). */
    public final Pos pos;

    /** Constructs an Exp node. */
    Exp(Pos pos) {
        this.pos = (pos==null ? Pos.UNKNOWN : pos);
    }

    /** Convenience method that constructs the expression "not this" */
    public final Exp not() {
        return new ExpUnary(null, ExprUnary.Op.NOT, this);
    }

    /** Returns a Pos object representing the entire span of this Exp and all its subexpressions. */
    public abstract Pos span();

    /**
     * Consults the current lexical context, and converts this Exp node into a corresponding Expr node
     * (along the way, if we detect any type warnings, add them to the listOfWarnings)
     *
     * @param listOfWarnings - the list that will receive any warning we generate; can be null if we wish to ignore warnings
     */
    public abstract Expr check(Context cx, List<ErrorWarning> listOfWarnings);
}
