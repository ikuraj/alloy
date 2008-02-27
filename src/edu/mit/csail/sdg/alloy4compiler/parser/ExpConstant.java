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
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.Op;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; represents a constant in the AST. */

final class ExpConstant extends Exp {

    /** The type of constant. */
    public final Op op;

    /** If this node is a number constant, then this field stores the number or index, else this field stores 0. */
    public final int num;

    /** Constructs a constant. */
    public ExpConstant(Pos pos, Op op, int num) {
        super(pos);
        this.op=op;
        this.num=(op==Op.NUMBER ? num : 0);
    }

    /** {@inheritDoc} */
    public Pos span() { return pos; }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) { return op.make(pos, num); }

    /** {@inheritDoc} */
    @Override public String toString() {
        return (op==Op.NUMBER) ? Integer.toString(num) : op.toString();
    }
}
