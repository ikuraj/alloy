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
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; represents an expression of the form (let a=b | x). */

final class ExpLet extends Exp {

    /** The LET variable. */
    public final ExpName left;

    /** The value bound to the LET variable. */
    public final Exp right;

    /** The body of the LET expression. */
    public final Exp sub;

    /**
     * Constructs a LET expression.
     * @param pos - the position of the original '=' token in the text file (or null if unknown)
     * @param left - the LET variable name
     * @param right - the LET variable's value
     * @param sub - the subexpression
     */
    public ExpLet(Pos pos, ExpName left, Exp right, Exp sub) {
        super(pos);
        this.left = left;
        this.right = right;
        this.sub = sub;
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=left.span().merge(sub.span()).merge(right.span()).merge(pos);
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr right = this.right.check(cx, warnings);
        right = right.resolve(right.type, warnings);
        ExprVar left = ExprVar.make(this.left.pos, this.left.name, right);
        cx.put(this.left.name, left);
        Expr sub = this.sub.check(cx, warnings);
        cx.remove(this.left.name);
        return ExprLet.make(pos, left, sub);
    }
}
