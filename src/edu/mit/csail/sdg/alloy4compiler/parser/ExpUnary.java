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
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary.Op;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; represents a unary expression of the form "(OP subexpression)" */

final class ExpUnary extends Exp {

    /** The unary operator. */
    public final Op op;

    /** The subexpression. */
    public final Exp sub;

    /** Constructs an ExpUnary object. */
    public ExpUnary(Pos pos, Op op, Exp sub) {
        super(pos);
        this.op=op;
        this.sub=sub;
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(sub.span());
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr newSub = sub.check(cx, warnings);
        return op.make(pos, newSub);
    }

    /** {@inheritDoc} */
    @Override public String toString() {
        String ans;
        switch(op) {
          case SOMEOF: ans="some "; break;
          case LONEOF: ans="lone "; break;
          case ONEOF: ans="one "; break;
          case SETOF: ans="set "; break;
          case CAST2INT: return "int[" + sub + "]";
          case CAST2SIGINT: return "Int[" + sub + "]";
          case NOOP: ans=""; break;
          default: ans=op.toString()+' ';
        }
        return ans+sub;
    }
}
