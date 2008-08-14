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

import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * Immutable; represents an expression of the form (let a=b | x).
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (var.type.unambiguos() && sub.mult==0)
 */

public final class ExprLet extends Expr {

    /** The LET variable. */
    public final ExprVar var;

    /** The body of the LET expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = var.span().merge(sub.span()));
        return p;
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("(let ").append(var.label).append("=... | ");
            sub.toString(out,-1);
            out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("let with type=").append(type).append('\n');
            var.toString(out, indent+2);
            sub.toString(out, indent+2);
        }
    }

    //=============================================================================================================//

    /** Constructs a LET expression. */
    private ExprLet(Pos pos, ExprVar var, Expr sub, JoinableList<Err> errs) {
        super(pos, null, sub.ambiguous, sub.type, 0, var.weight+sub.weight, errs);
        this.var=var;
        this.sub=sub;
    }

    //=============================================================================================================//

    /**
     * Constructs a LET expression.
     *
     * @param pos - the position of the '=' token in the original Alloy model (or null if unknown)
     * @param var - the LET variable
     * @param sub - the body of the LET expression (which may or may not contain "var" as a free variable)
     */
    public static Expr make(Pos pos, ExprVar var, Expr sub) {
        JoinableList<Err> errs = var.errors.join(sub.errors);
        if (sub.mult != 0)
            errs = errs.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
        if (var.expr.mult!=0)
            errs = errs.append(new ErrorSyntax(var.expr.span(), "Multiplicity expression not allowed here."));
        return new ExprLet(pos, var, sub, errs);
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) {
        if (errors.size()>0) return this;
        // The variable is always already fully resolved, so we only need to resolve sub
        Expr newSub = sub.resolve(p, warns);
        if (warns!=null && !newSub.hasVar(var)) warns.add(new ErrorWarning(var.pos, "This variable is unused."));
        return (sub==newSub) ? this : make(pos, var, newSub);
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    public int getDepth() {
        int a=var.getDepth(), b=sub.getDepth();
        if (a>=b) return 1+a; else return 1+b;
    }

    /** {@inheritDoc} */
    @Override final<T> T accept(VisitReturn<T> visitor) throws Err { return visitor.visit(this); }

    /** {@inheritDoc} */
    @Override public String getDescription() { return "<b>let</b>"; }

    /** {@inheritDoc} */
    @Override public List<? extends Browsable> getSubnodes() {
        Browsable a = make(var.pos, var.pos, "<b>var</b> "+var.label+" = ...", var.expr);
        Browsable b = make(sub.span(), sub.span(), "where...", sub);
        return Util.asList(a, b);
    }
}
