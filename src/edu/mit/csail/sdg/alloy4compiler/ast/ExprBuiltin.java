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
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.JoinableList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents the builtin disjoint[] predicate.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (all x:args | x.mult==0)
 */

public final class ExprBuiltin extends Expr {

    /** The unmodifiable list of arguments. */
    public final ConstList<Expr> args;

    /** Caches the span() result. */
    private Pos span=null;

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(closingBracket);
            for(Expr a:args) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append("disjoint[");
            for(int i=0; i<args.size(); i++) { if (i>0) out.append(", "); args.get(i).toString(out,-1); }
            out.append(']');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("disjoint[] with type=").append(type).append('\n');
            for(Expr a:args) { a.toString(out, indent+2); }
        }
    }

    //============================================================================================================//

    /** Constructs an ExprBuiltin node. */
    private ExprBuiltin
        (Pos pos, Pos closingBracket, boolean ambiguous, Type type, ConstList<Expr> args, long weight, JoinableList<Err> errs) {
        super(pos, closingBracket, ambiguous, type, 0, weight, errs);
        this.args = args;
    }

    //============================================================================================================//

    /** Generates the expression disj[arg1, args2, arg3...] */
    public static Expr makeDISJOINT(Pos pos, Pos closingBracket, List<Expr> args) {
        boolean ambiguous = false;
        JoinableList<Err> errs = emptyListOfErrors;
        TempList<Expr> newargs = new TempList<Expr>(args.size());
        Type commonArity=null;
        long weight=0;
        if (args.size()<2)
            errs=errs.append(new ErrorSyntax(pos, "The builtin disjoint[] predicate must be called with at least two arguments."));
        for(int i=0; i<args.size(); i++) {
            Expr a = args.get(i).typecheck_as_set();
            ambiguous = ambiguous || a.ambiguous;
            weight = weight + a.weight;
            if (a.mult!=0) errs = errs.append(new ErrorSyntax(a.span(), "Multiplicity expression not allowed here."));
            if (!a.errors.isEmpty()) errs=errs.join(a.errors);
               else if (commonArity==null) commonArity=a.type;
               else commonArity=commonArity.pickCommonArity(a.type);
            newargs.add(a);
        }
        if (commonArity==EMPTY) errs=errs.append(new ErrorType(pos,
           "The builtin predicate disjoint[] cannot be used among expressions of different arities."));
        return new ExprBuiltin(pos, closingBracket, ambiguous, Type.FORMULA, newargs.makeConst(), weight, errs);
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) {
        if (errors.size()>0) return this;
        for(int i=0; i<args.size(); i++) {
            if (i==0) p=Type.removesBoolAndInt(args.get(i).type); else p=p.unionWithCommonArity(args.get(i).type);
        }
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        boolean changed = false;
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i);
            Expr y=x.resolve(p, warns).typecheck_as_set();
            if (x!=y) changed=true;
            args.add(y);
        }
        return changed ? makeDISJOINT(pos, closingBracket, args.makeConst()) : this;
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (!errors.isEmpty()) throw errors.get(0);
        return visitor.visit(this);
    }
}
