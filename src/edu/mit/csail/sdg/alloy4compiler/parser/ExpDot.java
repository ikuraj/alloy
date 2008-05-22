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
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprChoice;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Context;

/** Immutable; represents a function/predicate call or a relational join. */

final class ExpDot extends Exp {

    /** If nonnull, it is the closing bracket. */
    public final Pos closingBracket;

    /** The left-hand side of the DOT operator. */
    public final Exp left;

    /** The right-hand side of the DOT operator. */
    public final Exp right;

    /** Constructs a call or a relational join expression. */
    public ExpDot(Pos pos, Pos closingBracket, Exp left, Exp right) {
        super(pos);
        this.closingBracket = closingBracket;
        this.left = left;
        this.right = right;
    }

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=left.span().merge(closingBracket).merge(right.span()).merge(pos);
            span=p;
        }
        return p;
    }

    /**
     * Returns true if the function's parameters have reasonable intersection with the list of arguments.
     * <br> If args.length() > f.params.size(), the extra arguments are ignored by this check
     */
    private static boolean applicable(Func f, List<Expr> args) {
        if (f.params.size() > args.size()) return false;
        int i=0;
        for(ExprVar d: f.params) {
            Type param=d.type, arg=args.get(i).type;
            i++;
            // The reason we don't directly compare "arg.arity()" with "param.arity()"
            // is because the arguments may not be fully resolved yet.
            if (!arg.hasCommonArity(param)) return false;
            if (arg.hasTuple() && param.hasTuple() && !arg.intersects(param)) return false;
        }
        return true;
    }

    private Expr process(List<Expr> choices, List<String> oldReasons, Expr arg) {
        TempList<Expr> list = new TempList<Expr>(choices.size());
        TempList<String> reasons = new TempList<String>(choices.size());
        for(int i=0; i<choices.size(); i++) {
            Expr x=choices.get(i), y=x;
            while(true) {
               if (y instanceof ExprUnary && ((ExprUnary)y).op==ExprUnary.Op.NOOP) y=((ExprUnary)y).sub;
               else if (y instanceof ExprChoice && ((ExprChoice)y).choices.size()==1) y=((ExprChoice)y).choices.get(0);
               else break;
            }
            if (y instanceof ExprBadCall) {
                ExprBadCall bc = (ExprBadCall)y;
                if (bc.args.size() < bc.fun.params.size()) {
                    ConstList<Expr> newargs = Util.append(bc.args, arg);
                    if (applicable(bc.fun, newargs))
                        y=ExprCall.make(bc.pos, bc.closingBracket, bc.fun, newargs, bc.extraWeight);
                    else
                        y=ExprBadCall.make(bc.pos, bc.closingBracket, bc.fun, newargs, bc.extraWeight);
                } else {
                    y=ExprBinary.Op.JOIN.make(pos, closingBracket, arg, y);
                }
            } else {
                y=ExprBinary.Op.JOIN.make(pos, closingBracket, arg, x);
            }
            list.add(y);
            reasons.add(oldReasons.get(i));
        }
        return ExprChoice.make(this.right.pos, list.makeConst(), reasons.makeConst());
    }

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Expr left = this.left.check(cx, warnings);
        Expr right = this.right.check(cx, warnings);
        // check to see if it is a macro invocation
        if (right instanceof ExpMacro) {
            // If we get here, that means it is a partial macro call
            return ((ExpMacro)right).addArg(left).instantiate(cx, warnings);
        }
        // check to see if it is the special builtin function "Int[]"
        if (left.type.is_int && right.isSame(Sig.SIGINT)) return left.cast2sigint();
        // otherwise, process as regular join or as method call
        left = left.typecheck_as_set();
        if (!left.errors.isEmpty() || !(right instanceof ExprChoice)) {
            return ExprBinary.Op.JOIN.make(pos, closingBracket, left, right);
        }
        return process(((ExprChoice)right).choices, ((ExprChoice)right).reasons, left);
    }

    /** {@inheritDoc} */
    @Override public String toString() {
        return left.toString()+'.'+right;
    }
}
