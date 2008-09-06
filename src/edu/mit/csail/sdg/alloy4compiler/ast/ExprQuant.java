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
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Util;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br>
 * <br>  ( all  a,b:t | formula )
 * <br>  ( no   a,b:t | formula )
 * <br>  ( lone a,b:t | formula )
 * <br>  ( one  a,b:t | formula )
 * <br>  ( some a,b:t | formula )
 * <br>  ( sum  a,b:t | integer expression )
 * <br>  { a,b:t | formula }
 * <br>  { a,b:t }
 * <br>
 *
 * <br> <b>Invariant:</b> type!=EMPTY => sub.mult==0
 * <br> <b>Invariant:</b> type!=EMPTY => vars.size()>0
 */

public final class ExprQuant extends Expr {

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** The unmodifiable list of variables. */
    public final ConstList<ExprVar> vars;

    /** The body of the quantified expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(closingBracket).merge(sub.span());
            // We intentionally do NOT merge the VAR's position into the span.
            // That allows us to control the highlighting of this component
            // simply by deciding this.pos and this.closingBracket
            span=p;
        }
        return p;
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            if (op!=Op.COMPREHENSION) out.append('(').append(op).append(' '); else out.append('{');
            for(int i=0; i<vars.size(); i++) { if (i>0) out.append(','); out.append(vars.get(i).label); }
            if (op!=Op.COMPREHENSION || !(sub instanceof ExprConstant) || ((ExprConstant)sub).op!=ExprConstant.Op.TRUE)
               {out.append(" | "); sub.toString(out,-1);}
            if (op!=Op.COMPREHENSION) out.append(')'); else out.append('}');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("Quantification(").append(op).append(") of ");
            out.append(vars.size()).append(" vars with type=").append(type).append('\n');
            for(ExprVar v:vars) { v.toString(out, indent+2); }
            sub.toString(out, indent+2);
        }
    }

    //=============================================================================================================//

    /** Constructs a new quantified expression. */
    private ExprQuant
        (Pos pos, Pos closingBracket, Op op, Type type, ConstList<ExprVar> vars, Expr sub, long weight, JoinableList<Err> errs) {
        super(pos, closingBracket, sub.ambiguous, type, 0, weight, errs);
        this.op=op;
        this.vars=vars;
        this.sub=sub;
    }

    //=============================================================================================================//

    /** This class contains all possible quantification operators. */
    public enum Op {
        /** all  a,b:x | formula       */  ALL("all"),
        /** no   a,b:x | formula       */  NO("no"),
        /** lone a,b:x | formula       */  LONE("lone"),
        /** one  a,b:x | formula       */  ONE("one"),
        /** some a,b:x | formula       */  SOME("some"),
        /** sum  a,b:x | intExpression */  SUM("sum"),
        /** { a,b:x | formula }        */  COMPREHENSION("comprehension");

        /** The constructor. */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator. */
        private final String label;

        /**
         * Constructs a quantification expression with "this" as the operator.
         *
         * @param pos - the position of the "quantifier" in the source file (or null if unknown)
         * @param closingBracket - the position of the "closing bracket" in the source file (or null if unknown)
         * @param vars - the list of variables (each must be a variable over a set or relation)
         * @param sub - the body of the expression
         */
        public final Expr make(Pos pos, Pos closingBracket, List<ExprVar> vars, Expr sub) {
            Type t = this==SUM ? Type.INT : (this==COMPREHENSION ? Type.EMPTY : Type.FORMULA);
            if (this!=SUM) sub=sub.typecheck_as_formula(); else sub=sub.typecheck_as_int();
            /*
            Expr subtest = sub.deNOP();
            if (this==Op.ALL && subtest instanceof ExprBinary && ((ExprBinary)subtest).op==ExprBinary.Op.AND) {
                // this transformation improves unsat core
                ExprBinary binary = (ExprBinary)subtest;
                Expr q1 = make(pos, closingBracket, vars, binary.left);
                Expr q2 = make(pos, closingBracket, vars, binary.right);
                return ExprBinary.Op.AND.make(binary.pos, binary.closingBracket, q1, q2);
            }
            if (this==Op.ALL && subtest instanceof ExprList && ((ExprList)subtest).op==ExprList.Op.AND) {
                // this transformation improves unsat core
                ExprList list = (ExprList)subtest;
                TempList<Expr> newlist = new TempList<Expr>(list.args.size());
                for(Expr e: list.args) newlist.add(make(Pos.UNKNOWN, Pos.UNKNOWN, vars, e));
                return ExprList.make(Pos.UNKNOWN, Pos.UNKNOWN, ExprList.Op.AND, newlist.makeConst());
            }
            */
            JoinableList<Err> errs = emptyListOfErrors;
            if (sub.mult!=0) errs = errs.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
            long weight = sub.weight;
            if (vars.size()==0) errs = errs.append(new ErrorSyntax(pos, "List of variables cannot be empty."));
            for(ExprVar v: vars) {
                weight = weight + v.weight;
                errs = errs.join(v.errors);
                if (v.errors.size()>0) {
                    continue;
                }
                if (v.type.size()==0) {
                    errs = errs.append(new ErrorType(v.expr.span(), "This must be a set or relation. Instead, its type is "+v.type));
                    continue;
                }
                if (this!=SUM && this!=COMPREHENSION) continue;
                if (!v.type.hasArity(1)) {
                    errs = errs.append(new ErrorType(v.expr.span(), "This must be a unary set. Instead, its type is "+v.type));
                    continue;
                }
                if (v.expr.mult==1 && v.expr instanceof ExprUnary) {
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SETOF)
                        errs = errs.append(new ErrorType(v.expr.span(), "This cannot be a set-of expression."));
                    else if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SOMEOF)
                        errs = errs.append(new ErrorType(v.expr.span(), "This cannot be a some-of expression."));
                    else if (((ExprUnary)(v.expr)).op == ExprUnary.Op.LONEOF)
                        errs = errs.append(new ErrorType(v.expr.span(), "This cannot be a lone-of expression."));
                }
                if (this==COMPREHENSION) { Type t1=v.type.extract(1); if (t==EMPTY) t=t1; else t=t.product(t1); }
            }
            if (errs.isEmpty()) errs = sub.errors; // if the vars have errors, then the subexpression's errors will be too confusing, so let's skip them
            return new ExprQuant(pos, closingBracket, this, t, ConstList.make(vars), sub, weight, errs);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) {
        if (warns!=null && op!=Op.COMPREHENSION) {
            again:
            for(int i=0; i<vars.size(); i++) {
                ExprVar a=vars.get(i);
                for(int j=i+1; j<vars.size(); j++) {
                    if (vars.get(j).expr.hasVar(a)) continue again;
                }
                if (!sub.hasVar(a)) warns.add(new ErrorWarning(a.pos, "This variable is unused."));
            }
        }
        return this;
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    public int getDepth() {
        int max = sub.getDepth();
        for(Expr x: vars) { int tmp=x.getDepth(); if (max<tmp) max=tmp; }
        return 1 + max;
    }

    /** {@inheritDoc} */
    @Override final<T> T accept(VisitReturn<T> visitor) throws Err { return visitor.visit(this); }

    /** {@inheritDoc} */
    @Override public String getDescription() { return "<b>" + op + "</b>" + " <i>Type = " + type + "</i>"; }

    /** {@inheritDoc} */
    @Override public List<? extends Browsable> getSubnodes() {
        Browsable v = make("<b>variables</b>", vars);
        Browsable b = make("<b>body</b>", sub);
        return Util.asList(v, b);
    }
}
