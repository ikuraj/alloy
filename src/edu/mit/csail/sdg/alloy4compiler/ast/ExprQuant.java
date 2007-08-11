/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
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
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.ccform;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.Resolver.ccint;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br>
 * <br>  (all  a,b:t | formula)
 * <br>  (no   a,b:t | formula)
 * <br>  (lone a,b:t | formula)
 * <br>  (one  a,b:t | formula)
 * <br>  (some a,b:t | formula)
 * <br>  (sum  a,b:t | integer expression)
 * <br>  {a,b:t | formula}
 * <br>  {a,b:t }
 * <br>
 *
 * <br> <b>Invariant:</b> type!=EMPTY => sub.mult==0
 * <br> <b>Invariant:</b> type!=EMPTY => vars.size()>0
 * <br> <b>Invariant:</b> type!=EMPTY => (all v:vars | v.type.unambiguous() && v.type.size()>0)
 */

public final class ExprQuant extends Expr {

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** If nonnull, it is the closing curly bracket's position. */
    public final Pos closingBracket;

    /** The unmodifiable list of variables. */
    public final ConstList<ExprVar> vars;

    /** The body of the quantified expression. */
    public final Expr sub;

    /** Caches the span() result. */
    private Pos span=null;

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(closingBracket).merge(sub.span());
            for(Expr v:vars) p=p.merge(v.span());
            span=p;
        }
        return p;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            if (op==Op.COMPREHENSION) out.append('{'); else out.append('(').append(op).append(' ');
            for(int i=0; i<vars.size(); i++) { if (i>0) out.append(','); out.append(vars.get(i).label); }
            if (op!=Op.COMPREHENSION || !(sub instanceof ExprConstant) || ((ExprConstant)sub).op!=ExprConstant.Op.TRUE)
               {out.append(" | "); sub.toString(out,-1);}
            if (op==Op.COMPREHENSION) out.append('}'); else out.append(')');
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("Quantification(").append(op).append(") of ");
            out.append(vars.size()).append(" vars with type=").append(type).append('\n');
            for(ExprVar v:vars) { v.toString(out, indent+2); }
            sub.toString(out, indent+2);
        }
    }

    /** Constructs a new quantified expression. */
    private ExprQuant(Pos pos, Pos close, Op op, Type type, ConstList<ExprVar> vars, Expr sub, long weight, JoinableList<Err> errs) {
        super(pos, type, 0, weight, errs);
        this.closingBracket=close;
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
            Type t = (this==SUM) ? Type.INT : (this==COMPREHENSION ? Type.EMPTY : Type.FORMULA);
            JoinableList<Err> errs = sub.errors;
            if (errs.size()==0) {
                if (this!=SUM) errs=errs.appendIfNotNull(ccform(sub)); else { sub=cint(sub); errs=errs.appendIfNotNull(ccint(sub)); }
            }
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
            return new ExprQuant(pos, closingBracket, this, t, ConstList.make(vars), sub, weight, errs);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //=============================================================================================================//

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warnings) {
        if (errors.size()==0) return this; // If there is already fatal error, then there's no need to proceed further
        boolean changed=false;
        TempList<ExprVar> newVars = new TempList<ExprVar>(vars.size());
        for(ExprVar v: vars) {
            ExprVar newV = v.resolve(v.type, warnings);
            if (v!=newV) changed=true;
            newVars.add(newV);
        }
        Expr newSub = sub.resolve((op==Op.SUM ? Type.INT : Type.FORMULA), warnings);
        // If at least one variable changed,
        // Then newSub will still contain only the old variables, and yet we are generating an ExprQuant node with the new variables.
        // This is bad! However, due to ExprVar's properties, that means at least one of the newVar has errors.size()>0
        // so it already means a fatal error has occurred, so the resulting ExprQuant node will also have errors.size()>0
        return (!changed && sub==newSub) ? this : op.make(pos, closingBracket, newVars.makeConst(), newSub);
    }

    //=============================================================================================================//

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
