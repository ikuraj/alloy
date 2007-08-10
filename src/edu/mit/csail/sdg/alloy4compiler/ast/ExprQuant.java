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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.parser.Context;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.ccint;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.ccform;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br>
 * <br>  (all    &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br>  (no     &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br>  (lone   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (one    &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (some   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br>  (sum    &nbsp;       a,b:t, c,d:v &nbsp; | integer expression)
 * <br>  {a,b:t, &nbsp; c,d:v &nbsp; | &nbsp; formula}
 * <br>  {a,b:t, &nbsp; c,d:v}
 * <br>
 *
 * <br> <b>Invariant:</b> type!=null => (sub.mult==0 && vars.size()>0 && (all v:vars | v.expr!=null))
 */

public final class ExprQuant extends Expr {

    /** The list of warnings. */
    private final ConstList<ErrorWarning> warnings;

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

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos.merge(sub.span());
            for(Expr v:vars) p=p.merge(v.span());
            if (closingBracket!=null) p=p.merge(closingBracket);
            span=p;
        }
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            if (op==Op.COMPREHENSION) out.append('{'); else out.append('(').append(op).append(' ');
            for(int i=0; i<vars.size(); i++) { if (i>0) out.append(','); out.append(vars.get(i)); }
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
    private ExprQuant(Pos pos, Pos closingBracket, Op op, Type type, ConstList<ExprVar> vars, Expr sub, long weight, Collection<ErrorWarning> warnings) {
        super(pos, type, 0, weight);
        this.closingBracket=closingBracket;
        this.op=op;
        this.vars=vars;
        this.sub=sub;
        this.warnings=ConstList.make(warnings);
    }

    //=============================================================================================================//

    /** This class contains all possible quantification operators. */
    public enum Op {
        /** all  a,b:x, c,d:y | formula       */  ALL("all"),
        /** no   a,b:x, c,d:y | formula       */  NO("no"),
        /** lone a,b:x, c,d:y | formula       */  LONE("lone"),
        /** one  a,b:x, c,d:y | formula       */  ONE("one"),
        /** some a,b:x, c,d:y | formula       */  SOME("some"),
        /** sum  a,b:x, c,d:y | intExpression */  SUM("sum"),
        /** { a,b:x,    c,d:y | formula }     */  COMPREHENSION("comprehension");

        /** The constructor. */
        private Op(String label) {this.label=label;}

        /** The human readable label for this operator. */
        private final String label;

        /**
         * Constructs a quantification expression with "this" as the operator.
         *
         * @param pos - the position of the "quantifier" in the source file (or null if unknown)
         * @param closingBracket - the position of the "closing bracket" in the source file (or null if unknown)
         * @param vars - the list of variables
         * @param sub - the body of the expression
         */
        public final Expr make(Pos pos, Pos closingBracket, List<ExprVar> vars, Expr sub) {
            return make(pos, closingBracket, vars, sub, null);
        }

        /**
         * Constructs a quantification expression with "this" as the operator.
         *
         * @param pos - the position of the "quantifier" in the source file (or null if unknown)
         * @param closingBracket - the position of the "closing bracket" in the source file (or null if unknown)
         * @param vars - the list of variables
         * @param sub - the body of the expression
         */
        public final Expr make(Pos pos, Pos closingBracket, List<ExprVar> vars, Expr sub, Collection<ErrorWarning> warnings) {
            JoinableList<Err> errs=sub.errors;
            Type t = (this==SUM) ? Type.INT : (this==COMPREHENSION ? Type.EMPTY : Type.FORMULA);
            long weight = sub.weight;
            if (vars.size()==0) errs=errs.append(new ErrorAPI(pos, "List of variables cannot be empty."));
            for(ExprVar v:vars) {
                errs = errs.join(v.errors);
                weight += v.weight;
                if (v.expr==null) { errs=errs.append(new ErrorAPI(v.span(), "This must be a quantified variable.")); continue; }
                if (v.type==null || v.type==EMPTY) { t=null; continue; }
                if (v.type.size()==0) { errs=errs.append(new ErrorType(v.expr.span(), "This must be a set or relation. Instead, its type is "+v.type)); continue; }
                if (this!=SUM && this!=COMPREHENSION) continue;
                if (!v.type.hasArity(1)) { errs=errs.append(new ErrorType(v.expr.span(), "This must be a unary set. Instead, its type is "+v.type)); continue; }
                Type t1 = v.type.extract(1);
                if (v.expr.mult==1 && v.expr instanceof ExprUnary) {
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SETOF) errs=errs.append(new ErrorType(v.expr.span(), "This cannot be a set-of expression."));
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SOMEOF) errs=errs.append(new ErrorType(v.expr.span(), "This cannot be a some-of expression."));
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.LONEOF) errs=errs.append(new ErrorType(v.expr.span(), "This cannot be a lone-of expression."));
                }
                if (t!=null && this==COMPREHENSION) { if (t.size()==0) t=t1; else t=t.product(t1); }
            }
            if (sub.mult!=0) errs=errs.append(new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here."));
            if (sub.type!=null && sub.type!=EMPTY) {
                if (this==SUM) { sub=cint(sub); errs=errs.appendIfNotNull(ccint(sub)); }
                else { errs=errs.appendIfNotNull(ccform(sub)); }
            }
            return new ExprQuant(pos, closingBracket, this, ((sub.type==null || sub.type==EMPTY) ? null : t), ConstList.make(vars), sub, weight, warnings);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //=============================================================================================================//

    /** Typechecks an ExprQuant object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        ArrayList<ErrorWarning> warns=new ArrayList<ErrorWarning>();
        boolean same=true;
        final Context cxx=(Context)cx;
        final TempList<ExprVar> tempvars=new TempList<ExprVar>(vars.size());
        for(int i=0; i<vars.size(); i++) {
            ExprVar v=vars.get(i);
            if (v.type==null || v.type==EMPTY) { v=ExprVar.makeTyped(v.pos, v.label, TypeCheckContext.addOne(cx.resolveSet(v.expr, warns))); same=false; }
            cxx.put(v.label, v);
            tempvars.add(v);
        }
        final Expr sub = (op==Op.SUM) ? cx.resolveInt(this.sub, warns) : cx.resolveFormula(this.sub, warns);
        for(ExprVar v:vars) cxx.remove(v.label);
        if (same && sub==this.sub && warns.size()==0 && this.warnings.size()==0) return this;
        return op.make(pos, closingBracket, tempvars.makeConst(), sub, warns);
    }

    //=============================================================================================================//

    /** Typechecks an ExprQuant object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p, Collection<ErrorWarning> warns) {
        if (this.warnings.size()>0) warns.addAll(this.warnings);
        return this;
    }

    //=============================================================================================================//

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
