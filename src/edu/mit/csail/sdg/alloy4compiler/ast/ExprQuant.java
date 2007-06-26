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

import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cint;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cform;

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
 * <br> <b>Invariant:</b> sub.mult==0
 * <br> <b>Invariant:</b> vars.size()>0
 * <br> <b>Invariant:</b> all v:vars | v.expr!=null (meaning each is a "quantified var" rather than a "substitution var")
 */

public final class ExprQuant extends Expr {

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** If nonnull, it is the closing curly bracket's position. */
    public final Pos closingBracket;

    /** The unmodifiable list of typechecked variable declarations; always nonempty. */
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
    private ExprQuant(Pos pos, Pos close, Op op, Type type, ConstList<ExprVar> vars, Expr sub, long weight) throws Err {
        super(pos, type, 0, weight);
        this.closingBracket=close;
        this.op=op;
        this.vars=vars;
        this.sub=sub;
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
         * @param vars - the list of variables (which must not be empty, and must not contain duplicates)
         * @param sub - the body of the expression
         *
         * @throws ErrorAPI if one or more variable is a "substitution variable" rather than a "quantified variable"
         * @throws ErrorAPI if the list of variables is empty, or the list contains duplicate variables
         * @throws ErrorSyntax if sub.mult!=0
         */
        public final Expr make(Pos pos, Pos closingBracket, List<ExprVar> vars, Expr sub) throws Err {
            if (vars.size()==0) throw new ErrorAPI("List of variables cannot be empty.");
            if (sub.mult!=0) throw new ErrorSyntax(sub.span(), "Multiplicity expression not allowed here.");
            if (sub.type!=null) { if (this==SUM) sub=cint(sub); else cform(sub); }
            Type t = (this==SUM) ? Type.INT : (this==COMPREHENSION ? Type.EMPTY : Type.FORMULA);
            long weight = sub.weight;
            for(ExprVar v:vars) {
                weight += v.weight;
                if (v.expr==null) throw new ErrorAPI(v.span(), "This must be a quantified variable rather than a substitution variable.");
                int a=v.type.arity();
                if (a==0) throw new ErrorType(v.expr.span(), "This must be a set or relation. Instead, its type is "+v.type);
                if (this!=SUM && this!=COMPREHENSION) continue;
                if (a>1) throw new ErrorType(v.expr.span(), "This must be a unary set. Instead, its type is "+v.type);
                if (v.expr.mult==1 && v.expr instanceof ExprUnary) {
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SETOF) throw new ErrorType(v.expr.span(), "This cannot be a set-of expression.");
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.SOMEOF) throw new ErrorType(v.expr.span(), "This cannot be a some-of expression.");
                    if (((ExprUnary)(v.expr)).op == ExprUnary.Op.LONEOF) throw new ErrorType(v.expr.span(), "This cannot be a lone-of expression.");
                }
                if (this==COMPREHENSION) { if (t.size()==0) t=v.type; else t=t.product(v.type); }
            }
            return new ExprQuant(pos, closingBracket, this, t, ConstList.make(vars), sub, weight);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }

    //=============================================================================================================//

    /** Typechecks an ExprQuant object (first pass). */
    @Override Expr check(final TypeCheckContext cx) throws Err {
        Expr sub=this.sub.check(cx);
        if (op==Op.SUM) sub=cint(sub); else cform(sub);
        if (sub==this.sub) return this; else return op.make(pos, closingBracket, vars, sub);
    }

    //=============================================================================================================//

    /** Typechecks an ExprQuant object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type p) throws Err {
        Expr sub=this.sub.check(cx, (op==Op.SUM ? Type.INT : Type.FORMULA));
        if (op==Op.SUM) sub=cint(sub); else cform(sub);
        if (sub==this.sub) return this; else return op.make(pos, closingBracket, vars, sub);
    }

    //=============================================================================================================//

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err { return visitor.visit(this); }
}
