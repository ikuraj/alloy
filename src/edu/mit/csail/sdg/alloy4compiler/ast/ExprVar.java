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
import java.util.Set;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.parser.Context;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents a variable in the AST.
 *
 * <p> It can be one of three possibilities:
 * <br> (1) It is a typechecked variable (this.type!=EMPTY && this.expr!=null)
 * <br> (2) It is a not-yet-typechecked variable (this.type==EMPTY && this.expr!=null)
 * <br> (3) It is a placeholder variable where we haven't resolved what it refers to yet (this.type==EMPTY && this.expr==null)
 */

public final class ExprVar extends Expr {

    /** Stores a String label associated with this variable; it's used for pretty-printing and does not have to be unique. */
    public final String label;

    /** The expression that this variable is quantified or substituted by; can be null if this is a placeholder variable. */
    public final Expr expr;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) span = (p = (expr==null ? pos : pos.merge(expr.span())));
        return p;
    }

    /** Print a textual description of it and all subnodes to a StringBuilder, with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(label);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("Var ").append(label).append(" with type=").append(type).append('\n');
        }
    }

    /** Constructs an ExprVar object */
    @SuppressWarnings("unchecked")
    private ExprVar(Pos pos, String label, Expr expr, Type type, Err err) {
        super(pos,
          type,
          0,
          (expr!=null ? expr.weight : 0),
          (expr!=null ? expr.errors : JoinableList.emptylist).appendIfNotNull(err)
        );
        this.label = (label==null ? "" : label);
        this.expr = expr;
    }

    /**
     * Constructs an ExprVar variable whose expr is well-typed (if not, the resulting node's error list will be nonempty)
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param expr - the quantification/substitution expression for this variable
     */
    public static ExprVar makeTyped(Pos pos, String label, Expr expr) {
        ErrorType e=null;
        if (expr.type==null /*TODO*/ || expr.type==EMPTY)
            e=new ErrorType(expr.span(), "This expression failed to be typechecked.");
        else if (expr.type.is_int && expr.type.is_bool)
            e=new ErrorType(expr.span(), "This expression is ambiguous. Its possible types include "+expr.type);
        else if (expr.type.size()>0 && (expr.type.arity()<0 || expr.type.is_int || expr.type.is_bool))
            e=new ErrorType(expr.span(), "This expression is ambiguous. Its possible types include "+expr.type);
        return new ExprVar(pos, label, expr, (e==null ? expr.type : null), e);
    }

    /**
     * Constructs an ExprVar variable where the resulting node will be untyped
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param expr - the quantification/substitution expression for this variable
     */
    public static ExprVar makeUntyped(Pos pos, String label, Expr expr) {
        return new ExprVar(pos, label, expr, null, null);
    }

    /**
     * Constructs a placeholder ExprVar variable where we don't know what it refers to
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     */
    public static ExprVar make(Pos pos, String label) {
        return new ExprVar(pos, label, null, null, null);
    }

    /** Typechecks an ExprName object (first pass). */
    @Override Expr check(final TypeCheckContext cxx) throws Err {
        if (this.type!=null /*TODO*/ && this.type!=EMPTY) return this;
        Context cx=(Context)cxx;
        TempList<Expr> objects=new TempList<Expr>();
        Type t=null;
        if (cx.has(label)) {
            // This handles an ExprVar that represents a LetVar/QuantVar/FunctionParameter
            Expr ex=cx.get(label, pos);
            t=ex.type;
            objects.add(ex);
        } else {
            // This handles an ExprVar that needs to be resolved
            Set<Object> choices = cx.resolve(pos, label);
            if (choices.size()==0) return new ExprVar(pos, label, expr, EMPTY, hint(pos, label));
            ConstList<Expr> args = ConstList.make();
            // If we're inside a sig, and there is a unary variable bound to "this", we should
            // consider it as a possible FIRST ARGUMENT of a fun/pred call
            Expr THIS = (cx.rootsig!=null) ? cx.get("this",null) : null;
            for(Object ch:choices) {
                Expr x=ExprCallOrJoin.makeCallOrJoin(pos, ch, args, THIS);
                objects.add(x);
                if (x.type!=null) t=x.type.merge(t);
            }
        }
        return ExprChoice.make(pos, objects.makeConst());
    }

    /** Typechecks an ExprVar object (second pass). */
    @Override Expr check(final TypeCheckContext cx, Type type, Collection<ErrorWarning> warns) throws Err {
        if (this.type!=null /*TODO*/ && this.type!=EMPTY) return this;
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /** Accepts the return visitor. */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (this.type!=null) return visitor.visit(this);
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /**
     * Convenience method that returns a syntax error exception saying the name "n" can't be found.
     * (In particular, if n is an old Alloy3 keyword, then the message will tell the user to consult
     * the documentation on how to migrate old models to use the new syntax.)
     *
     * @param pos - the original position in the file that triggered the error
     * @param name - the identifier
     */
    public static ErrorSyntax hint (Pos pos, String name) {
        String msg="The name \""+name+"\" cannot be found.";
        if ("exh".equals(name) || "exhaustive".equals(name) || "part".equals(name) || "partition".equals(name))
            msg=msg+" If you are migrating from Alloy 3, please see Help->QuickGuide on how to translate models that use the \""
            +name+"\" keyword.";
        return new ErrorSyntax(pos, msg);
    }
}
