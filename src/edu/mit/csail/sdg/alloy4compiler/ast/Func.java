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
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Util;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.addOne;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.unambiguous;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.cset;
import static edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext.ccform;

/**
 * Mutable; represents a predicate or function.
 *
 * <p> <b>Invariant:</b>  none of the parameter declaration contains a predicate/function call
 * <p> <b>Invariant:</b>  the return type declaration does not contain a predicate/function call
 */

public final class Func {

    /** The location in the original file where this predicate/function is declared; never null. */
    public final Pos pos;

    /** The label of this predicate/function; it does not need to be unique. */
    public final String label;

    /** True if this is a predicate; false if this is a function. */
    public final boolean isPred;

    /** The list of parameters; may be an empty list if this predicate/function has no parameters. */
    public final ConstList<ExprVar> params;

    /** The declared return type; never null. */
    public final Expr returnDecl;

    /**
     * Constructs a new predicate/function.
     *
     * <p>  The first parameter's bound should be an expression with no free variables.
     * <br> The second parameter's bound should be an expression with no free variables, except possibly the first parameter.
     * <br> The third parameter's bound should be an expression with no free variables, except possibly the first two parameters.
     * <br> etc.
     * <br> The return declaration should have no free variables, except possibly the list of input parameters.
     *
     * @param pos - the original position in the file
     * @param label - the label for this predicate/function (does not have to be unique)
     * @param vars - the list of parameters (can be null or an empty list if this predicate/function has no parameters)
     * @param returnDecl - the return declaration (null if this is a predicate rather than a function)
     *
     * @throws ErrorType if returnType!=null and returnType is not already unambiguously typechecked to be a set/relation
     * @throws ErrorSyntax if the list of parameters contain duplicates
     * @throws ErrorSyntax if at least one of the parameter declaration contains a predicate/function call
     * @throws ErrorSyntax if this function's return type declaration contains a predicate/function call
     */
    public Func(Pos pos, String label, List<ExprVar> vars, Expr returnDecl) throws Err {
        if (pos==null) pos=Pos.UNKNOWN;
        this.pos=pos;
        this.label=(label==null ? "" : label);
        this.isPred=(returnDecl==null);
        if (returnDecl==null) {
            this.body = (this.returnDecl = ExprConstant.FALSE);
        }
        else {
            this.returnDecl = addOne(unambiguous(cset(returnDecl)));
            Expr none = Sig.NONE;
            for(int i = this.returnDecl.type.arity(); i>1; i--) none=none.product(Sig.NONE);
            this.body = none;
        }
        this.params = ConstList.make(vars);
        for(int i=0; i<this.params.size(); i++)
          for(int j=i+1; j<this.params.size(); j++)
            if (this.params.get(i)==this.params.get(j))
              throw new ErrorSyntax(pos, "The same variable cannot appear more than once in a predicate/function's parameter list.");
        for(int i=0; i<this.params.size(); i++)
           if (this.params.get(i).expr.hasCall())
              throw new ErrorSyntax(this.params.get(i).expr.span(), "Parameter declaration cannot contain predicate/function calls.");
        if (this.returnDecl.hasCall())
            throw new ErrorSyntax(returnDecl.span(), "Return type declaration cannot contain predicate/function calls.");
    }

    /** The predicate/function body; never null. */
    private Expr body;

    /**
     * Changes the method body; "newBody" must already be fully typechecked.
     * The expression should have no free variables, except possibly the list of function parameters.
     *
     * @throws ErrorType if the newBody's type is incompatible with the original declared type of this predicate/function
     */
    public void setBody(Expr newBody) throws Err {
        if (isPred) {
            Err err = ccform(unambiguous(newBody));
            if (err!=null) throw err;
        } else {
            newBody = unambiguous(cset(newBody));
            if (newBody.type.arity() != returnDecl.type.arity())
                throw new ErrorType(newBody.span(),
                "Function return type is "+returnDecl.type+",\nso the body must be a relation with arity "
                +returnDecl.type.arity()+".\nSo the body's type cannot be: "+newBody.type);
        }
        this.body=newBody;
    }

    /**
     * Return the body of this predicate/function.
     * <br> If the user has not called setBody() to set the body,
     * <br> then the default body is "false" (if this is a predicate),
     * <br> or the empty set/relation of the appropriate arity (if this is a function).
     */
    public Expr getBody() { return body; }

    /**
     * Convenience method that constructs an ExprCall object representing "this[args...]"
     * @throws ErrorSyntax if the number of arguments does not match the number of parameters
     * @throws ErrorSyntax if one or more argument is a multiplicity expression
     * @throws ErrorType   if one or more argument cannot possibly have the correct legal type
     */
    public Expr call(Expr... args) throws Err {
        Pos p=Pos.UNKNOWN;
        for(Expr a:args) p=p.merge(a.span());
        return ExprCall.make(p, this, Util.asList(args), 0);
    }

    /** Returns a human-readable description for this predicate/function */
    @Override public final String toString() {
        return (isPred ? "pred " : "fun ") + label;
    }
}
