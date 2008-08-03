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

import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * Mutable; represents a predicate or function.
 *
 * <p> <b>Invariant:</b>  the list of parameters do not contain duplicates
 * <p> <b>Invariant:</b>  none of the parameter declaration contains a predicate/function call
 * <p> <b>Invariant:</b>  the return type declaration does not contain a predicate/function call
 */

public final class Func extends Browsable {

    /** The location in the original file where this predicate/function is declared; never null. */
    public final Pos pos;

    /** If nonnull, then this predicate/function is private (and this.isPrivate is the location of the "private" keyword) */
    public final Pos isPrivate;

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
     * @throws ErrorType if returnType!=null and returnType cannot be unambiguously typechecked to be a set/relation
     * @throws ErrorSyntax if the list of parameters contain duplicates
     * @throws ErrorSyntax if at least one of the parameter declaration contains a predicate/function call
     * @throws ErrorSyntax if this function's return type declaration contains a predicate/function call
     */
    public Func(Pos pos, String label, List<ExprVar> vars, Expr returnDecl) throws Err {
        this(pos, null, label, vars, returnDecl);
    }

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
     * @param isPrivate - if nonnull, then the user intended this func/pred to be "private"
     * @param label - the label for this predicate/function (does not have to be unique)
     * @param vars - the list of parameters (can be null or an empty list if this predicate/function has no parameters)
     * @param returnDecl - the return declaration (null if this is a predicate rather than a function)
     *
     * @throws ErrorType if returnType!=null and returnType cannot be unambiguously typechecked to be a set/relation
     * @throws ErrorSyntax if the list of parameters contain duplicates
     * @throws ErrorSyntax if at least one of the parameter declaration contains a predicate/function call
     * @throws ErrorSyntax if this function's return type declaration contains a predicate/function call
     */
    public Func(Pos pos, Pos isPrivate, String label, List<ExprVar> vars, Expr returnDecl) throws Err {
        if (pos==null) pos=Pos.UNKNOWN;
        this.pos=pos;
        this.isPrivate=isPrivate;
        this.label=(label==null ? "" : label);
        this.isPred=(returnDecl==null);
        if (returnDecl==null) {
            this.body = (this.returnDecl = ExprConstant.FALSE);
        }
        else {
            returnDecl = returnDecl.typecheck_as_set();
            if (returnDecl.ambiguous) returnDecl = returnDecl.resolve_as_set(null);
            if (!returnDecl.errors.isEmpty()) throw returnDecl.errors.pick();
            // If the return declaration is unary, and does not have any multiplicity symbol, we assume it's "one of"
            if (returnDecl.mult==0 && returnDecl.type.arity()==1) returnDecl=ExprUnary.Op.ONEOF.make(null, returnDecl);
            this.returnDecl = returnDecl;
            Expr none = Sig.NONE;
            for(int i = this.returnDecl.type.arity(); i>1; i--) none=none.product(Sig.NONE);
            this.body = none;
        }
        this.params = ConstList.make(vars);
        for(int i=0; i<this.params.size(); i++)
          for(int j=i+1; j<this.params.size(); j++)
            if (this.params.get(i)==this.params.get(j))
              throw new ErrorSyntax(this.params.get(j).span(),
                "The same variable cannot appear more than once in a predicate/function's parameter list.");
        for(int i=0; i<this.params.size(); i++)
           if (this.params.get(i).expr.hasCall())
              throw new ErrorSyntax(this.params.get(i).expr.span(),
                "Parameter declaration cannot contain predicate/function calls.");
        if (this.returnDecl.hasCall())
            throw new ErrorSyntax(returnDecl.span(),
                "Return type declaration cannot contain predicate/function calls.");
    }

    /** The predicate/function body; never null. */
    private Expr body;

    /**
     * Changes the method body.
     *
     * <b>Precondition:</b> The expression should have no free variables,
     * except possibly the list of function parameters.
     *
     * @throws ErrorSyntax if newBody.mult!=0
     * @throws ErrorType if newBody cannot be unambiguously resolved
     * @throws ErrorType if newBody's type is incompatible with the original declared type of this predicate/function
     */
    public void setBody(Expr newBody) throws Err {
        if (isPred) {
            newBody = newBody.typecheck_as_formula();
            if (newBody.ambiguous) newBody = newBody.resolve_as_formula(null);
            if (newBody.errors.size()>0) throw newBody.errors.pick();
        } else {
            newBody = newBody.typecheck_as_set();
            if (newBody.ambiguous) newBody = newBody.resolve_as_set(null);
            if (newBody.errors.size()>0) throw newBody.errors.pick();
            if (newBody.type.arity() != returnDecl.type.arity())
                throw new ErrorType(newBody.span(),
                "Function return type is "+returnDecl.type+",\nso the body must be a relation with arity "
                +returnDecl.type.arity()+".\nSo the body's type cannot be: "+newBody.type);
        }
        if (newBody.mult!=0) throw new ErrorSyntax(newBody.span(), "Multiplicity expression not allowed here.");
        this.body=newBody;
    }

    /**
     * Return the body of this predicate/function.
     * <br> If the user has not called setBody() to set the body,
     * <br> then the default body is "false" (if this is a predicate),
     * <br> or the empty set/relation of the appropriate arity (if this is a function).
     */
    public Expr getBody() { return body; }

    /** Convenience method that calls this function with the given list of arguments. */
    public Expr call(Expr... args) { return ExprCall.make(null, null, this, Util.asList(args), 0); }

    /** Returns a human-readable description for this predicate/function */
    @Override public final String toString() { return (isPred ? "pred " : "fun ") + label; }

    /** {@inheritDoc} */
    @Override public final Pos pos() { return pos; }

    /** {@inheritDoc} */
    @Override public final Pos span() { return pos; }

    /** {@inheritDoc} */
    @Override public String getDescription() { return (isPred ? "<b>pred</b> " : "<b>fun</b> ") + label; }

    /** {@inheritDoc} */
    @Override public List<? extends Browsable> getSubnodes() {
        Browsable p = make("parameters", params);
        Browsable r = make("return type", returnDecl);
        Browsable b = make("body", body);
        if (isPred) {
           if (params.size()==0) return Util.asList(b); else return Util.asList(p, b);
        } else {
           if (params.size()==0) return Util.asList(r, b); else return Util.asList(p, r, b);
        }
    }
}
