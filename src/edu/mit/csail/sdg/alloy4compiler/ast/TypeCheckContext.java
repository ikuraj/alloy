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

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4compiler.ast.Type.ProductType;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;

/**
 * This class stores the context during typechecking.
 *
 * <p>
 * During the first pass, the typechecker
 * computes the "bounding type" of parent expressions
 * based on the bounding types of its children.
 * During this phase, if an expression could not possibly have legal bounding types, we throw an exception.
 *
 * <p>
 * During the second pass, the typechecker
 * computes the "relevant type" top-down:
 * the type of the parent expression is used to determine
 * whether the subexpressions are relevant or not.
 * This second pass can be used to resolve field/function/predicate overloading,
 * and can also detect some irrelevant expressions.
 *
 * <p>
 * In general, during the first pass, we allow ambiguities to propagate as much as possible.
 * This enables more context-sensitivity for doing precise diambiguation later.
 */

public class TypeCheckContext {

    /** This configuration option is true if we want to automatically cast from int to Int when necessary. */
    public static final boolean auto_int2sigint=true;

    /** This configuration option is true if we want to automatically cast from Int to int when necessary. */
    public static final boolean auto_sigint2int=true;

    /** Construct a new TypeCheckContext. */
    public TypeCheckContext() { }

    /** Performs typechecking (first pass) on the given expression. */
    public final Expr check(Expr expr) throws Err { return expr.check(this); }

    /** Performs typechecking (second pass) on the given expression. */
    public final Expr check(Expr expr, Type type) throws Err { return expr.check(this,type); }

    /**
     * Helper method that throws a type error if x cannot possibly have any legal type or if x is ambiguous.
     * Otherwise it returns x.
     *
     * @throws ErrorType if X is not already unambiguously typechecked
     */
    public static final Expr unambiguous(final Expr x) throws ErrorType {
        final Type t=x.type;
        if (t==null || (!t.is_bool && !t.is_int && t.size()==0)) {
            throw new ErrorType(x.span(), "This expression fails to be typechecked.");
        }
        if (t.size()==0) {
            if (!t.is_bool || !t.is_int) return x;
        } else {
            if (!t.is_bool && !t.is_int && t.arity()>0) return x;
        }
        throw new ErrorType(x.span(), "This expression is ambiguous.\nIt has the following possible types:\n"+t);
    }

    /**
     * Helper method that adds a "one of" in front of the expression X if X is unary and is not already a multiplicity constraint.
     *
     * @throws ErrorType if X is not already unambiguously typechecked
     */
    public static final Expr addOne(Expr x) throws Err {
        unambiguous(x);
        if (x instanceof ExprUnary) switch(((ExprUnary)x).op) {
            case SETOF: case ONEOF: case LONEOF: case SOMEOF: return x;
        }
        return (x.type.arity()!=1) ? x : ExprUnary.Op.ONEOF.make(x.span(), x);
    }

    /**
     * Helper method that throws a type error if x cannot possibly have formula type.
     * <p> <b>Return</b> x if it can have formula type
     *
     * @throws ErrorType if x does not have formula type
     */
    public static final Expr cform(Expr x) throws ErrorType {
        if (x.type==null)
            throw new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (!x.type.is_bool)
            throw new ErrorType(x.span(), "This must be a formula expression.\nInstead, it has the following possible type(s):\n"+x.type);
        return x;
    }

    /**
     * Helper method that throws a type error if x cannot possibly have formula type.
     * <p> <b>Return</b> x if it can have formula type
     *
     * @throws ErrorType if x does not have formula type
     */
    public static final Err isForm(Expr x) {
        if (x.type==null)
            return new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (!x.type.is_bool)
            return new ErrorType(x.span(), "This must be a formula expression.\nInstead, it has the following possible type(s):\n"+x.type);
        return null;
    }

    /**
     * Helper method that throws a type error if x cannot possibly have integer type.
     *
     * <p>  <b>Return</b> x if it can have integer type
     * <br> <b>Return</b> x.cast2int() if x can be casted to an integer type, and auto casting is enabled
     *
     * @throws ErrorType if x does not and cannot be casted to have integer type
     */
    public static final Expr cint(Expr x) throws Err {
        if (x.type==null)
            throw new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (!x.type.is_int) {
            if (auto_sigint2int && x.type.intersects(SIGINT.type)) return x.cast2int();
            throw new ErrorType(x.span(), "This must be an integer expression.\nInstead, it has the following possible type(s):\n"+x.type);
        }
        return x;
    }

    /**
     * Helper method that throws a type error if x cannot possibly have set/relation type.
     *
     * <p>  <b>Return</b> x if it can have set/relation type
     * <br> <b>Return</b> x.cast2sigint() if x can be casted to a set/relation type, and auto casting is enabled
     *
     * @throws ErrorType if x does not and cannot be casted to have set/relation type
     */
    public static final Expr cset(Expr x) throws Err {
        if (x.type==null)
            throw new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (x.type.size()==0) {
            if (auto_int2sigint && x.type.is_int) return x.cast2sigint();
            throw new ErrorType(x.span(), "This must be a set or relation.\nInstead, it has the following possible type(s):\n"+x.type);
        }
        return x;
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it.
     *
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public final Expr resolve(Expr x) throws Err {
        x=x.check(this);
        Type t=x.type;
        if (t.arity()<0) {
            // If we can have multiple arities, but some of them are empty, then remove the empty ones.
            Type tt=(t.is_bool ? (t.is_int ? Type.INTANDFORMULA: Type.FORMULA) : (t.is_int ? Type.INT : Type.EMPTY));
            for(ProductType r:t) if (!r.isEmpty()) tt=tt.merge(r);
            if (tt.is_int || tt.is_bool || tt.size()>0) t=tt;
        }
        x=x.check(this,t);
        return unambiguous(x); // double check that the type info is unambiguous (and complain if it's not)
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be a set/relation expression
     *
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public final Expr resolve_set(Expr x) throws Err {
        x=cset(x.check(this));
        Type t=Type.removesBoolAndInt(x.type);
        if (t.arity()<0) {
            // If we can have multiple arities, but some of them are empty, then remove the empty ones.
            Type tt=Type.EMPTY;
            for(ProductType r:t) if (!r.isEmpty()) tt=tt.merge(r);
            if (tt.size()>0) t=tt;
        }
        x=x.check(this,t);
        return unambiguous(x); // double check that the type info is unambiguous (and complain if it's not)
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be an integer expression
     *
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public final Expr resolve_int(Expr x) throws Err {
        x=cint(x.check(this));
        x=x.check(this, Type.INT);
        return unambiguous(x); // double check that the type info is unambiguous (and complain if it's not)
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be a formula
     *
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public final Expr resolve_formula(Expr x) throws Err {
        x=cform(x.check(this));
        x=x.check(this, Type.FORMULA);
        return unambiguous(x); // double check that the type info is unambiguous (and complain if it's not)
    }
}
