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
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary.Op.NOOP;

public final class Resolver {

    /**
     * Resolves the expression "expr".
     * (And if t.size()>0, it represents the set of tuples whose presence/absence is relevent to the parent expression)
     * (Note: it's possible for t to be EMPTY, or even ambiguous!)
     *
     * <p> Postcondition: RESULT.errors.size()>0  or  "RESULT and all its subnodes are fully resolved and unambiguous"
     */
    public static Expr resolve(Expr expr, Type t, Collection<ErrorWarning> warnings) {
        return expr.resolve(t, warnings);
    }

    /**
     * Helper method that throws a type error if x cannot possibly have any legal type or if x is ambiguous.
     * Otherwise it returns x.
     *
     * @throws ErrorType if X is not already unambiguously typechecked
     */
    static Expr unambiguous(final Expr x) throws Err {
        if (x.errors.size()>0) throw x.errors.get(0);
        final Type t=x.type;
        if (!t.is_bool && !t.is_int && t.size()==0)
            throw new ErrorType(x.span(), "This expression fails to be typechecked.");
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
    public static Expr addOne(Expr x) {
        //unambiguous(x);
        if (x instanceof ExprUnary) switch(((ExprUnary)x).op) {
            case SETOF: case ONEOF: case LONEOF: case SOMEOF: return x;
        }
        return (x.type.arity()!=1) ? x : ExprUnary.Op.ONEOF.make(x.span(), x);
    }

    /** Converts x into a "formula" if possible; otherwise, returns an Expr with a nonempty error list */
    static Expr cform(Expr x) {
        String msg;
        if (!x.errors.isEmpty())
            return x;
        else if (x.type==Type.EMPTY)
            msg = "This expression failed to be typechecked.";
        else if (!x.type.is_bool)
            msg = "This must be a formula expression.\nInstead, it has the following possible type(s):\n" + x.type;
        else
            return x;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }

    /** Converts x into a "integer expression" if possible; otherwise, returns an Expr with a nonempty error list */
    public static Expr cint(Expr x) {
        if (!x.type.is_int && Type.SIGINT2INT && x.type.intersects(SIGINT.type)) return x.cast2int();
        String msg;
        if (!x.errors.isEmpty())
            return x;
        else if (x.type==Type.EMPTY)
            msg = "This expression failed to be typechecked.";
        else if (!x.type.is_int)
            msg = "This must be an integer expression.\nInstead, it has the following possible type(s):\n"+x.type;
        else
            return x;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }

    /** Converts x into a "set or relation" if possible; otherwise, returns an Expr with a nonempty error list */
    public static Expr cset(Expr x) {
        if (x.type.size()==0 && Type.INT2SIGINT && x.type.is_int) return x.cast2sigint();
        String msg;
        if (!x.errors.isEmpty())
            return x;
        else if (x.type==Type.EMPTY)
            msg = "This expression failed to be typechecked.";
        else if (x.type.size()==0)
            msg = "This must be a set or relation.\nInstead, it has the following possible type(s):\n"+x.type;
        else
            return x;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }
}
