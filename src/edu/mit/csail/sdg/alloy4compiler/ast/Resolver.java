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

import edu.mit.csail.sdg.alloy4.ErrorType;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary.Op.NOOP;

public final class Resolver {

    /** Converts x into a "formula" if possible; otherwise, returns an Expr with a nonempty error list */
    public static Expr cform(Expr x) {
        if (!x.errors.isEmpty() || x.type.is_bool) return x;
        String msg = "This must be a formula expression.\nInstead, it has the following possible type(s):\n" + x.type;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }

    /** Converts x into an "integer expression" if possible; otherwise, returns an Expr with a nonempty error list */
    public static Expr cint(Expr x) {
        if (!x.errors.isEmpty() || x.type.is_int) return x;
        if (Type.SIGINT2INT && x.type.intersects(SIGINT.type)) return x.cast2int();
        String msg = "This must be an integer expression.\nInstead, it has the following possible type(s):\n"+x.type;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }

    /** Converts x into a "set or relation" if possible; otherwise, returns an Expr with a nonempty error list */
    public static Expr cset(Expr x) {
        if (!x.errors.isEmpty() || x.type.size()>0) return x;
        if (Type.INT2SIGINT && x.type.is_int) return x.cast2sigint();
        String msg = "This must be a set or relation.\nInstead, it has the following possible type(s):\n"+x.type;
        return NOOP.make(null, x, 0, new ErrorType(x.span(), msg));
    }
}
