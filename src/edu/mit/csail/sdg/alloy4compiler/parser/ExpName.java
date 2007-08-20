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

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBad;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprChoice;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;

/** Immutable; represents an unresolved name in the AST. */

final class ExpName extends Exp {

    /** The name. */
    public final String name;

    /** Constructs an ExpName object. */
    public ExpName(Pos pos, String name) {
        super(pos);
        this.name=name;
    }

    /** {@inheritDoc} */
    public Pos span() {
        return pos;
    }

    /** This caches an unmodifiable empty list of Expr objects. */
    private static final ConstList<Expr> emptyList = ConstList.make();

    /** {@inheritDoc} */
    public Expr check(Context cx, List<ErrorWarning> warnings) {
        Collection<Object> choices = cx.resolve(pos, name);
        TempList<Expr> objects = new TempList<Expr>(choices.size());
        Expr THIS = (cx.rootsig!=null) ? cx.get("this",pos) : null;
        for(Object ch:choices) {
            if (ch instanceof Expr) {
                objects.add((Expr)ch);
            } else if (ch instanceof Func) {
                Func f = (Func)ch;
                int fn = f.params.size();
                if (fn==1 && THIS!=null && THIS.type.hasArity(1) && f.params.get(0).type.intersects(THIS.type)) {
                    // If we're inside a sig, and there is a unary variable bound to "this",
                    // we should consider it as a possible FIRST ARGUMENT of a fun/pred call
                    objects.add(ExprCall.make(pos, null, f, Util.asList(THIS), 1));
                    continue;
                }
                objects.add(fn==0 ? ExprCall.make(pos,null,f,emptyList,0) : ExprBadCall.make(pos,null,f,emptyList));
            }
        }
        if (objects.size()==0) return new ExprBad(pos, name, hint(pos, name));
        return ExprChoice.make(pos, objects.makeConst());
    }

    /**
     * Convenience method that returns a syntax error exception saying the name "n" can't be found.
     * (In particular, if n is an old Alloy3 keyword, then the message will tell the user to consult
     * the documentation on how to migrate old models to use the new syntax.)
     *
     * @param pos - the original position in the file that triggered the error
     * @param name - the identifier
     */
    static ErrorSyntax hint (Pos pos, String name) {
        String msg="The name \""+name+"\" cannot be found.";
        if ("exh".equals(name) || "exhaustive".equals(name) || "part".equals(name) || "partition".equals(name))
            msg=msg+" If you are migrating from Alloy 3, please see Help->QuickGuide on how to translate models that use the \""
            +name+"\" keyword.";
        return new ErrorSyntax(pos, msg);
    }
}
