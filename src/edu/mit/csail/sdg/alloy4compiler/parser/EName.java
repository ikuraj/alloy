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

import java.util.Set;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCustom;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext;

/**
 * Immutable; represents an identifier in the AST.
 *
 * <p> When type==null, that means this identifier has not been typechecked and so we don't know what it refers to.
 *
 * <p> When type!=null, that means this identifier is a Quantified Variable, or a Function/Predicate parameter, or a Let-bound variable.
 * (In all other cases, the typechecker will replace this node with the appropriate node that it should be replaced with.
 * For example, if the name in fact refers to an existing Sig, then the typechecker will replace this node with an ExprSig node)
 * In summary: when type!=null, then this node must be referring to a Quantified Variable, a Function/Predicate parameter, or a Let-bound variable.)
 *
 * <p> <b>Invariant:</b>  name!="" && type==null
 */

final class EName extends ExprCustom {

    /** The name. */
    final String name;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() { return pos; }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(name);
        } else {
            for(int i=0; i<indent; i++) out.append(' ');
            out.append(name).append(" with type=").append(type).append('\n');
        }
    }

    /** Returns a String representation. */
    @Override public String toString() { return name; }

    /**
     * Constructs an EName expression where we don't know what it references.
     *
     * @param pos - the original position in the file
     * @param name - the identifier
     *
     * @throws ErrorSyntax if name==""
     */
    private EName(Pos pos, String name) throws Err {
        super(pos, null, 0, 0); // weight can be set to anything (such as 0), since a EName will never be in the ultimate Expr
        this.name=name;
        if (name.length()==0) throw new ErrorSyntax(pos, "Variable name cannot be empty.");
    }

    /**
     * Constructs an EName expression where we don't know what it references, and don't know its type either.
     * <p> NOTE: the resulting expression will always be un-typechecked; you need to explicitly typecheck it
     *
     * @param pos - the original position in the file
     * @param name - the identifier
     *
     * @throws ErrorSyntax if name==""
     */
    static EName make(Pos pos, String name) throws Err {
        return new EName(pos, name);
    }

    /**
     * Convenience method that throws a syntax error exception saying the name "n" can't be found.
     * (In particular, if n is an old Alloy3 keyword, then
     * the message will tell the user to consult the documentation
     * on how to migrate old models to use the new syntax.)
     *
     * @param pos - the original position in the file that triggered the error
     * @param name - the identifier
     */
    static void hint (Pos pos, String name) throws ErrorSyntax {
        String msg="The name \""+name+"\" cannot be found.";
        if ("exh".equals(name) || "exhaustive".equals(name) || "part".equals(name) || "partition".equals(name))
            msg=msg+" If you are migrating from Alloy 3, please see Help->QuickGuide on how to translate models that use the \""+name+"\" keyword.";
        throw new ErrorSyntax(pos, msg);
    }

    /** Typechecks an EName object (first pass). */
    @Override public Expr check(final TypeCheckContext cxx) throws Err {
        Context cx=(Context)cxx;
        TempList<Expr> objects=new TempList<Expr>();
        Type t=null;
        if (cx.has(name)) {
            // This handles an EName that represents a LetVar/QuantVar/FunctionParameter
            Expr ex=cx.get(name);
            t=ex.type;
            objects.add(ex);
        } else {
            // This handles an EName that needs to be resolved
            Set<Object> choices = cx.resolve(pos,name);
            if (choices.size()==0) hint(pos,name);
            ConstList<Expr> args=ConstList.make();
            // If we're inside a sig, and there is a unary variable bound to "this", we should
            // consider it as a possible FIRST ARGUMENT of a fun/pred call
            Expr THIS = (cx.rootsig!=null) ? cx.get("this") : null;
            for(Object ch:choices) {
                Expr x=EBadCall.make(pos, ch, args, THIS);
                if (x!=null) { objects.add(x); if (x.type!=null) t=x.type.merge(t); }
            }
        }
        return EChoice.make(pos, objects.makeConst());
    }

    /** Typechecks an EName object (second pass). */
    @Override public Expr check(final TypeCheckContext cx, Type t) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }
}
