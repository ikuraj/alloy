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

import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.Env;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Resolver;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Type.ProductType;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;

/**
 * This class computes both the bounding type and the relevant type.
 *
 * <p>
 * During the first pass, this typechecker
 * computing the "bounding type" of parent expressions
 * based on the bounding types of its children.
 * During this phase, if an expression could not possibly have legal bounding types, we throw an exception.
 *
 * <p>
 * During the second pass, this typechecker
 * computes the "relevant type" top-down:
 * the type of the parent expression is used to determine
 * whether the subexpressions are relevant or not.
 * This second pass is needed to resolve field/function/predicate overloading,
 * and can also detect some irrelevant expressions.
 *
 * <p>
 * In general, during the first pass, we allow name overloading to propagate.
 * This enables more context-sensitivity for doing precise diambiguation.
 * However, there are a few places we force the name to be fully resolved
 * before proceeding:
 * <br> (1) Parameters to a function/predicate call
 * <br> (2) let x=a... (here we will always fully resolve a)
 * <br> (3) quantifer x:a|b (here we first fully resolve a, and then fully resolve b)
 */

public final class Context {

    // field&sig!=null    else if sig!=null     else if fun!=null     allelse
    public boolean rootfield=false;
    public Sig rootsig=null;
    public boolean rootfun=false;
    private Module rootmodule=null;

    /** This maps local names (eg. let/quantification variables and function parameters) to the objects they refer to. */
    private final Env<String,Expr> env=new Env<String,Expr>();

    /** Returns true if the name is in the current lexical scope. */
    public final boolean has(String name) {
        return env.has(name);
    }

    /** Returns the expression corresbonding to the given name, or returns null if the name is not in the current lexical scope. */
    public final Expr get(String name, Pos pos) {
        Expr ans = env.get(name);
        if (ans instanceof ExprVar) return ExprUnary.Op.NOOP.make(pos,ans); else return ans;
    }

    /** Associates the given name with the given expression in the current lexical scope. */
    public final void put(String name, Expr value) { env.put(name,value); }

    /** Removes the latest binding for the given name from the current lexical scope. */
    public final void remove(String name) { env.remove(name); }

    public Context() {
        this.rootmodule = null;
    }

    public Context(Module rootModule) {
        this.rootmodule = rootModule;
    }

    public Set<Object> resolve(Pos pos, String name) {
        Expr match = env.get(name);
        if (match!=null || name.equals("Int") || name.equals("univ") || name.equals("seq/Int") || name.equals("none") || name.equals("iden")) {
            HashSet<Object> ans = new HashSet<Object>(1);
            if (match!=null) ans.add(ExprUnary.Op.NOOP.make(pos, match));
            else if (name.charAt(0)=='I') ans.add(ExprUnary.Op.NOOP.make(pos, Sig.SIGINT));
            else if (name.charAt(0)=='u') ans.add(ExprUnary.Op.NOOP.make(pos, Sig.UNIV));
            else if (name.charAt(0)=='s') ans.add(ExprUnary.Op.NOOP.make(pos, Sig.SEQIDX));
            else if (name.charAt(0)=='n') ans.add(ExprUnary.Op.NOOP.make(pos, Sig.NONE));
            else if (name.charAt(0)=='i') ans.add(ExprUnary.Op.NOOP.make(pos, ExprConstant.IDEN));
            return ans;
        }
        if (rootmodule==null) return new HashSet<Object>(1);
        return rootmodule.populate(rootfield, rootsig, rootfun, pos, name, get("this",pos));
    }

    public Context dupWithEmptyEnvironment() {
        Context cx = new Context(rootmodule);
        cx.rootfield = rootfield;
        cx.rootsig = rootsig;
        cx.rootfun = rootfun;
        return cx;
    }


    /**
     * Helper method that throws a type error if x cannot possibly have formula type.
     * <p> <b>Return</b> x if it can have formula type
     *
     * @throws ErrorType if x does not have formula type
     */
    static final ErrorType ccform(Expr x) {
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
    static final Expr cint(Expr x) {
        if (x.type!=null && !x.type.is_int && Type.SIGINT2INT && x.type.intersects(SIGINT.type)) return x.cast2int();
        return x;
    }

    static final ErrorType ccint(Expr x) {
        if (x.type==null)
            return new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (!x.type.is_int)
            return new ErrorType(x.span(), "This must be an integer expression.\nInstead, it has the following possible type(s):\n"+x.type);
        return null;
    }

    /**
     * Helper method that throws a type error if x cannot possibly have set/relation type.
     *
     * <p>  <b>Return</b> x if it can have set/relation type
     * <br> <b>Return</b> x.cast2sigint() if x can be casted to a set/relation type, and auto casting is enabled
     *
     * @throws ErrorType if x does not and cannot be casted to have set/relation type
     */
    static final Expr cset(Expr x) {
        if (x.type!=null && x.type.size()==0 && Type.INT2SIGINT && x.type.is_int) return x.cast2sigint();
        return x;
    }

    static final ErrorType ccset(Expr x) {
        if (x.type==null)
            return new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (x.type.size()==0)
            return new ErrorType(x.span(), "This must be a set or relation.\nInstead, it has the following possible type(s):\n"+x.type);
        return null;
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be a set/relation expression
     * NOTE: result may have type==null or even type!=SET
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public static final Expr resolveExp(Expr x, Collection<ErrorWarning> warns) throws Err {
        Type t=x.type;
        if (t!=null) {
            if (t.arity()<0) {
                // If we can have multiple arities, but some of them are empty, then remove the empty ones.
                Type tt=(t.is_bool ? (t.is_int ? Type.INTANDFORMULA: Type.FORMULA) : (t.is_int ? Type.INT : Type.EMPTY));
                for(ProductType r:t) if (!r.isEmpty()) tt=tt.merge(r);
                if (tt.size()>0) t=tt;
            }
            x=Resolver.resolve(x, t, warns);
        }
        return x;
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be a set/relation expression
     * NOTE: result may have type==null or even type!=SET
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public static final Expr resolveExpSet(Expr x, Collection<ErrorWarning> warns) throws Err {
        x=Resolver.cset(x);
        if (x.type!=null) {
            Type t=Type.removesBoolAndInt(x.type);
            if (t.arity()<0) {
                // If we can have multiple arities, but some of them are empty, then remove the empty ones.
                Type tt=Type.EMPTY;
                for(ProductType r:t) if (!r.isEmpty()) tt=tt.merge(r);
                if (tt.size()>0) t=tt;
            }
            x=Resolver.resolve(x, t, warns);
        }
        return x;
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be an integer expression
     * NOTE: result may have type==null or even type!=INT
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public static final Expr resolveExpInt(Expr x, Collection<ErrorWarning> warns) throws Err {
        x=Resolver.cint(x);
        if (x.type!=null) x=Resolver.resolve(x, Type.INT, warns);
        return x;
    }

    /**
     * Typecheck a node bottom-up and then top-down in order to fully resolve it to be a formula
     * NOTE: result may have type==null or even type!=FORMULA
     * @return a possibly-copied version of X that is identical to X, except that all the type information are filled in
     * @throws ErrorType if the node or any of its subnodes cannot be fully resolved unambiguously
     */
    public static final Expr resolveExpFormula(Expr x, Collection<ErrorWarning> warns) throws Err {
        if (x.type!=null) x=Resolver.resolve(x, Type.FORMULA, warns);
        return x;
    }

}
