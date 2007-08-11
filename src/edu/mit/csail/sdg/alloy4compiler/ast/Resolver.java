package edu.mit.csail.sdg.alloy4compiler.ast;

import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;

public final class Resolver {

    /**
     * Helper method that throws a type error if x cannot possibly have any legal type or if x is ambiguous.
     * Otherwise it returns x.
     *
     * @throws ErrorType if X is not already unambiguously typechecked
     */
    public static final Expr unambiguous(final Expr x) throws Err {
        if (x.errors.size()>0) throw x.errors.get(0);
        final Type t=x.type;
        if (t==null || (!t.is_bool && !t.is_int && t.size()==0)) {
            // TODO
            StringBuilder sb = new StringBuilder();
            x.toString(sb, 0);
            System.err.println(sb.toString());
            System.err.flush();
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
    public static final Expr addOne(Expr x) {
        //unambiguous(x);
        if (x instanceof ExprUnary) switch(((ExprUnary)x).op) {
            case SETOF: case ONEOF: case LONEOF: case SOMEOF: return x;
        }
        return (x.type==null || x.type.arity()!=1) ? x : ExprUnary.Op.ONEOF.make(x.span(), x);
    }

    /**
     * Helper method that throws a type error if x cannot possibly have formula type.
     * <p> <b>Return</b> x if it can have formula type
     *
     * @throws ErrorType if x does not have formula type
     */
    public static final ErrorType ccform(Expr x) {
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
    public static final Expr cint(Expr x) {
        if (x.type!=null && !x.type.is_int && Type.SIGINT2INT && x.type.intersects(SIGINT.type)) return x.cast2int();
        return x;
    }

    public static final ErrorType ccint(Expr x) {
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
    public static final Expr cset(Expr x) {
        if (x.type!=null && x.type.size()==0 && Type.INT2SIGINT && x.type.is_int) return x.cast2sigint();
        return x;
    }

    public static final ErrorType ccset(Expr x) {
        if (x.type==null)
            return new ErrorType(x.span(), "This expression failed to be typechecked.");
        if (x.type.size()==0)
            return new ErrorType(x.span(), "This must be a set or relation.\nInstead, it has the following possible type(s):\n"+x.type);
        return null;
    }

}
