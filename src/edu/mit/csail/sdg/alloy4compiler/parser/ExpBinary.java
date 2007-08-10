package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary.Op;

public final class ExpBinary extends Exp {

    public final Op op;
    public final Exp left;
    public final Exp right;

    public ExpBinary(Pos pos, Op op, Exp left, Exp right) {
        super(pos);
        this.op=op;
        this.left=left;
        this.right=right;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(left.span()).merge(right.span()); span=p; }
        return p;
    }

    public Expr check(Context cx) throws Err {
        Expr a=left.check(cx);
        Expr b=right.check(cx);
        return op.make(pos, a, b);
    }
}
