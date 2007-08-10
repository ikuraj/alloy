package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary.Op;

public final class ExpUnary extends Exp {

    public final Op op;
    public final Exp sub;

    public ExpUnary(Pos pos, Op op, Exp sub) {
        super(pos);
        this.op=op;
        this.sub=sub;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(sub.span()); span=p; }
        return p;
    }

    public Expr check(Context cx) throws Err {
        Expr sub = this.sub.check(cx);
        return op.make(pos, sub);
    }
}
