package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprITE;

public final class ExpITE extends Exp {

    public final Exp formula;
    public final Exp left;
    public final Exp right;

    public ExpITE(Pos pos, Exp formula, Exp left, Exp right) {
        super(pos);
        this.formula=formula;
        this.left=left;
        this.right=right;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(formula.span()).merge(left.span()).merge(right.span()); span=p; }
        return p;
    }

    public Expr check(Context cx) throws Err {
        Expr f = formula.check(cx);
        Expr a = left.check(cx);
        Expr b = right.check(cx);
        return ExprITE.make(f, a, b);
    }
}
