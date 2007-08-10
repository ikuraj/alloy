package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;

public abstract class Exp {

    public final Pos pos;

    public Exp(Pos pos) { this.pos=(pos==null ? Pos.UNKNOWN : pos); }

    public abstract Pos span();

    public abstract Expr check(Context cx) throws Err ;

    public final Exp not() {
        return new ExpUnary(null, ExprUnary.Op.NOT, this);
    }

    public final Exp ite(Exp left, Exp right) {
        return new ExpITE(null, this, left, right);
    }
}
