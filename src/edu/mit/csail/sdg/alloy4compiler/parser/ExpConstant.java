package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant.Op;

public final class ExpConstant extends Exp {

    public final Op op;
    public final int num;

    public ExpConstant(Pos pos, Op op, int num) { super(pos); this.op=op; this.num=num; }

    public Pos span() { return pos; }

    public Expr check(Context cx) { return op.make(pos, num); }
}
