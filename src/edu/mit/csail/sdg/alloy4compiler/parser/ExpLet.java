package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.ArrayList;

import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;

public final class ExpLet extends Exp {

    public final ExpName left;
    public final Exp right;
    public final Exp sub;

    public ExpLet(Pos pos, ExpName left, Exp right, Exp sub) {
        super(pos);
        this.left=left;
        this.right=right;
        this.sub=sub;
    }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos.merge(left.span()).merge(right.span()).merge(sub.span()); span=p; }
        return p;
    }

    public Expr check(Context cx) throws Err {
        Expr right = cx.resolveExp(this.right.check(cx), new ArrayList<ErrorWarning>()); // TODO: warnings are discarded!
        ExprVar left = ExprVar.makeTyped(this.left.pos, this.left.name, right);
        cx.put(this.left.name, left);
        Expr sub = this.sub.check(cx);
        cx.remove(this.left.name);
        return ExprLet.make(pos, left, sub);
    }
}
