package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.Collection;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;

public final class ExpBuiltin extends Exp {

    public ConstList<Exp> args;

    public ExpBuiltin(Pos pos, Collection<Exp> args) { super(pos); this.args = ConstList.make(args); }

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=pos; for(Exp a:args) p=p.merge(a.span()); span=p; }
        return p;
    }

    public Expr check(Context cx) throws Err {
        TempList<Expr> args = new TempList<Expr>(this.args.size());
        for(int i=0; i<this.args.size(); i++) {
            Expr x=this.args.get(i).check(cx);
            args.add(x);
        }
        return ExprBuiltin.makeDISJOINT(pos, args.makeConst());
    }
}
