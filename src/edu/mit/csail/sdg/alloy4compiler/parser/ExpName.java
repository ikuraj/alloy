package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.List;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBad;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadJoin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprChoice;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;

public final class ExpName extends Exp {

    public final String name;

    public ExpName(Pos pos, String name) { super(pos); this.name=name; }

    public Pos span() { return pos; }

    private final ConstList<Expr> emptyList = ConstList.make();

    /**
     * Returns true if the function's parameters have reasonable intersection with the list of arguments.
     * <br> If args.length() > f.params.size(), the extra arguments are ignored by this check
     * <br> <b>Precondition</b>: args.length() >= f.params.size()
     */
    private static boolean applicable(Func f, List<Expr> args) {
        int i=0;
        for(ExprVar d:f.params) {
            Type dt=d.type;
            Type arg=args.get(i).type;
            i++;
            // The reason we don't say (arg.arity()!=d.value.type.arity())
            // is because the arguments may not be fully resolved yet.
            if (!arg.hasCommonArity(dt)) return false;
            if (arg.hasTuple() && dt.hasTuple() && !arg.intersects(dt)) return false;
        }
        return true;
    }

    /**
     * Construct the result of calling "ch" with the given list of arguments
     * <br> <b>Precondition</b>: (ch instanceof Expr) or (ch instanceof Func)
     * <br> <b>Precondition</b>: cset(args[i]) for all i
     * @return EBadCall, or EBadJoin, or a possibly well-typed Expr
     */
    static Expr makeCallOrJoin(Pos pos, Object ch, ConstList<Expr> args, Expr THISorNULL) {
        Expr ans;
        int i=0, n=args.size();
        if (ch instanceof Expr) {
            ans = (Expr)ch;
        } else {
            Func f = (Func)ch;
            i = f.params.size();
            if (i==n+1 && THISorNULL!=null && THISorNULL.type!=null && THISorNULL.type.hasArity(1)) {
                // If we're inside a sig, and there is a unary variable bound to "this",
                // we should consider it as a possible FIRST ARGUMENT of a fun/pred call
                ConstList<Expr> args2=Util.prepend(args, THISorNULL);
                if (applicable(f,args2)) return ExprCall.make(pos, f, args2, 1);
            }
            if (i>n) return new ExprBadCall(pos, f, args);
            if (!applicable(f,args)) return new ExprBadCall(pos, f, args.subList(0,i));
            ans = ExprCall.make(pos, f, args.subList(0,i), 0);
        }
        for(; i<n; i++) {
            Expr x = args.get(i);
            // TODO: you lost the original JOIN's pos
            if (x.type.join(ans.type).size()==0) return new ExprBadJoin(x.span().merge(ans.span()), x, ans);
            ans = ExprBinary.Op.JOIN.make(x.span().merge(ans.span()), x, ans);
        }
        return ans;
    }

    public Expr check(Context cx) {
        TempList<Expr> objects = new TempList<Expr>();
        Set<Object> choices = cx.resolve(pos, name);
        if (choices.size()==0) {
            return new ExprBad(pos, ExprVar.hint(pos, name));
        }
        // If we're inside a sig, and there is a unary variable bound to "this", we should
        // consider it as a possible FIRST ARGUMENT of a fun/pred call
        Expr THIS = (cx.rootsig!=null) ? cx.get("this",null) : null;
        for(Object ch:choices) {
            Expr x=makeCallOrJoin(pos, ch, emptyList, THIS);
            objects.add(x);
        }
        return ExprChoice.make(pos, objects.makeConst());
    }
}
