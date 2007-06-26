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

import java.util.ArrayList;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCustom;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.TypeCheckContext;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;

/**
 * Immutable; represents an unresolved node that has several possibilities.
 */

final class EChoice extends ExprCustom {

    /** The unmodifiable list of object(s) that this name can refer to; this list is never empty. */
    final ConstList<Expr> choices;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object spanning the entire expression. */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr a:choices) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    /** Produce a String representation with the given level of indentation. */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            choices.get(0).toString(out,indent); // Each choice's textual form is probably similar, so the first one would do
        } else {
            for(int i=0; i<indent; i++) out.append(' ');
            out.append(""+choices.size()+" choices with combined type=").append(type).append('\n');
        }
    }

    /**
     * Construct an EChoice node.
     * <br> Precondition: this method must only be called by the typechecker during first pass
     */
    static Expr make(Pos pos, ConstList<Expr> choices) throws Err {
        if (choices.size()==0) throw new ErrorAPI("EChoice cannot start with an empty list.");
        TempList<Expr> nonemptychoices = new TempList<Expr>(choices.size());
        Type type=null;
        for(Expr x:choices) {
            Type temp=x.type;
            if (temp==null) continue;
            type=temp.merge(type);
            if (temp.is_bool || temp.is_int || temp.hasTuple()) nonemptychoices.add(x);
        }
        if (nonemptychoices.size()>0) choices=nonemptychoices.makeConst();
        if (choices.size()==1 && type!=null) return choices.get(0);
        long weight=choices.get(0).weight;
        for(int i=1; i<choices.size(); i++) if (weight > choices.get(i).weight) weight = choices.get(i).weight;
        return new EChoice(pos, choices, type, weight);
    }

    private void complain() throws Err {
        StringBuilder sb=new StringBuilder("Name cannot be resolved; possible incorrect function/predicate call; perhaps you used ( ) when you should have used [ ]\n");
        for(Expr x:choices) {
          if (x instanceof EBadCall) {
            EBadCall xx=(EBadCall)x;
            sb.append("\nThis cannot be a correct call to ").append(xx.fun);
            sb.append(xx.fun.params.size()==0 ? ".\nIt has no parameters,\n" : ".\nThe parameters are\n");
            for(ExprVar v:xx.fun.params) {
              sb.append("  ").append(v.label).append(": ").append(v.type).append('\n');
            }
            sb.append(xx.args.size()==0 || xx.fun.params.size()==0 ? "so the arguments cannot be empty.\n" : "so the arguments cannot be\n");
            for(Expr v:xx.args.subList(0,xx.fun.params.size())) {
              sb.append("  ");
              v.toString(sb,-1);
              sb.append(" (type = ").append(v.type).append(")\n");
            }
          }
          else if (x instanceof EBadJoin) {
            EBadJoin xx=(EBadJoin)x;
            sb.append("\nThis cannot be a legal relational join where\nleft hand side is ");
            xx.left.toString(sb,-1);
            sb.append(" (type = ").append(xx.left.type).append(")\nright hand side is ");
            xx.right.toString(sb,-1);
            sb.append(" (type = ").append(xx.right.type).append(")\n");
          }
        }
        throw new ErrorType(span(), sb.toString());
    }

    /** Constructs an EChoice node. */
    private EChoice(Pos pos, ConstList<Expr> choices, Type type, long weight) throws Err {
        super(pos, type, 0, weight);
        this.choices=ConstList.make(choices);
        if (type==null) complain();
    }

    /** Typechecks an EChoice object (first pass). */
    @Override public Expr check(final TypeCheckContext cx) throws Err {
        throw new ErrorFatal("Internal typechecker invariant violated.");
    }

    /** Typechecks an EChoice object (second pass). */
    @Override public Expr check(final TypeCheckContext cx, Type t) throws Err {
        List<Expr> match=new ArrayList<Expr>(choices.size());
        // We first prefer exact matches
        for(Expr ch:choices) {
            Type tt=ch.type;
            if (tt!=null) if ((t.is_int && tt.is_int) || (t.is_bool && tt.is_bool) || t.intersects(tt)) match.add(ch);
        }
        // If none, we try any legal matches
        if (match.size()==0) for(Expr ch:choices) {
            if (ch.type!=null) if (ch.type.hasCommonArity(t)) match.add(ch);
        }
        // If none, we try sigint->int
        if (match.size()==0 && TypeCheckContext.auto_sigint2int && t.is_int) for(Expr ch:choices) {
            if (ch.type!=null) if (ch.type.intersects(SIGINT.type)) match.add(ch.cast2int());
        }
        // If none, we try int->sigint
        if (match.size()==0 && TypeCheckContext.auto_int2sigint && t.arity()==1) for(Expr ch:choices) {
            if (ch.type!=null) if (ch.type.is_int) match.add(ch.cast2sigint());
        }
        // If there are multiple matches, then keep only the ones with the smallest weight
        if (match.size()>1) {
            List<Expr> newmatch=new ArrayList<Expr>();
            long w=0;
            for(Expr x:match) {
                if (newmatch.size()==0 || x.weight<w) { newmatch.clear(); newmatch.add(x); w=x.weight; }
                else if (x.weight==w) { newmatch.add(x); }
            }
            match=newmatch;
        }
        // Finally, complain if there are more than one match, or zero match
        if (match.size()==1) return cx.check(match.get(0), t);
        StringBuilder msg=null;
        if (match.size()>1)
            msg=new StringBuilder("\nThe expression is ambiguous due to multiple matches:");
        else
            msg=new StringBuilder("\nThe expression cannot be resolved; its relevant type does not intersect with any of the following candidates:");
        for(Expr ch:match) { msg.append("\n\n"); ch.toString(msg,-1); msg.append(" (type: ").append(ch.type).append(")"); }
        throw new ErrorType(span(), msg.toString());
    }
}
