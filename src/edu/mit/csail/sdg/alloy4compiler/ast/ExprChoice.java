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

package edu.mit.csail.sdg.alloy4compiler.ast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;

/**
 * Immutable; represents an unresolved node that has several possibilities.
 */

public final class ExprChoice extends Expr {

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

    private static Err complain(Pos pos, ConstList<Expr> choices) {
        StringBuilder sb=new StringBuilder("Name cannot be resolved; possible incorrect function/predicate call; perhaps you used ( ) when you should have used [ ]\n");
        for(Expr x:choices) if (x instanceof ExprBadCall || x instanceof ExprBadJoin) sb.append('\n').append(x.errors.get(0).msg);
        for(Expr a:choices) pos=pos.merge(a.span());
        return new ErrorType(pos, sb.toString());
    }

    /** Constructs an EChoice node. */
    @SuppressWarnings("unchecked")
    private ExprChoice(Pos pos, ConstList<Expr> choices, Type type, long weight, JoinableList<Err> errors) {
        super(pos, type, 0, weight, errors);
        this.choices = choices;
    }

    /**
     * Construct an EChoice node.
     * <br> Precondition: choices.size()>0
     * <br> Precondition: this method must only be called by the typechecker during first pass
     */
    public static Expr make(Pos pos, ConstList<Expr> choices) {
        TempList<Expr> nonemptychoices = new TempList<Expr>(choices.size());
        Type type=null;
        for(Expr x:choices) {
            Type xt=x.type;
            if (xt!=null) { type=xt.merge(type); if (xt.is_bool || xt.is_int || xt.hasTuple()) nonemptychoices.add(x); }
        }
        if (nonemptychoices.size()>0) choices=nonemptychoices.makeConst();
        if (choices.size()==1 && type!=null) return choices.get(0);
        long weight=choices.get(0).weight;
        for(int i=1; i<choices.size(); i++) if (weight > choices.get(i).weight) weight = choices.get(i).weight;
        JoinableList<Err> errors = JoinableList.emptylist();
        if (type==null) errors = errors.append(complain(pos, choices));
        return new ExprChoice(pos, choices, type, weight, errors);
    }

    /** Typechecks an EChoice object (second pass). */
    @Override public Expr check(Type t, Collection<ErrorWarning> warns) throws Err {
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
        if (match.size()==0 && Type.SIGINT2INT && t.is_int) for(Expr ch:choices) {
            if (ch.type!=null) if (ch.type.intersects(SIGINT.type)) match.add(ch.cast2int());
        }
        // If none, we try int->sigint
        if (match.size()==0 && Type.INT2SIGINT && t.arity()==1) for(Expr ch:choices) {
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
        if (match.size()==1) return match.get(0).check(t, warns);
        StringBuilder msg=null;
        if (match.size()>1)
            msg=new StringBuilder("\nThe expression is ambiguous due to multiple matches:");
        else
            msg=new StringBuilder("\nThe expression cannot be resolved; its relevant type does not intersect with any of the following candidates:");
        for(Expr ch:match) { msg.append("\n\n"); ch.toString(msg,-1); msg.append(" (type: ").append(ch.type).append(")"); }
        throw new ErrorType(span(), msg.toString());
    }

    /**
     * Accepts the return visitor by immediately throwing an exception.
     * This is because the typechecker should have replaced/removed this node.
     */
    @Override final Object accept(VisitReturn visitor) throws Err {
        throw new ErrorAPI("The internal typechecker failed to simplify custom expressions:\n"+this);
    }
}
