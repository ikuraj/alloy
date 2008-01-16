/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4compiler.ast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.ConstList;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Type.EMPTY;

/**
 * Immutable; represents an unresolved node that has several possibilities.
 */

public final class ExprChoice extends Expr {

    /** The unmodifiable list of Expr(s) from that this ExprChoice can refer to. */
    public final ConstList<Expr> choices;

    /** Caches the span() result. */
    private Pos span=null;

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public Pos span() {
        Pos p=span;
        if (p==null) {
            p=pos;
            for(Expr a:choices) p=p.merge(a.span());
            span=p;
        }
        return p;
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            // Each choice's textual form is probably similar, so the first one would do
            //choices.get(0).toString(out,indent);
            //
            out.append("<");
            for(Expr e:choices) { e.toString(out,-1); out.append(";"); }
            out.append(">");
            //
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append(""+choices.size()+" choices with combined type=").append(type).append('\n');
        }
    }

    //============================================================================================================//

    /** Generate an appropriate error message in the case where there are no legal choices. */
    private static Err complain(Pos pos, ConstList<Expr> choices) {
        StringBuilder sb=new StringBuilder("Name cannot be resolved; possible incorrect function/predicate call; " +
           "perhaps you used ( ) when you should have used [ ]\n");
        for(Expr x:choices) {
            pos = pos.merge(x.span());
            if (x instanceof ExprBadCall || x instanceof ExprBadJoin) sb.append('\n').append(x.errors.get(0).msg);
        }
        return new ErrorType(pos, sb.toString());
    }

    //============================================================================================================//

    /** Constructs an ExprChoice node. */
    private ExprChoice(Pos pos, ConstList<Expr> choices, Type type, long weight) {
        super(pos, null, true, type, 0, weight, emptyListOfErrors.appendIfNotNull(type==EMPTY ? complain(pos,choices) : null));
        this.choices = choices;
    }

    //============================================================================================================//

    /** Construct an ExprChoice node. */
    public static Expr make(Pos pos, ConstList<Expr> choices) {
        if (choices.size()==0) return new ExprBad(pos, "", new ErrorType(pos, "This expression failed to be typechecked."));
        if (choices.size()==1 && choices.get(0).errors.isEmpty()) return choices.get(0); // Shortcut
        Type type=EMPTY;
        boolean first=true;
        long weight=0;
        // TODO: what should the weight be?
        for(Expr x:choices) {
            type=x.type.merge(type);
            if (first || weight>x.weight) if (x.type!=EMPTY) { weight=x.weight; first=false; }
        }
        return new ExprChoice(pos, choices, type, weight);
    }

    //============================================================================================================//

    private Expr resolveHelper(boolean firstPass, Type t, List<Expr> choices, Collection<ErrorWarning> warns) {
        List<Expr> match=new ArrayList<Expr>(choices.size());
        // We first prefer exact matches
        for(Expr ch:choices) {
            Type tt=ch.type;
            if ((t.is_int && tt.is_int) || (t.is_bool && tt.is_bool) || t.intersects(tt)) match.add(ch);
        }
        // If none, we try any legal matches
        if (match.size()==0) {
            for(Expr ch:choices) if (ch.type.hasCommonArity(t)) match.add(ch);
        }
        // If none, we try sigint->int
        if (match.size()==0 && Type.SIGINT2INT && t.is_int) {
            for(Expr ch:choices) if (ch.type.intersects(SIGINT.type)) match.add(ch.cast2int());
        }
        // If none, we try int->sigint
        if (match.size()==0 && Type.INT2SIGINT && t.hasArity(1)) {
            for(Expr ch:choices) if (ch.type.is_int) match.add(ch.cast2sigint());
        }
        // If too many, then keep the choices with the smallest weight
        if (match.size()>1) {
            List<Expr> newmatch=new ArrayList<Expr>(match.size());
            long w=0;
            for(Expr x:match) {
                if (newmatch.size()==0 || x.weight<w) { newmatch.clear(); newmatch.add(x); w=x.weight; }
                else if (x.weight==w) { newmatch.add(x); }
            }
            match=newmatch;
            // If still too many, but this is the first pass, then try to resolve them all and try again
            if (firstPass && match.size()>1) {
                newmatch = new ArrayList<Expr>(match.size());
                for(Expr x:match) newmatch.add(x.resolve(t, sink));
                return resolveHelper(false, t, newmatch, warns);
            }
        }
        // If we are down to exactly 1 match, return it
        if (match.size()==1) return match.get(0).resolve(t, warns);
        // Otherwise, complain!
        String txt;
        if (match.size()>1) {
            txt="\nThe expression is ambiguous due to multiple matches:";
        } else {
            txt="\nThe expression cannot be resolved; its relevant type does not intersect with any of the following candidates:";
            match.clear();
            match.addAll(choices);
        }
        StringBuilder msg=new StringBuilder(txt);
        for(Expr ch:match) { msg.append("\n\n"); ch.toString(msg,-1); msg.append(" (type: ").append(ch.type).append(")"); }
        Pos span=span();
        return new ExprBad(span, toString(), new ErrorType(span, msg.toString()));
    }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type t, Collection<ErrorWarning> warns) {
        if (errors.size()>0) return this; else return resolveHelper(true, t, choices, warns);
    }

    //============================================================================================================//

    /** {@inheritDoc} */
    @Override Object accept(VisitReturn visitor) throws Err {
        if (!errors.isEmpty()) throw errors.get(0);
        throw new ErrorType(span(), "This expression failed to be resolved.");
    }
}
