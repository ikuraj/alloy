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
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorWarning;

/**
 * Immutable; represents a LET or QUANTIFICATION variable in the AST.
 *
 * <p> <b>Invariant:</b>  type!=EMPTY => (type==expr.type && !expr.ambiguous)
 */

public final class ExprVar extends Expr {

    /** The label associated with this variable; it's used for pretty-printing and does not have to be unique. */
    public final String label;

    /** The expression that this variable is quantified over or substituted by; may be null. */
    public final Expr expr;

    /** {@inheritDoc} */
    @Override public Pos span() { return pos; }

    /** Returns true if we can determine the two expressions are equivalent; may sometimes return false. */
    @Override public boolean isSame(Expr obj) {
        while(obj instanceof ExprUnary && ((ExprUnary)obj).op==ExprUnary.Op.NOOP) obj=((ExprUnary)obj).sub;
        return this==obj;
    }

    /** {@inheritDoc} */
    @Override public void toString(StringBuilder out, int indent) {
        if (indent<0) {
            out.append(label);
        } else {
            for(int i=0; i<indent; i++) { out.append(' '); }
            out.append("Var ").append(label).append(" at position <").append(pos).append("> with type=").append(type).append('\n');
            if (expr!=null) expr.toString(out, indent+2);
        }
    }

    /** Constructs an ExprVar object */
    private ExprVar(Pos pos, String label, Type type, Expr expr) {
        super(pos, null, false, type, 0, (expr==null ? 0 : expr.weight), (expr==null ? uninitList : expr.errors));
        this.label = (label==null ? "" : label);
        this.expr = expr;
    }

    /** Preconstructed error object saying "this variable failed to be resolved" */
    private static final ErrorType uninit = new ErrorType(Pos.UNKNOWN, "This variable failed to be resolved.");

    /** Preconstructed list that contains a single error message. */
    private static final JoinableList<Err> uninitList = new JoinableList<Err>(uninit);

    /**
     * Constructs an ExprVar variable
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     */
    public static ExprVar make(Pos pos, String label) {
        return new ExprVar(pos, label, Type.EMPTY, null);
    }

    /**
     * Constructs an ExprVar variable
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param expr - the quantification/substitution expression for this variable; <b> it must already be fully resolved </b>
     */
    public static ExprVar make(Pos pos, String label, Expr expr) {
        if (expr.ambiguous) expr=expr.resolve(expr.type, null);
        return new ExprVar(pos, label, expr.type, expr);
    }

    /**
     * Constructs an ExprVar variable bound to the empty set or relation with the given type
     * @param pos - the original position in the source file (can be null if unknown)
     * @param label - the label for this variable (it is only used for pretty-printing and does not have to be unique)
     * @param type - the type; <b> it must be an unambiguous set or relation </b>
     */
    public static ExprVar make(Pos pos, String label, Type type) throws ErrorType {
        int a = type.arity();
        if (a<=0) throw new ErrorType(pos, "The variable's type must be an unambiguous set or relation.");
        Expr expr = Sig.NONE;
        while(a>1) { expr=expr.product(Sig.NONE); a--; }
        return new ExprVar(pos, label, type, expr);
    }

    /** {@inheritDoc} */
    public int getDepth() { return (expr!=null ? expr.getDepth() : 0) + 1; }

    /** {@inheritDoc} */
    @Override public Expr resolve(Type p, Collection<ErrorWarning> warns) { return this; }

    /** {@inheritDoc} */
    @Override final<T> T accept(VisitReturn<T> visitor) throws Err { return visitor.visit(this); }

    /** {@inheritDoc} */
    @Override public String getDescription() { return "<b>variable</b>: " + label + " <i>Type = " + type + "</i>"; }

    /** {@inheritDoc} */
    @Override public List<? extends Browsable> getSubnodes() { return new ArrayList<Browsable>(0); }
}
