package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br/> &nbsp; &nbsp; &nbsp; (all    &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br/> &nbsp; &nbsp; &nbsp; (no     &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br/> &nbsp; &nbsp; &nbsp; (lone   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/> &nbsp; &nbsp; &nbsp; (one    &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/> &nbsp; &nbsp; &nbsp; (some   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/> &nbsp; &nbsp; &nbsp; (sum    &nbsp;       a,b:t, c,d:v &nbsp; | expression)
 * <br/> &nbsp; &nbsp; &nbsp; {a,b:t, &nbsp; c,d:v &nbsp; | &nbsp; formula}
 * <br/> &nbsp; &nbsp; &nbsp; {a,b:t, &nbsp; c,d:v}
 *
 * <br/>
 * <br/> Invariant: op!=null
 * <br/> Invariant: op!=Op.NO
 *       <i> (since ExprQuant.Op.make() would desugar this) </i>
 * <br/> Invariant: op!=Op.ONE
 *       <i> (since ExprQuant.Op.make() would desugar this) </i>
 * <br/> Invariant: op!=Op.COMPREHENSION => list.size()==1
 *       <i> (since ExprQuant.Op.make() would desugar this) </i>
 * <br/> Invariant: op==Op.COMPREHENSION => list.size()>=1
 * <br/> Invariant: all x:list | x!=null
 * <br/> Invariant: count = (sum x:list | x.names.size())
 * <br/> Invariant: sub.mult == 0
 *
 * @author Felix Chang
 */

public final class ExprQuant extends Expr {

    /**
     * Accepts the return visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitReturn
     */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.accept(this);
    }

    /**
     * Accepts the typecheck visitor bottom-up.
     * @see edu.mit.csail.sdg.alloy4.VisitTypechecker
     */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.accept(this);
    }

    /**
     * Accepts the typecheck visitor top-down.
     * @see edu.mit.csail.sdg.alloy4.VisitTypechecker
     */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.accept(this,type);
    }

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** The unmodifiable list of variable declarations. */
    public final List<VarDecl> list;

    /**
     * The number of variables.
     *
     * Note that this may be larger than the number of variable declarations,
     * since each variable declaration contains one or more variables.
     */
    public final int count;

    /** The body of the quantified expression. */
    public final Expr sub;

    /**
     * Constructs a new quantified expression.
     *
     * @param p - the original position in the file
     * @param o - the operator
     * @param l - the list of variable declarations
     * @param s - the body of the quantified expression
     * @param t - the type (null if this expression has not been typechecked)
     *
     * @throws ErrorInternal if p==null, l==null, l.size()==0, s==null, or s.mult!=0
     * @throws ErrorInternal if one of the VarDecl is null
     * @throws ErrorInternal if (o==Op.NO || o==Op.ONE) since ExprQuant.Op.make() desugars them
     * @throws ErrorInternal if (o!=Op.COMPREHENSION && l.size()!=1) since ExprQuant.Op.make() desguars them
     */
    private ExprQuant(Pos p, Op o, List<VarDecl> l, Expr s, Type t) {
        super(p,t,0);
        op=o;
        list=Collections.unmodifiableList(new ArrayList<VarDecl>(nonnull(l)));
        for(int i=0; i<list.size(); i++) nonnull(list.get(i));
        sub=nonnull(s);
        count=VarDecl.nameCount(list);
        if (list.size()==0)
            throw internalError("The list of declarations cannot be empty!");
        if (sub.mult!=0)
            throw sub.syntaxError("Multiplicity expression not allowed here");
        if (o==Op.NO || o==Op.ONE)
            throw internalError("The "+o+" operator should have been desugared!");
        if (o!=Op.COMPREHENSION && list.size()!=1)
            throw internalError("The list should have been desugared to 1!");
    }

    /** This class contains all possible quantification operators. */
    public enum Op {
        /** all  a,b:x, c,d:y | formula       */  ALL("all"),
        /** no   a,b:x, c,d:y | formula       */  NO("no"),
        /** lone a,b:x, c,d:y | formula       */  LONE("lone"),
        /** one  a,b:x, c,d:y | formula       */  ONE("one"),
        /** some a,b:x, c,d:y | formula       */  SOME("some"),
        /** sum  a,b:x, c,d:y | intExpression */  SUM("sum"),
        /** { a,b:x,    c,d:y | formula }     */  COMPREHENSION("{comprehension}");

        /** The constructor */
        Op(String l) {label=l;}

        /** The human readable label for this operator */
        private final String label;

        /**
         * Constructs an ExprQuant expression with "this" as the operator.
         *
         * @param p - the original position in the file
         * @param l - the list of variable declarations
         * @param s - the body of the expression
         * @param t - the type (null if this expression has not been typechecked)
         *
         * @throws ErrorInternal if p==null, l==null, l.size()==0, s==null, or s.mult!=0
         * @throws ErrorInternal if one of the VarDecl is null
         */
        public final Expr make(Pos p, List<VarDecl> l, Expr s, Type t) {
            // Desugarings of ALL/NO/LONE/ONE/SOME/SUM:
            // ========================================
            //
            // no a,b,c:X "MORE|F"  ==> all a,b,c:X | "no MORE|F"
            //
            // no a,b,c:X | F  =======> all a,b,c:X | !F
            //
            // one.... = lone... && some...
            //
            // sum a,b,c:X "MORE|F" ==> sum a,b,c:X | "sum MORE|F"
            //
            // all a,b,c:X "MORE|F" ==> all a,b,c:X | "all MORE|F"
            //
            // some a,b,c:X "MORE|F" ==> some a,b,c:X | "some MORE|F"
            //
            // lone a,b,c:X "MORE|F" ==> (lone a,b,c:X | "some MORE|F")
            //                       and (all a,b,c:X | "lone MORE|F")
            //
            // So in the end, we only allow quantification over a single expression,
            // and we ony allow ALL, SOME, LONE, and SUM.
            //
            if (this!=Op.COMPREHENSION && this!=Op.SUM) t=Type.FORMULA;
            ArrayList<VarDecl> l2=new ArrayList<VarDecl>(1);
            if (l==null) throw new ErrorInternal(p,null,"NullPointerException");
            l=new ArrayList<VarDecl>(l);
            if (l.size()==0) throw new ErrorInternal(p,null,"The list cannot be empty");
            for(int i=0; i<l.size(); i++)
                if (l.get(i)==null) throw new ErrorInternal(p,null,"NullPointerException");
            if (this==NO) {
                if (l.size()==1) return new ExprQuant(p,ALL,l,ExprUnary.Op.NOT.make(s.pos,s,t),t);
                l2.add(l.get(0));
                l.remove(0);
                return new ExprQuant(p,ALL,l2,NO.make(p,l,s,t),t);
            }
            if (this==ONE) {
                Expr a=LONE.make(p,l,s,t), b=SOME.make(p,l,s,t);
                return ExprBinary.Op.AND.make(p,a,b,t);
            }
            if ((this==ALL || this==SOME || this==SUM) && l.size()>1) {
                l2.add(l.get(0));
                l.remove(0);
                return new ExprQuant(p,this,l2,this.make(p,l,s,t),t);
            }
            if (this==LONE && l.size()>1) {
                l2.add(l.get(0));
                l.remove(0);
                Expr a=new ExprQuant(p,LONE,l2,SOME.make(p,l,s,t),t);
                Expr b=new ExprQuant(p,ALL,l2,LONE.make(p,l,s,t),t);
                return ExprBinary.Op.AND.make(p,a,b,t);
            }
            return new ExprQuant(p,this,l,s,t);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }
}
