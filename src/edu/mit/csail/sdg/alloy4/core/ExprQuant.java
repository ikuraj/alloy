package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a quantified expression.
 *
 * It can have one of the following forms:
 *
 * <br/>
 * <br/>  (all    &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br/>  (no     &nbsp;&nbsp; a,b:t, c,d:v &nbsp; | formula)
 * <br/>  (lone   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/>  (one    &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/>  (some   &nbsp;       a,b:t, c,d:v &nbsp; | formula)
 * <br/>  (sum    &nbsp;       a,b:t, c,d:v &nbsp; | expression)
 * <br/>  {a,b:t, &nbsp; c,d:v &nbsp; | &nbsp; formula}
 * <br/>  {a,b:t, &nbsp; c,d:v}
 * <br/>
 *
 * <br/> <b>Invariant:</b> op!=null
 *
 * <br/> <b>Invariant:</b>
 *       op!=Op.NO  <i> (since ExprQuant.Op.make() would desugar this) </i>
 *
 * <br/> <b>Invariant:</b>
 *       op!=Op.ONE <i> (since ExprQuant.Op.make() would desugar this) </i>
 *
 * <br/> <b>Invariant:</b>
 *       op!=Op.COMPREHENSION => list.size()==1  <i> (since ExprQuant.Op.make() would desugar this) </i>
 *
 * <br/> <b>Invariant:</b>  op==Op.COMPREHENSION => list.size()>=1
 * <br/> <b>Invariant:</b>  all x:list | x!=null
 * <br/> <b>Invariant:</b>  count = (sum x:list | x.names.size())
 * <br/> <b>Invariant:</b>  sub.mult == 0
 *
 * @author Felix Chang
 */

public final class ExprQuant extends Expr {

    /** Accepts the return visitor. */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor bottom-up. */
    @Override public Expr accept(VisitTypechecker visitor) {
        return visitor.accept(this);
    }

    /** Accepts the typecheck visitor top-down. */
    @Override public Expr accept(VisitTypechecker visitor, Type type) {
        return visitor.accept(this,type);
    }

    /** The operator (ALL, NO, LONE, ONE, SOME, SUM, or COMPREHENSION) */
    public final Op op;

    /** The unmodifiable list of variable declarations. */
    public final List<VarDecl> list;

    /** The body of the quantified expression. */
    public final Expr sub;

    /**
     * Constructs a new quantified expression.
     *
     * @param pos - the original position in the file
     * @param op - the operator
     * @param list - the list of variable declarations
     * @param sub - the body of the quantified expression
     * @param type - the type (null if this expression has not been typechecked)
     *
     * @throws ErrorInternal if pos==null, list==null, list.size()==0, sub==null, or sub.mult!=0
     * @throws ErrorInternal if one of the VarDecl is null
     * @throws ErrorInternal if (op==Op.NO || op==Op.ONE) since ExprQuant.Op.make() desugars them
     * @throws ErrorInternal if (op!=Op.COMPREHENSION && list.size()!=1) since ExprQuant.Op.make() desugars them
     */
    private ExprQuant(Pos pos, Op op, List<VarDecl> list, Expr sub, Type type) {
        super(pos,type,0);
        this.op=op;
        this.list=Collections.unmodifiableList(new ArrayList<VarDecl>(nonnull(list)));
        for(int i=0; i<this.list.size(); i++) nonnull(this.list.get(i));
        this.sub=nonnull(sub);
        if (this.list.size()==0)
            throw internalError("The list of declarations cannot be empty!");
        if (sub.mult!=0)
            throw sub.syntaxError("Multiplicity expression not allowed here");
        if (op==Op.NO || op==Op.ONE)
            throw internalError("The "+op+" operator should have been desugared!");
        if (op!=Op.COMPREHENSION && this.list.size()!=1)
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

        /** The constructor. */
        Op(String label) {this.label=label;}

        /** The human readable label for this operator. */
        private final String label;

        /**
         * Constructs an ExprQuant expression with "this" as the operator.
         *
         * @param pos - the original position in the file
         * @param list - the list of variable declarations
         * @param sub - the body of the expression
         * @param type - the type (null if this expression has not been typechecked)
         *
         * @throws ErrorInternal if pos==null, list==null, list.size()==0, sub==null, or sub.mult!=0
         * @throws ErrorInternal if one of the VarDecl is null
         */
        public final Expr make(Pos pos, List<VarDecl> list, Expr sub, Type type) {
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
            // and we only allow ALL, SOME, LONE, and SUM.
            //
            if (this!=Op.COMPREHENSION && this!=Op.SUM)
                type=Type.FORMULA;
            if (list==null) throw new ErrorInternal(pos,null,"NullPointerException");
            list=new ArrayList<VarDecl>(list);
            ArrayList<VarDecl> list2=new ArrayList<VarDecl>();
            if (list.size()==0) throw new ErrorInternal(pos,null,"The list cannot be empty");
            for(int i=0; i<list.size(); i++)
                if (list.get(i)==null) throw new ErrorInternal(pos,null,"NullPointerException");
            if (this==NO) {
                if (list.size()==1) return new ExprQuant(pos,ALL,list,ExprUnary.Op.NOT.make(sub.pos,sub,type),type);
                list2.add(list.get(0));
                list.remove(0);
                return new ExprQuant(pos, ALL, list2, NO.make(pos,list,sub,type), type);
            }
            if (this==ONE) {
                Expr a=LONE.make(pos,list,sub,type);
                Expr b=SOME.make(pos,list,sub,type);
                return ExprBinary.Op.AND.make(pos,a,b,type);
            }
            if ((this==ALL || this==SOME || this==SUM) && list.size()>1) {
                list2.add(list.get(0));
                list.remove(0);
                return new ExprQuant(pos, this, list2, this.make(pos,list,sub,type), type);
            }
            if (this==LONE && list.size()>1) {
                list2.add(list.get(0));
                list.remove(0);
                Expr a=new ExprQuant(pos, LONE, list2, SOME.make(pos,list,sub,type), type);
                Expr b=new ExprQuant(pos, ALL, list2, LONE.make(pos,list,sub,type), type);
                return ExprBinary.Op.AND.make(pos,a,b,type);
            }
            return new ExprQuant(pos, this, list, sub, type);
        }

        /** Returns the human readable label for this operator */
        @Override public final String toString() { return label; }
    }
}
