package edu.mit.csail.sdg.alloy4.frontend;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import edu.mit.csail.sdg.alloy4.core.Env;
import edu.mit.csail.sdg.alloy4.core.Err;
import edu.mit.csail.sdg.alloy4.core.ErrorInternal;
import edu.mit.csail.sdg.alloy4.core.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.core.Expr;
import edu.mit.csail.sdg.alloy4.core.ExprBinary;
import edu.mit.csail.sdg.alloy4.core.ExprCall;
import edu.mit.csail.sdg.alloy4.core.ExprConstant;
import edu.mit.csail.sdg.alloy4.core.ExprITE;
import edu.mit.csail.sdg.alloy4.core.ExprJoin;
import edu.mit.csail.sdg.alloy4.core.ExprLet;
import edu.mit.csail.sdg.alloy4.core.ExprName;
import edu.mit.csail.sdg.alloy4.core.ExprQuant;
import edu.mit.csail.sdg.alloy4.core.ExprSequence;
import edu.mit.csail.sdg.alloy4.core.ExprUnary;
import edu.mit.csail.sdg.alloy4.core.Log;
import edu.mit.csail.sdg.alloy4.core.ParaAssert;
import edu.mit.csail.sdg.alloy4.core.ParaFact;
import edu.mit.csail.sdg.alloy4.core.ParaFun;
import edu.mit.csail.sdg.alloy4.core.ParaRuncheck;
import edu.mit.csail.sdg.alloy4.core.ParaSig;
import edu.mit.csail.sdg.alloy4.core.Type;
import edu.mit.csail.sdg.alloy4.core.Unit;
import edu.mit.csail.sdg.alloy4.core.VarDecl;
import edu.mit.csail.sdg.alloy4.core.VisitQuery;
import edu.mit.csail.sdg.alloy4.core.VisitReturn;
import kodkod.ast.IntExpression;
import kodkod.ast.Decls;
import kodkod.ast.IntConstant;
import kodkod.ast.Variable;
import kodkod.ast.Relation;
import kodkod.ast.Formula;
import kodkod.ast.Expression;
import kodkod.ast.BinaryExpression;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.Options;
import kodkod.engine.Solution.Outcome;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Tuple;
import kodkod.instance.Universe;

/**
 * This class takes a list of Unit(s) objects, and a list of ParaSig(s) objects,
 * and solve one or more commands using Kodkod.
 *
 * @author Felix Chang
 */

public final class VisitEval implements VisitReturn {

    public enum Result { SAT, UNSAT, TRIVIALLY_SAT, TRIVIALLY_UNSAT };

    /**
     * Convenience method that casts x to be a Kodkod Formula.
     * @return (Formula)x - if x is of type Formula
     * @throws ErrorInternal - if x is not of type Formula
     */
    private final Formula cform(Object x) {
        if (x instanceof Formula) return (Formula)x;
        throw new ErrorInternal(null,null,"This should have been a formula! Instead it is "+x);
    }

    /**
     * Convenience method that casts x to be a Kodkod IntExpression.
     * @return (IntExpression)x - if x is of type IntExpression
     * @throws ErrorInternal - if x is not of type IntExpression
     */
    private final IntExpression cint(Object x) {
        if (x instanceof IntExpression) return (IntExpression)x;
        throw new ErrorInternal(null,null,"This should have been an integer expression! Instead it is "+x);
    }

    /**
     * Convenience method that casts x to be a Kodkod Expression.
     * @return (Expression)x - if x is of type Expression
     * @throws ErrorInternal - if x is not of type Expression
     */
    private final Expression cset(Object x) {
        if (x instanceof Expression) return (Expression)x;
        throw new ErrorInternal(null,null,"This should have been a set or a relation! Instead it is "+x);
    }

    /*==============================*/
    /** Evaluates an ExprJoin node. */
    /*==============================*/

    public Object accept(ExprJoin x) {
        Expression a=cset(x.left.accept(this));
        Expression b=cset(x.right.accept(this));
        return a.join(b);
    }

    /*================================*/
    /** Evaluates an ExprBinary node. */
    /*================================*/

    public Object accept(ExprBinary x) {
        if (x.op==ExprBinary.Op.IN) return isIn(cset(x.left.accept(this)), x.right);
        Expression aa,bb;
        Object a=x.left.accept(this), b=x.right.accept(this);
        switch(x.op) {
        case LT: return cint(a).lt(cint(b));
        case LTE: return cint(a).lte(cint(b));
        case GT: return cint(a).gt(cint(b));
        case GTE: return cint(a).gte(cint(b));
        case AND: return cform(a).and(cform(b));
        case OR: return cform(a).or(cform(b));
        case IFF: return cform(a).iff(cform(b));
        case IMPLIES: return cform(a).implies(cform(b));
        case PLUSPLUS: return cset(a).override(cset(b));
        case PLUS:
            if (x.left.type.isInt) return cint(a).plus(cint(b)); return cset(a).union(cset(b));
        case MINUS:
            if (x.left.type.isInt) return cint(a).minus(cint(b)); return cset(a).difference(cset(b));
        case INTERSECT:
            return cset(a).intersection(cset(b));
        case ANY_ARROW_SOME: case ANY_ARROW_ONE: case ANY_ARROW_LONE:
        case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
        case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE: case ONE_ARROW_LONE:
        case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
            if (!demul) throw x.right.typeError("Multiplicity symbols are not allowed here");
            // intentional fall-through to the ARROW case
        case ARROW:
            return cset(a).product(cset(b));
        case DOMAIN:
            aa=cset(a);
            bb=cset(b);
            for(int i=1; i<bb.arity(); i++) aa=aa.product(Expression.UNIV);
            return aa.intersection(bb);
        case RANGE:
            aa=cset(a);
            bb=cset(b);
            for(int i=1; i<aa.arity(); i++) bb=Expression.UNIV.product(bb);
            return aa.intersection(bb);
        case EQUALS: if (x.left.type.isInt) return cint(a).eq(cint(b)); return cset(a).eq(cset(b));
        }
        throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprBinary.accept()");
    }

    private Formula isIn(Expression a, Expr right) {
        Expression b;
        if (right instanceof ExprUnary) {
            ExprUnary y=(ExprUnary)(right);
            if (y.op==ExprUnary.Op.ONEMULT) { b=cset(y.sub.accept(this)); return a.one().and(a.in(b)); }
            if (y.op==ExprUnary.Op.SETMULT) { b=cset(y.sub.accept(this)); return a.in(b); }
            if (y.op==ExprUnary.Op.LONEMULT) { b=cset(y.sub.accept(this)); return a.lone().and(a.in(b)); }
            if (y.op==ExprUnary.Op.SOMEMULT) { b=cset(y.sub.accept(this)); return a.some().and(a.in(b)); }
        }
        if (right instanceof ExprBinary) return isInBinary(a, (ExprBinary)right);
        return a.in(cset(right.accept(this)));
    }

    private Formula isInAM(Expression r, ExprBinary op, Expression a, Expression b) {
        int arity=a.arity();
        if (arity==1) {
            Variable v1=Variable.unary("@");
            Expression x=v1.join(r);
            Formula ans=x.in(b);
            switch(op.op) {
            case ANY_ARROW_SOME: case SOME_ARROW_SOME: case ONE_ARROW_SOME: case LONE_ARROW_SOME:
                ans=x.some().and(ans); break;
            case ANY_ARROW_ONE: case SOME_ARROW_ONE: case ONE_ARROW_ONE: case LONE_ARROW_ONE:
                ans=x.one().and(ans); break;
            case ANY_ARROW_LONE: case SOME_ARROW_LONE: case ONE_ARROW_LONE: case LONE_ARROW_LONE:
                ans=x.lone().and(ans); break;
            }
            return ans.forAll(v1.oneOf(a));
        }
        /*
         List<Variable> vars=new ArrayList<Variable>(arity);
         Expression temp=r;
         for(int i=0; i<arity; i++) { Variable v1=Variable.unary("@"); temp=v1.join(temp); vars.add(v1); }
         */
        throw op.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
    }

    private Formula isInNB(Expression r, ExprBinary op, Expression a, Expression b) {
        int arity=b.arity();
        if (arity==1) {
            Variable v2=Variable.unary("@");
            Expression x=r.join(v2);
            Formula ans=x.in(a);
            switch(op.op) {
            case ONE_ARROW_LONE: case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE:
                ans=x.one().and(ans); break;
            case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
                ans=x.lone().and(ans); break;
            case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
                ans=x.some().and(ans); break;
            }
            return ans.forAll(v2.oneOf(b));
        }
        throw op.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
    }

    private Formula isInBinary(Expression r, ExprBinary ab) {
        boolean old=demul;
        demul=true;
        Expression y0=cset(ab.accept(this));
        if (!ab.op.isArrow || ab.mult==0) { demul=old; return r.in(y0); }
        Expr a=ab.left;  Expression aa=cset(a.accept(this));
        Expr b=ab.right; Expression bb=cset(b.accept(this));
        demul=old;
        //if (a.type.arity()>1) throw a.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
        //if (b.type.arity()>1) throw b.internalError("Only unary and binary multiplicity are currently supported."/*zzz*/);
        Formula pa=isInAM(r,ab,aa,bb);
        Formula pb=isInNB(r,ab,aa,bb);
        return r.in(y0).and(pa).and(pb);
    }

    /*=============================*/
    /** Evaluates an ExprITE node. */
    /*=============================*/

    public Object accept(ExprITE x) {
        Formula c=cform(x.cond.accept(this));
        Object l=x.left.accept(this);
        Object r=x.right.accept(this);
        if (l instanceof Formula) return c.implies(cform(l)).and(c.not().implies(cform(r)));
        if (l instanceof Expression) return c.thenElse(cset(l),cset(r));
        return c.thenElse(cint(l),cint(r));
    }

    /*=============================*/
    /** Evaluates an ExprLet node. */
    /*=============================*/

    public Object accept(ExprLet x) {
        Object r=x.right.accept(this);
        env.put(x.left, r);
        Object ans=x.sub.accept(this);
        env.remove(x.left);
        return ans;
    }

    /*==================================*/
    /** Evaluates an ExprConstant node. */
    /*==================================*/

    public Object accept(ExprConstant x) {
        switch(x.op) {
        case NONE: return Expression.NONE;
        case IDEN: return Expression.IDEN;
        case UNIV: return kuniv;
        case SIGINT: return Expression.INTS;
        case NUMBER: return IntConstant.constant(x.num()); // zzz SHOULD WARN AGAINST SILENT TRUNCATION
        }
        throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprNamedConstant.accept()");
    }

    /*===============================*/
    /** Evaluates an ExprQuant node. */
    /*===============================*/

    public Object accept(ExprQuant x) {
        Decls dd=null;
        int ri=0; List<Expression> exprs=new ArrayList<Expression>();
        int vi=0; List<Variable> vars=new ArrayList<Variable>();
        if (x.op==ExprQuant.Op.LONE || x.op==ExprQuant.Op.ALL || x.op==ExprQuant.Op.SOME) {
            VarDecl d=x.list.get(0);
            Expr dex=d.value;
            if (dex.type.arity()==1) return quant1(x,d,dex); return quantN(x,d,dex);
        }
        if (x.op==ExprQuant.Op.SUM) throw x.internalError("Integer quantification is currently unimplemented."/*zzz*/);
        for(VarDecl d:x.list) {
            Expr dex=d.value;
            if (dex.type.arity()>1) throw x.typeError("Each variable in set comprehension can only range over unary sets");
            if (dex instanceof ExprUnary) {
                if (((ExprUnary)dex).op==ExprUnary.Op.SETMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
                if (((ExprUnary)dex).op==ExprUnary.Op.LONEMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
                if (((ExprUnary)dex).op==ExprUnary.Op.SOMEMULT) throw x.typeError("Each variable in set comprehension cannot use multiplicity symbols");
            }
            Expression dv=cset(stripSetMult(d.value).accept(this));
            exprs.add(dv);
            for(String n:d.names) { Variable var=Variable.nary("VAR<"+n+">",dv.arity()); vars.add(var); env.put(n,var); }
        }
        Formula ans=cform(x.sub.accept(this));
        for(VarDecl d:x.list) {
            Expression r=exprs.get(ri); ri++;
            for(String n:d.names) {
                env.remove(n);
                Variable v=vars.get(vi); vi++;
                if (dd==null) dd=v.oneOf(r); else dd=dd.and(v.oneOf(r));
            }
        }
        return ans.comprehension(dd);
    }

    private Expr stripSetMult(Expr x) {
        if (!(x instanceof ExprUnary)) return x;
        ExprUnary y=(ExprUnary)x;
        if (y.op==ExprUnary.Op.SETMULT) return y.sub;
        if (y.op==ExprUnary.Op.ONEMULT) return y.sub;
        if (y.op==ExprUnary.Op.LONEMULT) return y.sub;
        if (y.op==ExprUnary.Op.SOMEMULT) return y.sub;
        return x;
    }

    private Formula quantN(ExprQuant x, VarDecl d, Expr dex) {
        // all  a:MULT | F(a)     becomes  all  a  | (a in MULT) => F(a)
        // some a:MULT | F(a)     becomes  some a  | (a in MULT) && F(a)
        // lone a:MULT | F(a)     becomes  all a,b | (a in MULT && b in MULT) => (F(a) && F(b) => a==b)
        Decls dd=null;
        //int ri=0; List<Expression> exprs=new ArrayList<Expression>();
        List<Variable> vars=new ArrayList<Variable>();
        boolean old=demul;
        demul=true;
        Expression dv=cset(dex.accept(this));
        demul=old;
        for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
        Formula ans1=cform(x.sub.accept(this));
        for(String n:d.names) { env.remove(n); }
        if (x.op==ExprQuant.Op.LONE) {
            for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
            Formula ans2=cform(x.sub.accept(this));
            for(String n:d.names) { env.remove(n); }
            Formula ans3=Formula.TRUE;
            for(int n=d.names.size(),i=0; i<n; i++) ans3=vars.get(i).eq(vars.get(i+n)).and(ans3);
            ans1=ans1.and(ans2).implies(ans3);
        }
        Formula guard=Formula.TRUE;
        for(Variable v:vars) guard=isIn(v,dex).and(guard);
        for(Variable v:vars) if (dd==null) dd=v.setOf(dv); else dd=dd.and(v.setOf(dv));
        if (x.op==ExprQuant.Op.SOME) return guard.and(ans1).forSome(dd); else return guard.implies(ans1).forAll(dd);
    }

    private Formula quant1(ExprQuant x, VarDecl d, Expr dex) {
        // lone a:MULT | F(a)   becomes  all a,b:MULT | (F(a) && F(b) => a==b)
        Decls dd=null;
        List<Variable> vars=new ArrayList<Variable>();
        ExprUnary.Op op=ExprUnary.Op.ONEMULT;
        if (dex instanceof ExprUnary) {
            if (((ExprUnary)dex).op==ExprUnary.Op.SETMULT) op=ExprUnary.Op.SETMULT;
            else if (((ExprUnary)dex).op==ExprUnary.Op.SOMEMULT) op=ExprUnary.Op.SOMEMULT;
            else if (((ExprUnary)dex).op==ExprUnary.Op.LONEMULT) op=ExprUnary.Op.LONEMULT;
        }
        Expression dv=cset(stripSetMult(dex).accept(this));
        for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
        Formula ans1=cform(x.sub.accept(this));
        for(String n:d.names) { env.remove(n); }
        if (x.op==ExprQuant.Op.LONE) {
            for(String n:d.names) { Variable var=Variable.nary("@"+n, dv.arity()); vars.add(var); env.put(n,var); }
            Formula ans2=cform(x.sub.accept(this));
            for(String n:d.names) { env.remove(n); }
            Formula ans3=Formula.TRUE;
            for(int n=d.names.size(),i=0; i<n; i++) ans3=vars.get(i).eq(vars.get(i+n)).and(ans3);
            ans1=ans1.and(ans2).implies(ans3);
        }
        for(Variable v:vars) switch(op) {
        case ONEMULT: if (dd==null) dd=v.oneOf(dv); else dd=dd.and(v.oneOf(dv)); break;
        case SETMULT: if (dd==null) dd=v.setOf(dv); else dd=dd.and(v.setOf(dv)); break;
        case LONEMULT: if (dd==null) dd=v.loneOf(dv); else dd=dd.and(v.loneOf(dv)); break;
        case SOMEMULT: if (dd==null) dd=v.someOf(dv); else dd=dd.and(v.someOf(dv)); break;
        }
        if (x.op==ExprQuant.Op.SOME) return ans1.forSome(dd); else return ans1.forAll(dd);
    }

    /*===================================*/
    /** Evaluates an ExprSequuence node. */
    /*===================================*/

    public Object accept (ExprSequence x) {
        Formula ans=Formula.TRUE;
        for(int i=0; i<x.list.size(); i++) {
            Expr sub=x.list.get(i);
            ans=ans.and(cform(sub.accept(this)));
        }
        return ans;
    }

    /*===============================*/
    /** Evaluates an ExprUnary node. */
    /*===============================*/

    public Object accept(ExprUnary x) {
        Object y=x.sub.accept(this);
        switch(x.op) {
            case SOMEMULT: case LONEMULT: case ONEMULT: case SETMULT:
                if (demul) return y;
                throw x.sub.typeError("Multiplicity symbols are not allowed here");
            case NOT: return cform(y).not();
            case SOME: return cset(y).some();
            case LONE: return cset(y).lone();
            case ONE: return cset(y).one();
            case NO: return cset(y).no();
            case TRANSPOSE: return cset(y).transpose();
            case RCLOSURE: return cset(y).reflexiveClosure();
            case CLOSURE: return cset(y).closure();
            case CARDINALITY: return cset(y).count();
            case SUM: return cset(y).sum();
            case INTTOATOM: return cint(y).toExpression();
        }
        throw x.internalError("Unsupported operator ("+x.op+") encountered during ExprUnary.accept()");
    }

    /*==============================*/
    /** Evaluates an ExprName node. */
    /*==============================*/

    public Object accept(ExprName x) {
        Object r = x.object;
        if (r instanceof ParaSig.Field.Full) {
            ParaSig.Field.Full y=(ParaSig.Field.Full)r;
            return rel(y);
        }
        if (r instanceof ParaSig) {
            ParaSig y=(ParaSig)r;
            return rel(y);
        }
        Object ans;
        if (r instanceof ParaFun) {
            ParaFun y=(ParaFun)r;
            if (y.argCount!=0) throw x.internalError("ExprName \""+x.name+"\" is not resolved prior to code gen! Its resolution == "+r);
            Env oldenv=this.env; this.env=new Env(); ans=y.value.accept(this); this.env=oldenv; return ans;
        }
        ans=env.get(x.name);
        if (ans==null) throw x.internalError("ExprName \""+x.name+"\" cannot be found during code gen! r=="+r);
        return ans;
    }

    /*==============================*/
    /** Evaluates an ExprCall node. */
    /*==============================*/

    public Object accept(ExprCall x) {
        // zzz: Should make sure there are no recursion.
        ParaFun y=x.fun;
        if (y==null) throw x.internalError("ExprCall should now refer to a Function or Predicate");
        Env newenv=new Env();
        int r=0;
        for(VarDecl d:y.decls) {
            for(String n:d.names) {
                newenv.put(n,cset(x.args.get(r).accept(this))); r++;
            }
        }
        Env oldenv=this.env; this.env=newenv; Object ans=y.value.accept(this); this.env=oldenv; return ans;
    }

//################################################################################################

    private boolean demul=false;
    private final List<Unit> units;
    private Env env=new Env();
    private final Log log;
    private final int codeindex;
    public VisitEval(int i, Log log, List<Unit> units) { codeindex=i; this.log=log; this.units=units; }

    private Map<ParaSig,Expression> sig2rel = new LinkedHashMap<ParaSig,Expression>();
    private Expression rel(ParaSig x) {
        if (x==ParaSig.UNIV) return kuniv;
        if (x==ParaSig.NONE) return Expression.NONE;
        if (x==ParaSig.SIGINT) return Expression.INTS;
        return sig2rel.get(x); }
    private void rel(ParaSig x,Expression y) { sig2rel.put(x,y); }

    private Map<ParaSig.Field.Full, Expression> field2rel = new LinkedHashMap<ParaSig.Field.Full,Expression>();
    private Expression rel(ParaSig.Field.Full x) { return field2rel.get(x); }
    private void rel(ParaSig.Field.Full x,Expression y) { field2rel.put(x,y); }

    private Map<ParaSig,Integer> sig2bound = new LinkedHashMap<ParaSig,Integer>();
    private int sig2bound(ParaSig x) { Integer y=sig2bound.get(x); if (y==null) return -1; return y; }
    private void sig2bound(ParaSig x,int y) { sig2bound.put(x,y); }

    private Map<ParaSig,TupleSet> sig2ts = new LinkedHashMap<ParaSig,TupleSet>();
    private TupleSet sig2ts(ParaSig x) { return sig2ts.get(x); }
    private void sig2ts(ParaSig x,TupleSet y) { sig2ts.put(x,y); }

    private Set<ParaSig> sig2exact = new LinkedHashSet<ParaSig>();
    private boolean exact(ParaSig x) { return sig2exact.contains(x); }
    private void exact(ParaSig x,boolean y) { if (y) sig2exact.add(x); else sig2exact.remove(x); }

    private Map<ParaSig,List<String>> sig2lowerbound = new LinkedHashMap<ParaSig,List<String>>();
    private List<String> sig2lowerbound(ParaSig x) {
        List<String> ans=sig2lowerbound.get(x);
        if (ans==null) { ans=new ArrayList<String>(); sig2lowerbound.put(x,ans); }
        return ans;
    }
    private void sig2lowerbound(ParaSig x, Collection<String> y) {
        List<String> ans=sig2lowerbound(x);
        ans.clear();
        ans.addAll(y);
    }

    private Map<ParaSig,List<String>> sig2upperbound = new LinkedHashMap<ParaSig,List<String>>();
    private List<String> sig2upperbound(ParaSig x) {
        List<String> ans=sig2upperbound.get(x);
        if (ans==null) { ans=new ArrayList<String>(); sig2upperbound.put(x,ans); }
        return ans;
    }
    private void sig2upperbound(ParaSig x, Collection<String> y) {
        List<String> ans=sig2upperbound(x);
        ans.clear();
        ans.addAll(y);
    }

    private void bound(String debug, ParaRuncheck c, ParaSig s, int b) {
        if (b<0)
            throw c.syntaxError("Cannot set a negative bound for signature \""+s.fullname+"\"");
        if (sig2bound(s)>=0)
            throw c.syntaxError("The signature \""+s.fullname+"\" already has a bound of "+sig2bound(s)+", so we cannot set it to "+b);
        if (b>=0 && (s==ParaSig.UNIV || s==ParaSig.NONE))
            throw c.syntaxError("You cannot specify a scope for the builtin signature \""+s.name+"\"");
        sig2bound(s,b);
        log.log(debug+"Sig \""+s.fullname+"\" bound to be <= "+b+"\n");
    }

    private boolean derive(ParaRuncheck cmd, List<ParaSig> sigs) {
        for(ParaSig s:sigs) {
            if (s.abs && sig2bound(s)<0 && s.subs.size()>0) {
                int sum=0;
                for(ParaSig c:s.subs) {if (sig2bound(c)<0) {sum=(-1);break;} sum=sum+sig2bound(c);}
                if (sum>=0) {bound("#1: ",cmd,s,sum);return true;}
            }
            if (s.abs && sig2bound(s)>=0 && s.subs.size()>0) {
                int sum=0; ParaSig cc=null;
                for(ParaSig c:s.subs) {if (sig2bound(c)>=0) sum=sum+sig2bound(c); else if (cc==null) cc=c; else {cc=null;break;}}
                if (cc!=null) {bound("#2: ",cmd,cc,(sig2bound(s)<sum)?0:sig2bound(s)-sum); return true;}
            }
            if (sig2bound(s)!=1 && !s.subset && s.one) {bound("#3: ",cmd,s,1); return true;}
            if ((sig2bound(s)<0 || sig2bound(s)>1) && !s.subset && s.lone) {bound("#4: ",cmd,s,1); return true;}
        }
        return false;
    }

    private boolean derive2(ParaRuncheck cmd, List<ParaSig> sigs) {
        boolean chg=false;
        for(ParaSig s:sigs) if (s.sup()!=null && sig2bound(s)<0 && sig2bound(s.sup())>=0) {
            bound("#5: ", cmd, s, sig2bound(s.sup()));
            chg=true;
        }
        return chg;
    }

    private int compute(Unit root, List<ParaSig> sigs, ParaRuncheck cmd) {
        final int overall;
        int bitwidth=(-1); // The bound on "int".
        for(ParaSig s:sigs) { sig2bound(s,-1); if (s.one && !s.subset) exact(s,true); }
        if (cmd.names.size()==0 && cmd.overall<0) overall=3; else overall=cmd.overall;
        for(String n:cmd.names) {
            if (n.equals(ParaSig.BITWIDTH_NAME)) { bitwidth=cmd.getScope(n); continue; }
            Set<Object> set=root.lookup_sigORparam(n);
            Iterator<Object> it=set.iterator();
            if (set.size()>1) {
                ParaSig choice1=(ParaSig)(it.next());
                ParaSig choice2=(ParaSig)(it.next());
                throw cmd.syntaxError("The name \""+n+"\" is ambiguous: it could be "+choice1.fullname+" or "+choice2.fullname);
            }
            if (set.size()<1) throw cmd.syntaxError("The name \""+n+"\" cannot be found");
            ParaSig s=(ParaSig)(it.next());
            if (s.subset) throw cmd.syntaxError("Can not specify a scope for a subset signature \""+s.fullname+"\"");
            int sc=cmd.getScope(n);
            //if (s.one) {exact(s,true); if (sc!=1) throw cmd.syntaxError("The signature \""+s.fullname+"\" was declared to be one, so its scope cannot be "+sc); }
            if (cmd.isExact(n)==true) exact(s,true);
            if (sig2bound(s)>=0) throw cmd.syntaxError("The signature \""+s.fullname+"\" already has a specified scope");
            bound("#6: ",cmd, s, sc);
        }
        // Ensure "int" and "Int" are consistent
        if (bitwidth<0) bitwidth=4;
        if (bitwidth>=0) {
            if (bitwidth>30) throw cmd.syntaxError("Can not specify a bitwidth of greater than 30");
            int i=(1<<bitwidth);
            if (sig2bound(ParaSig.SIGINT)>i) throw cmd.syntaxError("With an integer bitwidth of "+bitwidth+" you must have exactly "+i+" Int atoms, thus the bound on Int cannot be "+sig2bound(ParaSig.SIGINT));
            if (sig2bound(ParaSig.SIGINT)<i && exact(ParaSig.SIGINT)) throw cmd.syntaxError("With an integer bitwidth of "+bitwidth+" you must allow exactly "+i+" Int atoms, thus the bound on Int cannot be exactly "+sig2bound(ParaSig.SIGINT));
            sig2bound(ParaSig.SIGINT, i);
        }
        // Derive the implicit bounds
        again: while(true) {
            while(derive(cmd,sigs)) {}
            // TopLevel sigs must have bounds!
            for(ParaSig s:sigs) if (!s.subset && s.sup()==null && sig2bound(s)<0) {
                if (overall<0) throw cmd.syntaxError("The signature \""+s.fullname+"\" needs a specific scope!");
                if (s.lone || s.one) bound("#7: ",cmd, s, 1); else bound("#8: ",cmd, s, overall);
                continue again;
            }
            break;
        }
        while(derive2(cmd,sigs)) {}
        return bitwidth;
    }

    /*
     Here, we will ignore subsetsigs.

     The bounds are determined as follows:

     . "run x": every topsig is bound to <= 3 elements.

     . "run x for N": every topsif is bound to <= N elements.

     . "run x for N but N1 SIG1, N2 SIG2...":
     Every sig following "but" is constrained explicitly.
     Any topsig that is
     a) not listed, and
     b) its bound is not derived implicitly
     will be bound to have <= N elements.

     . "run x for N1 SIG1, N2 SIG2..."
     Every sig following "but" is constrained explicitly.
     Any topsig that is
     a) not listed, and
     b) its bound is not derived implicitly
     we will give an error message.

     Implicit bounds are determined as follows:

     . If an abstract signature X has no explicit bound,
     but its subsignatures have bounds (implicit or explicit),
     then X is implicitly bound to be <= the sum of its subsigs.

     . If an abstract signature X has a bound (implicit or explicit),
     and all but one of its subsignatures have bounds (implicit or explicit),
     the remaining subsignature is implicitly bound to
     have <= the difference between the parent and the sibling sigs.

     . A signature declared with the multiplicity keyword one has a bound == 1.

     . A signature declared with the multiplicity keyword lone has a bound <= 1.
     */

    /*==========================================================================*/
    /** Code Generation                                                         */
    /*==========================================================================*/

    private TupleSet comp(Type t, TupleFactory factory) {
        int a=t.arity(); if (a<1) throw new ErrorInternal(null,null,"Attempting to create a 0-arity TupleSet!");
        TupleSet ans=factory.noneOf(a);
        for(Type.Rel r:t) {
            TupleSet temp1=null,temp2;
            for(int i=0; i<r.basicTypes.size(); i++) {
                ParaSig b=r.basicTypes.get(i);
                if (b==ParaSig.UNIV) temp2=factory.allOf(1);
                else if (b==ParaSig.NONE) temp2=factory.noneOf(1);
                else temp2=sig2ts(b);
                if (temp1==null) temp1=temp2; else temp1=temp1.product(temp2);
            }
            for(Tuple xx:temp1) ans.add(xx);
        }
        return ans;
    }

    /*==========================================================================*/
    /**                                                                         */
    /*==========================================================================*/

    private Expression kuniv=Relation.UNIV;//INTS;

    // zzz: Should make sure we don't load Int unless we need to

    public List<Result> codegen(List<ParaSig> sigs)  {
        Formula kfact=Formula.TRUE;
        // Generate the relations for the SIGS.
        for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
            Relation r=Relation.unary(s.fullname);
            rel(s,r);
            if (kuniv!=Relation.UNIV) kuniv=kuniv.union(r);
        }
        // Generate the relations for the FIELDS
        for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
            Unit u=units.get(0).lookupPath(s.path);
            // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 below
            if (s.pos!=null && s.pos.filename!=null
                    && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")
                    && u.params.get("elem")!=null
            ) {
                Relation first=Relation.unary("first_");
                Relation last=Relation.unary("last_");
                Relation next=Relation.binary("next_");
                ParaSig s2=u.params.get("elem");
                rel(s.fields.get(0).full, rel(s).product(first));
                rel(s.fields.get(1).full, rel(s).product(last));
                rel(s.fields.get(2).full, rel(s).product(next));
                rel(s.fields.get(3).full, rel(s).product(next.transpose()));
                kfact=next.totalOrder((Relation)(rel(s2)), first, last).and(kfact);
                continue;
            }
            // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 above
            int fi=0;
            for(VarDecl fd:s.decls) {
                for(String fn:fd.names) {
                    ParaSig.Field f=s.fields.get(fi); fi++;
                    int a=fd.value.type.arity()+1;
                    rel(f.full, Relation.nary(s.fullname+"."+fn, a));
                }
            }
        }
        // Add the constraints among SIGs
        for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
            if (s.subs.size()>1) {
                // If X and Y both extends P, then X and Y are disjoint
                for(int x1=0; x1<s.subs.size(); x1++)
                    for(int x2=x1+1; x2<s.subs.size(); x2++) {
                        Expression x11=rel(s.subs.get(x1));
                        Expression x22=rel(s.subs.get(x2));
                        kfact=x11.intersection(x22).no().and(kfact);
                    }
            }
            if (s.abs && s.subs.size()>0) {
                // abstract sig with children == union of all children
                List<ParaSig> subs=s.subs;
                Expression x1=rel(subs.get(0));
                for(int x2=1; x2<subs.size(); x2++) x1=x1.union(rel(subs.get(x2)));
                kfact=x1.eq(rel(s)).and(kfact);
            }
            if (s.sup()!=null && s.sup()!=ParaSig.UNIV && !s.sup().abs) {
                // If X extends Y, then X in Y
                kfact=rel(s).in(rel(s.sup())).and(kfact);
            }
            if (s.sups().iterator().hasNext()) {
                // If X in Y1+..+Yn, then X in Y1+..+Yn
                Expression x1=null;
                for(ParaSig x2:s.sups()) {
                    if (x1==null) x1=rel(x2); else x1=x1.union(rel(x2));
                }
                kfact=rel(s).in(x1).and(kfact);
            }
            if (s.lone) kfact=rel(s).lone().and(kfact); // "lone"
            if (s.one) kfact=rel(s).one().and(kfact);   // "one"
            if (s.some) kfact=rel(s).some().and(kfact); // "some"
        }
        // Add the sig declaration facts, and sig appended facts
        VisitQuery hasThis=new VisitQuery() {
          @Override public Object accept(ExprName x) { if (x.name.equals("this")) return this; return null; }
        };
        for(Unit u:units) for(Map.Entry<String,ParaSig> e:u.sigs.entrySet()) {
          ParaSig s=e.getValue();
          // zzz ALLOY3 compatiblity hack below
          if (s.pos!=null && s.pos.filename!=null && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) continue;
          // zzz ALLOY3 compatiblity hack above
          Expr temp=s.appendedFacts;
          int f=0;
          for(VarDecl d:s.decls) {
            boolean noThis=!hasThis.query(d.value);
            for(int n=0; n<d.names.size(); n++) {
              ParaSig.Field x00=s.fields.get(f); f++;
              Expr x22=new ExprName(d.value.pos, x00.full.fullname, x00.full, x00.full.fulltype);
              Expr x5=new ExprName(s.pos, s.fullname, s, s.type);
              if (noThis && d.value.mult==0)
                 { kfact=kfact.and((Formula)(x22.in(x5.product(d.value)).accept(this))); continue; }
              if (noThis && d.value.isSetOf1ary())
                 { kfact=kfact.and((Formula)(x22.in(x5.product(d.value.getUnarySub())).accept(this))); continue; }
              for(int i=x22.type.arity(); i>1; i--) x5=x5.product(Expr.univ(x5.pos));
              kfact=kfact.and((Formula) (x22.in(x5).accept(this)));
              ExprName x11=new ExprName(d.value.pos, "this", null, s.type);
              Expr x33=x11.join(x22);
              Expr x44=x33.in(d.value);
              if (temp==null) temp=x44; else temp=temp.and(x44);
            }
          }
          if (temp!=null) {
             Expr x1=new ExprName(s.pos, s.fullname, s, s.type);
             Expr x2=ExprUnary.Op.ONEMULT.make(s.pos, x1, s.type);
             VarDecl x3=new VarDecl("this",x2);
             List<VarDecl> x4=new ArrayList<VarDecl>(1); x4.add(x3);
             Expr x5=ExprQuant.Op.ALL.make(s.pos,x4,temp,Type.FORMULA);
             kfact=kfact.and((Formula)(x5.accept(this)));
          }
        }
        // Add the regular facts
        for(Unit u:units) for(Map.Entry<String,ParaFact> e:u.facts.entrySet()) {
            kfact=((Formula)(e.getValue().value.accept(this))).and(kfact);
        }
        // Go thru the commands
        List<Result> result=new ArrayList<Result>();
        for(int xi=0; xi<units.get(0).runchecks.size(); xi++)
            if (codeindex==(-1) || codeindex==xi) {
                ParaRuncheck x=units.get(0).runchecks.get(xi);
                log.logBold("\nComputing the bounds for the command \""+x+"\"...\n");
                log.flush();
                sig2bound.clear();
                sig2ts.clear();
                sig2exact.clear();
                sig2lowerbound.clear();
                sig2upperbound.clear();
                int bitwidth=compute(units.get(0), sigs, x);
                // zzz ALLOY3 compatiblity hack below
                for(ParaSig s:sigs) if (s!=ParaSig.SIGINT && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) {
                    ParaSig s2=units.get(0).lookupPath(s.path).params.get("elem");
                    if (sig2bound(s2)<=0) throw x.syntaxError("The signature "+s2.fullname+" must have a bound >= 1, since it is used to instantiate the util/ordering.als module");
                    log.log("Compatibility hack: "+s2.fullname+" set to exactly "+sig2bound(s2)+"\n");
                    if (s2!=null) exact(s2,true);
                }
                // zzz ALLOY3 compatiblity hack above
                this.env.clear();
                result.add(runcheck(x, units, bitwidth, sigs, kfact));
            }
        return result;
    }

    private final Map<String,Integer> unique=new LinkedHashMap<String,Integer>();

    private String makeAtom(ParaSig s) {
        int i=0;
        if (unique.containsKey(s.fullvname)) i=unique.get(s.fullvname);
        String ans = (i==0) ? s.fullvname : s.fullvname+"_"+i;
        unique.put(s.fullvname, i+1);
        return new String(ans);
    }

    private Relation right(Expression x) { return (Relation) (((BinaryExpression)x).right()); }

    private IntConstant makeIntConstant(int bitwidth,int i) {
        // We know 1 <= bitwidth <= 30
        // bitwidth==1 ==> MIN==-1 MAX==0
        // bitwidth==2 ==> MIN==-2 MAX==1
        // bitwidth==3 ==> MIN==-4 MAX==3
        int min = 0-(1<<(bitwidth-1));
        int max = (0-min)-1;
        if (i<min || i>max) throw new ErrorSyntax(null,"The scope of "+i+" is too small to fit in a 2's complement integer with bitwidth=="+bitwidth);
        return IntConstant.constant(i);
    }

    private void computeLowerBound(ParaSig s) {
        int n=sig2bound(s);
        Set<String> x=new LinkedHashSet<String>();
        for(ParaSig c:s.subs) {
            computeLowerBound(c);
            x.addAll(sig2lowerbound(c));
        }
        if (n<x.size()) throw new ErrorInternal(null,null,"Scope for sig "+s.fullname+" was miscalculated");
        if (n>x.size() && exact(s)) {
            for(n=n-x.size(); n>0; n--) x.add(makeAtom(s));
        }
        sig2lowerbound(s,x);
        sig2upperbound(s,x);
    }

    private void computeUpperBound(ParaSig s) {
        if (s.sup()==null) {
            int n=sig2bound(s);
            int nn=sig2upperbound(s).size();
            while(n>nn) { sig2upperbound(s).add(makeAtom(s)); nn++; }
        }
        Set<String> x=new LinkedHashSet<String>(sig2upperbound(s));
        for(ParaSig c:s.subs) {
            x.removeAll(sig2lowerbound(c));
        }
        for(ParaSig c:s.subs) {
            if (sig2bound(c) > sig2lowerbound(c).size()) {
                sig2upperbound(c, sig2lowerbound(c));
                sig2upperbound(c).addAll(x);
                computeUpperBound(c);
            }
        }
    }

    // Result = SAT, UNSAT, TRIVIALLY_SAT, TRIVIALLY_UNSAT, or null.
    private Result runcheck(ParaRuncheck cmd, List<Unit> units, int bitwidth, List<ParaSig> sigs, Formula kfact)  {
        Result mainResult=null;
        Unit root=units.get(0);
        unique.clear();
        Set<String> atoms=new LinkedHashSet<String>();
        if (bitwidth<1 || bitwidth>30) throw cmd.syntaxError("The integer bitwidth must be between 1..30");
        if (ParaSig.SIGINT.subs.size()>0) throw new ErrorInternal(null,null,"SIGINT can no longer be extended!");
        for(int i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++) {
            String ii=new String("Int_"+i);
            atoms.add(ii);
            sig2lowerbound(ParaSig.SIGINT).add(ii);
            sig2upperbound(ParaSig.SIGINT).add(ii);
        }
        for(ParaSig s:sigs) if (!s.subset && s.sup()==null) { computeLowerBound(s); }
        for(ParaSig s:sigs) if (!s.subset && s.sup()==null) { computeUpperBound(s); atoms.addAll(sig2upperbound(s)); }
        final Universe universe = new Universe(atoms);
        final TupleFactory factory = universe.factory();
        final Bounds bounds = new Bounds(universe);
        sig2ts(ParaSig.UNIV, factory.allOf(1));
        sig2ts(ParaSig.NONE, factory.noneOf(1));
        sig2ts(ParaSig.SIGINT, factory.noneOf(1));
        for(int j=0,i=0-(1<<(bitwidth-1)); i<(1<<(bitwidth-1)); i++,j++) {
            Tuple ii=factory.tuple(sig2upperbound(ParaSig.SIGINT).get(j));
            bounds.boundExactly(i,factory.range(ii,ii));
            sig2ts(ParaSig.SIGINT).add(ii);
        }
        for(int si=sigs.size()-1; si>=0; si--) {
            ParaSig s=sigs.get(si);
            if (s.subset) continue;
            TupleSet upper=factory.noneOf(1);
            TupleSet lower=factory.noneOf(1);
            for(String a:sig2upperbound(s)) upper.add(factory.tuple(a));
            for(String a:sig2lowerbound(s)) lower.add(factory.tuple(a));
            sig2ts(s,upper);
            bounds.bound((Relation)(rel(s)),lower,upper);
            if (s.subset) continue;
            if (exact(s) && upper.size()==sig2bound(s)) {
                log.log("SIG "+s.fullname+" BOUNDEXACTLY=<"+upper.toString()+">\n");
            }
            else if (exact(s)) {
                log.log("SIG "+s.fullname+" BOUND=<"+upper.toString()+"> and #=="+sig2bound(s)+"\n");
                if (sig2bound(s)==0) kfact=rel(s).no().and(kfact);
                else if (sig2bound(s)==1) kfact=rel(s).one().and(kfact);
                else kfact=rel(s).count().eq(makeIntConstant(bitwidth, sig2bound(s))).and(kfact);
            }
            else if (upper.size()>sig2bound(s)) {
                log.log("SIG "+s.fullname+" BOUND=<"+upper.toString()+"> and #<="+sig2bound(s)+"\n");
                if (sig2bound(s)==0) kfact=rel(s).no().and(kfact);
                else if (sig2bound(s)==1) kfact=rel(s).lone().and(kfact);
                else kfact=rel(s).count().lte(makeIntConstant(bitwidth, sig2bound(s))).and(kfact);
            }
            else {
                log.log("SIG "+s.fullname+" BOUND=<"+upper.toString()+">\n");
            }
        }
        // Bound the SUBSETSIGS
        for(int si=0; si<sigs.size(); si++) {
            ParaSig s=sigs.get(si);
            if (!s.subset) continue;
            TupleSet ts=factory.noneOf(1);
            for(ParaSig sup:s.sups()) for(Tuple temp:sig2ts(sup)) ts.add(temp);
            log.log("SUBSETSIG "+s.fullname+" BOUND=<"+ts.toString()+">\n");
            bounds.bound((Relation)(rel(s)),ts); sig2ts(s,ts);
        }
        // Bound the FIELDS
        for(ParaSig s:sigs) if (s!=ParaSig.SIGINT) {
            // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 below
            if (s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) {
                Relation first=null, last=null, next=null;
                TupleSet ts1=null, ts2=null;
                for(ParaSig.Field f:s.fields) {
                    if (f.name.equals("first_")) { first=right(rel(f.full)); ts1=comp(f.halftype, factory); }
                    if (f.name.equals("last_")) { last=right(rel(f.full)); }
                    if (f.name.equals("next_")) { next=right(rel(f.full)); ts2=comp(f.halftype, factory); }
                }
                bounds.bound(first,ts1);
                bounds.bound(last,ts1);
                bounds.bound(next,ts2);
                continue;
            }
            // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 above
            int fi=0;
            for(VarDecl fd:s.decls) {
                for(int fn=fd.names.size(); fn>0; fn--) {
                    ParaSig.Field f=s.fields.get(fi); fi++;
                    TupleSet ts=comp(s.type.product_of_anyEmptyness(fd.value.type), factory); // fulltype
                    bounds.bound((Relation)(rel(f.full)),ts);
                }
            }
        }
        try {
            Formula f;
            String fname;
            if (cmd.check) {
                ParaAssert e=root.asserts.get(cmd.name);
                if (e==null) throw cmd.syntaxError("The assertion \""+cmd.name+"\" cannot be found.");
                f=((Formula)(e.value.accept(this))).not().and(kfact);
                fname="Checking \""+e.name+"\"";
            } else {
                List<ParaFun> ee=root.funs.get(cmd.name);
                if (ee==null || ee.size()<1) throw cmd.syntaxError("The predicate/function \""+cmd.name+"\" cannot be found.");
                if (ee.size()>1) throw cmd.syntaxError("There are more than 1 predicate/function with the name \""+cmd.name+"\"!");
                ParaFun e=ee.get(0);
                Expr v=e.value;
                if (e.type!=null) {
                    Expr vv=e.type;
                    if (vv instanceof ExprUnary) vv=((ExprUnary)vv).makeMult();
                    v=ExprBinary.Op.IN.make(v.pos, v, vv, Type.FORMULA);
                }
                if (e.argCount>0) v=ExprQuant.Op.SOME.make(v.pos, e.decls, v, Type.FORMULA);
                f=((Formula)(v.accept(this))).and(kfact);
                fname="Running \""+e.name+"\"";
            }
            Solver solver = new Solver();
            solver.options().setSolver(SATFactory.MiniSat);
            //solver.options().setSolver(SATFactory.ZChaffBasic);
            //solver.options().setSolver(SATFactory.DefaultSAT4J);
            solver.options().setBitwidth(bitwidth);
            solver.options().setIntEncoding(Options.IntEncoding.BINARY);
            log.log("Solver="+solver.options().solver()+" Bitwidth="+bitwidth+" "+fname+"...\t ");
            log.flush();
            //new MakeJava(f,bitwidth,bounds);
            Solution sol = solver.solve(f,bounds);
            long t1=sol.stats().translationTime();
            long t2=sol.stats().solvingTime();
            switch(sol.outcome()) {
            case TRIVIALLY_SATISFIABLE:
                mainResult=Result.TRIVIALLY_SAT;
                log.log("TIME="+t1+"+"+t2+"="+(t1+t2));
                if (cmd.check) log.log(" TRIVIALLY VIOLATED (SAT)\n"); else log.log(" TRIVIALLY SAT\n");
                break;
            case TRIVIALLY_UNSATISFIABLE:
                mainResult=Result.TRIVIALLY_UNSAT;
                log.log("TIME="+t1+"+"+t2+"="+(t1+t2));
                if (cmd.check) log.log(" TRIVIALLY OK (UNSAT)\n"); else log.log(" TRIVIALLY UNSAT\n");
                break;
            case SATISFIABLE:
                mainResult=Result.SAT;
                log.log("TIME="+t1+"+"+t2+"="+(t1+t2));
                if (cmd.check) log.log(" VIOLATED (SAT)"); else log.log(" SAT");
                log.log(" TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".\n");
                writeXML(sol, units, sigs);
                if (cmd.expects==0) for(Relation r:sol.instance().relations()) log.log("REL "+r+" = "+sol.instance().tuples(r)+"\n");
                break;
            case UNSATISFIABLE:
                mainResult=Result.UNSAT;
                log.log("TIME="+t1+"+"+t2+"="+(t1+t2));
                if (cmd.check) log.log(" OK (UNSAT)"); else log.log(" UNSAT");
                log.log(" TotalVar="+sol.stats().variables()+". Clauses="+sol.stats().clauses()+". PrimaryVar="+sol.stats().primaryVariables()+".\n");
                break;
            }
        } catch(HigherOrderDeclException ex) { log.log("Analysis cannot be performed because it contains higher-order quanitifcation that could not be skolemized.\n");
        } catch(Err ex) { log.log(ex.msg+"\n");
        }
        log.flush();
        return mainResult;
    }

    private void writeXML_tuple(PrintWriter out, String firstatom, Tuple tp) {
        out.printf("    <tuple>");
        if (firstatom!=null) out.printf("  <atom name=\"%s\"/>", firstatom);
        for(int i=0; i<tp.arity(); i++) out.printf(" <atom name=\"%s\"/>", (String)(tp.atom(i)));
        out.printf(" </tuple>%n");
    }

    private void writeXML_tupleset(PrintWriter out, String firstatom, TupleSet tps) {
        for(Tuple tp:tps) writeXML_tuple(out, firstatom, tp);
    }

    private void writeXML(Solution sol, List<Unit> units, List<ParaSig> sigs) {
        FileWriter fw=null;
        BufferedWriter bw=null;
        PrintWriter out=null;
        try {
            fw=new FileWriter(".alloy.xml");
            bw=new BufferedWriter(fw);
            out=new PrintWriter(bw);
        } catch(IOException ex) {
            if (out!=null) { out.close(); out=null; }
            if (bw!=null) { try {bw.close();} catch(IOException exx) {} bw=null; }
            if (fw!=null) { try {fw.close();} catch(IOException exx) {} fw=null; }
            throw new ErrorInternal(null,null,"writeXML failed: "+ex.toString());
        }
        if (sol.outcome()!=Outcome.SATISFIABLE) return;
        Instance inst=sol.instance();
        Evaluator eval=new Evaluator(inst);
        out.printf("<solution name=\"%s\">%n", "this");
        Set<Relation> rels=new LinkedHashSet<Relation>(inst.relations());
        for(Unit u:units) {
            String n=u.aliases.get(0);
            out.printf("%n<module name=\"%s\">%n", (n.length()==0?"this":n));
            if (u==units.get(0)) {
                out.printf("<sig name=\"univ\">%n");
                for(Tuple t:eval.evaluate(kuniv)) {
                    String atom=(String)(t.atom(0));
                    if (atom.startsWith("Int_")) continue;
                    out.printf("  <atom name=\"%s\"/>%n", atom);
                }
                out.printf("</sig>%n");
                /*
                out.printf("<sig name=\"Int\" extends=\"univ\">%n");
                for(Tuple t:eval.evaluate(Relation.INTS)) {
                    String atom=(String)(t.atom(0));
                    out.printf("  <atom name=\"%s\"/>%n", atom);
                }
                out.printf("</sig>%n");
                */
            }
            for(Map.Entry<String,ParaSig> e:u.sigs.entrySet()) {
                String lastatom="";
                ParaSig s=e.getValue();
                Relation r=(Relation)(rel(s));
                rels.remove(r);
                if (s.sup()!=null)
                    out.printf("<sig name=\"%s\" extends=\"%s\">%n", s.fullvname, s.sup().fullvname);
                else if (!s.subset)
                    out.printf("<sig name=\"%s\" extends=\"univ\">%n", s.fullvname);
                else
                    out.printf("<sig name=\"%s\" extends=\"zzz\">%n", s.fullvname);
                for(Tuple t:inst.tuples(r)) {
                    lastatom=(String)(t.atom(0));
                    out.printf("  <atom name=\"%s\"/>%n", lastatom);
                }
                if (s.pos!=null && s.pos.filename!=null && s.pos.filename.endsWith("util/ordering.als")
                        && s.name.equals("Ord") && u.params.get("elem")!=null) {
                    // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 below
                    Relation rfirst = right(rel(s.fields.get(0).full));
                    Relation rlast = right(rel(s.fields.get(1).full));
                    Relation rnext = right(rel(s.fields.get(2).full));
                    rels.remove(rfirst);
                    rels.remove(rlast);
                    rels.remove(rnext);
                    out.printf("  <field name=\"first\" arity=\"2\">%n");
                    out.printf("    <type> %s </type>%n", u.params.get("elem").fullvname);
                    writeXML_tupleset(out, lastatom, inst.tuples(rfirst));
                    out.printf("  </field>");
                    out.printf("  <field name=\"last\" arity=\"2\">%n");
                    out.printf("    <type> %s </type>%n", u.params.get("elem").fullvname);
                    writeXML_tupleset(out, lastatom, inst.tuples(rlast));
                    out.printf("  </field>");
                    out.printf("  <field name=\"next\" arity=\"3\">%n");
                    out.printf("    <type> ( %s ) -> ( %s ) </type>%n", u.params.get("elem").fullvname, u.params.get("elem").fullvname);
                    writeXML_tupleset(out, lastatom, inst.tuples(rnext));
                    out.printf("  </field>%n");
                    // zzz SPECIAL COMPATIBILITY HACK WITH ALLOY3 above
                } else {
                    int fi=0;
                    for(VarDecl fd:s.decls) for(String fn:fd.names) {
                        ParaSig.Field f=s.fields.get(fi); fi++;
                        Relation rf=(Relation)(rel(f.full));
                        rels.remove(rf);
                        out.printf("  <field name=\"%s\" arity=\"%d\">%n", fn, rf.arity());
                        Type type=f.full.fulltype;
                        for(Type.Rel t:type) {
                            out.printf("    <type>");
                            for(int ti=1; ti<t.basicTypes.size(); ti++) {
                                if (ti>1) out.printf(" -> ");
                                out.printf(" (%s) ", t.basicTypes.get(ti).fullvname);
                            }
                            out.printf("</type>%n");
                            writeXML_tupleset(out, null, inst.tuples(rf));
                        }
                        out.printf("  </field>%n");
                    }
                }
                out.printf("</sig>%n");
            }
            out.printf("</module>%n");
        }
        //for(Relation r:rels) { }
        out.printf("%n</solution>%n");
        out.flush();
        out.close();
        try {bw.close();} catch(IOException ex) {throw new ErrorInternal(null,null,"writeXML failed: "+ex.toString());}
        try {fw.close();} catch(IOException ex) {throw new ErrorInternal(null,null,"writeXML failed: "+ex.toString());}
    }

}
