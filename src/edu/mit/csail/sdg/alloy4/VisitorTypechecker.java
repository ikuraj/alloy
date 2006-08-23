package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import salvo.jesus.graph.DirectedGraph;
import salvo.jesus.graph.DirectedGraphImpl;
import salvo.jesus.graph.Vertex;

/**
 * This class computes both the bounding type and the relevant types.
 *
 * This typechecker is first used as a Return Visitor during the first pass,
 * computing the "bounding type" of parent expressions
 * based on the bounding types of its children.
 * It can detect some irrelevant expressions.
 *
 * During the second pass, it is used as a Void Visitor,
 * computing the "relevant type" top-down:
 * the relevant type of the parent is used to compute
 * the relevant types of its children.
 * The second pass is needed to resolve field/function/predicate overloading.
 * It also detects some more irrelevant expressions.
 *
 * In general, during 1st pass, we allow unresolved types to propagate
 * until the 2nd pass (this allows more overloading...)
 * But there are a few places we require resolved types before proceeding:
 * (1) Parameters to a function/predicate call
 * (2) let x=a | (b)           We fully resolve a, then fully resolve b as well
 * (3) all/some... x:a | (b)   We fully resolve a, then fully resolve b as well
 * etc.
 */

public final class VisitorTypechecker extends VisitDesugar implements VisitDesugar2 {
	
//	################################################################################################
	
	private Map<Expr,List<Object>> objChoices = new LinkedHashMap<Expr,List<Object>>();
	private Map<Expr,List<Type>> typeChoices = new LinkedHashMap<Expr,List<Type>>();
	public Env env=new Env();
	private final Log log;
	public VisitorTypechecker(Log log) { this.log=log; }
	
//	Helper functions
	
	private void cform(Expr x) { if (!x.type.isBool) throw x.typeError("This must be a formula expression! Instead, it has the following possible type(s): "+x.type); }
	
	private void cint(Expr x) { if (!x.type.isInt) throw x.typeError("This must be an integer expression! Instead, it has the following possible type(s): "+x.type); }
	
	private Type cset(Expr x) { if (x.type.size()==0) throw x.typeError("This must be a set or relation! Instead, it has the following possible type(s): "+x.type); return x.type; }
	
	private void cform(Type p,Expr x) { if (!p.isBool) throw x.typeError("This must be a formula expression! Instead, it has the following possible type(s): "+p); }
	
	private void cset(Type p,Expr x) { if (p.size()==0) throw x.typeError("This must be a set or relation! Instead, it has the following possible type(s): "+p); }
	
	private boolean isbad(Type x) { return !x.isBool && !x.isInt && x.size()==0; }
	
	private void resolved(Type p,Expr x) {
		if (!p.isBool && !p.isInt && p.size()==0) throw x.typeError("This expression failed to be typechecked, because it has no possible type!");
		if (p.isBool && p.isInt) throw x.typeError("This expression is ambiguous! It has the following possible types: "+p);
		if ((p.isBool || p.isInt) && p.size()>0) throw x.typeError("This expression is ambiguous! It has the following possible types: "+p);
		if (p.size()>0 && p.arity()<=0) throw x.typeError("This expression is ambiguous! It has the following possible types: "+p);
	}
	
	private void resolved(Expr x) {
		if (!x.type.isBool && !x.type.isInt && x.type.size()==0) throw x.typeError("This expression failed to be typechecked, because it has no possible type!");
		if (x.type.isBool && x.type.isInt) throw x.typeError("This expression is ambiguous! It has the following possible types: "+x.type);
		if ((x.type.isBool || x.type.isInt) && x.type.size()>0) throw x.typeError("This expression is ambiguous! It has the following possible types: "+x.type);
		if (x.type.size()>0 && x.type.arity()<=0) throw x.typeError("This expression is ambiguous! It has the following possible types: "+x.type);
	}
	
//	################################################################################################
	
	/*
	 // 1. ExprCall will always be calls to a Predicate or Function with 1 or more arguments.
	  // 2. ExprCall will not contain excess arguments. (Those will be converted into ExprJoin expressions)
	   // 3. ExprName may still resolve to calls to a Predicate or Function with 0 arguments.
	    */
	
	public Expr resolve(Expr x) { // If this method succeeds, x will have a fully-resolved unique-arity Type
		x=x.accept(this);
		x=x.accept(this, x.type);
		resolved(x);
		return x;
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprBinary x) {
		Expr left=x.left.accept(this);
		Expr right=x.right.accept(this);
		Type a,b,c=null;
		bigbreak: switch(x.op) {
		
		case LT: case LTE: case GT: case GTE: cint(left); cint(right); c=Type.FORMULA; break;
		
		case AND: case OR: case IFF: case IMPLIES: cform(left); cform(right); c=Type.FORMULA; break;
		
		case PLUSPLUS:
			a=cset(left); b=cset(right);
			// RESULT TYPE = LEFT+RIGHT, if exists a in left, b in right, such that "a.arity==b.arity" && "dom(a)&dom(b) is non-empty"
			for (Type.Rel bb:b)
				if (a.hasArity(bb.arity()))
					for (Type.Rel aa:a)
						if (aa.arity()==bb.arity() && aa.basicTypes.get(0).intersect(bb.basicTypes.get(0)).isNonEmpty())
						{ c=a.union(b); break bigbreak; }
			throw x.typeError("The ++ operator failed because its right hand side cannot override the left hand side!",a,b);
			
		case PLUS:
			// RESULT TYPE = LEFT+RIGHT, if exists a in left, b in right, such that a.arity==b.arity
			a=left.type; b=right.type;
			if (a.hasCommonArity(b)) { c=a.union(b); if (a.isInt && b.isInt) c=Type.makeInt(c); break; }
			if (a.isInt && b.isInt) { c=Type.INT; break; } // left.type=right.type=c=Type.INT; break; }
			throw x.typeError("The + operator can only be used between 2 sets and relations of the same arity, or between 2 integer expressions!",a,b);
			
		case MINUS:
			// RESULT TYPE = LEFT TYPE, if { a&b | a in leftType, b in rightType, a.arity==b.arity } is nonempty()
			a=left.type; b=right.type;
			if (a.size()>0 || b.size()>0) {
				if (a.intersect(b).hasTuple()) { c=a; if (a.isInt && b.isInt) c=Type.makeInt(c); break; }
				throw x.typeError("The - operator failed here, because the two expressions are disjoint!",a,b);
			}
			if (a.isInt && b.isInt) { c=Type.INT; break; } // left.type=right.type=c=Type.INT; break; }
			throw x.typeError("The - operator can only be used between 2 sets and relations of the same arity, or between 2 integer expressions!",a,b);
			
		case INTERSECT:
			// RESULT TYPE = { a&b | a in leftType, b in rightType, a.arity==b.arity } if it's nonempty()
			a=cset(left); b=cset(right); c=a.intersect(b); if (c.hasTuple()) break;
			throw x.typeError("The & operator failed because there is an arity mismatch, or the 2 expressions are disjoint!",a,b);
			
		case ARROW: case ANY_ARROW_SOME: case ANY_ARROW_ONE: case ANY_ARROW_LONE:
		case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
		case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE: case ONE_ARROW_LONE:
		case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:
			// RESULT TYPE = { a->b  |  a in leftType,  b in rightType, a.isEmpty()==b.isEmpty() } if its size()>0
			a=cset(left); b=cset(right); c=a.product_of_sameEmptyness(b);  if (c.size()>0) break;
			throw x.typeError("The -> operator cannot combine empty and non-empty types!",a,b);
			
		case DOMAIN:
			// RESULT TYPE = { (B1&A)->B2->B3 | exists unary A in left, exists B1->B2->B3 in right } if nonempty()
			a=cset(left); b=cset(right); c=b.domainRestrict(a);  if (c.hasTuple()) break;
			throw x.typeError("The <: operator failed, because the left and domain(right) are disjoint!",a,b);
			
		case RANGE:
			// RESULT TYPE = { A1->A2->(A3&B) | exists unary B in right, exists A1->A2->A3 in left } if nonempty()
			a=cset(left); b=cset(right); c=a.rangeRestrict(b); if (c.hasTuple()) break;
			throw x.typeError("The :> operator failed, because range(left) and right are disjoint!",a,b);
			
		case IN:
			// SUCCESS if exists a in left, and b in right, such that (a.arity==b.arity && a&b!=NONE)
			a=cset(left); b=cset(right); c=a.intersect(b);
			if (c.size()==0 || (a.hasTuple() && b.hasTuple() && c.hasNoTuple())) throw x.typeError("Subset operator failed, because the types are disjoint",a,b);
			if (a.hasNoTuple()) throw x.typeError("Redundant constraint: left type is empty",a,b);
			c=Type.FORMULA;
			break;
			
		case EQUALS:
			// SUCCESS if exists a in left, and b in right, such that (a.arity==b.arity && a&b!=NONE)
			a=left.type; b=right.type; c=a.intersect(b);
			if (c.size()!=0 && (a.hasNoTuple() || b.hasNoTuple() || c.hasTuple())) {c=Type.FORMULA; break;}
			if (a.isInt && b.isInt) { /*left.type=right.type=Type.INT;*/ c=Type.FORMULA; break; }
			throw x.typeError("The = operator can only be used between 2 nondisjoint sets and relations, or 2 integer expressions!",a,b);
		}
		if (c==null) throw x.internalError("Unexpected operator ("+x.op+") encountered in ExprBinary typechecker");
		return x.op.make(x.pos, left, right, c);
	}
	
//	################################################################################################
	
	public final Expr accept(ExprBinary x, Type p) {
		Type a=x.left.type, b=x.right.type;
		switch(x.op) {
		
		case IN: b=a.intersect(b);
		// Intentional fall-through to the "case EQUALS" case.
		case AND: case OR: case IFF: case IMPLIES: case LT: case LTE: case GT: case GTE: case EQUALS:
			if (!p.isBool) throw x.typeError("This must be a set or relation!");
			break;
			
		case INTERSECT:
			// leftType'=parentType & leftType.  rightType'=parentType & rightType.
			if (p.size()==0) throw x.typeError("This must be a set or relation!");
			a=p.intersect(a); b=p.intersect(b);
			break;
			
		case MINUS:
			if (p.isInt && p.size()>0) throw x.typeError("This expression is ambiguous! Possible type(s) include: "+p);
			if (p.isInt) { a=Type.INT; b=Type.INT; break; }
			// leftType'=parentType.   rightType'=parentType & rightType.
			if (p.size()==0) throw x.typeError("This must be an integer, a set or a relation!");
			a=p;  b=p.intersect(b);
			if (b.hasNoTuple()) throw x.typeError("Inessential difference (right expression is redundant)",a,b);
			break;
			
		case PLUS:
			if (p.isInt && p.size()>0) throw x.typeError("This expression is ambiguous! Possible type(s) include: "+p);
			if (p.isInt) { a=Type.INT; b=Type.INT; break; }
			if (p.size()==0) throw x.typeError("This must be an integer, a set or a relation!");
			// Intentional fall-through to the PLUSPLUS case
		case PLUSPLUS:
			// If child.type & parent.type is empty, an inessential union error is reported.
			// Otherwise, child.type := child.type & parent.type
			// ALSO: for OVERRIDE, make sure the essential left and right types are override-compatible
			if (p.size()==0) throw x.typeError("This must be a set or a relation!");
			a=p.intersect(a); if (a.hasNoTuple()) throw x.typeError("Inessential union: the left expression is redundant",a,b);
			b=p.intersect(b); if (b.hasNoTuple()) throw x.typeError("Inessential union: the right expression is redundant",a,b);
			if (x.op==ExprBinary.Op.PLUSPLUS && !b.canOverride(a)) throw x.typeError("Relevant types incompatible for relational override",a,b);
			break;
			
		case ARROW: case ANY_ARROW_SOME: case ANY_ARROW_ONE: case ANY_ARROW_LONE:
		case SOME_ARROW_ANY: case SOME_ARROW_SOME: case SOME_ARROW_ONE: case SOME_ARROW_LONE:
		case ONE_ARROW_ANY: case ONE_ARROW_SOME: case ONE_ARROW_ONE: case ONE_ARROW_LONE:
		case LONE_ARROW_ANY: case LONE_ARROW_SOME: case LONE_ARROW_ONE: case LONE_ARROW_LONE:{
			// leftType'  == {r1 | r1 in leftType and there exists r2 in rightType such that r1->r2 in parentType}
			// rightType' == {r2 | r2 in rightType and there exists r1 in leftType such that r1->r2 in parentType}
			if (p.size()==0) throw x.typeError("This must be a set or a relation!");
			Type leftType = Type.make();
			Type rightType = Type.make();
			for (Type.Rel ar:a)
				for (Type.Rel br:b)
					if (ar.isEmpty()==br.isEmpty() && p.hasArity(ar.arity()+br.arity())) {
						for (Type.Rel cr:p.intersect(Type.make(ar.product(br)))) {
							if (cr.isEmpty()) continue;
							List<ParaSig> bts = cr.basicTypes;
							leftType = leftType.union(Type.make(bts,0,ar.arity()));
							rightType = rightType.union(Type.make(bts,ar.arity(),bts.size()));
						}
					}
			a=leftType; b=rightType; break;}
		
		case DOMAIN:{
			// leftType' = {r1 | r1 in leftType and there exists r2 in rightType such that r1<:r2 in parentType}
			// rightType' = {r2 | r2 in rightType and there exists r1 in leftType such that r1<:r2 in parentType}
			if (p.size()==0) throw x.typeError("This must be a set or a relation!");
			Type leftType = Type.make();
			Type rightType = Type.make();
			for (Type.Rel ar:a) if (ar.arity()==1)
				for (Type.Rel br:b) if (p.hasArity(br.arity())) {
					Type.Rel r=br.columnRestrict(ar.basicTypes.get(0), 0);
					if (r.isEmpty()) continue;
					for (Type.Rel cr:p.intersect(Type.make(r))) {
						List<ParaSig> bts = cr.basicTypes;
						leftType = leftType.union(Type.make(bts, 0, 1));
						rightType = rightType.union(Type.make(bts, 0, bts.size()));
					}
				}
			a=leftType; b=rightType; break;}
		
		case RANGE:{
			// leftType' = {r1 | r1 in leftType and there exists r2 in rightType such that r1:>r2 in parentType}
			// rightType' = {r2 | r2 in rightType and there exists r1 in leftType such that r1:>r2 in parentType}
			if (p.size()==0) throw x.typeError("This must be a set or a relation!");
			Type leftType = Type.make();
			Type rightType = Type.make();
			for (Type.Rel br:b) if (br.arity()==1)
				for (Type.Rel ar:a) if (p.hasArity(ar.arity())) {
					Type.Rel r=ar.columnRestrict(br.basicTypes.get(0), ar.arity()-1);
					if (r.isEmpty()) continue;
					for (Type.Rel cr:p.intersect(Type.make(r))) {
						List<ParaSig> bts = cr.basicTypes;
						leftType = leftType.union(Type.make(bts,0,bts.size()));
						rightType = rightType.union(Type.make(bts,bts.size()-1, bts.size()));
					}
				}
			a=leftType; b=rightType; break;}
		}
		Expr left=x.left.accept(this,a);
		Expr right=x.right.accept(this,b);
		return x.op.make(x.pos, left, right, p);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprITE x) {
		Expr cond=x.cond.accept(this); cform(cond);
		Expr left=x.left.accept(this);
		Expr right=x.right.accept(this);
		Type a=left.type, b=right.type, c=null;
		// RESULT TYPE = LEFT+RIGHT, if exists a in left, b in right, such that a.arity==b.arity
		if (a.size()>0 && b.size()>0 && a.hasCommonArity(b)) c=a.union(b);
		if (a.isInt && b.isInt) { if (c==null) c=Type.INT; else c=Type.makeInt(c); }
		if (a.isBool && b.isBool) { if (c==null) c=Type.FORMULA; else c=Type.makeBool(c); }
		if (c==null || isbad(c)) throw x.typeError("The THEN-clause and the ELSE-clause must match! Its THEN-clause has type "+a+" and the ELSE clause has type "+b);
		return new ExprITE(x.pos, cond, left, right, c);
	}
	
//	################################################################################################
	
	public final Expr accept(ExprITE x, Type p) {
		Type a=x.left.type, b=x.right.type;
		resolved(p,x);
		if (p.size()>0) {
			// If child.type & parent.type is empty, an inessential union error is reported.
			// Otherwise, child.type := child.type & parent.type
			if (a.hasTuple()) {
				a=a.intersect(p);
				if (a.hasNoTuple()) throw x.typeError("Inessential If-Then-Else: the left expression is redundant");
			}
			if (b.hasTuple()) {
				b=b.intersect(p);
				if (b.hasNoTuple()) throw x.typeError("Inessential If-Then-Else: the right expression is redundant");
			}
		}
		Expr cond=x.cond.accept(this, x.cond.type);
		Expr left=x.left.accept(this, a);
		Expr right=x.right.accept(this, b);
		return new ExprITE(x.pos, cond, left, right, p);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprLet x) {
		Expr right=resolve(x.right);
		env.put(x.left, right.type);
		Expr sub=x.sub.accept(this);
		env.remove(x.left);
		if (isbad(sub.type)) throw sub.typeError("The body of a LET expression must be a set, an integer, or a formula!");
		return new ExprLet(x.pos, x.left, right, sub, sub.type);
	}
	
//	################################################################################################
	
	public Expr accept(ExprLet x, Type p) {
		resolved(p,x);
		env.put(x.left, x.right.type);
		Expr sub=x.sub.accept(this,p);
		env.remove(x.left);
		return new ExprLet(x.pos, x.left, x.right, sub, p);
	}
	
//	################################################################################################
	
	public Object root=null;
	public Unit rootunit=null;
	
	private Set<Object> populate(String x1) {
		Set<Object> y;
		String x2=(x1.charAt(0)=='@') ? x1.substring(1) : x1;
		Object y3=env.get(x2); if (y3!=null) { y=new LinkedHashSet<Object>(); y.add(y3); return y; }
		if (root instanceof ParaSig.Field) {
			ParaSig.Field rt=(ParaSig.Field)root;
			ParaSig rts=rt.parent();
			if (x2.equals("this")) { y=new LinkedHashSet<Object>(); y.add(rts.type); return y; }
			y=rootunit.lookup_sigORparam(x2);
			ParaSig.Field y2=rootunit.lookup_Field(rts, x2, rt.name);
			if (y2!=null) { y.add(y2); if (y2.halftype==null) throw new ErrorInternal(y2.pos, y2, "This field is being referenced before it is typechecked!"); }
		}
		else if (root instanceof ParaFun) {
			y=rootunit.lookup_sigORparam(x2);
			for(Object y2:rootunit.lookup_Field(x2)) if (y2 instanceof ParaSig.Field) y.add(((ParaSig.Field)y2).full);
		}
		else if (root instanceof ParaSig) {
			if (x2.equals("this")) { y=new LinkedHashSet<Object>(); y.add( ((ParaSig)root).type ); return y; }
			y=rootunit.lookup_SigParamFunPred(x2);
			ParaSig.Field y22=rootunit.lookup_Field((ParaSig)root,x2);
			for(Object y2:rootunit.lookup_Field(x2)) if (y2 instanceof ParaSig.Field)
			{if (y2==y22) y.add(y2); else if (y22==null) y.add(((ParaSig.Field)y2).full); }
		}
		else if (root==null) {
			y=rootunit.lookup_SigParamFunPred(x2);
			for(Object y2:rootunit.lookup_Field(x2)) if (y2 instanceof ParaSig.Field) y.add(((ParaSig.Field)y2).full);
		}
		else {
			y=new LinkedHashSet<Object>();
		}
		return y;
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprConstant x) { return x; }
	
//	################################################################################################
	
	public Expr accept(ExprConstant x, Type p) {
		if (x.op==ExprConstant.Op.NUMBER) {
			if (!p.isInt) throw x.typeError("This must be an integer expression");
		} else if (x.op==ExprConstant.Op.IDEN) {
			if (p.arity()!=2) throw x.typeError("This must be a binary relation.");
		} else {
			if (p.arity()!=1) throw x.typeError("This must be a set.");
		}
		return x;
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprName x) {
		List<Object> objects=new ArrayList<Object>();
		List<Type> types=new ArrayList<Type>();
		Set<Object> y=populate(x.name); if (y.size()==0) ExprName.hint(x.pos, x.name);
		Type t=null,tt;
		for(Object z:y) {
			Object obj=z;
			if (z instanceof Type) tt=(Type)z;
			else if (z instanceof ParaSig) tt=((ParaSig)z).type;
			else if (z instanceof ParaSig.Field) {if (x.name.charAt(0)!='@') tt=((ParaSig.Field)z).halftype; else {obj=((ParaSig.Field)z).full; tt=((ParaSig.Field)z).full.fulltype;}}
			else if (z instanceof ParaSig.Field.Full) tt=((ParaSig.Field.Full)z).fulltype;
			else if (z instanceof ParaFun && ((ParaFun)z).argCount==0 && ((ParaFun)z).type!=null) tt=((ParaFun)z).type.type;
			else if (z instanceof ParaFun && ((ParaFun)z).argCount==0 && ((ParaFun)z).type==null) tt=Type.FORMULA;
			else continue;
			if (t==null) t=tt; else t=t.merge(tt);
			objects.add(obj);
			types.add(tt);
		}
		if (t==null || isbad(t)) throw x.typeError("The name \""+x.name+"\" failed to be typechecked here!");
		ExprName ans=new ExprName(x.pos, x.name, null, t);
		objChoices.put(ans, objects);
		typeChoices.put(ans, types);
		return ans;
	}
	
//	################################################################################################
	
	public Expr accept(ExprName x, Type t) {
		List<Object> objects=objChoices.get(x);
		objChoices.remove(x);
		List<Type> types=typeChoices.get(x);
		typeChoices.remove(x);
		Object match=null;
		for(int i=0; i<objects.size(); i++) {
			Object z=objects.get(i);
			Type tt=types.get(i);
			if (t==tt
					||(!t.isInt && !t.isBool && t.hasNoTuple())
					||(t.isInt && tt.isInt)
					||(t.isBool && tt.isBool)
					||t.intersect(tt).hasTuple()) {
				if (match!=null) throw x.typeError("The name \""+x.name+"\" is ambiguous here due to multiple match: "+match+" and "+z);
				match=z;
			}
		}
		if (match==null) throw x.typeError("The name \""+x.name+"\" failed to be typechecked here due to no match!");
		if (match instanceof ParaSig)
			return new ExprName(x.pos, ((ParaSig)match).fullname, match, t);
		if (match instanceof ParaSig.Field)
			return new ExprName(x.pos, ((ParaSig.Field)match).fullname, match, t);
		if (match instanceof ParaSig.Field.Full)
			return new ExprName(x.pos, ((ParaSig.Field.Full)match).fullname, match, t);
		if (match instanceof ParaFun)
			return new ExprName(x.pos, x.name, match, t);
		return new ExprName(x.pos, x.name, null, t);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprQuant x) {
		List<VarDecl> list=new ArrayList<VarDecl>();
		Type comp=null; // Stores the Union Type for a Set Comprehension expression
		for(int i=0;i<x.list.size();i++) {
			VarDecl d=x.list.get(i);
			VarDecl dd=new VarDecl(d, resolve(d.value));
			Expr v=dd.value;
			if (v.type.size()==0) cset(v);
			if (v.type.hasNoTuple()) throw v.typeError("This expression must not be an empty set!");
			if (x.op==ExprQuant.Op.COMPREHENSION) {
				if (v.type.arity()!=1) throw v.typeError("This expression must be a unary set!");
				for(int j=0; j<d.names.size(); j++) if (comp==null) comp=v.type; else comp=comp.product_of_sameEmptyness(v.type);
			}
			for(String j:d.names) env.put(j, v.type);
			list.add(dd);
		}
		Expr sub=x.sub.accept(this);
		for(int i=0; i<list.size(); i++) {
			VarDecl d=list.get(i);
			for(String j:d.names) env.remove(j);
		}
		if (x.op==ExprQuant.Op.COMPREHENSION) {
			if (comp==null || comp.hasNoTuple()) throw x.typeError("This set comprehension expression is always empty!");
			cform(sub);
		}
		else if (x.op==ExprQuant.Op.SUM) { cint(sub); comp=Type.INT; }
		else { cform(sub); comp=Type.FORMULA; }
		return x.op.make(x.pos, list, sub, comp);
	}
	
//	################################################################################################
	
	public Expr accept(ExprQuant x, Type p) {
		resolved(p,x);
		for(VarDecl d:x.list) {
			for(String j:d.names) env.put(j, d.value.type);
		}
		Expr sub=x.sub.accept(this, x.sub.type);
		for(VarDecl d:x.list) {
			for(String j:d.names) env.remove(j);
		}
		return x.op.make(x.pos, x.list, sub, p);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprSequence x) {
		List<Expr> list=new ArrayList<Expr>();
		for(int i=0; i<x.list.size(); i++) {
			Expr newvalue=x.list.get(i).accept(this);
			cform(newvalue);
			list.add(newvalue);
		}
		return new ExprSequence(x.pos, list);
	}
	
//	################################################################################################
	
	public Expr accept(ExprSequence x, Type t) {
		List<Expr> list=new ArrayList<Expr>();
		cform(t,x);
		for(int i=0; i<x.list.size(); i++) {
			Expr sub=x.list.get(i);
			if (sub==null) break;
			list.add(sub.accept(this, sub.type));
		}
		return new ExprSequence(x.pos, list);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprUnary x) {
		Type ans=null;
		Expr sub=x.sub.accept(this);
		switch(x.op) {
		
		case NOT:
			cform(sub); ans=Type.FORMULA; break;
			
		case SOMEMULT: case LONEMULT: case ONEMULT: case SETMULT:
			cset(sub); ans=sub.type; break;
			
		case SOME: case LONE: case ONE: case NO:
			cset(sub); ans=Type.FORMULA; break;
			
		case TRANSPOSE:
			cset(sub); ans=sub.type.transpose(); break;
			
		case RCLOSURE: case CLOSURE:
			// TYPE(^X) = ^TYPE(X)
			// TYPE(^X) = ^TYPE(X) + UNIV->UNIV
			// If TYPE(X) doesn't contain at least one Relation Type of arity 2, report an error!
			// If size of ^TYPE(X).^TYPE(X) is 0, report an error!
			cset(sub); if (!sub.type.hasArity(2)) throw sub.typeError("This expression's arity must be 2!");
			ans=sub.type.closure();
			if (ans.join(ans).size()==0) throw x.typeError("redundant closure operation (domain and range are disjoint)");
			if (x.op==ExprUnary.Op.RCLOSURE) ans=ans.union(Type.make(ParaSig.UNIV,ParaSig.UNIV));
			break;
			
		case CARDINALITY:
			cset(sub); ans=Type.INT; break;
			
		case INTTOATOM:
			cint(sub); ans=ParaSig.SIGINT.type; break;
			
		case SUM:
			// Report an error if TYPE(Subexpression) has empty intersection with SIGINT
			cset(sub); if (sub.type.intersect(ParaSig.SIGINT.type).hasTuple()) {ans=Type.INT; break;}
			throw sub.typeError("This expression must contain integer atoms! Instead, its possible type(s) are: "+x.sub.type);
		}
		return x.op.make(x.pos, sub, ans);
	}
	
//	################################################################################################
	
	public final Expr accept(ExprUnary x, Type p) {
		Type subtype=x.sub.type;
		resolved(p,x);
		switch(x.op) {
		case SOMEMULT: case LONEMULT: case ONEMULT: case SETMULT:
			cset(p,x); subtype=p;
			break;
		case TRANSPOSE:
			// exprType' = {r1 | r1 in exprType AND ~r1 in unaryExprType}
			cset(p,x); subtype=subtype.transpose().intersect(p).transpose();
			if (p.hasTuple() && subtype.hasNoTuple()) throw x.sub.typeError("Subexpression does not contribute to relevant type of parent");
			break;
		case RCLOSURE: case CLOSURE:
			// exprType' = {r1 | r1 in exprType AND there exist basic types
			// b1 and b2 such that b1->b2 in unaryExprType AND r1 is on the path from b1 to b2}
			cset(p,x); subtype = closureResolveChild(p, subtype);
			if (p.hasTuple() && subtype.hasNoTuple()) throw x.sub.typeError("Subexpression does not contribute to relevant type of parent");
			break;
		case INTTOATOM:
			cset(p,x); if (!p.isSubsetOf(ParaSig.SIGINT.type)) throw x.typeError("This expression should have been a subset of Int!");
			break;
		}
		Expr sub=x.sub.accept(this, subtype);
		return x.op.make(x.pos, sub, p);
	}
	
//	################################################################################################
	
	private boolean applicable(ParaFun f,List<Expr> args) {
		int argi=0;
		for(VarDecl d:f.decls) {
			for(int j=0; j<d.names.size(); j++) {
				Type arg=args.get(argi).type;
				argi++;
				if (arg.size()==0) continue;
				if (d.value.type.size()==0) continue; // This should not happen, though.
				if (arg.hasNoTuple() || d.value.type.hasNoTuple()) if (arg.arity()==d.value.type.arity()) continue;
				if (arg.intersect(d.value.type).hasTuple()) continue;
				return false;
			}
		}
		return true;
	}
	
	private boolean containsApplicable(Set<Object> x) {
		for(Object y:x) if (y instanceof ParaFun && ((ParaFun)y).argCount>0) return true;
		return false;
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprJoin x) {
		// This is not optimal. eg. given b.a.(func[x,y,z]), "a" and "b" will be forced to be locally-unambiguous.
		// Another inefficiency: we don't jump forward, so sublists are re-Desugared again and again until the end of list.
		Expr ptr=x.right;
		while(ptr instanceof ExprJoin) ptr=((ExprJoin)ptr).right;
		if (ptr instanceof ExprName) {
			String name=((ExprName)ptr).name;
			Set<Object> y=populate(name);
			List<Object> objects=new ArrayList<Object>();
			List<Type> types=new ArrayList<Type>();
			if (y.size()>0 && containsApplicable(y)) {
				List<Expr> args=new ArrayList<Expr>();
				ptr=x;
				while(ptr instanceof ExprJoin) {
					Expr left=((ExprJoin)ptr).left;
					left=resolve(left); cset(left); ptr=((ExprJoin)ptr).right; args.add(0,left);
				}
				Type ans=null, temp;
				for(Object z:y) {
					if (z instanceof ParaFun && ((ParaFun)z).argCount>0 && ((ParaFun)z).type==null) {
						ParaFun f=(ParaFun)z;
						if (f.argCount!=args.size()) continue;
						if (!applicable(f,args)) continue;
						objects.add(f); types.add(temp=Type.FORMULA);
					} else if (z instanceof ParaFun && ((ParaFun)z).argCount>0 && ((ParaFun)z).type!=null) {
						ParaFun f=(ParaFun)z;
						if (f.argCount>args.size()) continue;
						if (!applicable(f,args)) continue;
						temp=f.type.type;
						for(int fi=f.argCount; fi<args.size(); fi++) temp=args.get(fi).type.join(temp);
						if (temp.hasNoTuple()) continue;
						objects.add(f); types.add(temp);
					} else if ((z instanceof Type) || (z instanceof ParaSig) || (z instanceof ParaSig.Field) || (z instanceof ParaSig.Field.Full)) {
						if (z instanceof Type) temp=(Type)z;
						else if (z instanceof ParaSig) temp=((ParaSig)z).type;
						else if (z instanceof ParaSig.Field.Full) temp=((ParaSig.Field.Full)z).fulltype;
						else if (name.charAt(0)=='@') {temp=((ParaSig.Field)z).full.fulltype; z=((ParaSig.Field)z).full;}
						else temp=((ParaSig.Field)z).halftype;
						for(int fi=0; fi<args.size(); fi++) temp=args.get(fi).type.join(temp);
						if (temp.hasNoTuple()) continue;
						objects.add(z); types.add(temp);
					} else continue;
					if (ans==null) ans=temp; else ans=ans.merge(temp);
				}
				if (ans!=null) { ExprCall xx=new ExprCall(x.pos, name, null, args, ans); objChoices.put(xx,objects); typeChoices.put(xx,types); return xx; }
			}
		}
		// TYPE[A.B] = TYPE[A].TYPE[B] if there exist r1 in TYPE[A] and r2 in TYPE[B],
		// such that r1.arity+r2.arity>2, and range(r1)&dom(r2) nonempty.
		Expr left=x.left.accept(this); cset(left);
		Expr right=x.right.accept(this); cset(right);
		Expr newx=new ExprJoin(x.pos, left, right, left.type.join(right.type));
		if (newx.type.hasNoTuple()) throw newx.typeError("The join operation here always yields an empty set! LeftType="+left.type+" RightType="+right.type);
		return newx;
	}
	
	public Expr accept(ExprJoin x, Type p) {
		// leftType' = {r1 | r1 in leftType and there exists r2 in rightType such that r1.r2 in parentType}
		// rightType' = {r2 | r2 in rightType and there exists r1 in leftType such that r1.r2 in parentType}
		Type leftType = Type.make();
		Type rightType = Type.make();
		for (Type.Rel a: x.left.type)
			for (Type.Rel b: x.right.type)
				if (p.hasArity(a.arity() + b.arity() - 2)) {
					ParaSig joinType = a.basicTypes.get(a.arity()-1).intersect(b.basicTypes.get(0));
					if (joinType.isEmpty()) continue;
					for (Type.Rel c: p.intersect(Type.make(a.join(b)))) {
						if (c.isEmpty()) continue;
						List<ParaSig> bts = new LinkedList<ParaSig>(c.basicTypes);
						bts.add(a.arity()-1, joinType);
						leftType = leftType.union(Type.make(bts, 0, a.arity()));
						rightType = rightType.union(Type.make(bts, a.arity()-1, bts.size()));
					}
				}
		Expr left=x.left.accept(this,leftType);
		Expr right=x.right.accept(this,rightType);
		return new ExprJoin(x.pos, left, right, p);
	}
	
//	################################################################################################
	
	@Override public Expr accept(ExprCall x) { throw x.internalError("ExprCall objects shouldn't be encountered during the first pass!"); }
	
	public Expr accept(ExprCall x, Type t) {
		resolved(t,x);
		List<Object> objects=objChoices.get(x);
		objChoices.remove(x);
		List<Type> types=typeChoices.get(x);
		typeChoices.remove(x);
		if (objects==null || types==null) throw x.internalError("Unknown ExprCall object encountered!");
		Object match=null;
		for(int i=0; i<objects.size(); i++) {
			Object a=objects.get(i);
			Type b=types.get(i);
			if (t==b
					||(!t.isInt && !t.isBool && t.hasNoTuple())
					||(t.isInt && b.isInt)
					||(t.isBool && b.isBool)
					||t.intersect(b).hasTuple())
			{ if (match==null) match=a; else throw x.typeError("The name is ambiguous here. There are at least 2 matches: "+match+" and "+a); }
		}
		Expr ans=null;
		int r=0;
		if (match instanceof ParaFun) {
			ParaFun y=(ParaFun)match;
			if (y.argCount==0) ans=new ExprName(x.pos, y.name, y, (y.type==null?Type.FORMULA:y.type.type));
			else
			{
				List<Expr> newlist=new ArrayList<Expr>();
				for(int newlen=0; newlen<y.argCount; newlen++) {newlist.add(x.args.get(r)); r++;}
				ans=new ExprCall(x.pos, y.name, y, newlist, (y.type==null?Type.FORMULA:y.type.type));
			}
			if (ans.type==null || (!ans.type.isInt && !ans.type.isBool && ans.type.size()==0))
				throw x.internalError("ExprCall encountered before all function/predicate return types are typechecked");
		} else if (match instanceof ParaSig) {
			ParaSig s=(ParaSig)match;
			ans=new ExprName(x.pos, s.fullname, match, s.type);
		} else if (match instanceof ParaSig.Field) {
			ParaSig.Field s=(ParaSig.Field)match;
			ans=new ExprName(x.pos, s.fullname, match, s.halftype);
		} else if (match instanceof ParaSig.Field.Full) {
			ParaSig.Field.Full s=(ParaSig.Field.Full)match;
			ans=new ExprName(x.pos, s.fullname, match, s.fulltype);
		} else if (match instanceof Type) {
			ans=new ExprName(x.pos, x.name, null, (Type)match);
		} else throw x.internalError("ExprCall resolved to an unknown object type: "+match);
		for(;r<x.args.size();r++) {
			Expr ans3=x.args.get(r);
			Expr ans2=new ExprJoin(x.pos, ans3, ans, ans3.type.join(ans.type));
			if (ans2.type.size()==0) throw x.internalError("Resolved function call argument should have been valid sets or relations");
			ans=ans2;
		}
		return ans;
	}
	
//	################################################################################################
	
	public void accept(ParaAssert x, Unit u) {
		x.value=resolve(x.value);
	}
	
//	################################################################################################
	
	public void accept(ParaFact x, Unit u) {
		x.value=resolve(x.value);
	}
	
//	################################################################################################
	
	public ParaSig accept(ParaSig x, Unit u) {
		// When typechecking the fields:
		// * each field is allowed to refer to earlier fields in the same SIG,
		//   as well as fields declared in any ancestor sig (as long as those ancestor sigs are visible from here)
		//   as well as any visible SIG.
		// * For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
		//   then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
		int fi=0;
		List<VarDecl> newdecl=new ArrayList<VarDecl>();
		List<VarDecl> olddecl=x.decls;
		x.decls=newdecl;
		for(VarDecl d:olddecl) {
			Expr value=d.value;
			for(int ni=0; ni<d.names.size(); ni++) {
				ParaSig.Field f=x.fields.get(fi);
				if (ni==0) {
					this.root=f; this.rootunit=u; value=this.resolve(value);
					if (value.type.arity()<1) throw x.typeError("Field declaration must be a set or relation, but its type is "+value.type);
				}
				f.halftype = value.type;
				f.full.fulltype = x.type.product_of_anyEmptyness(value.type);
				//f=new Field(f.pos, f.sig, f.name, value.type, x.type.product_of_anyEmptyness(value.type));
				//x.fields.set(fi,f);
				fi++;
				log.log("Unit ["+u.aliases.get(0)+"], Sig "+x.name+", Field "+f.name+": "+f.full.fulltype);
			}
			newdecl.add(new VarDecl(d, value));
		}
		return x; // zzz SHOULD ACTUALLY MAKE A NEW SIG
	}
	
//	################################################################################################
	
	public ParaFun accept(ParaFun fun, Unit u) {
		// Now, typecheck all function/predicate PARAMETERS and RETURNTYPE
		// Each PARAMETER can refer to earlier parameter in the same function, and any SIG or FIELD visible from here.
		// Each RETURNTYPE can refer to the parameters of the same function, and any SIG or FIELD visible from here.
		this.env.clear();
		this.root=fun;
		this.rootunit=u;
		List<VarDecl> newdecls=new ArrayList<VarDecl>();
		for(VarDecl d:fun.decls) {
			Expr value=d.value;
			for(int ni=0; ni<d.names.size(); ni++) {
				String n=d.names.get(ni);
				if (ni==0) value=this.resolve(value);
				if (value.type.arity()<1) throw value.typeError("Function parameter must be a set or relation, but its type is "+value.type);
				this.env.put(n, value.type);
				log.log("Unit ["+u.aliases.get(0)+"], Pred/Fun "+fun.name+", Param "+n+": "+value.type);
			}
			newdecls.add(new VarDecl(d, value));
		}
		Expr type=fun.type;
		if (type!=null) {
			type=this.resolve(type);
			if (type.type.arity()<1) throw type.typeError("Function return type must be a set or relation, but its type is "+type.type);
			log.log("Unit ["+u.aliases.get(0)+"], Pred/Fun "+fun.name+", RETURN: "+type.type);
		}
		this.env.clear();
		fun.decls=newdecls; fun.type=type;
		//fun=new ParaFun(fun.pos, fun.path, fun.aliasnum, fun.name, null, newdecls, type, fun.value);
		return fun; // zzz SHOULD ACTUALLY MAKE A NEW PARAFUN
	}
	
//	################################################################################################
	
	public Unit accept(Unit u) {
		// Now, typecheck (1) function/predicate BODIES. (2) Signature facts. (3) Standalone facts (4) Assertions.
		// These can refer to any SIG/FIELD/FUN/PRED visible from here.
		String uu=u.aliases.iterator().next();
		for(Map.Entry<String,List<ParaFun>> xi:u.funs.entrySet()) for(int xii=0; xii<xi.getValue().size(); xii++) {
			ParaFun x=xi.getValue().get(xii);
			this.env.clear(); this.root=null; this.rootunit=u;
			for(VarDecl d:x.decls) for(String n:d.names) this.env.put(n, d.value.type);
			Expr value=this.resolve(x.value);
			log.log("Unit ["+uu+"], Pred/Fun "+x.name+", BODY:"+value.type);
			if (x.type==null) {
				if (!value.type.isBool) throw x.typeError("Predicate body must be a formula, but it has type "+value.type);
			} else {
				if (value.type.arity()<1) throw x.typeError("Function body must be a set or relation, but its type is "+value.type);
				if (value.type.arity()!=x.type.type.arity()) throw x.typeError("Function body has type "+value.type+" but the return type must be "+x.type.type);
				if (value.type.intersect(x.type.type).hasNoTuple()) throw x.typeError("Function return value is disjoint from its return type! Function body has type "+value.type+" but the return type must be "+x.type.type);
			}
			x.value=value;
		}
		this.env.clear();
		for(Map.Entry<String,ParaSig> xi:u.sigs.entrySet()) {
			ParaSig x=xi.getValue();
			if (x.appendedFacts==null) continue;
			this.root=x; this.rootunit=u; x.appendedFacts=this.resolve(x.appendedFacts);
			if (!x.appendedFacts.type.isBool) throw x.typeError("Appended facts must be a formula, but it has type "+x.appendedFacts.type);
			log.log("Unit ["+uu+"], Sig "+x.name+", Appended: "+x.appendedFacts.type);
		}
		for(Map.Entry<String,ParaFact> xi:u.facts.entrySet()) {
			ParaFact x=xi.getValue();
			this.root=null; this.rootunit=u; this.accept(x,u);
			log.log("Unit ["+uu+"], Fact ["+x.name+"]: "+x.value.type);
			if (!x.value.type.isBool) throw x.typeError("Fact must be a formula, but it has type "+x.value.type);
		}
		for(Map.Entry<String,ParaAssert> xi:u.asserts.entrySet()) {
			ParaAssert x=xi.getValue();
			this.root=null; this.rootunit=u; this.accept(x,u);
			log.log("Unit ["+uu+"], Assert ["+x.name+"]: "+x.value.type);
			if (!x.value.type.isBool) throw x.typeError("Assertion must be a formula, but it has type "+x.value.type);
		}
		return u;
	}
	
//	################################################################################################
	
//	If basic2Vertex contains key bt, then the Vertex associated with bt is returned.
//	Otherwise, a new Vertex vnew containing bt is created, a mapping from bt to vnew is
//	added to basic2Vertex, and vnew returned.
	private static Vertex _getVertex(ParaSig bt, Map<ParaSig,Vertex> basic2Vertex) {
		Vertex v = basic2Vertex.get(bt);
		if (v==null) { v=new Vertex(bt); basic2Vertex.put(bt,v); }
		return v;
	}
	
//	EFFECTS: After the method returns, the supplied graph will
//	contain an edge from vertex v1 to vertex v2 iff there exists an r1 in ut such
//	that r1 = v1->v2 OR (v1 != v2 AND v1 & v2 is non-empty).
//	EFFECTS:  the supplied map will contain the mapping from all basic types used
//	in ut to their corresponding vertices in the supplied graph.
	private static void _getClosureGraph(Type ut, DirectedGraph graph, Map<ParaSig,Vertex> basic2vertex) {
		// add edges between all basic types v1 and v2 for which r1 = v1->v2 and r1 in ut
		for (Type.Rel rt:ut)
			if (rt.arity()==2)
				graph.addEdge(graph.createEdge(_getVertex(rt.basicTypes.get(0), basic2vertex),
						_getVertex(rt.basicTypes.get(1), basic2vertex)));
		// add edges between all basic types v1 and v2 for which (v1 != v2) && (v1 & v2 is non-empty)
		for (Iterator iter1 = basic2vertex.values().iterator(); iter1.hasNext(); ) {
			Vertex v1 = (Vertex) iter1.next();
			ParaSig b1 = (ParaSig) v1.getObject();
			for (Iterator iter2 = basic2vertex.values().iterator(); iter2.hasNext(); ) {
				Vertex v2 = (Vertex) iter2.next();
				if (v1!=v2 && b1.intersect((ParaSig)v2.getObject()).isNonEmpty()) graph.addEdge(v1, v2);
			}
		}
	}
	
//	PRECONDITION:  (1) basic2vertex contains mappings from BasicTypes to nodes in graph;
//	(2) there exist bt1' such that bt1' is mapped by basic2vertex and bt1'&bt1 is non-empty.
//	EFFECTS: After the method returns, the supplied graph will be augmented with an edge
//	from bt to its subtypes or supertypes that are already in the graph (and vice versa),
//	unless bt itself is in the graph, in which case nothing happens
//	EFFECTS:  the supplied map will be augmented with a mapping from bt to its corresponding
//	corresponding vertices in the supplied graph, unless bt is already mapped, in which case
//	nothing happens
	private static void _expandClosureGraph(ParaSig bt, DirectedGraph graph, Map<ParaSig,Vertex> basic2vertex) {
		if (!basic2vertex.containsKey(bt)) {
			Vertex btVertex = new Vertex(bt);
			graph.add(btVertex);
			// add edges between b1 and all of its subtypes and supertypes
			for (Iterator bIter = basic2vertex.values().iterator(); bIter.hasNext(); ) {
				Vertex vertex = (Vertex) bIter.next();
				ParaSig other = (ParaSig) vertex.getObject();
				if (!bt.intersect(other).isEmpty())
				{ graph.addEdge(btVertex,vertex); graph.addEdge(vertex,btVertex); }
			}
			basic2vertex.put(bt, btVertex);
		}
	}
	
//	PRECONDITION:  (1) basic2vertex contains mappings from BasicTypes to nodes in graph;
//	(2) for each bt1->bt2 in ut, there exist bt1' and bt2' such that bt1' and bt2' are mapped by
//	basic2vertex and bt1'&bt1 and bt2'&bt2 are non-empty.
//	EFFECTS: After the method returns, the supplied graph will be augmented with edges
//	from BasicTypes in ut to their subtypes or supertypes that are already in the graph
//	(and vice versa)
//	EFFECTS:  the supplied map will be augmented with mappings from all basic types used
//	in ut to their corresponding vertices in the supplied graph.
	private static void _expandClosureGraph(Type ut, DirectedGraph graph, Map<ParaSig,Vertex> basic2vertex) {
		for (Type.Rel rt:ut) {
			_expandClosureGraph(rt.basicTypes.get(0), graph, basic2vertex);
			_expandClosureGraph(rt.basicTypes.get(1), graph, basic2vertex);
		}
	}
	
//	childType' := {r1 | r1 in childType AND there exist basic types
//	b1 and b2 such that b1->b2 in parentType AND r1 is on a path from b1 to b2}
	private static Type closureResolveChild(Type parentType, Type childType) {
		Type resolvedType = Type.make();
		if (parentType.size() > 0) {
			Map<ParaSig,Vertex> basic2vertex = new LinkedHashMap<ParaSig,Vertex>();
			DirectedGraph closure = new DirectedGraphImpl();
			_getClosureGraph(childType, closure, basic2vertex);
			_expandClosureGraph(parentType, closure, basic2vertex);
			// For each bt1->bt2 in childType, add it to resolvedType if there is a bt1'->bt2' in
			// parentType such that closure contains a path from bt1' to bt1 and a path from bt2 to bt2'
			for (Type.Rel rtChild: childType) {
				Vertex cVertex0 = (Vertex) basic2vertex.get(rtChild.basicTypes.get(0));
				Vertex cVertex1 = (Vertex) basic2vertex.get(rtChild.basicTypes.get(1));
				for (Type.Rel rtParent: parentType) {
					Vertex pVertex0 = (Vertex) basic2vertex.get(rtParent.basicTypes.get(0));
					Vertex pVertex1 = (Vertex) basic2vertex.get(rtParent.basicTypes.get(1));
					if (closure.getPath(pVertex0,cVertex0)!=null && closure.getPath(cVertex1,pVertex1)!=null)
					{ resolvedType=resolvedType.union(Type.make(rtChild)); break; }
				}
			}
		}
		return resolvedType;
	}
	
//	################################################################################################
}
