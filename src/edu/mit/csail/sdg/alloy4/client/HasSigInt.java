package edu.mit.csail.sdg.alloy4.client;

import kodkod.ast.ConstantExpression;
import kodkod.ast.IntToExprCast;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstDetector;

public final class HasSigInt extends DepthFirstDetector {

	public HasSigInt() { super(); }
	
	protected Boolean lookup(Node n) {
		return null;
	}
	
	protected Boolean cache(Node n, boolean val) {
		return Boolean.valueOf(val);
	}
	
	public Boolean visit(Relation relation) {
		return Boolean.valueOf(relation==Relation.INTS);
	}
	
	public Boolean visit(Variable variable) {
		return Boolean.FALSE;
	}
	
	public Boolean visit(ConstantExpression constExpr) {
		return Boolean.valueOf(constExpr==Relation.INTS);
	}
	
	public Boolean visit(IntToExprCast castExpr) {
		return Boolean.TRUE;
	}
}
