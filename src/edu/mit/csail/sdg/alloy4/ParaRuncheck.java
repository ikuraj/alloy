package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.Map;
import java.util.LinkedHashMap;

/**
 * Mutable; reresents a "run" or "check" command.
 * 
 * <br/>
 * <br/> Invariant: all X:String | exact.contains(X) => scope.containsKey(X)
 * <br/> Invariant: all X:String | scope.containsKey(X) => (names.contains(X) && scope.get(X)>=0)
 * <br/> Invariant: all X:names  | (x is not "", and x does not contain '@')
 * <br/> Invariant: the "names" array does not contain any duplicate entries
 * <br/> Invariant: overall >= -1
 * <br/> Invariant: expects == -1, 0, or 1
 * 
 * @author Felix Chang
 */

public final class ParaRuncheck extends Para {

	/** true if this is a "check"; false if this is a "run". */
	public final boolean check;

	/** The integer bitwidth (-1 if there is no bitwidth specified. */
	public final int bitwidth;

	/** The overall scope (-1 if there is no overall scope). */
	public final int overall;

	/** The expected answer (either 0 or 1) (-1 if there is no expected answer) */
	public final int expects;

	/** This maps each signature to its specified scope. */
	public final Map<ParaSig,Pair<Integer,Boolean>> scope;

	/**
	 * Given the name of a signature, this method returns its specified scope (-1 if no scope was specified)
	 *
	 * @param n - the name of a signature
	 * @return a nonnegative integer if the sig has a specified scope; -1 if no scope was specified.
	 */
	public int getScope(String n) { Pair p=scope.get(n); if (p!=null) return (Integer)(p.a); else return -1; }

	/**
	 * Given the name of a signature, this method returns whether its scope is exact or not.
	 *
	 * @param n - the name of a signature
	 * @return true if and only if the sig has an exact scope
	 */
	public boolean isExact(String n) { Pair p=scope.get(n); if (p!=null) return (Boolean)(p.b); else return false; }

	/** Returns a human-readable string representing this Run or Check command. */
	@Override public final String toString() {
		String a=(check?"check":"run")+" "+name;
		if (overall>=0 && scope.size()>0) a=a+" for "+overall+" but";
		else if (overall>=0) a=a+" for "+overall;
		else if (scope.size()>0) a=a+" for";
		boolean f=false;
		for(Map.Entry<ParaSig,Pair<Integer,Boolean>> e:scope.entrySet()) {
			Pair<Integer,Boolean> ee=e.getValue();
			String n;
			if (e.getKey()==ParaSig.UNIV) n="univ";
				else if (e.getKey()==ParaSig.SIGINT) n="Int";
				else if (e.getKey()==ParaSig.NONE) n="none";
				else n=e.getKey().placeholder;
			a=a+(f?", ":" ")+(ee.b==true?"exactly ":"")+ee.a+" "+n;
			f=true;
		}
		if (bitwidth>=0) a=a+(f?", ":" ")+bitwidth+" int";
		if (expects>0) a=a+" expects "+expects;
		return a;
	}

	/**
	 * Constructs a new ParaRuncheck object.
	 *
	 * @param pos - the original position in the file
	 * @param path - a valid path to the Unit containing this paragraph (can be "" if it's the main unit)
	 * @param name - the name of the assertion/predicate (cannot be "")
	 * @param check - true if this is a "check"; false if this is a "run".
	 * @param overall - the overall scope (-1 if no overall scope was specified)
	 * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
	 * @param scope - String-to-Integer map that maps signature names to nonnegative integer scopes
	 * @param exact - a set of String indicating which signatures have exact scope
	 *
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if the name is equal to ""
	 * @throws ErrorSyntax if at least one of the signature name is ""
	 * @throws ErrorSyntax if at least one of the value in "scope" is negative
	 * @throws ErrorSyntax if at least one signature name is in "exact" but not in "scope"
	 * @throws ErrorInternal if pos==null, path==null, name==null, scope==null, or exact==null
	 */
	public ParaRuncheck(Pos pos, String path, String name,
			boolean check, int overall, int expects, int bitwidth,
			Map<ParaSig,Pair<Integer,Boolean>> scope) {
		super(pos,path,name);
		this.check=check;
		if (name.length()==0)
			throw this.syntaxError("The \"run\" and \"check\" statement must give the name of the predicate or assertion you want to check.");
		this.bitwidth=bitwidth;
		this.overall=overall;
		this.expects=expects;
		this.scope=Collections.unmodifiableMap(new LinkedHashMap<ParaSig,Pair<Integer,Boolean>>(nonnull(scope)));
	}
}
