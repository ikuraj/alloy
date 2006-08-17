package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;

/**
 * Immutable; reresents a "run" or "check" command.
 * @author Felix Chang
 */

public final class ParaRuncheck extends Para {

	/** true if this is a "check"; false if this is a "run" */
	public final boolean check;

	/** The overall scope (-1 if there is no overall scope) */
	public final int overall;

	/** The expected answer (either 0 or 1) (-1 if there is no expected answer) */
	public final int expects;

	/** An unmodifiable list of signatures listed in the "run" or "check" command line */
	public final List<String> names;

	/** This maps each signature to its specified scope */
	private final Map<String,Integer> scope;

	/** If a sig is in this set, then the sig's scope is exact (else it's just an upperbound) */
	private final Set<String> exact;

	/**
	 * Given the name of a signature, this method returns its specified scope (-1 if no scope was specified)
	 *
	 * @param n - the name of a signature
	 * @return a nonnegative integer if the sig has a specified scope; -1 if no scope was specified.
	 */
	public int scope(String n) { Integer ans=scope.get(n); if (ans!=null) return ans; return -1; }

	/**
	 * Given the name of a signature, this method returns whether its scope is "exact" or not.
	 *
	 * @param n - the name of a signature
	 * @return true if and only if the sig has an exact scope
	 */
	public boolean isExact(String n) { return exact.contains(n); }

	/**
	 * Returns a human-readable string representing this Run or Check command.
	 */
	@Override public final String toString() {
		String a=(check?"check":"run");
		a=a+" "+name;
		if (overall>=0 && scope.size()>0) a=a+" for "+overall+" but";
		else if (overall>=0) a=a+" for "+overall;
		else if (scope.size()>0) a=a+" for";
		boolean f=false;
		for(Map.Entry<String,Integer> e:scope.entrySet()) {
			a=a+(f?", ":" ");
			if (exact.contains(e.getKey())) a=a+"exactly ";
			a=a+e.getValue()+" "+e.getKey();
			f=true;
		}
		if (expects>0) a=a+" expects "+expects;
		return a;
	}

	/**
	 * Constructs a new ParaRuncheck object.
	 *
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing the paragraph
	 * @param name - the name of the assertion (can be an empty string)
	 * @param check - true if this is a "check"; false if this is a "run".
	 * @param overall - the overall scope (-1 if no overall scope was specified)
	 * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
	 * @param scope - String-to-Integer map that maps signature name to a nonnegative scope
	 * @param exact - a set of String indicating which sigs have exact scope
	 *
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if name is equal to ""
	 * @throws ErrorSyntax if at least one of the signature name is ""
	 * @throws ErrorSyntax if at least one of the value in "scope" is negative
	 * @throws ErrorSyntax if at least onesignature name is in "exact" but not in "scope"
	 * @throws ErrorInternal if path==null, name==null, scope==null, or exact==null
	 */
	public ParaRuncheck(Pos pos, String path, String name, boolean check, int overall, int expects, Map<String,Integer> scope, Set<String> exact) {
		super(pos,path,name);
		this.check=check;
		if (name.length()==0) throw this.syntaxError("The \"run\" and \"check\" statement must give the name of the predicate or assertion you want to check");
		this.overall=overall;
		this.expects=expects;
		this.scope=new LinkedHashMap<String,Integer>(scope);
		this.exact=new LinkedHashSet<String>(exact);
		List<String> newlist=new ArrayList<String>();
		for(Map.Entry<String,Integer> e:this.scope.entrySet()) {
			String a=e.getKey();
			int b=e.getValue();
			if (a.length()==0) throw syntaxError("\"\" is not a valid scope");
			if (b<0) throw syntaxError("sig \""+a+"\" cannot have a negative scope!");
			newlist.add(a);
		}
		for(String e:this.exact) {
			if (!scope.containsKey(e)) throw syntaxError("sig \""+e+"\" cannot be exact without a specified scope!");
		}
		names=Collections.unmodifiableList(newlist);
	}
}
