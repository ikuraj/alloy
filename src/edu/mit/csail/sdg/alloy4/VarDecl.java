package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a variable/parameter declaration such as "a,b,c: X"
 * @author Felix Chang
 */

public final class VarDecl {
	
	/** The unmodifiable list of names */
	public final List<String> names;
	
	/** The expression that these names are quantified over */
	public final Expr value;
	
	/**
	 * Constructs a new VarDecl object with <b>a</b> as the list of names.
	 * @param a - the list of names
	 * @param b - the expression that the names are quantified over
	 */
	public VarDecl(List<ExprName> a, Expr b) {
		if (a==null || b==null)
			throw new ErrorInternal(null,null,"NullPointerException");
		if (a.size()==0)
			throw b.syntaxError("The list of declarations cannot be empty!");
		List<String> list=new ArrayList<String>(a.size());
		for(int i=0;i<a.size();i++) {
			String j=a.get(i).name;
			if (j==null)
				throw b.internalError("NullPointerException");
			if (j.length()==0)
				throw b.syntaxError("The name of a variable cannot be empty!");
			if (j.indexOf('/')>=0)
				throw b.syntaxError("The name of a variable cannot contain \'/\'");
			if (j.indexOf('@')>=0)
				throw b.syntaxError("The name of a variable cannot contain \'@\'");
			if (j.equals("none") ||
					j.equals("iden") ||
					j.equals("univ") ||
					j.equals("Int"))
				throw b.syntaxError("The name of a variable cannot be \""+j+"\"");
			list.add(j);
		}
		names=Collections.unmodifiableList(list);
		// See ExprUnary.java for why we have to call makeMult() here.
		if (b instanceof ExprUnary) b=((ExprUnary)b).makeMult();
		value=b;
	}
	
	/**
	 * Constructs a new VarDecl object with <b>a</b> as the only name.
	 * @param a - the only name
	 * @param b - the expression that the name is quantified over
	 */
	public VarDecl(String a, Expr b) {
		if (a==null || b==null)
			throw new ErrorInternal(null,null,"NullPointerException");
		List<String> list=new ArrayList<String>(1);
		list.add(a);
		names=Collections.unmodifiableList(list);
		if (a.length()==0)
			throw b.syntaxError("The name of a variable must not be empty!");
		if (a.indexOf('/')>=0)
			throw b.syntaxError("The name of a variable cannot contain \'/\'");
		if (a.indexOf('@')>=0)
			throw b.syntaxError("The name of a variable cannot contain \'@\'");
		if (a.equals("none") ||
				a.equals("iden") ||
				a.equals("univ") ||
				a.equals("Int"))
			throw b.syntaxError("The name of a variable cannot be \""+a+"\"");
		// See ExprUnary.java for why we have to call makeMult() here.
		if (b instanceof ExprUnary) b=((ExprUnary)b).makeMult();
		value=b;
	}
	
	/**
	 * Constructs a new VarDecl object with <b>a.names</b> as the list of names.
	 * @param a - the new VarDecl object will have the same names as <b>a</b>
	 * @param b - the expression that the names are quantified over
	 */
	public VarDecl(VarDecl a,Expr b) {
		if (a==null || b==null)
			throw new ErrorInternal(null,null,"NullPointerException");
		names=a.names;
		// See ExprUnary.java for why we have to call makeMult() here.
		if (b instanceof ExprUnary) b=((ExprUnary)b).makeMult();
		value=b;
	}

	/**
	 * Returns the number of names in a List of VarDecl.
	 * @return the number of names in a List of VarDecl.
	 */
	public static int nameCount (List<VarDecl> list) {
		int c=0;
		for(int i=list.size()-1; i>=0; i--) c=c+list.get(i).names.size();
		return c;
	}
	
	/**
	 * Checks whether the name <b>n</b> appears in a list of VarDecl.
	 * @return true if and only if the name appears in the list.
	 */
	public static boolean hasName (List<VarDecl> list, String n) {
		for(int i=list.size()-1; i>=0; i--)
			if (list.get(i).names.contains(n))
				return true;
		return false;
	}
	
	/**
	 * Checks whether the same name appears more than once in a List of VarDecl.
	 * @return the duplicate name (if duplicates exist),
	 * and returns null otherwise
	 */
	public static String hasDuplicateName (List<VarDecl> list) {
		for(int i=0; i<list.size(); i++) {
			VarDecl d=list.get(i);
			for(int j=0; j<d.names.size(); j++) {
				String n=d.names.get(j);
				for(int k=j+1; k<d.names.size(); k++)
					if (d.names.get(k).equals(n))
						return n;
				for(int k=i+1; k<list.size(); k++)
					if (list.get(k).names.contains(n))
						return n;
			}
		}
		return null;
	}
}
