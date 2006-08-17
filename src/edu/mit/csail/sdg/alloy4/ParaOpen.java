package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; reresents an "open" declaration.
 * @author Felix Chang
 */

public final class ParaOpen extends Para {
	
	/** The unmodifiable list of instantiating arguments */
	public final List<String> list;
	
	/** The alias for the imported module (always a non-empty string) */
	public final String filename;
	
	/**
	 * Convenience method that computes 
	 *
	 * @param p - the original position in the file (null if unknown)
	 * @param f - a valid path to the Unit containing the paragraph
	 * @param a - the alias for the imported module
	 * @param l - the list of instantiating arguments
	 * @param n - the name of the module being imported
	 *
	 * @return
	 */
	private static String computeAlias(Pos p, String f, String a, boolean parametric) {
		if (a.length()>0) {
			if (a.indexOf('@')>=0) throw new ErrorSyntax(p, "Alias \""+a+"\" must not contain \'@\'");
			if (a.indexOf('/')>=0) throw new ErrorSyntax(p, "Alias \""+a+"\" must not contain \'/\'");
			if (a.equals("none") ||
				a.equals("iden") ||
				a.equals("univ") ||
				a.equals("Int")) throw new ErrorSyntax(p, "Alias cannot be \""+a+"\"");
			return a;
		}
		if (parametric) throw new ErrorSyntax(p, "This open statement has parametric arguments, so you must supply the ALIAS via the AS command.");
		if (f.indexOf('/')>=0) throw new ErrorSyntax(p, "This open statement has \'/\' in the pathname, so you must supply the ALIAS via the AS command.");
		if (f.indexOf('@')>=0) throw new ErrorSyntax(p, "The filename \""+f+"\" must not contain \'@\'");
		if (f.length()==0 ||
			f.equals("none") ||
			f.equals("iden") ||
			f.equals("univ") ||
			f.equals("Int")) throw new ErrorSyntax(p, "The filename cannot be \""+f+"\"");
		return f;
	}
	
	/**
	 * Constructs a new ParaOpen object.
	 * 
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing the paragraph
	 * @param a - the alias for the imported module
	 * @param l - the list of instantiating arguments
	 * @param n - the name of the module being imported
	 * 
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if n contains '@'
	 * @throws ErrorSyntax if n is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax if d contains duplicate names
	 * @throws ErrorSyntax if a is equal to "" and yet (l.size()>0 or n.contains('/')) 
	 * @throws ErrorInternal if pos==null, path==null, a==null, or n==null
	 */
	public ParaOpen(Pos pos, String path, String a, List<ExprName> l, String n) {
		super(pos, path, computeAlias(pos,n,a,l!=null && l.size()>0));
		nonnull(n);
		nonnull(a);
		filename=n;
		List<String> newlist=new ArrayList<String>(l==null ? 0 : l.size());
		if (l!=null) for(int i=0; i<l.size(); i++) {
			String x=nonnull(l.get(i)).name;
			nonnull(x);
			if (x.length()==0) throw this.syntaxError("The import parameter must not be empty.");
			if (x.indexOf('@')>=0) throw this.syntaxError("The import parameter must not contain \'@\'.");
			if (x.equals("none")) throw this.syntaxError("The import parameter cannot be \"none\"");
			if (x.equals("iden")) throw this.syntaxError("The import parameter cannot be \"iden\"");
			newlist.add(x);
		}
		list=Collections.unmodifiableList(newlist);
	}
}
