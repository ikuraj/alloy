package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; reresents an "open" declaration.
 * @author Felix Chang
 */

public final class ParaOpen extends Para {
	
	/** The immutable list of instantiating arguments */
	public final List<String> list;
	
	/** The alias for the imported module (always a non-empty string) */
	public final String as;
	
	/**
	 * Constructs a new ParaOpen object.
	 * 
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing the paragraph
	 * @param n - the name of the module being imported
	 * @param l - the list of instantiating arguments
	 * @param a - the alias for the imported module
	 * 
	 * @throws ErrorSyntax - if n contains '@'
	 * @throws ErrorSyntax - if n is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax - if d contains duplicate names
	 * @throws ErrorSyntax - if a is equal to "" and yet (l.size()>0 or n.contains('/')) 
	 * @throws ErrorInternal - if path==null, n==null, or a==null
	 */
	public ParaOpen(Pos pos, String path, String n, List<ExprName> l, String a) {
		super(pos,path,n);
		nonnull(n);
		if (n.length()==0) throw this.syntaxError("The \"open\" statement must give the name of the module to include.");
		if (n.indexOf('@')>=0) throw this.syntaxError("Name \""+n+"\" must not contain \'@\' in it.");
		if (n.equals("none") ||
			n.equals("iden") ||
			n.equals("univ") ||
			n.equals("Int")) throw this.syntaxError("Name cannot be \""+n+"\"");
		nonnull(a);
		if (a.length()==0) {
			if (l!=null && l.size()!=0) throw this.syntaxError("This open statement has parametric arguments, so you must supply the ALIAS via the AS command.");
			if (n.indexOf('/')>=0) throw this.syntaxError("This open statement has \'/\' in the pathname, so you must supply the ALIAS via the AS command.");
			as=n;
		} else as=a;
		nonnull(as);
		if (as.indexOf('/')>=0) throw this.syntaxError("Name \""+as+"\" must not contain \'/\' in it.");
		if (as.indexOf('@')>=0) throw this.syntaxError("Name \""+as+"\" must not contain \'@\' in it.");
		if (as.equals("none") ||
			as.equals("iden") ||
			as.equals("univ") ||
			as.equals("Int")) throw this.syntaxError("Name cannot be \""+as+"\"");
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
