package edu.mit.csail.sdg.alloy4;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; reresents an "open" declaration.
 * @author Felix Chang
 */

public final class ParaOpen extends Para {

	/** The unmodifiable list of instantiating arguments. */
	public final List<String> list;

	/** The filename for the imported module (always a non-empty string) */
	public final String filename;

	/**
	 * Convenience method that computes what the alias should be.
	 *
	 * @param pos - the original position in the file (null if unknown)
	 * @param filename - the filename of the file being opened
	 * @param alias - the alias that the user specified for the file (could be "")
	 * @param hasArg - true if and only if the user specified instantiating arguments
	 *
	 * @return if alias!="", then it is returned. Otherwise, if the filename
	 * does not contain any '/' and there are no arguments, then the filename is returned.
	 * Failing both, then it will throw an ErrorSyntax exception.
	 *
	 * @throws ErrorSyntax if alias contains '@' or '/'
	 * @throws ErrorSyntax if alias is equal to "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax if filename contains '@'
	 * @throws ErrorSyntax if filename contains '/' and alias is equal to ""
	 * @throws ErrorSyntax if filename is equal to "", "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax if hasArg==true and alias is equal to ""
	 */
	private static String computeAlias(Pos pos, String filename, String alias, boolean hasArg) {
		if (alias.indexOf('@')>=0) throw new ErrorSyntax(pos, "Alias \""+alias+"\" must not contain \'@\'");
		if (alias.indexOf('/')>=0) throw new ErrorSyntax(pos, "Alias \""+alias+"\" must not contain \'/\'");
		if (alias.equals("none") ||
			alias.equals("iden") ||
			alias.equals("univ") ||
			alias.equals("Int")) throw new ErrorSyntax(pos, "Alias cannot be \""+alias+"\"");
		if (filename.indexOf('@')>=0) throw new ErrorSyntax(pos, "The filename \""+filename+"\" must not contain \'@\'");
		if (filename.length()==0 ||
			filename.equals("none") ||
			filename.equals("iden") ||
			filename.equals("univ") ||
			filename.equals("Int")) throw new ErrorSyntax(pos, "The filename cannot be \""+filename+"\"");
		if (alias.length()>0) return alias;
		if (filename.indexOf('/')>=0) throw new ErrorSyntax(pos, "This open statement has \'/\' in the pathname, so you must supply the ALIAS via the AS command.");
		if (hasArg) throw new ErrorSyntax(pos, "This open statement has parametric arguments, so you must supply the ALIAS via the AS command.");
		return filename;
	}

	/**
	 * Constructs a new ParaOpen object.
	 *
	 * @param pos - the original position in the file (null if unknown)
	 * @param path - a valid path to the Unit containing the paragraph
	 * @param alias - the alias for the imported module ("" if the user intends to use the filename as the alias)
	 * @param list - the list of instantiating arguments
	 * @param filename - the name of the module being imported
	 *
	 * @throws ErrorSyntax if the path contains '@'
	 * @throws ErrorSyntax if the filename contains '@'
	 * @throws ErrorSyntax if the filename is equal to "", "none", "iden", "univ", or "Int"
	 * @throws  ErrorSyntax if the alias contains '@' or '/'
	 * @throws ErrorSyntax if the alias is equal to "none", "iden", "univ", or "Int"
	 * @throws ErrorSyntax if the alias is equal to "" and yet (list.size()>0 or name.contains('/'))
	 * @throws ErrorSyntax if the list contains duplicate names
	 * @throws ErrorSyntax if at least one argument is "", "none", or "iden"
	 * @throws ErrorSyntax if at least one argument contains '@'
	 * @throws ErrorInternal if pos==null, path==null, alias==null, list==null, or filename==null
	 */
	public ParaOpen(Pos pos, String path, String alias, List<ExprName> list, String filename) {
		super(pos, path, computeAlias(pos,filename,alias,list.size()>0));
		nonnull(filename);
		nonnull(alias);
		this.filename=filename;
		List<String> newlist=new ArrayList<String>(list.size());
		for(int i=0; i<list.size(); i++) {
			String x=nonnull(list.get(i)).name;
			nonnull(x);
			if (x.length()==0) throw this.syntaxError("The import argument must not be empty.");
			if (x.indexOf('@')>=0) throw this.syntaxError("The import argument must not contain \'@\'.");
			if (x.equals("none")) throw this.syntaxError("The import argument cannot be \"none\"");
			if (x.equals("iden")) throw this.syntaxError("The import argument cannot be \"iden\"");
			newlist.add(x);
		}
		this.list=Collections.unmodifiableList(newlist);
	}
}
