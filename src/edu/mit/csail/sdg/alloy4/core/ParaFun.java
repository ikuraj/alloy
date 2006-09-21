package edu.mit.csail.sdg.alloy4.core;

import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Mutable; represents a "predicate" or "function".
 *
 * <p/> <b>Invariant:</b>  name is not ""
 * <p/> <b>Invariant:</b>  decls!=null and there are no duplicate names in the list
 * <p/> <b>Invariant:</b>  all x:decls | x!=null
 * <p/> <b>Invariant:</b>  argCount == (sum x:decls | x.names.size())
 * <p/> <b>Invariant:</b>  value!=null
 *
 * @author Felix Chang
 */

public final class ParaFun extends Para {

	private ParaFun(String name) {
		super(new Pos("$builtin$",1,1), "", name);
		this.decls=null;
		this.argCount=0;
		this.type=null;
		this.value=null;
	}

	public static final ParaFun INT    = new ParaFun("int");
	public static final ParaFun SIGINT = new ParaFun("Int");
	public static final ParaFun DISJ   = new ParaFun("disj");

	private void check() {
		if (this==INT || this==SIGINT || this==DISJ) throw internalError("Builtin function cannot be directly accessed!");
	}

    /** The mutable list of parameters (its number of names must match this.argCount). */
    private List<VarDecl> decls;
    public List<VarDecl> getDecls() { check(); return decls; }
    public void setDecls(List<VarDecl> d) { check(); decls=d; }

    /** The number of parameters (it must equal the number of names in this.decls). */
    private int argCount;
    public int getArgCount() { check(); return argCount; }

    /** The return type (null if this is a "predicate" rather than a "function"). */
    private Expr type;
    public Expr getType() { check(); return type; }
    public void setType(Expr t) { check(); type=t; }

    /** The body of the predicate/function. */
    private Expr value;
    public Expr getValue() { check(); return value; }
    public void setValue(Expr v) { check(); value=v; }

    /**
     * Constructs a new ParaFun object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this predicate/function
     * @param name - the name of the predicate/function (it cannot be "")
     * @param decls - the list of parameters
     * @param type - the return type (null if this is a predicate rather than a function)
     * @param value - the body of the predicate/function
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name contains '@' or '/'
     * @throws ErrorSyntax if the name is equal to ""
     * @throws ErrorSyntax if decls contains duplicate names
     * @throws ErrorInternal if pos==null, path==null, name==null, decls==null, or value==null
     */
    public ParaFun(Pos pos, String path, String name, List<VarDecl> decls, Expr type, Expr value) {
        super(pos, path, name);
        if (name.length()==0)
            throw syntaxError("Name of a predicate/function cannot be \"\"");
        this.decls=new ArrayList<VarDecl>(nonnull(decls));
        String dup=VarDecl.hasDuplicateName(this.decls);
        if (dup!=null)
            throw syntaxError("The parameter name \""
            +dup+"\" cannot appear more than once in this predicate/function declaration.");
        this.argCount=VarDecl.nameCount(this.decls);
        this.type=type;
        this.value=nonnull(value);
    }

    /** Returns a human-readable label for this predicate/function */
    @Override public String toString() {
        return (type==null ? "(pred " : "(fun ")
             + (path.length()==0 ? "this/" : path+"/")
             + name
             + ")";
    }
}
