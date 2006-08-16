package edu.mit.csail.sdg.alloy4;


/**
 * Immutable; represents an identifier in the AST.
 * @author Felix Chang
 */

public final class ExprName extends Expr {

    /**
     * Accepts the return visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitReturn
     */
    @Override public Object accept(VisitReturn visitor) {
        return visitor.accept(this);
    }

    /**
     * Accepts the desugar visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitDesugar
     */
    @Override public Expr accept(VisitDesugar visitor) {
        return visitor.accept(this);
    }

    /**
     * Accepts the desugar2 visitor.
     * @see edu.mit.csail.sdg.alloy4.VisitDesugar2
     */
    @Override public Expr accept(VisitDesugar2 visitor, Type type) {
        return visitor.accept(this,type);
    }

    /** The name of the procedure being called. */
    public final String name;
    
    /**
     *  The object that this name refers to (null if this node hasn't been typechecked).
     *  
     *  Note: After typechecking, there are the following possibilities:
     *  (1) Type (which means the name is a quantification/let/functioncall parameter)
     *  (2) ParaSig
     *  (3) ParaFun
     *  (4) Field
     *  (5) Full
     */
    public final Object object;

    /**
     * Constructs an untypechecked ExprName expression.
     * @param pos - the original position in the file
     * @param name - the identifier
     */
    public ExprName(Pos pos, String name) { this(pos, name, null, null); }

    /**
     * Constructs a typechecked ExprName expression.
     * @param pos - the original position in the file
     * @param name - the identifier
     * @param object - the object being referred to
     * @param type - the type
     */
    public ExprName(Pos pos, String name, Object object, Type type) {
        super(pos, type, 0);
        this.name=nonnull(name);
        this.object=object;
        if (name.length()==0)
            throw syntaxError("The name of a variable must not be empty!");
        if (name.length()==1 && name.charAt(0)=='@')
            throw syntaxError("The name of a variable must not be \"@\"");
        if (name.lastIndexOf('@')>0)
            throw syntaxError("If a variable name contains @,"
            +" it must be the first character!");
    }

    /**
     * Throw a syntax error exception indicating the name "n" cannot be found.
     * (In particular, if the name is an old Alloy3 keyword, then
     * the message will tell the user to consult the documentation
     * on how to migrate old models to the new syntax.)
     * @param p - the original position in the file
     * @param n - the identifier
     */
    public static void hint(Pos p, String n) {
        String msg="The name \""+n+"\" cannot be found.";
        if ("disj".equals(n) || "disjoint".equals(n) ||
            "exh".equals(n) || "exhaustive".equals(n) ||
            "part".equals(n) || "partition".equals(n) )
            msg=msg+" If you are migrating from Alloy 3, please see the "
            +"online documentation on how to translate models that use the \""
            +n+"\" keyword.";
        throw new ErrorSyntax(p, msg);
    }
}
