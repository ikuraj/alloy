package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

import edu.mit.csail.sdg.alloy4.helper.ErrorInternal;
import edu.mit.csail.sdg.alloy4.helper.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.helper.Pos;

/**
 * Immutable; represents a field/variable/parameter declaration such as "a,b,c: X".
 *
 * <p/> <b>Invariant:</b>  pos!=null, names!=null, and value!=null
 * <p/> <b>Invariant:</b>  names.size()>0
 * <p/> <b>Invariant:</b>  all x:names | x!=null, x is not "", and doesn't contain '/' or '@'
 *
 * @author Felix Chang
 */

public final class VarDecl {

    /**
     * The filename, line, and column position
     * in the original Alloy model file (cannot be null).
     */
    public final Pos pos;

    /** The unmodifiable list of names. */
    public final List<String> names;

    /** The expression that these names are quantified over. */
    public final Expr value;

    /**
     * Constructs a new VarDecl object with x as the list of names.
     *
     * @param pos - the original position in the file
     * @param x - the list of names
     * @param y - the expression that the names are quantified over
     *
     * @throws ErrorInternal if pos==null or x==null or y==null
     * @throws ErrorInternal if x.size()==0
     * @throws ErrorInternal if any of the name is null or ""
     * @throws ErrorInternal if any of the name contains '/' or '@'
     */
    public VarDecl (Pos pos, List<ExprName> x, Expr y) {
        this.pos=pos;
        if (pos==null || x==null || y==null)
            throw new ErrorInternal(pos,null,"NullPointerException");
        List<String> newlist=new ArrayList<String>();
        if (x.size()==0)
            throw new ErrorSyntax(pos,"The list of declarations cannot be empty!");
        for(int i=0; i<x.size(); i++) {
            ExprName e=x.get(i);
            if (e==null || e.name==null)
                throw new ErrorInternal(pos, e, "NullPointerException");
            String n=e.name;
            if (n.length()==0)
                throw e.syntaxError("Variable name cannot be empty!");
            if (n.indexOf('/')>=0)
                throw e.syntaxError("Variable name cannot contain \'/\'");
            if (n.indexOf('@')>=0)
                throw e.syntaxError("Variable name cannot contain \'@\'");
            newlist.add(n);
        }
        names=Collections.unmodifiableList(newlist);
        value=y;
    }

    /**
     * Constructs a new VarDecl object with x as the only name.
     *
     * @param pos - the original position in the file
     * @param x - the only name
     * @param y - the expression that the name is quantified over
     *
     * @throws ErrorInternal if pos==null or x==null or y==null
     * @throws ErrorInternal if x contains '/' or '@'
     * @throws ErrorInternal if x is equal to ""
     */
    public VarDecl (Pos pos, String x, Expr y) {
        this.pos=pos;
        if (pos==null || x==null || y==null)
            throw new ErrorInternal(pos,null,"NullPointerException");
        if (x.length()==0)
            throw y.syntaxError("Variable name must not be empty!");
        if (x.indexOf('/')>=0)
            throw y.syntaxError("Variable name cannot contain \'/\'");
        if (x.indexOf('@')>=0)
            throw y.syntaxError("Variable name cannot contain \'@\'");
        List<String> list=new ArrayList<String>(1);
        list.add(x);
        names=Collections.unmodifiableList(list);
        value=y;
    }

    /**
     * Constructs a new VarDecl object with the same names as x.
     *
     * @param pos - the original position in the file
     * @param x - an existing VarDecl object
     * @param y - the expression that the name is quantified over
     *
     * @throws ErrorInternal if pos==null or x==null or y==null
     */
    public VarDecl (Pos pos, VarDecl x, Expr y) {
        this.pos=pos;
        if (pos==null || x==null || y==null)
            throw new ErrorInternal(pos,null,"NullPointerException");
        names=x.names;
        value=y;
    }

    /**
     * Convenience method that returns the number of names in a list of VarDecl.
     * @return the number of names in the list
     */
    public static int nameCount (List<VarDecl> list) {
        int c=0;
        for(int i=list.size()-1; i>=0; i--) c=c+list.get(i).names.size();
        return c;
    }

    /**
     * Convenience method that checks if there are duplicate names in a VarDecl list.
     * @return one of the duplicate name (if duplicates exist),
     *         and returns null otherwise
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
