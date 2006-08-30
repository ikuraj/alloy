package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a field/variable/parameter declaration such as "a,b,c: X".
 *
 * <br/>
 * <br/> Invariant: names!=null
 * <br/> Invariant: names.size()>0
 * <br/> Invariant: all x:names | x!=null, x is not "", and doesn't contain '/' or '@'
 * <br/> Invariant: value!=null
 *
 * @author Felix Chang
 */

public final class VarDecl {

    /** The unmodifiable list of names. */
    public final List<String> names;

    /** The expression that these names are quantified over. */
    public final Expr value;

    /**
     * Constructs a new VarDecl object with x as the list of names.
     *
     * @param x - the list of names
     * @param y - the expression that the names are quantified over
     *
     * @throws ErrorInternal if x==null or y==null
     * @throws ErrorInternal if x.size()==0
     * @throws ErrorInternal if any of the name is null or ""
     * @throws ErrorInternal if any of the name contains '/' or '@'
     */
    public VarDecl (List<ExprName> x, Expr y) {
        if (x==null || y==null)
            throw new ErrorInternal(null,null,"NullPointerException");
        List<String> newlist=new ArrayList<String>();
        if (x.size()==0)
            throw y.syntaxError("The list of declarations cannot be empty!");
        for(int i=0; i<x.size(); i++) {
            ExprName e=x.get(i);
            if (e==null || e.name==null)
                throw y.internalError("NullPointerException");
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
        // See ExprUnary.java for why we have to call makeMult() here.
        if (y instanceof ExprUnary) y=((ExprUnary)y).makeMult();
        value=y;
    }

    /**
     * Constructs a new VarDecl object with x as the only name.
     *
     * @param x - the only name
     * @param y - the expression that the name is quantified over
     *
     * @throws ErrorInternal if x==null or y==null
     * @throws ErrorInternal if x contains '/' or '@'
     * @throws ErrorInternal if x is equal to ""
     */
    public VarDecl (String x, Expr y) {
        if (x==null || y==null)
            throw new ErrorInternal(null,null,"NullPointerException");
        if (x.length()==0)
            throw y.syntaxError("Variable name must not be empty!");
        if (x.indexOf('/')>=0)
            throw y.syntaxError("Variable name cannot contain \'/\'");
        if (x.indexOf('@')>=0)
            throw y.syntaxError("Variable name cannot contain \'@\'");
        List<String> list=new ArrayList<String>(1);
        list.add(x);
        names=Collections.unmodifiableList(list);
        // See ExprUnary.java for why we have to call makeMult() here.
        if (y instanceof ExprUnary) y=((ExprUnary)y).makeMult();
        value=y;
    }

    /**
     * Constructs a new VarDecl object with the same names as x.
     *
     * @param x - an existing VarDecl object
     * @param y - the expression that the name is quantified over
     *
     * @throws ErrorInternal if x==null or y==null
     */
    public VarDecl (VarDecl x, Expr y) {
        if (x==null || y==null)
            throw new ErrorInternal(null,null,"NullPointerException");
        names=x.names;
        // See ExprUnary.java for why we have to call makeMult() here.
        if (y instanceof ExprUnary) y=((ExprUnary)y).makeMult();
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
     * Convenience method that checks if the name n appears in a list of VarDecl.
     * @return true if and only if the name n appears in the list
     */
    public static boolean hasName (List<VarDecl> list, String n) {
        for(int i=list.size()-1; i>=0; i--)
            if (list.get(i).names.contains(n))
                return true;
        return false;
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
