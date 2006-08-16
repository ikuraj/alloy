package edu.mit.csail.sdg.alloy4;

import java.util.List;
import java.util.ArrayList;

/**
 * Immutable; represents a field declaration of the form (a,b,c: X)
 * @author Felix Chang
 */

public final class FieldDecl {

  /** The list of names */
  private final List<String> names;

  /** The right-hand-side expression */
  public final Expr value;

  /** The parent type */
  public final Type sigtype;

  /** Returns the number of names */
  public int size() { return names.size(); }

  /**
   * Returns the i-th name
   * @return the i-th name if i is in 0..size()-1, and returns null otherwise.
   */
  public String get(int i) {
    if (i<0 || i>=names.size()) return null; else return names.get(i);
  }

  /**
   * Constructs a new FieldDecl object with <b>a</b> as the list of names.
   * @param a - the list of names
   * @param b - the expression that the names are quantified over
   */
  public FieldDecl(List<String> a, Expr b) {
    if (a==null || b==null)
       throw new ErrorInternal(null,null,"NullPointerException");
    if (a.size()==0)
       throw b.syntaxError("The list of declarations cannot be empty!");
    names=new ArrayList<String>(a);
    for(int i=0;i<a.size();i++) {
      String j=names.get(i);
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
    }
    // See ExprUnary.java for why we have to call makeMult() here.
    if (b instanceof ExprUnary) b=((ExprUnary)b).makeMult();
    value=b;
    sigtype=null;
  }

  /**
   * Constructs a new FieldDecl object with <b>a.names</b> as the list of names.
   * @param a - the new FieldDecl object will have the same names as <b>a</b>
   * @param b - the expression that the names are quantified over
   */
  public FieldDecl(FieldDecl a,Expr b,Type t) {
    if (a==null || b==null || b.type==null || t==null || b.type.arity()<1)
       throw new ErrorInternal(null,null,"NullPointerException");
    names=a.names;
    // See ExprUnary.java for why we have to call makeMult() here.
    if (b instanceof ExprUnary) b=((ExprUnary)b).makeMult();
    value=b;
    sigtype=t;
  }

  /**
   * Returns the number of names in a List of FieldDecl.
   * @return the number of names in a List of FieldDecl.
   */
  public static int count (List<FieldDecl> lst) {
    int c=0;
    for(int i=lst.size()-1; i>=0; i--) c=c+lst.get(i).names.size();
    return c;
  }

  /**
   * Checks whether the name <b>n</b> appears in a List of FieldDecl.
   * @return true if and only if the name appears in the list.
   */
  public static boolean hasName (List<FieldDecl> lst, String n) {
    for(int i=lst.size()-1; i>=0; i--)
      if (lst.get(i).names.contains(n))
         return true;
    return false;
  }

  /**
   * Checks whether the same name appears more than once in a List of FieldDecl.
   * @return the duplicate name (if duplicates exist),
   * and returns null otherwise
   */
  public static String hasDuplicateName (List<FieldDecl> lst) {
    for(int i=0; i<lst.size(); i++) {
      FieldDecl d=lst.get(i);
      for(int j=0; j<d.names.size(); j++) {
        String n=d.names.get(j);
        for(int k=j+1; k<d.names.size(); k++)
          if (d.names.get(k).equals(n))
             return n;
        for(int k=i+1; k<lst.size(); k++)
          if (lst.get(k).names.contains(n))
             return n;
      }
    }
    return null;
  }
}
