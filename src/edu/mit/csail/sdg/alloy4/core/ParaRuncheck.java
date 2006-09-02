package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Map;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;

/**
 * Immutable; reresents a "run" or "check" command.
 *
 * <p/> <b>Invariant:</b>  all X:String | exact.contains(X) => scope.containsKey(X)
 * <p/> <b>Invariant:</b>  all X:String | scope.containsKey(X) => (names.contains(X) && scope.get(X)>=0)
 * <p/> <b>Invariant:</b>  all X:names  | (x is not "", and x does not contain '@')
 * <p/> <b>Invariant:</b>  the "names" array does not contain any duplicate entries
 * <p/> <b>Invariant:</b>  expects == -1, 0, or 1
 * <p/> <b>Invariant:</b>  overall >= -1
 *
 * @author Felix Chang
 */

public final class ParaRuncheck extends Para {

    /** true if this is a "check"; false if this is a "run". */
    public final boolean check;

    /** The overall scope (-1 if there is no overall scope). */
    public final int overall;

    /** The expected answer (either 0 or 1) (-1 if there is no expected answer). */
    public final int expects;

    /** An unmodifiable list of signatures listed in the "run" or "check" command line. */
    public final List<String> names;

    /** This maps each signature to its specified scope. */
    private final Map<String,Integer> scope;

    /** If a sig is in this set, then its scope is exact (else its scope is just an upperbound). */
    private final Set<String> exact;

    /**
     * Given the name of a signature,
     * this method returns its specified scope (-1 if no scope was specified)
     *
     * @param n - the name of a signature
     * @return a nonnegative integer if the sig has a specified scope; -1 if no scope was specified.
     */
    public int getScope(String n) {
        Integer i=scope.get(n);
        if (i!=null) return i; else return -1;
    }

    /**
     * Given the name of a signature,
     * this method returns whether its scope is exact or not.
     *
     * @param n - the name of a signature
     * @return true if and only if the sig has an exact scope
     */
    public boolean isExact(String n) { return exact.contains(n); }

    /** Returns a human-readable string representing this Run or Check command. */
    @Override public final String toString() {
        String a=(check?"check ":"run ")+name;
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
        if (expects>=0) a=a+" expects "+expects;
        return a;
    }

    /**
     * Constructs a new ParaRuncheck object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this paragraph (can be "" if it's the main unit)
     * @param name - the name of the assertion/predicate being checked
     * @param check - true if this is a "check"; false if this is a "run".
     * @param overall - the overall scope (-1 if no overall scope was specified)
     * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
     * @param scope - String-to-Integer map that maps signature names to nonnegative integer scopes
     * @param exact - a set of Signature Names indicating which signatures have exact scope
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name is equal to ""
     * @throws ErrorSyntax if at least one of the signature name is ""
     * @throws ErrorSyntax if at least one of the value in "scope" is negative
     * @throws ErrorSyntax if at least one signature name is in "exact" but not in "scope"
     * @throws ErrorInternal if pos==null, path==null, name==null, scope==null, or exact==null
     */
    public ParaRuncheck(Pos pos, String path, String name,
            boolean check, int overall, int expects,
            Map<String,Integer> scope, Set<String> exact) {
        super(pos,path,name);
        this.check=check;
        if (name.length()==0)
            throw this.syntaxError(
               "The \"run\" and \"check\" statement must give the name of the pred/fun/assert to check.");
        this.overall=overall;
        this.expects=expects;
        this.scope=Collections.unmodifiableMap(new LinkedHashMap<String,Integer>(nonnull(scope)));
        this.exact=Collections.unmodifiableSet(new LinkedHashSet<String>(nonnull(exact)));
        List<String> newlist=new ArrayList<String>();
        for(Map.Entry<String,Integer> e:this.scope.entrySet()) {
            String a=e.getKey();
            int b=e.getValue();
            if (a.length()==0) throw syntaxError("\"\" is not a valid signature name!");
            if (a.indexOf('@')>=0) throw syntaxError("Signature name \""+a+"\" cannot contain '@'");
            if (b<0) throw syntaxError("sig \""+a+"\" cannot have a negative scope!");
            newlist.add(a);
        }
        for(String e:this.exact) {
            if (!scope.containsKey(e)) throw syntaxError("sig \""+e+"\" cannot be exact without a specified scope!");
        }
        names=Collections.unmodifiableList(newlist);
    }
}
