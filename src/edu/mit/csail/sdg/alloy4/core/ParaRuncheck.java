package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.Set;
import java.util.Map;
import java.util.LinkedHashMap;
import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; reresents a "run" or "check" command.
 *
 * <p/> <b>Invariant:</b>  the map does not contain any duplicate names
 * <p/> <b>Invariant:</b>  expects == -1, 0, or 1
 * <p/> <b>Invariant:</b>  overall >= -1
 *
 * @author Felix Chang
 */

public final class ParaRuncheck extends Para {

    /** true if this is a "check"; false if this is a "run". */
    public final boolean check;

    /** The overall scope (0 or higher) (-1 if there is no overall scope). */
    public final int overall;

    /** The expected answer (either 0 or 1) (-1 if there is no expected answer). */
    public final int expects;

    /**
     * This maps each signature to a number that represents its bound as well as its exactness.
     * <p/> If the number N is >= 0: the sig is bound to have at most N atoms.
     * <p/> Otherwise: the sig is bound to have exactly (0-(N+1)) atoms.
     */
    public final Map<String,Integer> map;
    
    /** Returns a human-readable string representing this Run or Check command. */
    @Override public final String toString() {
        String a=(check?"check ":"run ")+name;
        if (overall>=0 && map.size()>0) a=a+" for "+overall+" but";
           else if (overall>=0) a=a+" for "+overall;
           else if (map.size()>0) a=a+" for";
        boolean first=true;
        for(Map.Entry<String,Integer> e:map.entrySet()) {
            a=a+(first?" ":", ");
        	int num=e.getValue();
            if (num<0) { a=a+"exactly "; num=0-(num+1); }
            a=a+num+" "+e.getKey();
            first=false;
        }
        if (expects>=0) a=a+" expects "+expects;
        return a;
    }

    /**
     * Constructs a new ParaRuncheck object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this paragraph
     * @param name - the name of the assertion/predicate being checked
     * @param check - true if this is a "check"; false if this is a "run"
     * @param overall - the overall scope (-1 if no overall scope was specified)
     * @param expects - the expected value (0 or 1) (-1 if no expectation was specified)
     * @param scope - String-to-Integer map that maps signature names to nonnegative integer scopes
     * @param exact - a set of signature names indicating which signatures have exact scope
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name contains '@' or '/', or is equal to ""
     * @throws ErrorSyntax if at least one of the signature name is ""
     * @throws ErrorSyntax if at least one of the value in "scope" is negative
     * @throws ErrorInternal if pos==null, path==null, name==null, scope==null, or exact==null
     */
    public ParaRuncheck(Pos pos, String path, String name, boolean check,
    	int overall, int expects, Map<String,Integer> scope, Set<String> exact) {
        super(pos,path,name);
        if (name.length()==0)
            throw this.syntaxError(
               "The \"run\" and \"check\" statement must give the name of the pred/fun/assert to check.");
        nonnull(scope);
        nonnull(exact);
        Map<String,Integer> map=new LinkedHashMap<String,Integer>();
        for(Map.Entry<String,Integer> e:scope.entrySet()) {
            String a=e.getKey();
            int b=e.getValue();
            if (a.length()==0)
            	throw syntaxError("Signature name cannot be empty!");
            if (b<0)
            	throw syntaxError("Sig \""+a+"\" cannot have a negative scope of "+b+"!");
            if (map.containsKey(a))
            	throw syntaxError("Sig \""+a+"\" cannot have multiple scope in the same command!");
            if (exact.contains(a)) map.put(a,(0-b)-1); else map.put(a,b);
        }
        this.map=Collections.unmodifiableMap(map);
        this.check=check;
        this.overall=(overall<0 ? -1 : overall);
        this.expects=(expects<0 ? -1 : (expects>0 ? 1 : 0));
    }
}
