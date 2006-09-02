package edu.mit.csail.sdg.alloy4.core;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; reresents an "open" declaration.
 *
 * <p/> <b>Invariant:</b>  name!=null and name is not ""
 * <p/> <b>Invariant:</b>  file!=null and file is not ""
 * <p/> <b>Invariant:</b>  list!=null and (all x:list | x is not null nor "")
 *
 * @author Felix Chang
 */

public final class ParaOpen extends Para {

    /** The unmodifiable list of instantiating arguments. */
    public final List<String> list;

    /**
     * The relative filename for the file being imported, without ".als"
     * (this field is always a non-empty string).
     *
     * <br/> eg. "util/ordering"
     * <br/> eg. "myexample"
     */
    public final String filename;

    /**
     * Convenience method that computes what the actual alias should be.
     *
     * @param pos - the original position in the file
     * @param filename - the relative filename of the file being imported (without the ".als" part)
     * @param alias - the alias that the user specified for the file (could be "")
     * @param list - the list of instantiating arguments
     *
     * @return If the alias argument is a legal alias, then it's returned.
     * <br/> Else if the filename is a legal alias and there are no arguments, then it's returned.
     * <br/> Failing both, this method throws an ErrorSyntax exception.
     *
     * @throws ErrorSyntax if filename is ""
     * @throws ErrorSyntax if alias contains '@' or '/'
     * @throws ErrorSyntax if alias is "", and list.size()!=0
     * @throws ErrorSyntax if alias is "", and filename is not a legal alias (eg. it contains '/')
     * @throws ErrorInternal if pos==null, filename==null, alias==null, or list==null
     */
    private static String computeAlias(Pos pos, String filename, String alias, List<ExprName> list) {
        if (pos==null || filename==null || alias==null || list==null)
            throw new ErrorInternal(pos,null,"NullPointerException");
        if (filename.length()==0) throw new ErrorSyntax(pos,"The filename cannot be \"\"");
        if (alias.indexOf('@')>=0) throw new ErrorSyntax(pos,"Alias \""+alias+"\" must not contain \'@\'");
        if (alias.indexOf('/')>=0) throw new ErrorSyntax(pos,"Alias \""+alias+"\" must not contain \'/\'");
        if (alias.length()>0) return alias;
        if (list.size()!=0)
            throw new ErrorSyntax(pos,
                "The module being imported has parameters, so you must supply an alias using AS.");
        for(int i=0; i<filename.length(); i++) {
            char c=filename.charAt(i);
            if ((c>='a' && c<='z') || (c>='A' && c<='Z')) continue;
            if (i==0)
                throw new ErrorSyntax(pos,
                    "This filename does not start with a-z or A-Z, so you must supply an alias using AS.");
            if (!(c>='0' && c<='9') && c!='_' && c!='\'' && c!='\"')
                throw new ErrorSyntax(pos,
                    "Filename contains \'"+c+"\' which is illegal in an alias, so you must supply an alias using AS.");
        }
        return filename;
    }

    /**
     * Constructs a new ParaOpen object.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing the paragraph
     * @param alias - the alias for the imported module ("" if the user intends to use the filename as the alias)
     * @param list - the list of instantiating arguments
     * @param filename - the relative filename of the file being imported (without the ".als" part)
     *
     * @throws ErrorSyntax if path contains '@'
     * @throws ErrorSyntax if filename is ""
     * @throws ErrorSyntax if alias contains '@' or '/'
     * @throws ErrorSyntax if alias is "" and list.size()!=0
     * @throws ErrorSyntax if alias is "" and filename is not a legal alias (eg. it contains '/')
     * @throws ErrorSyntax if at least one argument is "" or contains '@'
     * @throws ErrorInternal if pos==null, path==null, alias==null, list==null, or filename==null
     */
    public ParaOpen(Pos pos, String path, String alias, List<ExprName> list, String filename) {
        super(pos, path, computeAlias(pos,filename,alias,list));
        this.filename=filename;
        List<String> newlist=new ArrayList<String>();
        for(int i=0; i<list.size(); i++) {
            String arg=nonnull(list.get(i)).name;
            newlist.add(arg); // ExprName's invariants ensure arg will not be "", null, nor contains '@'.
        }
        this.list=Collections.unmodifiableList(newlist);
    }
}
