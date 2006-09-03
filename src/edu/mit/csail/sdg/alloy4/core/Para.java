package edu.mit.csail.sdg.alloy4.core;

import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.ErrorType;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Immutable; represents a toplevel paragraph in the model.
 *
 * <p/> <b>Invariant:</b>  pos!=null && path!=null && name!=null
 * <p/> <b>Invariant:</b>  path does not contain '@'
 * <p/> <b>Invariant:</b>  name does not contain '@', nor does it contain '/'
 *
 * @author Felix Chang
 */

public abstract class Para {

    /** The filename, line, and column position in the original file (cannot be null). */
    public final Pos pos;

    /**
     * A valid path to the Unit containing this Para (can be "" if the Unit is the main unit).
     *
     * <p/>
     * The main unit that the user runs the analyzer on is the "" unit.
     *
     * <p/>
     * If the main unit imports a file as "a" and imports another file as "b",
     * then these 2 units can be referred to by "a" and "b", respectively.
     *
     * <p/>
     * If "a" imports some file as "aa", then the imported file can be referred to as "a/aa".
     * Etc. Etc. Etc.
     *
     * <p/>
     * Note: the same unit can be referred to by multiple paths.
     * For example, if a unit is opened from many places,
     * but the user has indicated that these places should all import a common shared copy,
     * then there will be many paths that refer to that same unit.
     *
     * <p/>
     * (Hence the wording above: the Para.path field only has to be ONE of the valid paths.
     * In fact, if the main unit is accessible from other paths also,
     * then the main unit would be accessible via "" as well as some other string.
     * And a paragraph in the main unit does not have to have its path=="".
     * For any paragraph, its path field just has to be ONE of the valid paths.)
     */
    public final String path;

    /** The name of this paragraph (can be "") (cannot contain '/' nor '@') */
    public final String name;

    /**
     * Constructs a new paragraph.
     *
     * @param pos - the original position in the file
     * @param path - a valid path to the Unit containing this paragraph
     * @param name - the name of the paragraph (can be "")
     *
     * @throws ErrorSyntax if the path contains '@'
     * @throws ErrorSyntax if the name contains '@' or '/'
     * @throws ErrorInternal if pos==null, path==null, or name==null
     */
    public Para(Pos pos, String path, String name) {
        this.pos=nonnull(pos);
        this.path=nonnull(path);
        this.name=nonnull(name);
        if (path.indexOf('@')>=0) throw syntaxError("Path \""+path+"\" must not contain \'@\'");
        if (name.indexOf('@')>=0) throw syntaxError("Name \""+name+"\" must not contain \'@\'");
        if (name.indexOf('/')>=0) throw syntaxError("Name \""+name+"\" must not contain \'/\'");
    }

    /** Convenience method that constructs a syntax error exception. */
    public final ErrorSyntax syntaxError(String msg) {
        return new ErrorSyntax(pos, msg);
    }

    /** Convenience method that constructs a type error exception. */
    public final ErrorType typeError(String msg) {
        return new ErrorType(pos, this, msg);
    }

    /** Convenience method that constructs an internal error exception. */
    public final ErrorInternal internalError(String msg) {
        return new ErrorInternal(pos, this, msg);
    }

    /**
     * Convenience method that checks if x is null or not;
     * (it returns x if nonnull, and throws an exception if null).
     *
     * @return x if it is not null
     * @throws ErrorInternal if x is null
     */
    public final<T> T nonnull(T x) {
        if (x==null) throw internalError("NullPointerException"); else return x;
    }
}
