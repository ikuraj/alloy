/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.util.List;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/**
 * Immutable; reresents an "open" declaration.
 *
 * <p> <b>Invariant:</b>  filename!=null and filename is not ""
 * <p> <b>Invariant:</b>  alias!=null    and alias    is not "" and does not contain '/' nor '@'
 * <p> <b>Invariant:</b>  list!=null     and (all x:list | x!=null && x is not "" && x does not contain '@')
 */

final class Open {

    /** The position in the original model where this "open" statement was declared; never null. */
    public final Pos pos;

    /** The alias for this open declaration; always a nonempty string. */
    public final String alias;

    /** The unmodifiable list of instantiating arguments. */
    public final ConstList<String> args;

    /**
     * The relative filename for the file being imported, without the final ".als" part
     * (thus this must be a nonempty string).
     *
     * <br> eg. "util/ordering"
     * <br> eg. "myModel"
     */
    public final String filename;

    /**
     * Constructs a new Open object.
     *
     * @param pos - the original position in the file (must not be null)
     * @param alias - the alias for the imported module ("" if the user intends to use the filename as the alias)
     * @param args - the list of instantiating arguments
     * @param filename - the relative filename of the file being imported (without final ".als" part)
     *
     * @throws ErrorSyntax if filename is ""
     * @throws ErrorSyntax if alias contains '/' or '@'
     * @throws ErrorSyntax if alias is "" and args.size()!=0
     * @throws ErrorSyntax if alias is "" and filename does not match the regular expression [A-Za-z][A-Za-z0-9_'"]*
     * @throws ErrorSyntax if at least one argument is "" or contains '@'
     */
    public Open(Pos pos, String alias, List<ExpName> args, String filename) throws Err {
        if (pos==null) pos=Pos.UNKNOWN;
        if (filename.length()==0) throw new ErrorSyntax(pos,"The filename cannot be \"\"");
        if (alias.indexOf('@')>=0) throw new ErrorSyntax(pos,"Alias \""+alias+"\" must not contain \'@\'");
        if (alias.indexOf('/')>=0) throw new ErrorSyntax(pos,"Alias \""+alias+"\" must not contain \'/\'");
        if (alias.length()==0) {
            if (args.size()!=0)
                throw new ErrorSyntax(pos,
                "The module being imported has parameters, so you must supply an alias using the AS keyword.");
            for(int i=0; i<filename.length(); i++) {
                char c=filename.charAt(i);
                if ((c>='a' && c<='z') || (c>='A' && c<='Z')) continue;
                if (i==0)
                   throw new ErrorSyntax(pos,
                   "This filename does not start with a-z or A-Z, so you must supply an alias using the AS keyword.");
                if (!(c>='0' && c<='9') && c!='_' && c!='\'' && c!='\"')
                   throw new ErrorSyntax(pos, "Filename contains \'"+c
                   +"\' which is illegal in an alias, so you must supply an alias using the AS keyword.");
            }
            alias=filename;
        }
        final TempList<String> newlist = new TempList<String>(args.size());
        for(int i=0; i<args.size(); i++) {
            ExpName arg=args.get(i);
            if (arg.name.length()==0)
                throw new ErrorSyntax(arg.span(), "Module \""+filename+"\"\'s instantiation argument cannot be empty");
            if (arg.name.indexOf('@')>=0)
                throw new ErrorSyntax(arg.span(), "Module \""+filename+"\"\'s instantiation argument cannot contain \'@\'");
            newlist.add(arg.name);
        }
        this.args = newlist.makeConst();
        this.alias = alias;
        this.pos = pos;
        this.filename = filename;
    }
}
