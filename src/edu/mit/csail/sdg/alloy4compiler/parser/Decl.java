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
import edu.mit.csail.sdg.alloy4.ConstList;

/** Immutable; this declaration binds a list of names to an expression. */

final class Decl {

    /** If true, then this decl is a private decl that is only visible within the current module. */
    public final boolean isPrivate;

    /** If nonnull, then this Decl is disjoint (and this.disjoint is the location of the "disjoint" keyword) */
    public final Pos disjoint;

    /** The list of names. */
    public final ConstList<ExpName> names;

    /** The value that the list of names are bound to. */
    public final Exp expr;

    /** Caches the span() result. */
    private Pos span=null;

    /** Returns a Pos object representing the entire span of this expression and all its subexpressions. */
    public Pos span() {
        Pos p=span;
        if (p==null) {
            p=expr.span().merge(disjoint);
            for(ExpName n:names) p=p.merge(n.span());
            span=p;
        }
        return p;
    }

    /** This constructs a declaration. */
    public Decl(boolean isPrivate, Pos disjoint, List<ExpName> names, Exp expr) {
        this.isPrivate = isPrivate;
        this.disjoint = disjoint;
        this.names = ConstList.make(names);
        this.expr = expr;
    }

    /** If the list of declaration contains a duplicate name, return one such duplicate name, else return null. */
    public static ExpName findDuplicateName (List<Decl> list) {
        for(int i=0; i<list.size(); i++) {
            Decl d=list.get(i);
            for(int j=0; j<d.names.size(); j++) {
                ExpName n=d.names.get(j);
                for(int k=j+1; k<d.names.size(); k++) if (d.names.get(k).name.equals(n.name)) return n;
                for(int k=i+1; k<list.size(); k++) if (list.get(k).hasName(n.name)) return n;
            }
        }
        return null;
    }

    /** Returns true if this declaration contains the given name. */
    public boolean hasName(String name) {
        for(int i=0; i<names.size(); i++) if (names.get(i).name.equals(name)) return true;
        return false;
    }
}
