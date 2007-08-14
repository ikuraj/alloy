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
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Pos;

final class ExpDecl {

    /** If nonnull, then this Decl is disjoint */
    public final Pos disjoint;

    public final ConstList<ExpName> names;

    public final Exp expr;

    private Pos span=null;
    public Pos span() {
        Pos p=span;
        if (p==null) { p=expr.span().merge(disjoint); for(ExpName n:names) p=p.merge(n.span()); span=p; }
        return p;
    }

    public ExpDecl(Pos disjoint, List<ExpName> names, Exp expr) {
        this.disjoint=disjoint;
        this.names=ConstList.make(names);
        this.expr=expr;
    }

    public boolean hasName(String name) {
        for(int i=0; i<names.size(); i++) if (names.get(i).name.equals(name)) return true;
        return false;
    }

    public static String findDuplicateName (List<ExpDecl> list) {
        for(int i=0; i<list.size(); i++) {
            ExpDecl d=list.get(i);
            for(int j=0; j<d.names.size(); j++) {
                String n=d.names.get(j).name;
                for(int k=j+1; k<d.names.size(); k++) if (d.names.get(k).name.equals(n)) return n;
                for(int k=i+1; k<list.size(); k++) if (list.get(k).hasName(n)) return n;
            }
        }
        return null;
    }

}
