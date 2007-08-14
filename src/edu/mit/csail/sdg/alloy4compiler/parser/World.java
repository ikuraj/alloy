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

import java.io.File;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/** Mutable; this stores the sig/field/fun/pred configuration. */
public final class World {

    private int z;

    /**
     * This helper function determines whether "s" is an instance of the util/ordering "Ord" sig.
     */
    public static boolean is_alloy3ord(String paramname, String filename) {
        return paramname.equals("elem") && filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als");
    }

    /**
     * This helper function determines whether "s" is an instance of the util/ordering "Ord" sig.
     */
    public static Sig is_alloy3ord(Sig s) {
        Object x = s.anno.get("orderingSIG");
        if (x instanceof Sig) return (Sig)x;
        return null;
    }

    /** Generates a new signature. */
    PrimSig makeSIG(Pos pos, Module module, PrimSig parent, String name, boolean isAbstract, boolean lone, boolean one, boolean some, boolean isLeaf) throws Err {
        String fullname = module.paths.contains("") ? "this/"+name : (module.paths.get(0)+"/"+name);
        PrimSig x=new PrimSig(pos, parent, fullname, isAbstract, lone, one, some, isLeaf);
        sigs.add(x);
        return x;
    }

    /** Generates a new signature. */
    SubsetSig makeSUBSETSIG(Pos pos, Module module, Collection<Sig> parents, String name, boolean isAbstract, boolean lone, boolean one, boolean some, boolean isLeaf) throws Err {
        if (isAbstract) throw new ErrorType(pos, "Subset signature \""+name+"\" cannot be abstract.");
        String fullname = module.paths.contains("") ? "this/"+name : (module.paths.get(0)+"/"+name);
        SubsetSig x=new SubsetSig(pos, parents, fullname, lone, one, some);
        sigs.add(x);
        return x;
    }

    /** List of all signatures (NOTE: this field must follow this.{UNIV/SIGINT/SEQIDX/NONE} since it reads them) */
    private SafeList<Sig> sigs=new SafeList<Sig>(Util.asList(UNIV, SIGINT, SEQIDX, NONE));

    /** Returns an unmodifiable list of all signatures. */
    public SafeList<Sig> getAllSigs() { return sigs.dup(); }

    /** Construct a blank new world (except it has three builtin signatures UNIV, SIGINT, SEQIDX, and NONE) */
    public World() { }

    /** This lists all modules in this world; it must be consistent with this.path2module */
    private SafeList<Module> modules = new SafeList<Module>();

    /** This maps pathname to the Module it refers to; it must be consistent with this.modules */
    private Map<String,Module> path2module = new LinkedHashMap<String,Module>();

    /** Returns an unmodifiable list of all modules. */
    public SafeList<Module> getAllModules() { return modules.dup(); }

    /** Returns the main module; returns null if the main module has not been created yet. */
    public Module getRootModule() { return path2module.get(""); }

    /**
     * Returns the module with the given path; returns null if there is no module with that path.
     *
     * <p> Note: a module can have more than 1 valid paths to it;
     * thus, calling lookupModule() with different arguments may return the same Module.
     * <br> To avoid receiving duplicates, call world.getAllModules() to get the list of all modules.
     */
    public Module lookupModule(String path) { return path2module.get(path); }

    /**
     * Find the module with the given path, then return it and all its descendent modules.
     * (it will return an empty list if there are no modules matching this criteria)
     *
     * <p>   For example, suppose you have the following modules "", "A", "A/SUB1", "A/SUB2", "ANOTHER".
     * <br>  Then "" will return every module.
     * <br>  But "A" will only return "A", "A/SUB1", and "A/SUB2" (it will NOT return "ANOTHER")
     * <br>  And "A/SUB1" will only return "A/SUB1"
     * <br>  And "A/SUB" will return an empty list.
     *
     * <p>   Let's take another example, suppose you have the following modules "", "A/SUB1", "A/SUB2".
     * <br>  Then "A" will return "A/SUB1" and "A/SUB2" (even though there is no module called "A")
     */
    SafeList<Module> lookupModuleAndSubmodules(String path) {
        if (path.length()==0) return modules.dup();
        SafeList<Module> ans=new SafeList<Module>();
        Module top=path2module.get(path);
        if (top!=null) ans.add(top);
        path=path+"/";
        again: for(Module u:modules) for(String n:u.paths) if (n.startsWith(path)) {ans.add(u); continue again;}
        return ans.dup();
    }

    /** Create a new module with the given list of paths. */
    public Module establishModule(Pos pos, List<String> paths) throws Err {
        if (paths.size()==0) throw new ErrorSyntax("A module must have at least 1 path pointing to it.");
        TempList<String> temp=new TempList<String>(paths);
        temp.sort(Util.slashComparator);
        int n=temp.size();
        for(int i=0; i<n; i++)
          if (path2module.containsKey(temp.get(i)))
            throw new ErrorSyntax("A module with the path \""+temp.get(i)+"\" already exists.");
        Module u=new Module(this, pos, temp.makeConst());
        for(int i=0; i<n; i++) { path2module.put(temp.get(i), u); }
        modules.add(u);
        return u;
    }
}
