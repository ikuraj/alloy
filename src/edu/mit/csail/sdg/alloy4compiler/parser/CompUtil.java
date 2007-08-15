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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.SigAST;

/** This class provides convenience methods for calling the parser and the compiler. */

public final class CompUtil {

    /** Constructor is private, since this class never needs to be instantiated. */
    private CompUtil() { }

    //=============================================================================================================//

    /** Returns true if exists some entry (a,b) in the map, such that b==value (using object identity as the comparison) */
    private static<V> boolean isin(V value, Map<String,V> map) {
        for(Map.Entry<String,V> e:map.entrySet()) if (e.getValue()==value) return true;
        return false;
    }

    //=============================================================================================================//

    /**
     * Go up the directory hierachy 0 or more times.
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",1) will return "/home/abc"
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",2) will return "/home"
     * @param filepath - this must be an absolute path
     * @param numberOfSteps - the number of steps to go up
     */
    private static String up(String filepath, int numberOfSteps) {
        while(numberOfSteps > 0) {
            numberOfSteps--;
            int i=filepath.lastIndexOf(File.separatorChar);
            if (i<=0) return "";
            filepath=filepath.substring(0,i);
        }
        return filepath;
    }

    //=============================================================================================================//

    /**
     * Given the name of a module, and the filename for that module, compute the filename for another module
     * @param moduleA - must be a legal Alloy modulepath (eg. name) (eg. name/name/name) (must not start or end in '/')
     * @param fileA - the filename corresponding to moduleA
     * @param moduleB - must be a legal Alloy modulepath (eg. name) (eg. name/name/name) (must not start or end in '/')
     * @return the filename corresponding to moduleB
     */
    private static String computeModulePath(String moduleA, String fileA, String moduleB) {
        fileA=Util.canon(fileA); // Make sure it's a canonical absolute path
        if (moduleA.length()==0) moduleA="anything"; // Harmonizes the boundary case
        while(moduleA.length()>0 && moduleB.length()>0) {
            int a=moduleA.indexOf('/'), b=moduleB.indexOf('/');
            String headOfA = (a>=0) ? moduleA.substring(0,a) : moduleA;
            String headOfB = (b>=0) ? moduleB.substring(0,b) : moduleB;
            if (!headOfA.equals(headOfB) || a<0 || b<0) {
                // eg. util/ordering==/home/models/util/ordering.als, then test=>/home/models/test.als"
                // eg. util/ordering==/home/models/util/ordering.als, then sub/test=>/home/models/sub/test.als
                // eg. main==/home/models/main.als, then test=>/home/models/test.als
                // eg. main==/home/models/main.als, then sub/test=>/home/models/sub/test.als"
                int numberOfSlash=0;
                for(int i=0; i<moduleA.length(); i++)  if (moduleA.charAt(i)=='/') numberOfSlash++;
                return up(fileA, numberOfSlash+1)+File.separatorChar+moduleB.replace('/',File.separatorChar)+".als";
            }
            moduleA=moduleA.substring(a+1);
            moduleB=moduleB.substring(b+1);
        }
        return ""; // This shouldn't happen, since there should always be some character after '/' in the module name
    }

    //=============================================================================================================//

    /** Helper method that recursively open more files. */
    private static Module parseRecursively (
            Map<String,String> fc, String rootdir,
            Pos pos, String name, Module parent, String parentFileName, String prefix,
            ArrayList<Module> modules,
            ArrayList<String> thispath) throws Err, FileNotFoundException, IOException {
        // Figure out the exact filename
        File f=new File(name);
        String canon=f.getCanonicalPath();
        if (!f.exists() && !fc.containsKey(canon) && parent!=null) {
            String parentPath = (parent.moduleName.length()>0) ? parent.moduleName : "anything";
            f=new File(CompUtil.computeModulePath(parentPath, parentFileName, name));
            canon=f.getCanonicalPath();
        }
        if (!f.exists() && !fc.containsKey(canon) && rootdir!=null && rootdir.length()>0) {
            f=new File((rootdir+"/models/"+name+".als").replace('/',File.separatorChar));
            canon=f.getCanonicalPath();
        }
        if (!f.exists() && !fc.containsKey(canon)) {
            String content;
            try {
                content = Util.readAll(true, "models/"+name+".als");
            } catch(IOException ex) {
                throw new ErrorSyntax(pos, "The module \""+name+"\" cannot be found.\nIt is not a built-in library module, and it cannot be found at \""+(new File(name)).getAbsolutePath()+"\".\n");
            }
            f=new File("/models/"+name+".als");
            canon=f.getCanonicalPath();
            fc.put(canon,content);
        }
        name=canon;
        // Add the filename into a ArrayList, so that we can detect cycles in the module import graph
        // How? I'll argue that (filename appears > 1 time along a chain) <=> (infinite loop in the import graph)
        // => As you descend down the chain via OPEN, if you see the same FILE twice, then
        //    you will go into an infinite loop (since, regardless of the instantiating parameter,
        //    that file will attempt to OPEN the exact same set of files. leading back to itself, etc. etc.)
        // <= If there is an infinite loop, that means there is at least 1 infinite chain of OPEN (from root).
        //    Since the number of files is finite, at least 1 filename will be repeated.
        if (thispath.contains(name)) throw new ErrorSyntax(pos,"Circular dependency in module import. The file \""+name+"\" is imported infinitely often.");
        thispath.add(name);
        // No cycle detected so far. So now we parse the file.
        Module u=CompParser.alloy_parseStream(fc, parent==null?null:parent.world, 0, name, prefix);
        modules.add(u);
        // The returned Module object is fully-filled-in except
        // * Module.{opens,params}
        // * Sig.{type,sup,sups,subs}
        // * Field.halftype, Field.Full.fulltype, Expr*.type, and ExprName.resolved
        // Also, there will not be any ExprCall. Only ExprJoin.
        for(Map.Entry<Open,Module> e:u.imports.entrySet()) {
            // Here, we recursively open the included files (to fill out the "Module.opens" field)
            Open x=e.getKey();
            Module y=parseRecursively(fc, rootdir, x.pos, x.filename, u, name, prefix.length()==0 ? x.alias : prefix+"/"+x.alias, modules, thispath);
            if (x.args.size() != y.params.size()) throw new ErrorSyntax(x.pos, "You supplied "+x.args.size()+" arguments to the import statement, but the imported module requires "+y.params.size()+" arguments.");
            e.setValue(y);
        }
        thispath.remove(thispath.size()-1); // Remove this file from the CYCLE DETECTION LIST.
        return u;
    }

    //=============================================================================================================//

    /** This is step 1 of the postprocessing: figure out the instantiating parameters of each module. */
    private static boolean alloy_fillParams(Module root) throws Err {
        boolean chg=false;
        Open missing=null;
        for(Module u:root.modules) for(Map.Entry<Open,Module> f:u.imports.entrySet()) {
            Module uu=f.getValue();
            int j=uu.params.size();
            if (f.getKey().args.size() != j)
                throw new ErrorSyntax(u.pos, "To import the \""+uu.pos.filename+"\" module, you must provide exactly "+j+" parameters.");
            int i=0;
            for(Map.Entry<String,SigAST> pp:uu.params.entrySet()) {
                String kn=pp.getKey();
                SigAST old=pp.getValue();
                String vn=f.getKey().args.get(i); i++;
                Set<SigAST> v=u._lookup_sigORparam(vn);
                if (v.size()<1) {if (old==null) missing=f.getKey(); continue;}
                if (v.size()>1) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because the signature named \""+vn+"\" is ambiguous");
                SigAST vv=v.iterator().next();
                if (old==vv) continue;
                if (old!=null) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because it is being imported more than once, with different arguments.");
                //if (vv==vv.world.NONE) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because you cannot use \"none\" as an instantiating argument.");
                chg=true;
                pp.setValue(vv);
                if (uu.pos!=null && uu.pos.filename!=null && f.getKey().pos!=null && Module.is_alloy3ord(kn, uu.pos.filename))
                    vv.orderingPosition=f.getKey().pos;
                A4Reporter.getReporter().parse("RESOLVE: "+f.getKey().alias+"/"+kn+" := "+vv+"\n");
            }
        }
        if (chg==false && missing!=null) throw new ErrorSyntax(missing.pos, "Failed to import the module, because one of the instantiating signature cannot be found");
        return chg;
    }

    //=============================================================================================================//

    /** This is step 2 of the postprocessing: merging modules that have same filename and instantiating arguments. */
    private static boolean alloy_mergeModules(Module root) {
        // Before merging, the only pointers that go between Module objects are
        // (1) a module's "params" may point to a sig in another module
        // (2) a module's "opens" may point to another module
        // So when we find that two modules A and B should be merged,
        // we iterate through every module (except B), and replace
        // pointers into B with pointers into A.
        boolean chg=false;
        List<Module> modules=root.modules;
        for(int i=0; i<modules.size(); i++) {
            Module a=modules.get(i);
            for(int j=i+1; j<modules.size(); j++) {
                Module b=modules.get(j);
                if (a.pos.filename.equals(b.pos.filename) && a.params.equals(b.params)) {
                    chg=true;
                    A4Reporter.getReporter().parse("MATCH FOUND ON "+a.pos.filename+"\n");
                    if (i!=0 && Util.slashComparator.compare(a.path, b.path)>0) { a=b; b=modules.get(i); modules.set(i,a); }
                    modules.remove(j);
                    j--;
                    for(String c:b.paths) root.path2module.put(c,a);
                    a.paths.addAll(b.paths);
                    Collections.sort(a.paths, Util.slashComparator);
                    for(Module c:modules) {
                      for(Map.Entry<String,SigAST> p:c.params.entrySet())
                         if (isin(p.getValue(), b.sigs)) p.setValue(a.sigs.get(p.getValue().name));
                      for(Map.Entry<Open,Module> p:c.imports.entrySet())
                         if (p.getValue()==b) p.setValue(a);
                    }
                }
            }
        }
        return chg;
    }

    //=============================================================================================================//

    /** This is step 3 of the postprocessing: converting from "Exp" to "Expr" */
    private static Module alloy_resolve(final Module world) throws Err {
        JoinableList<Err> errors = new JoinableList<Err>();
        final List<ErrorWarning> warns = new ArrayList<ErrorWarning>();
        final A4Reporter rep = A4Reporter.getReporter();
        final List<Module> modules = world.modules;

        for(Module x:modules) for(Map.Entry<String,SigAST> e:x.sigs.entrySet()) e.getValue().realModule=x;

        for(Module x:modules) for(Map.Entry<String,SigAST> e:x.sigs.entrySet()) Module.checkSig(e.getValue());

        for(Module x:modules) {
            SigAST elemX=x.params.get("elem");
            if (elemX==null) continue;
            Sig elem=elemX.realSig;
            if (!elem.builtin && x.getAllSigs().size()==1) {
                Sig ord=x.getAllSigs().get(0);
                if (!ord.builtin && ord.label.endsWith("/Ord")) {
                    if (ord.pos!=null && ord.pos.filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als")) {
                        ord.anno.put("orderingSIG", elem);
                    }
                }
            }
        }

        for(Module uu:modules) for(SigAST oldS:uu.sigs.values()) {
            // When typechecking the fields:
            // * each field is allowed to refer to earlier fields in the same SIG,
            //   as well as fields declared in any ancestor sig (as long as those ancestor sigs are visible from here)
            // * each field decl is allowed to refer to visible sigs
            // * each field decl is NOT allowed to refer to any predicate or function
            // * For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
            //   then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
            final Sig s=oldS.realSig;
            final Context cx = new Context(uu);
            for(final Decl d:oldS.fields) {
                cx.rootfield=true;
                cx.rootsig=s;
                // The name "this" does matter, since the parser and the typechecker both refer to it as "this"
                final ExprVar THIS = s.oneOf("this");
                cx.put("this", THIS);
                Expr bound = d.expr.check(cx, warns).resolve_as_set(warns), disjA=null, disjF=ExprConstant.TRUE;
                cx.remove("this");
                for(final ExpName n:d.names) {
                    for(Field f:s.getFields())
                        if (f.label.equals(n.name))
                          throw new ErrorSyntax(d.span(), "Sig \""+s+"\" cannot have 2 fields named \""+n.name+"\"");
                    final Field f=s.addTrickyField(d.span(), n.name, THIS, bound);
                    rep.typecheck("Sig "+s+", Field "+f.label+": "+f.type+"\n");
                    //if (s.anno.get("orderingSIG") instanceof Sig) continue;
                    if (d.disjoint!=null) { if (disjA==null) disjA=f; else disjF = ExprBinary.Op.AND.make(d.disjoint, null, disjA.intersect(f).no(), disjF); disjA=disjA.plus(f); }
                }
                if (d.disjoint!=null && disjF!=ExprConstant.TRUE) uu.addFact(Pos.UNKNOWN, ""+s+"#disjoint", disjF);
            }
        }

        // The Alloy language forbids two overlapping sigs from having fields with the same name.
        // In other words: if 2 fields have the same name, then their type's first column must not intersect.
        final Map<String,List<Field>> fieldname2fields=new LinkedHashMap<String,List<Field>>();
        for(Module uu:modules) for(Map.Entry<String,SigAST> ss:uu.sigs.entrySet()) {
            Sig s=ss.getValue().realSig;
            for(Field field:s.getFields()) {
                List<Field> peers=fieldname2fields.get(field.label);
                if (peers==null) { peers=new ArrayList<Field>(); fieldname2fields.put(field.label, peers); }
                for(Field field2:peers)
                    if (field.type.firstColumnOverlaps(field2.type))
                        throw new ErrorType(field.pos,
                        "Two overlapping signatures cannot have\ntwo fields with the same name \""+field.label
                        +"\":\n\n1) one is in sig \""+field.sig+"\"\n"+field.pos
                        +"\n\n2) the other is in sig \""+field2.sig+"\"\n"+field2.pos);
                peers.add(field);
            }
        }

        for(Module x:modules) errors = x.checkFunctionDecls(errors, warns);
        for(Module x:modules) errors = x.checkFunctionBodies(errors, warns);
        for(Module x:modules) {
            errors = x.checkAssertions(errors, warns);
            errors = x.checkFacts(errors, warns);
            if (x.paths.contains("")) errors = x.checkCommands(errors, warns);
        }

        for(ErrorWarning w:warns) rep.warning(w);
        if (!errors.isEmpty()) throw errors.get(0); else return world;
    }

    //=============================================================================================================//

    /**
     * Parses 1 module from the input stream (without loading any subfiles)
     * @throws Err if any error occurred
     * @return an array of 0 or more Command if no error occurred
     */
    public static ConstList<Command> parseOneModule_fromString(String content) throws Err {
        try {
            Map<String,String> fc=new LinkedHashMap<String,String>();
            fc.put("",content);
            Module u=CompParser.alloy_parseStream(fc, null, 0, "", "");
            return u.getAllCommands();
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Parses 1 module from the file (without loading any subfiles)
     * @throws Err if any error occurred
     * @return an array of 0 or more Command if no error occurred
     */
    public static ConstList<Command> parseOneModule_fromFile(String filename) throws Err {
        try {
            Map<String,String> fc=new LinkedHashMap<String,String>();
            Module u=CompParser.alloy_parseStream(fc, null, 0, filename, "");
            return u.getAllCommands();
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Parses the input as an Alloy expression from that world
     * @throws Err if world==null or if any error occurred
     * @return the expression if no error occurred (it will be fully typechecked)
     */
    public static Expr parseOneExpression_fromString(Module world, String input) throws Err {
        try {
            if (world==null) throw new ErrorAPI("parseOneExpression() cannot be called with null World");
            Map<String,String> fc=new LinkedHashMap<String,String>();
            fc.put("", "run {\n"+input+"}"); // We prepend the line "run{"
            Module u = CompParser.alloy_parseStream(fc, world, -1, "", "");
            Exp body = u.getFirstFunc();
            if (body == null) throw new ErrorSyntax("The input does not correspond to an Alloy expression.");
            Context cx = new Context(world);
            ArrayList<ErrorWarning> warnings = new ArrayList<ErrorWarning>();
            Expr ans = body.check(cx, warnings);
            ans = ans.resolve(ans.type, warnings);
            if (ans.errors.size()>0) throw ans.errors.get(0);
            return ans;
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Read everything from "file" and parse it; if it mentions submodules, open them and parse them too.
     * @param fc - a cache of files that have been pre-fetched (can be null if there were no prefetching)
     * @param rootdir - the directory for Alloy's builtin modules (eg. util/ordering.als, util/integer.als, ...); can be null.
     * @param filename - the main module we are parsing
     * @return the fully parsed World object (if no error occurred)
     * @throws Err if an error occurred
     * <p>Note: if we read more files, these will be stored into "fc" (if fc is nonnull)
     */
    public static Module parseEverything_fromFile(Map<String,String> fc, String rootdir, String filename) throws Err {
        try {
            filename=Util.canon(filename);
            if (fc==null) fc=new LinkedHashMap<String,String>();
            ArrayList<Module> modules=new ArrayList<Module>();
            ArrayList<String> thispath=new ArrayList<String>();
            parseRecursively(fc, rootdir, Pos.UNKNOWN, filename, null, null, "", modules, thispath);
            Module root = modules.get(0);
            while(alloy_fillParams(root)) {}
            while(alloy_mergeModules(root)) {}
            return alloy_resolve(root);
        } catch(FileNotFoundException ex) {
            throw new ErrorSyntax("File cannot be found.\n"+ex.getMessage());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }
}
