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
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
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
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule.FunAST;
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule.SigAST;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/**
 * This class provides convenience methods for calling the parser and the compiler
 */

public final class CompUtil {

    /** Constructor is private, since this class never needs to be instantiated. */
    private CompUtil() { }

    /**
     * Parses the input as an Alloy expression from that world
     * @throws Err if world==null or if any error occurred
     * @return the expression if no error occurred (it will be fully typechecked)
     */
    public static Expr parseOneExpression_fromString(World world, String input) throws Err {
        try {
            if (world==null) throw new ErrorAPI("parseOneExpression() cannot be called with null World");
            Map<String,String> fc=new LinkedHashMap<String,String>();
            fc.put("", "run{\n"+input+"}"); // We prepend the line "run{"
            CompModule u=CompParser.alloy_parseStream(fc, world, -1, "", "");
            if (u.funs.size()==0) throw new ErrorSyntax("The input does not correspond to an Alloy expression.");
            Exp body = u.funs.get(0).body;
            Context cx = new Context(world.getRootModule());
            Expr ans = Context.resolveExp(body.check(cx), new ArrayList<ErrorWarning>());
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
     * Parses 1 module from the input stream (without loading any subfiles)
     * @throws Err if any error occurred
     * @return an array of 0 or more Command if no error occurred
     */
    public static ConstList<Command> parseOneModule_fromString(String content) throws Err {
        try {
            Map<String,String> fc=new LinkedHashMap<String,String>();
            fc.put("",content);
            CompModule u=CompParser.alloy_parseStream(fc, null, 0, "", "");
            TempList<Command> ans=new TempList<Command>(u.commands.size());
            for(Pair<String,Command> x: u.commands) ans.add(x.b);
            return ans.makeConst();
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
            CompModule u=CompParser.alloy_parseStream(fc, null, 0, filename, "");
            TempList<Command> ans=new TempList<Command>(u.commands.size());
            for(Pair<String,Command> x: u.commands) ans.add(x.b);
            return ans.makeConst();
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Go up the directory hierachy 0 or more times.
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",1) will return "/home/abc"
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",2) will return "/home"
     * @param filepath - this must be an absolute path
     * @param numberOfSteps - the number of steps to go up
     */
    private static String goUp(String filepath, int numberOfSteps) {
        while(numberOfSteps>0) {
            numberOfSteps--;
            int i=filepath.lastIndexOf(File.separatorChar);
            if (i<=0) return "";
            filepath=filepath.substring(0,i);
        }
        return filepath;
    }

    /**
     * Given the name of a module, and the filename for that module, compute the filename for another module
     * @param moduleA - must be a legal Alloy modulepath (eg. name) (eg. name/name/name); must not start or end in '/'
     * @param fileA - the filename corresponding to moduleA
     * @param moduleB - must be a legal Alloy modulepath
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
                // Case 1: different
                // Eg. if  util/ordering is at /home/models/util/ordering.als, and we want puzzle/handshake
                // then we go up 2 steps to get /home/models
                // then append "/puzzle/handshake.als" to get "/home/models/puzzle/handshake.als"
                //
                // Case 2: a<0 and b<0
                // Eg. if "main" is at /home/models/main.als, and we want "main2",
                // then we go up 1 step to get /home/models
                // then append "/main2.als" to get "/home/models/main2.als"
                //
                // Case 3: a<0 but b>=0
                // Eg. if "main" is at /home/models/main.als, and we want "main/test",
                // then we go up 1 step to get /home/models
                // then append "/main/test.als" to get "/home/models/main/test.als"
                //
                // Case 4: a>=0 but b<0
                // Eg. if "util/ordering" is at /home/models/util/ordering.als, and we want "main",
                // then we go up 2 steps to get /home/models
                // then append "/main.als" to get "/home/models/main.als"
                int numberOfSlash=0;
                for(int i=0; i<moduleA.length(); i++)  if (moduleA.charAt(i)=='/') numberOfSlash++;
                return goUp(fileA, numberOfSlash+1)+File.separatorChar+moduleB.replace('/',File.separatorChar)+".als";
            }
            moduleA=moduleA.substring(a+1);
            moduleB=moduleB.substring(b+1);
        }
        return ""; // This shouldn't happen, since there should always be some character after '/' in the module name
    }

    /** This is step 1 of the postprocessing: figure out the instantiating parameters of each module. */
    private static boolean alloy_fillParams(ArrayList<CompModule> modules) throws Err {
        boolean chg=false;
        CompOpen missing=null;
        for(CompModule u:modules) for(Map.Entry<String, CompOpen> f:u.opencmds.entrySet()) {
            CompModule uu=u.opens.get(f.getKey());
            int j=uu.params.size();
            if (f.getValue().args.size() != j)
                throw new ErrorSyntax(u.pos, "To import the \""+uu.pos.filename+"\" module, you must provide exactly "+j+" parameters.");
            int i=0;
            for(Map.Entry<String,SigAST> pp:uu.params.entrySet()) {
                String kn=pp.getKey();
                SigAST old=pp.getValue();
                String vn=f.getValue().args.get(i); i++;
                Set<SigAST> v=u.lookup_sigORparam(vn);
                if (v.size()<1) {if (old==null) missing=f.getValue(); continue;}
                if (v.size()>1) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because the signature named \""+vn+"\" is ambiguous");
                SigAST vv=v.iterator().next();
                if (old==vv) continue;
                if (old!=null) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because it is being imported more than once, with different arguments.");
                //if (vv==vv.world.NONE) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because you cannot use \"none\" as an instantiating argument.");
                chg=true;
                uu.params.put(kn,vv);
                if (uu.pos!=null && uu.pos.filename!=null && f.getValue().pos!=null && World.is_alloy3ord(kn, uu.pos.filename))
                    vv.orderingPosition=f.getValue().pos;
                A4Reporter.getReporter().parse("RESOLVE: "+f.getKey()+"/"+kn+" := "+vv+"\n");
            }
        }
        if (chg==false && missing!=null) throw new ErrorSyntax(missing.pos, "Failed to import the module, because one of the instantiating signature cannot be found");
        return chg;
    }

    /** This is step 2 of the postprocessing: merging modules that have same filename and instantiating arguments. */
    private static boolean alloy_mergeModules(ArrayList<CompModule> modules) {
        // Before merging, the only pointers that go between Module objects are
        // (1) a module's "params" may point to a sig in another module
        // (2) a module's "opens" may point to another module
        // So when we find that two modules A and B should be merged,
        // we iterate through every module (except B), and replace
        // pointers into B with pointers into A.
        boolean chg=false;
        for(int i=0; i<modules.size(); i++) {
            CompModule a=modules.get(i);
            for(int j=i+1; j<modules.size(); j++) {
                CompModule b=modules.get(j);
                if (a.pos.filename.equals(b.pos.filename) && a.params.equals(b.params)) {
                    A4Reporter.getReporter().parse("MATCH FOUND ON "+a.pos.filename+"\n");
                    if (Util.slashComparator.compare(a.paths.get(0), b.paths.get(0)) > 0) {
                        CompModule temp=a;
                        a=b;
                        b=temp;
                        modules.set(i, a);
                    }
                    modules.remove(j);
                    a.paths.addAll(b.paths);
                    Collections.sort(a.paths, Util.slashComparator);
                    Map<String,SigAST> asigs=new LinkedHashMap<String,SigAST>(a.sigs);
                    for(CompModule c:modules) if (c!=b && c!=a) {
                        for(Map.Entry<String,SigAST> p:c.params.entrySet()) {
                            if (isin(p.getValue(),asigs)) p.setValue(a.sigs.get(p.getValue().name));
                            if (isin(p.getValue(),b.sigs)) p.setValue(a.sigs.get(p.getValue().name));
                        }
                        for(Map.Entry<String,CompModule> p:c.opens.entrySet()) if (p.getValue()==b) p.setValue(a);
                    }
                    chg=true;
                }
            }
        }
        return chg;
    }

    private static<V> boolean isin(V x, Map<String,V> y) {
        for(Map.Entry<String,V> e:y.entrySet()) if (e.getValue()==x) return true;
        return false;
    }

    private static Set<SigAST> lookupSigOrParameterOrFunctionOrPredicate (Map<String,CompModule> path2module, CompModule u, String name) {
        SigAST s;
        Set<SigAST> ans=new LinkedHashSet<SigAST>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:u.paths) {
                if (p.length()==0) {
                    for(CompModule uu:path2module.values()) if ((s=uu.sigs.get(name))!=null) ans.add(s);
                } else {
                    for(Map.Entry<String,CompModule> uu:path2module.entrySet())
                      if (uu.getKey().equals(p) || uu.getKey().startsWith(p+"/"))
                        if ((s=uu.getValue().sigs.get(name))!=null)
                          ans.add(s);
                }
            }
            s=u.params.get(name); if (s!=null) ans.add(s);
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        s=u.params.get(name); if (s!=null) ans.add(s);
        int i=name.lastIndexOf('/');
        if (i>=0) u = path2module.get((u.path.length()==0?"":(u.path+"/"))+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            if ((s=u.sigs.get(name))!=null) ans.add(s);
        }
        return ans;
    }

    private static Sig xfill(World world, Map<String,CompModule> path2module, SigAST oldS) throws Err {
        if (oldS.topoSig!=null) return oldS.topoSig;
        if (oldS.topo) throw new ErrorType("Sig "+oldS+" is involved in a cyclic inheritance.");
        oldS.topo=true;
        CompModule u=oldS.topoParent;
        Sig s;
        if (oldS.subset)  {
            ArrayList<Sig> parents = new ArrayList<Sig>();
            for(String n:oldS.parents) {
                Sig parent;
                if (n.equals("univ")) parent=UNIV;
                else if (n.equals("Int")) parent=SIGINT;
                else if (n.equals("seq/Int")) parent=SEQIDX;
                else if (n.equals("none")) parent=NONE;
                else {
                  Set<SigAST> anss=lookupSigOrParameterOrFunctionOrPredicate(path2module,u,n);
                  if (anss.size()>1) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" tries to be a subset of \""+n+"\", but the name \""+n+"\" is ambiguous.");
                  if (anss.size()<1) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" tries to be a subset of a non-existent signature \""+n+"\"");
                  parent = xfill(world, path2module, anss.iterator().next());
                }
                if (parent==UNIV) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" is already implicitly a subset of the builtin \"univ\" signature");
                if (parent==SIGINT) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot be a subset of the builtin \"Int\" signature");
                if (parent==SEQIDX) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot be a subset of the builtin \"seq/Int\" signature");
                if (parent==NONE) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot be a subset of the builtin \"none\" signature");
                parents.add(parent);
            }
            s = u.topoModule.addSubsetSig(oldS.pos, oldS.name, parents, oldS.abs, oldS.lone, oldS.one, oldS.some);
        } else {
            Sig parent;
            String sup="univ";
            if (oldS.parents.size()==1) {sup=oldS.parents.get(0); if (sup==null || sup.length()==0) sup="univ";}
            if (sup.equals("univ")) parent=UNIV;
            else if (sup.equals("Int")) parent=SIGINT;
            else if (sup.equals("seq/Int")) parent=SEQIDX;
            else if (sup.equals("none")) parent=NONE;
            else {
                Set<SigAST> anss=lookupSigOrParameterOrFunctionOrPredicate(path2module,u,sup);
                if (anss.size()>1) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" tries to extend \""+sup+"\", but that name is ambiguous.");
                if (anss.size()<1) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" tries to extend a non-existent signature \""+sup+"\"");
                parent = xfill(world, path2module, anss.iterator().next());
            }
            if (parent==SIGINT) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot extend the builtin \"Int\" signature");
            if (parent==SEQIDX) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot extend the builtin \"seq/Int\" signature");
            if (parent==NONE)   throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot extend the builtin \"none\" signature");
            if (!(parent instanceof PrimSig)) throw new ErrorSyntax(oldS.pos, "Sig "+oldS+" cannot extend a subset signature "+parent+"\".\nA signature can only extend a toplevel signature or a subsignature.");
            s = u.topoModule.addSig(oldS.pos, oldS.name, (PrimSig)parent, oldS.abs, oldS.lone, oldS.one, oldS.some, oldS.hint_isLeaf);
        }
        oldS.topoSig=s;
        if (oldS.absPosition!=null) s.anno.put("abstract", oldS.absPosition);
        if (oldS.lonePosition!=null) s.anno.put("lone", oldS.lonePosition);
        if (oldS.onePosition!=null) s.anno.put("one", oldS.onePosition);
        if (oldS.somePosition!=null) s.anno.put("some", oldS.somePosition);
        if (oldS.extendsPosition!=null) s.anno.put("extends", oldS.extendsPosition);
        if (oldS.inPosition!=null) s.anno.put("in", oldS.inPosition);
        if (oldS.orderingPosition!=null) s.anno.put("ordering", oldS.orderingPosition);
        return s;
    }

    /** This performs the postprocessing, converting from "List of CompModule" to "List of Module" */
    private static World alloy_resolve(final ArrayList<CompModule> modules) throws Err {
        JoinableList<Err> errors = JoinableList.emptylist();
        List<ErrorWarning> warns = new ArrayList<ErrorWarning>();
        final A4Reporter rep=A4Reporter.getReporter();
        final World world=modules.get(0).world;
        final LinkedHashMap<String,CompModule> path2module=new LinkedHashMap<String,CompModule>();
        final ArrayList<Module> ans=new ArrayList<Module>();

        for(CompModule x:modules) {
            for(String e:x.paths) path2module.put(e,x);
            Module y=world.establishModule(x.pos, x.paths);
            x.topoModule=y;
            ans.add(y);
            for(Map.Entry<String,SigAST> e:x.sigs.entrySet()) e.getValue().topoParent=x;
        }

        for(CompModule uu:modules) for(Map.Entry<String,SigAST> ss:uu.sigs.entrySet()) {
            SigAST oldS=ss.getValue();
            xfill(world, path2module, oldS);
        }

        for(CompModule x:modules) for(Map.Entry<String,SigAST> e:x.params.entrySet()) {
            // Record the parameters
            x.topoModule.addParameter(e.getKey(), e.getValue().topoSig);
            // If this is Ord, and the parameter is a valid elem, then note it in Ord's annotation map
            Sig elem = x.topoModule.getParameter("elem");
            if (elem!=null && !elem.builtin && x.topoModule.getAllSigs().size()==1) {
                Sig ord=x.topoModule.getAllSigs().get(0);
                if (!ord.builtin && ord.label.endsWith("/Ord")) {
                    if (ord.pos!=null && ord.pos.filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als")) {
                        ord.anno.put("orderingSIG", elem);
                    }
                }
            }
        }

        for(CompModule uu:modules) for(Map.Entry<String,SigAST> entry:uu.sigs.entrySet()) {
            // When typechecking the fields:
            // * each field is allowed to refer to earlier fields in the same SIG,
            //   as well as fields declared in any ancestor sig (as long as those ancestor sigs are visible from here)
            // * each field decl is allowed to refer to visible sigs
            // * each field decl is NOT allowed to refer to any predicate or function
            // * For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
            //   then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
            final Module y=uu.topoModule;
            final SigAST oldS=entry.getValue();
            final Sig s=oldS.topoSig;
            final Context cx = new Context(y);
            for(final ExpDecl d:oldS.fields) {
                cx.rootfield=true;
                cx.rootsig=s;
                // The name "this" does matter, since the parser and the typechecker both refer to it as "this"
                final ExprVar THIS = s.oneOf("this");
                cx.put("this", THIS);
                Expr bound=Context.resolveExpSet(d.expr.check(cx), warns), disjA=null, disjF=ExprConstant.TRUE;
                cx.remove("this");
                for(final ExpName n:d.names) {
                    for(Field f:s.getFields())
                        if (f.label.equals(n.name))
                          throw new ErrorSyntax(d.span(), "Sig \""+s+"\" cannot have 2 fields named \""+n.name+"\"");
                    final Field f=s.addTrickyField(d.span(), n.name, THIS, bound);
                    rep.typecheck("Sig "+s+", Field "+f.label+": "+f.type+"\n");
                    if (s.anno.get("orderingSIG") instanceof Sig) continue;
                    if (d.disjoint!=null) { if (disjA==null) disjA=f; else disjF = ExprBinary.Op.AND.make(d.disjoint, disjA.intersect(f).no(), disjF); disjA=disjA.plus(f); }
                }
                if (d.disjoint!=null && disjF!=ExprConstant.TRUE) y.addFact(""+s+"#disjoint", disjF);
            }
        }

        // The Alloy language forbids two overlapping sigs from having fields with the same name.
        // In other words: if 2 fields have the same name, then their type's first column must not intersect.
        final Map<String,List<Field>> fieldname2fields=new HashMap<String,List<Field>>();
        for(CompModule uu:modules) for(Map.Entry<String,SigAST> ss:uu.sigs.entrySet()) {
            Sig s=ss.getValue().topoSig;
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

        IdentityHashMap<FunAST,Func> funast2fun = new IdentityHashMap<FunAST,Func>();

        for(CompModule x:modules) {
            Module y=x.topoModule;
            for(FunAST f:x.funs) {
                String fullname = (y.path.length()==0 ? "this/" : (y.path+"/")) + f.name;
                Context cx = new Context(y);
                cx.rootfun=true;
                String dup = f.args==null ? null : ExpDecl.findDuplicateName(f.args);
                if (dup!=null) throw new ErrorSyntax(f.pos, "The parameter name \""+dup+"\" cannot appear more than once in this predicate/function declaration.");
                // Each PARAMETER can refer to earlier parameter in the same function, and any SIG or FIELD visible from here.
                // Each RETURNTYPE can refer to the parameters of the same function, and any SIG or FIELD visible from here.
                TempList<ExprVar> tmpvars=new TempList<ExprVar>();
                if (f.args!=null) {
                  for(ExpDecl d:f.args) {
                    Expr val = Context.resolveExpSet(d.expr.check(cx), warns);
                    errors=errors.join(val.errors);
                    for(ExpName n: d.names) {
                        ExprVar v=ExprVar.make(n.span(), n.name, val);
                        cx.put(n.name, v);
                        tmpvars.add(v);
                        rep.typecheck((f.returnType==null?"pred ":"fun ")+fullname+", Param "+n.name+": "+v.type+"\n");
                    }
                  }
                }
                Expr ret = f.returnType==null ? null : Context.resolveExpSet(f.returnType.check(cx), warns);
                if (ret!=null) errors=errors.join(ret.errors);
                Func ff = y.addFun(f.pos, f.name, tmpvars.makeConst(), ret);
                rep.typecheck(""+ff+", RETURN: "+ff.returnDecl.type+"\n");
                funast2fun.put(f, ff);
            }
        }

        Map<Func,Expr> func_2_formula = new LinkedHashMap<Func,Expr>();
        for(CompModule x:modules) {
            for(FunAST f:x.funs) {
                Func ff=funast2fun.get(f);
                Expr disj=ExprConstant.TRUE;
                Context cx=new Context(x.topoModule);
                Iterator<ExprVar> vv=ff.params.iterator();
                for(ExpDecl d:f.args) {
                    List<Expr> disjvars = (d.disjoint!=null && d.names.size()>0) ? (new ArrayList<Expr>()) : null;
                    for(ExpName n:d.names) {
                        ExprVar newvar=vv.next();
                        cx.put(n.name, newvar);
                        if (disjvars!=null) disjvars.add(newvar);
                    }
                    if (disjvars!=null) disj=disj.and(ExprBuiltin.makeDISJOINT(d.disjoint, disjvars));
                }
                Expr newBody;
                if (ff.isPred) {
                    newBody = Context.resolveExpFormula(f.body.check(cx), warns);
                } else {
                    newBody = Context.resolveExpSet(f.body.check(cx), warns);
                }
                errors = errors.join(newBody.errors);
                ff.setBody(newBody);
                for(ExpDecl d:f.args) for(ExpName n:d.names) cx.remove(n.name);
                if (ff.isPred) {
                    rep.typecheck(""+ff+", BODY:"+ff.getBody().type+"\n");
                } else {
                    if (ff.getBody().type.hasTuple() && ff.returnDecl.type.hasTuple() && !ff.getBody().type.intersects(ff.returnDecl.type))
                        rep.warning(new ErrorWarning(ff.getBody().span(),
                        "Function return value is disjoint from its return type.\n"
                        +"Function body has type "+ff.getBody().type+"\nbut the return type is "+ff.returnDecl.type));
                    rep.typecheck(""+ff+", BODY:"+ff.getBody().type+"\n");
                }
                Expr v=ff.getBody();
                if (!ff.isPred) v=v.in(ff.returnDecl);
                if (ff.params.size()>0) v=ExprQuant.Op.SOME.make(null, null, ff.params, v.and(disj));
                func_2_formula.put(ff, v);
            }
        }

        for(CompModule x:modules) {
            Module y=x.topoModule;
            Context cx = new Context(y);
            for(Map.Entry<String,Exp> e:x.asserts.entrySet()) {
                Expr formula = Context.resolveExpFormula(e.getValue().check(cx), warns);
                if (formula.errors.size()>0) errors=errors.join(formula.errors);
                else y.addAssertion(e.getKey(), formula);
            }
            for(Map.Entry<String,Exp> e:x.facts.entrySet()) {
                Expr formula = Context.resolveExpFormula(e.getValue().check(cx), warns);
                if (formula.errors.size()>0) errors=errors.join(formula.errors);
                else y.addFact(e.getKey(), formula);
            }
            for(Map.Entry<String,SigAST> e:x.sigs.entrySet()) {
                Sig s=e.getValue().topoSig;
                if (s.anno.get("orderingSIG") instanceof Sig) continue;
                Exp f=e.getValue().appendedFact;
                if (f==null) continue;
                ExprVar THIS = s.oneOf("this");
                cx.rootsig=s;
                cx.put("this", THIS);
                Expr formula = Context.resolveExpFormula(f.check(cx), warns);
                cx.remove("this");
                formula = formula.forAll(THIS);
                if (formula.errors.size()>0) errors=errors.join(formula.errors);
                else y.addFact(""+s+"#fact", formula);
            }
        }

        for(CompModule x:modules) if (x.paths.contains("")) {
            Module y=x.topoModule;
            for(Pair<String,Command> pair:x.commands) {
                String cname=pair.a;
                Command cmd=pair.b;
                Expr e=null;
                if (cmd.check) {
                    e=y.getAssertion(cname);
                    if (e==null) {
                        Set<Expr> ee=y.lookupAssertion(cname);
                        if (ee.size()>1) throw new ErrorSyntax(cmd.pos, "There are more than 1 assertion with the name \""+cname+"\".");
                        if (ee.size()<1) throw new ErrorSyntax(cmd.pos, "The assertion \""+cname+"\" cannot be found.");
                        e=ee.iterator().next();
                    }
                } else {
                    SafeList<Func> ee=y.getFunc(cname);
                    if (ee.size()<1) ee=y.lookupFunctionOrPredicate(cname);
                    if (ee.size()<1) throw new ErrorSyntax(cmd.pos, "The predicate/function \""+cname+"\" cannot be found.");
                    if (ee.size()>1) throw new ErrorSyntax(cmd.pos, "There are more than 1 predicate/function with the name \""+cname+"\".");
                    e=func_2_formula.get(ee.get(0));
                }
                y.addCommand(cmd.make(e));
            }
        }

        return world;
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
    public static World parseEverything_fromFile(Map<String,String> fc, String rootdir, String filename)
    throws Err {
        try {
            filename=Util.canon(filename);
            if (fc==null) fc=new LinkedHashMap<String,String>();
            ArrayList<CompModule> modules=new ArrayList<CompModule>();
            ArrayList<String> thispath=new ArrayList<String>();
            alloy_totalparseHelper(fc, rootdir, Pos.UNKNOWN, filename, null, null, "", modules, thispath);
            while(alloy_fillParams(modules)) {}
            while(alloy_mergeModules(modules)) {}
            World ans=alloy_resolve(modules);
            return ans;
        } catch(FileNotFoundException ex) {
            throw new ErrorSyntax("File cannot be found.\n"+ex.getMessage());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /**
     * Read everything from "reader" and parse it; if it mentions submodules, open them and parse them too.
     * @param rootdir - the directory for Alloy's builtin modules (eg. uti/ordering.als, util/integer.als, ...) (can be null)
     * @param filename - the location in the file system that the main module represents; (this file does not
     *        need to actually exist; it is given as a hint for deriving the other modules' locations)
     * @param filecontent - this contains the main module we are parsing
     * @return the fully parsed World object (if no error occurred)
     * @throws Err if an error occurred
     */
    public static World parseEverything_fromString(String rootdir, String filename, String filecontent)
    throws Err {
        try {
            filename=Util.canon(filename);
            LinkedHashMap<String,String> fc=new LinkedHashMap<String,String>();
            fc.put(filename,filecontent);
            ArrayList<CompModule> modules=new ArrayList<CompModule>();
            ArrayList<String> thispath=new ArrayList<String>();
            alloy_totalparseHelper(fc, rootdir, Pos.UNKNOWN, filename, null, null, "", modules, thispath);
            while(alloy_fillParams(modules)) {}
            while(alloy_mergeModules(modules)) {}
            World ans=alloy_resolve(modules);
            return ans;
        } catch(FileNotFoundException ex) {
            throw new ErrorSyntax("File cannot be found.\n"+ex.getMessage());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage());
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex;
            throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    /** Helper method that recursively open more files. */
    private static CompModule alloy_totalparseHelper(Map<String,String> fc, String rootdir,Pos pos,String name,CompModule parent,String parentFileName,String prefix,ArrayList<CompModule> modules,ArrayList<String> thispath)
    throws Err, FileNotFoundException, IOException {
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
        CompModule u=CompParser.alloy_parseStream(fc, parent==null?null:parent.world, 0, name, prefix);
        modules.add(u);
        // The returned Module object is fully-filled-in except
        // * Module.{opens,params}
        // * Sig.{type,sup,sups,subs}
        // * Field.halftype, Field.Full.fulltype, Expr*.type, and ExprName.resolved
        // Also, there will not be any ExprCall. Only ExprJoin.
        for(Map.Entry<String, CompOpen> opens:u.opencmds.entrySet()) {
            // Here, we recursively open the included files (to fill out the "Module.opens" field)
            CompOpen y=opens.getValue();
            CompModule uu=alloy_totalparseHelper(fc, rootdir, y.pos, y.filename, u, name, prefix.length()==0 ? y.alias : prefix+"/"+y.alias, modules, thispath);
            if (y.args.size() != uu.params.size()) throw new ErrorSyntax(y.pos, "You supplied "+y.args.size()+" arguments to the import statement, but the imported module requires "+uu.params.size()+" arguments.");
            u.opens.put(y.alias, uu);
        }
        thispath.remove(thispath.size()-1); // Remove this file from the CYCLE DETECTION LIST.
        return u;
    }
}
