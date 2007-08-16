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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
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
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBinary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBuiltin;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprQuant;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;

/** Mutable; this class represents an Alloy module; equals() uses object identity. */

public final class Module {

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy module import statement; equals() uses object identity. */
    static final class Open {
        /** The position in the original model where this "open" statement was declared; never null. */
        final Pos pos;
        /** The alias for this open declaration; always a nonempty string. */
        final String alias;
        /** The unmodifiable list of instantiating arguments. */
        final ConstList<String> args;
        /** The relative filename for the file being imported, without the final ".als" part; always a nonempty string. */
        final String filename;
        /** The actual Module object that it points to; null until we resolve it. */
        Module realModule=null;
        /** Constructs an Open object. */
        private Open(Pos pos, String alias, ConstList<String> args, String filename) {
            this.pos=pos; this.alias=alias; this.args=args; this.filename=filename;
        }
    }

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy function; equals() uses object identity. */
    private static final class FunAST {
        /** Only initialized by typechecker. */ private Func realFunc=null;
        /** Only initialized by typechecker. */ private Expr realFormula=null;
        /** The original position.           */ private final Pos pos;
        /** The short name.                  */ private final String name;
        /** The parameters.                  */ private final ConstList<Decl> params;
        /** The return type.                 */ private final Exp returnType;
        /** The body.                        */ private final Exp body;
        FunAST(Pos pos, String name, List<Decl> params, Exp returnType, Exp body) {
            this.pos=pos; this.name=name; this.params=ConstList.make(params); this.returnType=returnType; this.body=body;
        }
        @Override public String toString() { return name; }
    }

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy signature; equals() uses object identity. */
    static final class SigAST {
        private boolean topo=false;     // This flag is set to "true" during topological sort
        private final Module realModule; // This value is set to its Module during topological sort
        private Sig realSig=null;       // This value is set to its corresponding Sig during topological sort
        private boolean hint_isLeaf=false;
        private final Pos pos;
        private final String name,fullname;
        private final Pos abs,lone,one,some,subset;
        private final ConstList<String> parents;
        private final ConstList<Decl> fields;
        private final Exp appendedFact;
        Pos isOrdered=null;
        SigAST(Pos pos, String fullname, String name, Pos abs, Pos lone, Pos one, Pos some, Pos subset,
            List<String> parents, List<Decl> fields, Exp appendedFacts, Module realModule, Sig realSig) {
            this.pos=pos;
            this.fullname=fullname;
            this.name=name;
            this.abs=abs;
            this.lone=lone;
            this.one=one;
            this.some=some;
            this.subset=subset;
            this.parents=ConstList.make(parents);
            this.fields=ConstList.make(fields);
            this.appendedFact=appendedFacts;
            this.realModule=realModule;
            this.realSig=realSig;
        }
        @Override public String toString() { return fullname; }
    }

    //======== ROOT ONLY =========================================================================================================//

    /** IF ROOT: This lists all modules in this world; it must be consistent with this.path2module */
    private final ArrayList<Module> modules = new ArrayList<Module>();

    /** IF ROOT: This maps pathname to the Module it refers to; it must be consistent with this.modules */
    private final Map<String,Module> path2module = new LinkedHashMap<String,Module>();

    /** Create a new module with the given list of paths. */
    Module lookupOrCreateModule(Pos pos, String path) throws Err {
        Module u=path2module.get(path);
        if (u==null) u=new Module(world, pos, path); // The constructor will automatically add it to path2module if not there already
        return u;
    }

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
    private SafeList<Module> lookupModuleAndSubmodules(String path) {
        if (path.length()==0) return getAllModules();
        SafeList<Module> ans=new SafeList<Module>();
        Module top=path2module.get(path);
        if (top!=null) ans.add(top);
        path=path+"/";
        again: for(Module u:modules) if (u!=top) for(String n:u.paths) if (n.startsWith(path)) {ans.add(u); continue again;}
        return ans.dup();
    }

    /** Returns the root module in this world. */
    public Module getRootModule() { return this; }

    /** Returns an unmodifiable list of all modules in this world. */
    public SafeList<Module> getAllModules() { return (new SafeList<Module>(modules)).dup(); }

    /** Returns an unmodifiable list of all signatures in this world. */
    public SafeList<Sig> getAllSigsInTheWorld() {
        SafeList<Sig> x = new SafeList<Sig>();
        x.add(UNIV);
        x.add(SIGINT);
        x.add(SEQIDX);
        x.add(NONE);
        for(Module m:modules) for(SigAST s:m.sigs.values()) x.add(s.realSig);
        return x.dup();
    }

    //============================================================================================================================//

    /**
     * Constructs a new Module object
     * @param world - the world that this Module belongs to (null if this is the beginning of a new World)
     * @param pos - the position of the "module" line at the top of the file (it can be null if unknown)
     * @param path - one of the path pointing to this module
     */
    Module(Module world, Pos pos, String path) throws Err {
        if (world==null) { if (path.length()>0) throw new ErrorAPI(pos, "Root module misparsed."); else world=this; }
        if (world.path2module.containsKey(path)) throw new ErrorSyntax("A module with the path \""+path+"\" already exists.");
        world.path2module.put(path,this);
        world.modules.add(this);
        this.world=world;
        this.pos=(pos==null ? Pos.UNKNOWN : pos);
        this.path=path;
        this.paths=new ArrayList<String>(1);
        this.paths.add(path);
    }

    //=======GOOD BELOW===========================================================================================================//

    /** The world that this Module belongs to. */
    final Module world;

    /** The simplest path pointing to this Module; it is always equal to this.paths.get(0) */
    private final String path;

    /** The list of paths pointing to this Module; it is always nonempty and already sorted by Util.slashComparator */
    private final List<String> paths;

    /** The position of the "MODULE" line at the top of the file; Pos.UNKNOWN if the line has not been parsed from the file yet. */
    private Pos pos = Pos.UNKNOWN;

    /** The text of the "MODULE" line at the top of the file; "unknown" if the line has not be parsed from the file yet. */
    String moduleName="unknown";

    /** Each param is mapped to its corresponding SigAST (or null if we have not resolved it). */
    final Map<String,SigAST> params=new LinkedHashMap<String,SigAST>(); // Must be a LinkedHashMap since the order must be preserved

    /** Each alias is mapped to its corresponding "open" statement. */
    final Map<String,Open> opens=new LinkedHashMap<String,Open>();

    /** Each sig name is mapped to its corresponding SigAST. */
    private final Map<String,SigAST> sigs = new LinkedHashMap<String,SigAST>();

    /** Each func name is mapped to a nonempty list of FunAST objects. */
    private final Map<String,SafeList<FunAST>> funcs = new LinkedHashMap<String,SafeList<FunAST>>();

    /** Each assertion name is mapped to either an untypechecked Exp or a typechecked Expr. */
    private final Map<String,Object> asserts = new LinkedHashMap<String,Object>();

    /** Each fact name is mapped to either an untypechecked Exp or a typechecked Expr. */
    private final Map<String,Object> facts = new LinkedHashMap<String,Object>();

    /** The list of (CommandName,Command) pairs; NOTE: duplicate command names are allowed. */
    private final List<Pair<String,Command>> commands = new ArrayList<Pair<String,Command>>();

    /** Returns a short description for the Module. */
    @Override public String toString() {
        String answer=null;
        for(String x:paths) { if (answer==null) answer="module{"+x; else answer=answer+", "+x; }
        return answer+"}";
    }

    //============================================================================================================================//

    /** Helper method for getRawSIG; it looks up non-fully-qualified SigAST from the current module; it skips PARAMs. */
    private SigAST getRawNQSIG (Pos pos, String name) throws Err {
        SigAST x=sigs.get(name);
        for(Map.Entry<String,Open> i:opens.entrySet()) {
            SigAST y=i.getValue().realModule.getRawNQSIG(pos, name);
            if (y==null || x==y) continue;
            if (x==null) x=y; else throw new ErrorSyntax(pos, "The signature name \""+name+"\" is ambiguous.");
        }
        return x;
    }

    /** Helper method for getRawSIG; it looks up fully-qualified SigAST from the current module; it skips PARAMs. */
    private SigAST getRawQSIG (Pos pos, String name) { // It ignores "params"
        Module u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) return u.sigs.get(name);
            String alias=name.substring(0,i);
            Open uu=u.opens.get(alias);
            if (uu==null) return null;
            u=uu.realModule;
            name=name.substring(i+1,name.length());
        }
    }

    /** Looks up a SigAST from the current module (and it will also search this.params) */
    private SigAST getRawSIG (Pos pos, String name) throws Err {
        SigAST s=null, s2=null;
        if (name.equals("seq/Int")) return SEQIDXast;
        if (name.equals("Int"))     return SIGINTast;
        if (name.equals("univ"))    return UNIVast;
        if (name.equals("none"))    return NONEast;
        if (name.indexOf('/')<0) {
            s2=params.get(name);
            s=getRawNQSIG(pos, name);
        } else {
            if (name.startsWith("this/")) { name=name.substring(5); s2=params.get(name); }
            s=getRawQSIG(pos, name);
        }
        if (s!=null && s2!=null && s!=s2) throw new ErrorSyntax(pos, "The signature name \""+name+"\" is ambiguous.");
        return (s!=null) ? s : s2;
    }

    //============================================================================================================================//

    void addModelLine(Pos pos, String moduleName, List<ExpName> list) throws Err {
        if (this.pos!=Pos.UNKNOWN) throw new ErrorSyntax(pos,"The \"module\" declaration can not occur more than once in the file.");
        this.moduleName=moduleName;
        this.pos=pos;
        for(ExpName expr:list) {
            String name=expr.name;
            if (name.length()==0)          throw new ErrorSyntax(expr.span(), "Parameter name cannot be empty");
            if (name.indexOf('/')>=0)      throw new ErrorSyntax(expr.span(), "Parameter name cannot contain '/'");
            if (name.indexOf('@')>=0)      throw new ErrorSyntax(expr.span(), "Parameter name cannot contain '@'");
            if (name.equals("univ"))       throw new ErrorSyntax(expr.span(), "\'univ\' is a builtin keyword.");
            if (name.equals("Int"))        throw new ErrorSyntax(expr.span(), "\'Int\' is a builtin keyword.");
            if (name.equals("none"))       throw new ErrorSyntax(expr.span(), "\'none\' is a builtin keyword.");
            if (params.containsKey(name) || sigs.containsKey(name))
                throw new ErrorSyntax(expr.span(), "Parameter name cannot be the same as another sig or param in the same module.");
            if (funcs.containsKey(name))
                throw new ErrorSyntax(expr.span(), "Parameter name cannot be the same as a function/predicate in the same module.");
            if (facts.containsKey(name))
                throw new ErrorSyntax(expr.span(), "Parameter name cannot be the same as a fact in the same module.");
            if (asserts.containsKey(name))
                throw new ErrorSyntax(expr.span(), "Parameter name cannot be the same as a function/predicate in the same module.");
            if (path.length()==0) addSig(null, expr.span(), name, null, null, null, null, null, null, null, null, null);
            else params.put(name, null);
        }
    }

    void addOpen(Pos pos, ExpName filename, List<ExpName> args, ExpName alias) throws Err {
        String name=filename.name, as=(alias==null ? "" : alias.name);
        if (name.length()==0) throw new ErrorSyntax(filename.span(), "The filename cannot be empty.");
        if (as.indexOf('@')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'@\' character");
        if (as.indexOf('/')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'/\' character");
        if (as.length()==0) {
            if (args!=null && args.size()!=0)
                throw new ErrorSyntax(pos,
                "The module being imported has parameters, so you must supply an alias using the AS keyword.");
            for(int i=0; i<name.length(); i++) {
                char c=name.charAt(i);
                if ((c>='a' && c<='z') || (c>='A' && c<='Z')) continue;
                if (i==0) throw new ErrorSyntax(pos, "This filename does not start with a-z or A-Z,\n"
                   + "so you must supply an alias using the AS keyword.");
                if (!(c>='0' && c<='9') && c!='_' && c!='\'' && c!='\"') throw new ErrorSyntax(pos, "Filename contains \'"
                   + c + "\' which is illegal in an alias,\n" + "so you must supply an alias using the AS keyword.");
            }
            as=name;
        }
        final TempList<String> newlist = new TempList<String>(args==null ? 0 : args.size());
        if (args!=null) for(int i=0; i<args.size(); i++) {
            ExpName arg=args.get(i);
            if (arg.name.length()==0)      throw new ErrorSyntax(arg.span(), "Argument cannot be empty.");
            if (arg.name.indexOf('@')>=0)  throw new ErrorSyntax(arg.span(), "Argument cannot contain the \'@\' chracter.");
            newlist.add(arg.name);
        }
        Open y=opens.get(as), x=new Open(pos, as, newlist.makeConst(), name);
        if (y==null) { opens.put(as,x); return; }
        if (x.args.equals(y.args) && x.filename.equals(y.filename)) return; // we allow this, especially because of util/sequniv
        throw new ErrorSyntax(pos, "You cannot import two different modules using the same alias.");
    }

    /** Every param in every module will now point to a nonnull SigAST. */
    private static void resolveParams(Module root) throws Err {
      while(true) {
         boolean chg=false;
         Open missing=null;
         String missingName="";
         for(Module mod:root.modules) for(Map.Entry<String,Open> entry:mod.opens.entrySet()) {
            Open open=entry.getValue();
            Module sub=open.realModule;
            int i=0;
            for(Map.Entry<String,SigAST> p:sub.params.entrySet()) {
               SigAST old=p.getValue();
               String kn=p.getKey(), vn=open.args.get(i);
               i++;
               SigAST vv=mod.getRawSIG(open.pos, vn); //TODO
               if (vv==null) {if (old==null) {missing=open; missingName=vn;} continue;}
               if (old==vv) continue;
               if (old!=null) throw new ErrorFatal(open.pos, "Internal error (module re-instantiated with different argument)");
               if (vv==Module.NONEast) throw new ErrorSyntax(open.pos, "You cannot use \"none\" as an instantiating argument.");
               chg=true;
               p.setValue(vv);
               if (kn.equals("elem"))
                  if (sub.pos.filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als"))
                     vv.isOrdered = open.pos; // This detects for the Alloy3 behavior of util/ordering
               A4Reporter.getReporter().parse("RESOLVE: "+(sub.path.length()==0?"this/":sub.path)+"/"+kn+" := "+vv+"\n");
            }
         }
         if (chg==false && missing==null) return;
         if (chg==false) throw new ErrorSyntax(missing.pos, "The signature name \""+missingName+"\" cannot be found.");
      }
    }

    //============================================================================================================================//

    SigAST addSig(List<ExpName> hints, Pos pos, String name,
    Pos isAbstract, Pos isLone, Pos isOne, Pos isSome, Pos subSet,
    List<String> in, String extend, List<Decl> fields, Exp fact) throws Err {
        String full=(path.length()==0) ? "this/"+name : path+"/"+name;
        boolean s=(in!=null && in.size()>0);
        if (s && subSet==null) subSet=Pos.UNKNOWN;
        SigAST obj=new SigAST(pos, full, name, isAbstract,isLone,isOne,isSome, subSet, s?in:Util.asList(extend), fields,fact,this,null);
        if (hints!=null) for(ExpName hint:hints) if (hint.name.equals("leaf")) {obj.hint_isLeaf=true; break;}
        if (params.containsKey(name) || sigs.containsKey(name))
            throw new ErrorSyntax(pos, "Sig name \""+name+"\" cannot be the same as another sig or param in the same module.");
        if (funcs.containsKey(name))
            throw new ErrorSyntax(pos, "Sig name \""+name+"\" cannot be the same as a function/predicate in the same module.");
        if (facts.containsKey(name))
            throw new ErrorSyntax(pos, "Sig name \""+name+"\" cannot be the same as a fact in the same module.");
        if (asserts.containsKey(name))
            throw new ErrorSyntax(pos, "Sig name \""+name+"\" cannot be the same as a function/predicate in the same module.");
        sigs.put(name, obj);
        return obj;
    }

    /** The given SigAST will now point to a nonnull Sig. */
    private static Sig resolveSig(SigAST oldS) throws Err {
        if (oldS.realSig!=null) return oldS.realSig;
        if (oldS.topo) throw new ErrorType("Sig "+oldS+" is involved in a cyclic inheritance.");
        oldS.topo=true;
        final Pos pos = oldS.pos;
        final Module u = oldS.realModule;
        final String name = oldS.name;
        final String fullname = u.paths.contains("") ? "this/"+name : (u.paths.get(0)+"/"+name);
        final Sig s;
        if (oldS.subset!=null)  {
            if (oldS.abs!=null) throw new ErrorSyntax(pos, "Subset signature \""+name+"\" cannot be abstract.");
            ArrayList<Sig> parents = new ArrayList<Sig>();
            for(String n:oldS.parents) {
                SigAST parentAST = u.getRawSIG(pos, n);
                if (parentAST==null) throw new ErrorSyntax(pos, "The sig \""+n+"\" cannot be found.");
                parents.add(resolveSig(parentAST));
            }
            s = new SubsetSig(pos, parents, fullname, oldS.subset, oldS.lone, oldS.one, oldS.some, oldS.isOrdered);
        } else {
            String sup="univ";
            if (oldS.parents.size()==1) {sup=oldS.parents.get(0); if (sup==null || sup.length()==0) sup="univ";}
            SigAST parentAST = u.getRawSIG(pos, sup);
            if (parentAST==null) throw new ErrorSyntax(pos, "The sig \""+sup+"\" cannot be found.");
            Sig parent = resolveSig(parentAST);
            if (!(parent instanceof PrimSig)) throw new ErrorSyntax(pos, "Cannot extend a subset signature "+parent
               +"\".\nA signature can only extend a toplevel signature or a subsignature.");
            s = new PrimSig(pos, (PrimSig)parent, fullname, oldS.abs, oldS.lone, oldS.one, oldS.some, oldS.isOrdered, oldS.hint_isLeaf);
        }
        oldS.realSig=s;
        return s;
    }

    /** Returns an unmodifiable list of all signatures defined inside this module. */
    public SafeList<Sig> getAllSigs() {
        SafeList<Sig> x = new SafeList<Sig>(sigs.size());
        for(SigAST s:sigs.values()) x.add(s.realSig);
        return x.dup();
    }

    //============================================================================================================================//

    void addFunc(Pos p, String n, Exp f, List<Decl> d, Exp t, Exp v) throws Err {
        d=new ArrayList<Decl>(d);
        if (f!=null) d.add(0, new Decl(null, Util.asList(new ExpName(f.span(), "this")), f));
        FunAST ans = new FunAST(p, n, d, t, v);
        SafeList<FunAST> list = funcs.get(n);
        if (list==null) { list = new SafeList<FunAST>(); funcs.put(n, list); }
        list.add(ans);
    }

    private JoinableList<Err> resolveFuncDecls(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        for(SafeList<FunAST> list:funcs.values()) for(FunAST f:list) {
            /*
            if (this.params.containsKey(name)) throw new ErrorSyntax(pos,
                "Within the same file, a function/predicate cannot have the same name as a polymorphic type.");
            if (this.sigs.containsKey(name)) throw new ErrorSyntax(pos,
                "Within the same file, a function/predicate cannot have the same name as another signature.");
            */
            String fullname = (path.length()==0 ? "this/" : (path+"/")) + f.name;
            Context cx = new Context(this);
            cx.rootfun=true;
            ExpName dup = Decl.findDuplicateName(f.params);
            if (dup!=null) throw new ErrorSyntax(dup.span(), "The parameter name \""+dup.name+"\" cannot appear more than once.");
            // Each PARAMETER can refer to earlier parameter in the same function, and any SIG or FIELD visible from here.
            // Each RETURNTYPE can refer to the parameters of the same function, and any SIG or FIELD visible from here.
            TempList<ExprVar> tmpvars = new TempList<ExprVar>();
            for(Decl d:f.params) {
                Expr val = d.expr.check(cx, warns).resolve_as_set(warns);
                errors = errors.join(val.errors);
                for(ExpName n: d.names) {
                    ExprVar v=ExprVar.make(n.span(), n.name, val);
                    cx.put(n.name, v);
                    tmpvars.add(v);
                    A4Reporter.getReporter().typecheck((f.returnType==null?"pred ":"fun ")+fullname+", Param "+n.name+": "+v.type+"\n");
                }
            }
            Expr ret = null;
            if (f.returnType!=null) {
                ret = f.returnType.check(cx, warns).resolve_as_set(warns);
                errors = errors.join(ret.errors);
            }
            f.realFunc = new Func(f.pos, fullname, tmpvars.makeConst(), ret);
            A4Reporter.getReporter().typecheck(""+f.realFunc+", RETURN: "+f.realFunc.returnDecl.type+"\n");
        }
        return errors;
    }

    private JoinableList<Err> resolveFuncBodys(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        A4Reporter rep = A4Reporter.getReporter();
        for(SafeList<FunAST> list:funcs.values()) for(FunAST f:list) {
            Func ff = f.realFunc;
            Expr disj = ExprConstant.TRUE;
            Context cx=new Context(this);
            Iterator<ExprVar> vv=ff.params.iterator();
            for(Decl d:f.params) {
                List<Expr> disjvars = (d.disjoint!=null && d.names.size()>0) ? (new ArrayList<Expr>()) : null;
                for(ExpName n:d.names) {
                    ExprVar newvar=vv.next();
                    cx.put(n.name, newvar);
                    if (disjvars!=null) disjvars.add(newvar);
                }
                if (disjvars!=null) disj=disj.and(ExprBuiltin.makeDISJOINT(d.disjoint, null, disjvars));
            }
            Expr newBody = f.body.check(cx, warns);
            if (ff.isPred) newBody=newBody.resolve_as_formula(warns); else newBody=newBody.resolve_as_set(warns);
            errors = errors.join(newBody.errors);
            ff.setBody(newBody);
            for(Decl d:f.params) for(ExpName n:d.names) cx.remove(n.name);
            if (!ff.isPred && newBody.type.hasTuple() && ff.returnDecl.type.hasTuple() && !newBody.type.intersects(ff.returnDecl.type))
                warns.add(new ErrorWarning(ff.getBody().span(),
                    "Function return value is disjoint from its return type.\n"
                    +"Function body has type "+ff.getBody().type+"\nbut the return type is "+ff.returnDecl.type));
            rep.typecheck(""+ff+", BODY:"+ff.getBody().type+"\n");
            //
            if (!ff.isPred) newBody=newBody.in(ff.returnDecl);
            if (ff.params.size()>0) newBody=ExprQuant.Op.SOME.make(null, null, ff.params, newBody.and(disj));
            f.realFormula=newBody;
        }
        return errors;
    }

    public Exp getFirstFunAST() {
        return (funcs.size()==0) ? null : funcs.entrySet().iterator().next().getValue().get(0).body;
    }

    public SafeList<Func> getAllFunc() {
        SafeList<Func> ans = new SafeList<Func>();
        for(SafeList<FunAST> list: funcs.values()) {
            for(FunAST func:list) {
                ans.add(func.realFunc);
            }
        }
        return ans.dup();
    }

    //============================================================================================================================//

    String addAssertion(Pos pos, String name, Exp value) throws Err {
        if (name==null || name.length()==0) name="assert#"+(1+asserts.size());
        if (name.indexOf('/')>=0) throw new ErrorSyntax(pos, "Assertion name \""+name+"\" cannot contain \'/\'");
        if (name.indexOf('@')>=0) throw new ErrorSyntax(pos, "Asserition name \""+name+"\" cannot contain \'@\'");
        if (asserts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same module, two assertions cannot have the same name.");
        asserts.put(name, value);
        return name;
    }

    private JoinableList<Err> resolveAssertions(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        Context cx = new Context(this);
        for(Map.Entry<String,Object> e:asserts.entrySet()) {
            Object x = e.getValue();
            Expr expr = null;
            if (x instanceof Expr) expr=(Expr)x;
            if (x instanceof Exp) {
                expr=((Exp)x).check(cx, warns).resolve_as_formula(warns);
                if (expr.errors.isEmpty()) e.setValue(expr);
            }
            if (expr.errors.size()>0) errors=errors.join(expr.errors);
            else A4Reporter.getReporter().typecheck("Assertion " + e.getKey() + ": " + expr.type+"\n");
        }
        return errors;
    }

    /** If typecheck was successful, return an unmodifiable list of all facts in this module. */
    public ConstList<Pair<String,Expr>> getAllAssertions() {
        TempList<Pair<String,Expr>> ans = new TempList<Pair<String,Expr>>(asserts.size());
        for(Map.Entry<String,Object> e:asserts.entrySet()) {
            String a=e.getKey();
            Object b=e.getValue();
            if (b instanceof Expr) ans.add(new Pair<String,Expr>(a, (Expr)b));
        }
        return ans.makeConst();
    }

    /** If typecheck was successful, return the assertion with that name (it returns null if there's no match) */
    public Expr getAssertion(String name) {
        Object ans = asserts.get(name);
        return (ans instanceof Expr) ? ((Expr)ans) : null;
    }

    //============================================================================================================================//

    void addFact(Pos pos, String name, Exp value) throws Err {
        if (name==null || name.length()==0) name="fact#"+(1+facts.size());
        if (facts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same file, a fact cannot have the same name as another fact.");
        facts.put(name,value);
    }

    void addFact(Pos pos, String name, Expr value) throws Err { // It must be unambiguous formula
        if (!value.errors.isEmpty()) throw value.errors.get(0);
        if (name==null || name.length()==0) name="fact#"+(1+facts.size());
        if (facts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same file, a fact cannot have the same name as another fact.");
        facts.put(name,value);
        A4Reporter.getReporter().typecheck("Fact " + name + ": " + value.type+"\n");
    }

    private JoinableList<Err> resolveFacts(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        Context cx = new Context(this);
        for(Map.Entry<String,Object> e:facts.entrySet()) {
            Object x = e.getValue();
            Expr expr = null;
            if (x instanceof Expr) expr=(Expr)x;
            if (x instanceof Exp) {
                expr=((Exp)x).check(cx, warns).resolve_as_formula(warns);
                if (expr.errors.isEmpty()) e.setValue(expr);
            }
            if (expr.errors.size()>0) errors=errors.join(expr.errors);
            else A4Reporter.getReporter().typecheck("Fact " + e.getKey() + ": " + expr.type+"\n");
        }
        for(Map.Entry<String,SigAST> e:sigs.entrySet()) {
            Sig s=e.getValue().realSig;
            if (s.getOrderingTarget()!=null) continue;
            Exp f=e.getValue().appendedFact;
            if (f==null) continue;
            ExprVar THIS = s.oneOf("this");
            cx.rootsig=s;
            cx.put("this", THIS);
            Expr formula = f.check(cx, warns).resolve_as_formula(warns);
            cx.remove("this");
            formula = formula.forAll(THIS);
            if (formula.errors.size()>0) errors=errors.join(formula.errors);
            else {
                facts.put(""+s+"#fact", formula);
                A4Reporter.getReporter().typecheck("Fact "+s+"#fact: " + formula.type+"\n");
            }
        }
        return errors;
    }

    /** If typecheck was successful, return an unmodifiable list of all facts in this module. */
    public SafeList<Pair<String,Expr>> getAllFacts() {
        SafeList<Pair<String,Expr>> ans = new SafeList<Pair<String,Expr>>();
        for(Map.Entry<String,Object> e:facts.entrySet()) {
            String a=e.getKey();
            Object b=e.getValue();
            if (b instanceof Expr) ans.add(new Pair<String,Expr>(a, (Expr)b));
        }
        return ans.dup();
    }

    //============================================================================================================================//

    void addCommand(Pos p,String n,boolean c,int o,int b,int seq,int exp,Map<String,Integer> s, String label) throws Err {
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s)));
    }

    void addCommand(Pos p,Exp e,boolean c,int o,int b,int seq,int exp,Map<String,Integer> s, String label) throws Err {
        String n;
        if (c) n=addAssertion(p,"",e); else addFunc(e.span(),n="run#"+(1+commands.size()),null,new ArrayList<Decl>(),null,e);
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s)));
    }

    private Set<Expr> lookupAssertion(String name) {
        // Look up an assertion visible from this module.
        // The name can either be fully-qualified (eg. "alias/alias/somename") or not
        Expr s;
        Set<Expr> ans=new LinkedHashSet<Expr>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) {
                for(Module u:world.lookupModuleAndSubmodules(p)) {
                    if ((s=u.getAssertion(name))!=null) ans.add(s);
                }
            }
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.path2module.get((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            if ((s=u.getAssertion(name))!=null) ans.add(s);
        }
        return ans;
    }

    private SafeList<FunAST> lookupFunctionOrPredicate(String name) {
        SafeList<FunAST> ans=new SafeList<FunAST>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) for(Module u:world.lookupModuleAndSubmodules(p)) {
                SafeList<FunAST> list=u.funcs.get(name);
                if (list!=null) ans.addAll(list);
            }
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.path2module.get((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            SafeList<FunAST> list=u.funcs.get(name);
            if (list!=null) ans.addAll(list);
        }
        return ans;
    }

    private JoinableList<Err> resolveCommands(JoinableList<Err> errors, List<ErrorWarning> warnings) throws Err {
        for(int i=0; i<commands.size(); i++) {
            String cname = commands.get(i).a;
            Command cmd = commands.get(i).b;
            Expr e=null;
            if (cmd.check) {
                e=getAssertion(cname);
                if (e==null) {
                    Set<Expr> ee=lookupAssertion(cname);
                    if (ee.size()>1) throw new ErrorSyntax(cmd.pos, "There are more than 1 assertion with the name \""+cname+"\".");
                    if (ee.size()<1) throw new ErrorSyntax(cmd.pos, "The assertion \""+cname+"\" cannot be found.");
                    e=ee.iterator().next();
                }
            } else {
                SafeList<FunAST> ee=null;
                if (cname.indexOf('/')<0) ee=lookupFunctionOrPredicate("this/"+cname);
                if (ee==null || ee.size()<1) ee=lookupFunctionOrPredicate(cname);
                if (ee.size()<1) throw new ErrorSyntax(cmd.pos, "The predicate/function \""+cname+"\" cannot be found.");
                if (ee.size()>1) throw new ErrorSyntax(cmd.pos, "There are more than 1 predicate/function with the name \""+cname+"\".");
                e=ee.get(0).realFormula;
            }
            commands.set(i, new Pair<String,Command>(cname, cmd.changeFormula(e)));
        }
        return errors;
    }

    /** Return an unmodifiable list of commands in this module. */
    public SafeList<Command> getAllCommands() {
        SafeList<Command> ans = new SafeList<Command>(commands.size());
        for(Pair<String,Command> c:commands) ans.add(c.b);
        return ans.dup();
    }

    //============================================================================================================================//

    /** Returns true if exists some entry (a,b) in the map, such that b==value (using object identity as the comparison) */
    private static<V> boolean isin(V value, Map<String,V> map) {
        for(Map.Entry<String,V> e:map.entrySet()) if (e.getValue()==value) return true;
        return false;
    }

    //============================================================================================================================//

    /** This is step 2 of the postprocessing: merging modules that have same filename and instantiating arguments. */
    private static boolean alloy_mergeModules(Module root) {
       // Before merging, the only pointers that go between Module objects are
       // (1) a module's "params" may point to a sig in another module
       // (2) a module's "imports" may point to another module
       // So when we find that two modules A and B should be merged,
       // we go through every module and replace "pointers into B" with equivalent "pointers into A".
       boolean chg=false;
       List<Module> modules=root.modules;
       for(int i=0; i<modules.size(); i++) {
          Module a=modules.get(i);
          for(int j=i+1; j<modules.size(); j++) {
             Module b=modules.get(j);
             if (!a.pos.filename.equals(b.pos.filename) || !a.params.equals(b.params)) continue;
             chg=true;
             A4Reporter.getReporter().parse("MATCH FOUND ON "+a.pos.filename+"\n");
             if (i!=0 && Util.slashComparator.compare(a.path, b.path)>0) { a=b; b=modules.get(i); modules.set(i,a); }
             modules.remove(j);
             j--;
             for(String c:b.paths) root.path2module.put(c,a); // This ensures root.modules and root.path2module are consistent
             a.paths.addAll(b.paths);
             Collections.sort(a.paths, Util.slashComparator);
             for(Module c:modules) {
                for(Map.Entry<String,SigAST> p:c.params.entrySet()) if (isin(p.getValue(), b.sigs)) p.setValue(a.sigs.get(p.getValue().name));
                for(Map.Entry<String,Open> p:c.opens.entrySet()) if (p.getValue().realModule==b) p.getValue().realModule=a;
             }
          }
       }
       return chg;
    }

    //============================================================================================================================//

    /** This is step 3 of the postprocessing: converting from "Exp" to "Expr" */
    static Module resolveAll(final Module root) throws Err {
        resolveParams(root);
        while(alloy_mergeModules(root)) {}
        JoinableList<Err> errors = new JoinableList<Err>();
        final List<ErrorWarning> warns = new ArrayList<ErrorWarning>();
        final A4Reporter rep = A4Reporter.getReporter();
        final List<Module> modules = root.modules;
        // Resolves SigAST -> Sig
        for(Module m:modules) for(Map.Entry<String,SigAST> e:m.sigs.entrySet()) Module.resolveSig(e.getValue());
        // Label any Sig that are used in util/ordering
        for(Module m:modules) {
           SigAST elemX=m.params.get("elem");                 if (elemX==null) continue;
           Sig elem=elemX.realSig;                            if (elem.builtin || m.sigs.size()!=1) continue;
           Sig ord=m.sigs.values().iterator().next().realSig; if (ord.builtin  || !ord.label.endsWith("/Ord")) continue;
           if (!ord.pos.filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als")) continue;
           ord.addOrderfields(null, elem);
        }
        // Add the fields
        for(Module m:modules) for(SigAST oldS:m.sigs.values()) {
           // When typechecking each field:
           // * it is allowed to refer to earlier fields in the same SIG or in any visible ancestor sig
           // * it is allowed to refer to visible sigs
           // * it is NOT allowed to refer to any predicate or function
           // For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
           // then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
           final Sig s=oldS.realSig;
           final Context cx=new Context(m);
           final ExpName dup=Decl.findDuplicateName(oldS.fields);
           if (dup!=null) throw new ErrorSyntax(dup.span(), "sig \""+s+"\" cannot have 2 fields named \""+dup.name+"\"");
           for(final Decl d:oldS.fields) {
              // The name "this" does matter, since the parser and the typechecker both refer to it as "this"
              final ExprVar THIS = s.oneOf("this");
              cx.rootfield=true;
              cx.rootsig=s;
              cx.put("this", THIS);
              Expr bound = d.expr.check(cx, warns).resolve_as_set(warns), disjA=null, disjF=ExprConstant.TRUE;
              cx.remove("this");
              for(final ExpName n:d.names) {
                 final Field f=s.addTrickyField(d.span(), n.name, THIS, bound);
                 rep.typecheck("Sig "+s+", Field "+f.label+": "+f.type+"\n");
                 if (d.disjoint==null) continue;
                 if (disjA==null) disjA=f; else disjF=ExprBinary.Op.AND.make(d.disjoint, null, disjA.intersect(f).no(), disjF);
                 disjA=disjA.plus(f);
              }
              if (d.disjoint!=null && disjF!=ExprConstant.TRUE) m.addFact(Pos.UNKNOWN, ""+s+"#disjoint", disjF);
          }
        }
        // The Alloy language forbids two overlapping sigs from having fields with the same name.
        // In other words: if 2 fields have the same name, then their type's first column must not intersect.
        final Map<String,List<Field>> fieldname2fields=new LinkedHashMap<String,List<Field>>();
        for(Module m:modules) {
          for(Map.Entry<String,SigAST> sig: m.sigs.entrySet()) {
            for(Field field: sig.getValue().realSig.getFields()) {
               List<Field> peers=fieldname2fields.get(field.label);
               if (peers==null) { peers=new ArrayList<Field>(); fieldname2fields.put(field.label, peers); }
               for(Field field2: peers)
                  if (field.type.firstColumnOverlaps(field2.type))
                     throw new ErrorType(field.pos,
                     "Two overlapping signatures cannot have\ntwo fields with the same name \""+field.label
                     +"\":\n\n1) one is in sig \""+field.sig+"\"\n"+field.pos
                     +"\n\n2) the other is in sig \""+field2.sig+"\"\n"+field2.pos);
               peers.add(field);
            }
          }
        }
        // Typecheck the function declarations and function bodies
        for(Module x:modules) errors=x.resolveFuncDecls(errors, warns);
        for(Module x:modules) errors=x.resolveFuncBodys(errors, warns);
        // Typecheck the assertions and facts
        for(Module x:modules) { errors=x.resolveAssertions(errors,warns); errors=x.resolveFacts(errors,warns); }
        // Typecheck the run/check commands
        errors=root.resolveCommands(errors, warns);
        // Issue the warnings, and generate the errors if there are any
        for(ErrorWarning w:warns) rep.warning(w);
        if (!errors.isEmpty()) throw errors.get(0); else return root;
    }

    //============================================================================================================================//

    private static final SigAST UNIVast   = new SigAST(Pos.UNKNOWN, "univ",    "univ", null,null,null,null,null, new ArrayList<String>(), new ArrayList<Decl>(), null, null, UNIV);
    private static final SigAST SIGINTast = new SigAST(Pos.UNKNOWN, "Int",     "Int",  null,null,null,null,null, Util.asList("univ"),     new ArrayList<Decl>(), null, null, SIGINT);
    private static final SigAST SEQIDXast = new SigAST(Pos.UNKNOWN, "seq/Int", "Int",  null,null,null,null,null, Util.asList("Int"),      new ArrayList<Decl>(), null, null, SEQIDX);
    private static final SigAST NONEast   = new SigAST(Pos.UNKNOWN, "none",    "none", null,null,null,null,null, new ArrayList<String>(), new ArrayList<Decl>(), null, null, NONE);

    /**
     * Look up a parameter, or a signature/function/predicate visible from this module.
     * @param name - the name which can either be fully-qualified (eg. "alias/alias/somename") or not
     * @param look_for_function_and_predicate - true if we want to include functions and predicates as well
     * @return an empty set if there is no match
     */
    public Set<Object> lookupSigOrParameterOrFunctionOrPredicate (String name, boolean look_for_function_and_predicate) {
        SigAST s;
        Set<Object> ans=new LinkedHashSet<Object>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) {
                for(Module u:world.lookupModuleAndSubmodules(p)) {
                    s=u.sigs.get(name); if (s!=null) ans.add(s.realSig);
                    SafeList<FunAST> list = look_for_function_and_predicate ? u.funcs.get(name) : null;
                    if (list!=null) for(FunAST f:list) ans.add(f.realFunc);
                }
            }
            s=params.get(name); if (s!=null) ans.add(s.realSig);
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        s=params.get(name); if (s!=null) ans.add(s.realSig);
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.path2module.get((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            s=u.sigs.get(name); if (s!=null) ans.add(s.realSig);
            SafeList<FunAST> list = look_for_function_and_predicate ? u.funcs.get(name) : null;
            if (list!=null) for(FunAST f:list) ans.add(f.realFunc);
        }
        return ans;
    }

    //============================================================================================================================//

    /** Returns true if this module can see that module. */
    private static boolean canSee(String a, String b) {
        // We assume "a" and "b" are the canonical minimum fullname for sig a and sig b, respectively.
        if (a.startsWith("this/")) a=a.substring(5);
        if (b.startsWith("this/")) b=b.substring(5);
        int ai=a.lastIndexOf('/');
        int bi=b.lastIndexOf('/');
        if (ai<0) return true; // That means a is in the root module
        if (bi<0) return false; // That means a is not in the root module, but b is in the root module
        a = a.substring(0, ai);
        b = b.substring(0, bi);
        return (b.startsWith(a+"/") || b.equals(a));
    }

    /**
     * Look up a visible ancestor field (and returns null if there is no match).
     *
     * @param origin - the sig where we started the search
     * @param current - the sig we're currently looking (we will recursively call this method on parent sig(s))
     * @param name - the name of the field
     *
     * @throws ErrorSyntax if "current" and at least one of "current"'s parent sig(s) both have a field named "name"
     *
     * <p> Note: this won't catch other field conflicts (such as two overlapping sibling sigs that both
     * have a field with the same name...) Thus, World.typecheck() will do a complete intersection of all the fields
     * to look for conflicts as well. Thus, the conflict check in this method isn't necessary.
     * But, by having the check here, we can report an error sooner than later (so that the error message
     * will be less confusing to the user hopefully)
     */
    private Field lookupField(Sig origin, Sig current, String name) {
        Field ans=null;
        if (!origin.builtin && !current.builtin && canSee(origin.label, current.label))
            for(Field f:current.getFields())
                if (f.label.equals(name))
                    {ans=f; break;}
        if (current instanceof PrimSig) {
            PrimSig s=(PrimSig)current;
            if (s.parent==null || s.parent.builtin) return ans;
            Field ans2=lookupField(origin, s.parent, name);
            if (ans==null) return ans2;
        }
        if (current instanceof SubsetSig) for(Sig p:((SubsetSig)current).parents) {
            Field ans2=lookupField(origin, p, name);
            if (ans==null) ans=ans2;
        }
        return ans;
    }

    /** Look up field "name" from any visible sig (and returns an empty set if there is no match) */
    private Set<Field> lookupField(String name) {
        Set<Field> ans=new LinkedHashSet<Field>();
        for(String p:paths)
          for(Module u:world.lookupModuleAndSubmodules(p))
            for(SigAST s:u.sigs.values())
              for(Field f:s.realSig.getFields())
                if (f.label.equals(name))
                  ans.add(f);
        return ans;
    }

    //============================================================================================================================//

    /** Resolve the name based on the current context and this module. */
    public Set<Object> populate(boolean rootfield, Sig rootsig, boolean rootfun, Pos pos, String fullname, Expr THIS) {
        // Return object can be FuncN or Expr
        Set<Object> ans;
        final String name = (fullname.charAt(0)=='@') ? fullname.substring(1) : fullname;
        if (rootfield) {
            // Within a field decl
            // You cannot call function or predicates
            // And You can refer to field in this sig (defined earlier than you) and fields in a visible ancestor sig
            ans=lookupSigOrParameterOrFunctionOrPredicate(name, false);
            Field f=lookupField(rootsig, rootsig, name);
            if (f!=null) { Expr ff=ExprUnary.Op.NOOP.make(pos,f); if (fullname.charAt(0)=='@') ans.add(ff); else ans.add(THIS.join(ff)); }
        }
        else if (rootsig!=null) {
            // Within an appended facts, you can refer to any visible sig/param/func/predicate
            ans=lookupSigOrParameterOrFunctionOrPredicate(name, true);
            Field f=lookupField(rootsig, rootsig, name);
            if (f!=null) { Expr ff=ExprUnary.Op.NOOP.make(pos,f); if (fullname.charAt(0)=='@') ans.add(ff); else ans.add(THIS.join(ff)); }
            for(Field ff:lookupField(name)) if (f!=ff) ans.add(ExprUnary.Op.NOOP.make(pos, ff, ff.weight+1, null));
        }
        else {
            // If Within a function paramDecl/returnDecl
            //    we cannot call, but can refer to anything else visible.
            // Else
            //    we can call, and can refer to anything visible.
            ans=lookupSigOrParameterOrFunctionOrPredicate(name, !rootfun);
            for(Field ff:lookupField(name)) ans.add(ExprUnary.Op.NOOP.make(pos,ff));
        }
        // Convert Sig/Field/Func0 into references
        Set<Object> realAns=new LinkedHashSet<Object>();
        for(Object x:ans) {
            if (x instanceof Sig || x instanceof Field)
                realAns.add(ExprUnary.Op.NOOP.make(pos, (Expr)x));
            else if (x instanceof Func && ((Func)x).params.size()==0)
                realAns.add(ExprCall.make(pos, null, (Func)x, null, 0));
            else if (x instanceof Func || x instanceof Expr)
                realAns.add(x);
        }
        return realAns;
    }
}
