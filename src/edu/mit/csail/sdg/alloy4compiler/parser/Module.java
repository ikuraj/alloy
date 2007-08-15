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

// moduleline
// pos
// world
// path
// paths
//
// sigs
// open opencmds
// params
//
// commands
// facts
// asserts
// _funs    ---VS---   funs+fun_2_formula

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.io.File;
import java.util.ArrayList;
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

    /** Mutable; this class represents an untypechecked Alloy function; equals() uses object identity. */
    private static final class FunAST {
        Func realFunc=null; // This value is set to its corresponding Func during typechecking
        final Pos pos;
        final String name;
        final ConstList<Decl> args;
        final Exp returnType;
        final Exp body;
        FunAST(Pos p, String n, List<Decl> a, Exp r, Exp b) {
            pos=p; name=n; args=ConstList.make(a); returnType=r; body=b;
        }
        @Override public String toString() { return name; }
    }

    /** Mutable; this class represents an untypechecked Alloy signature; equals() uses object identity. */
    static final class SigAST {
        boolean topo=false;     // This flag is set to "true" during topological sort
        Module realModule=null; // This value is set to its Module during topological sort
        Sig realSig=null;       // This value is set to its corresponding Sig during topological sort
        boolean hint_isLeaf=false;
        final Pos pos;
        final String name,fullname;
        final boolean abs,lone,one,some,subset;
        final ConstList<String> parents;
        final ConstList<Decl> fields;
        final Exp appendedFact;
        Pos orderingPosition, absPosition, lonePosition, onePosition, somePosition, extendsPosition, inPosition;
        SigAST(Pos pos, String fullname, String name, boolean abs, boolean lone, boolean one, boolean some, boolean subset,
            List<String> parents, List<Decl> fields, Exp appendedFacts, Sig topoSig) {
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
            this.realSig=topoSig;
        }
        @Override public String toString() { return fullname; }
    }

    /** This helper function determines whether "s" is an instance of the util/ordering "Ord" sig. */
    public static boolean is_alloy3ord(String paramname, String filename) {
        return paramname.equals("elem") && filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als");
    }

    //======== ROOT ONLY =====

    /** IF ROOT: This lists all modules in this world; it must be consistent with this.path2module */
    final ArrayList<Module> modules = new ArrayList<Module>();

    /** IF ROOT: This maps pathname to the Module it refers to; it must be consistent with this.modules */
    final Map<String,Module> path2module = new LinkedHashMap<String,Module>();

    /** Create a new module with the given list of paths. */
    public Module lookupOrCreateModule(Pos pos, String path) throws Err {
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

    /** Returns an unmodifiable list of all modules. */
    public SafeList<Module> getAllModules() { return (new SafeList<Module>(modules)).dup(); }

    /** Returns an unmodifiable list of all signatures. */
    public ConstList<Sig> all() {
        TempList<Sig> x = new TempList<Sig>();
        x.add(UNIV);
        x.add(SIGINT);
        x.add(SEQIDX);
        x.add(NONE);
        for(Module m:modules) x.addAll(m.getAllSigs());
        return x.makeConst();
    }

    //===============================================================================================================

    /** The position of the "MODULE" line at the top of the file; never null. */
    public Pos pos = Pos.UNKNOWN;

    /** The text of the "MODULE" line at the top of the file; "unknown" if the line is missing from the file. */
    String moduleName="unknown";

    /** The world that this Module belongs to. */
    public final Module world;

    /** The simplest path pointing to this Module; it is always equal to this.paths.get(0) */
    public final String path;

    /** The list of paths pointing to this Module; it is always nonempty and already sorted by Util.slashComparator */
    public final List<String> paths;

    /** Returns a short description for the Module. */
    @Override public String toString() {
        String answer=null;
        for(String x:paths) { if (answer==null) answer="module{"+x; else answer=answer+", "+x; }
        return answer+"}";
    }

    /**
     * Constructs a new Module object
     *
     * @param world - the world that this Module belongs to (it must be nonnull)
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

    final Map<String,SigAST> params=new LinkedHashMap<String,SigAST>();

    void addModelLine(Pos pos, String moduleName, List<ExpName> list) throws Err {
        this.moduleName=moduleName;
        this.pos=pos;
        for(ExpName expr:list) {
            String name=expr.name;
            //if (paramNames.contains(name)) throw new ErrorSyntax(expr.span(), "You cannot use the same name for more than 1 instantiating parameter.");
            if (path.length()==0)
                addSig(null, pos, name, false, false, false, false, null, null, new ArrayList<Decl>(), null);
            else
                params.put(name, null);
        }
    }

//    /**
//     * Add a new parameter to a module
//     *
//     * @param name - the name of the parameter
//     * @param target - the sig that this name will refer to
//     *
//     * @throws ErrorSyntax  if the module already has a parameter, sig, or function/predicate with that name
//     *
//     * @throws ErrorSyntax if this is the main module
//     * @throws ErrorSyntax if the name is "", or if the name contains '/' or '@' or '[' or ']'
//     * @throws ErrorSyntax if the module already has a signature or parameter or function/predicate with that name
//     */
//    void addParameter(String name, Sig target) throws Err {
//        Pos p = this.pos;
//        if (target==NONE) throw new ErrorSyntax(p,
//            "You cannot instantiate a module using \"none\"");
//        if (path.length()==0) throw new ErrorSyntax(p,
//            "The main module cannot have parameters.");
//        if (name.length()==0) throw new ErrorSyntax(p,
//            "Module parameter name cannot be empty.");
//        if (name.indexOf('/')>=0) throw new ErrorSyntax(p,
//            "Module parameter name \""+name+"\" cannot contain \'/\'");
//        if (name.indexOf('@')>=0) throw new ErrorSyntax(p,
//            "Module parameter name \""+name+"\" cannot contain \'@\'");
//        if (name.indexOf('[')>=0) throw new ErrorSyntax(p,
//            "Module parameter name \""+name+"\" cannot contain \'[\'");
//        if (name.indexOf(']')>=0) throw new ErrorSyntax(p,
//            "Module parameter name \""+name+"\" cannot contain \']\'");
//        if (params.containsKey(name)) throw new ErrorSyntax(p,
//            "A module cannot have two parameters with the same name: \""+name+"\"");
//        if (sigs.containsKey(name)) throw new ErrorSyntax(p,
//            "A module cannot have a parameter and a signature with the same name: \""+name+"\"");
//        if (funs.containsKey(name)) throw new ErrorSyntax(p,
//            "A module cannot have a parameter and a function/predicate with the same name: \""+name+"\"");
//        this.params.put(name, target);
//    }

    //=============================================================================================================//

    final Map<String,Module> opens=new LinkedHashMap<String,Module>();

    final Map<String,Open> opencmds=new LinkedHashMap<String,Open>();

    void addOpen(Pos pos, String name, List<ExpName> args, String alias) throws Err {
        Open x=new Open(pos, alias, args, name);
        Open y=opencmds.get(x.alias);
        // Special case, especially needed for auto-import of "util/sequniv"
        if (y!=null && x.alias.equals(y.alias) && x.args.equals(y.args) && x.filename.equals(y.filename)) return;
        if (opencmds.containsKey(x.alias))
            throw new ErrorSyntax(pos, "You cannot import more than 1 module using the same alias.");
        opencmds.put(x.alias, x);
    }

    //=============================================================================================================//

    /**
     * This maps each signature declared in this file to its Sig object.
     * <p> Each Sig object must be originally and solely declared within this module.
     * <p> Each name cannot be empty, and cannot have "/" or "@".
     */
    final Map<String,SigAST> sigs = new LinkedHashMap<String,SigAST>();

    /** Returns a unmodifiable list of Sig objects declared originally and solely within this module. */
    SafeList<Sig> getAllSigs() {
        SafeList<Sig> ans=new SafeList<Sig>(sigs.size());
        for(Map.Entry<String,SigAST> e:sigs.entrySet()) ans.add(e.getValue().realSig);
        return ans.dup();
    }

    SigAST addSig(List<ExpName> hints, Pos p,String n,
        boolean fa,boolean fl,boolean fo,boolean fs,List<String> i,String e,List<Decl> d,Exp f) throws Err {
        SigAST obj;
        String fullname = (path.length()==0) ? "this/"+n : path+"/"+n;
        if (i!=null && i.size()>0)
            obj=new SigAST(p, fullname, n, fa, fl, fo, fs, true, i, d, f, null);
        else
            obj=new SigAST(p, fullname, n, fa, fl, fo, fs, false, Util.asList(e), d, f, null);
        if (hints!=null) for(ExpName hint:hints) {
           if (hint.name.equals("leaf")) {obj.hint_isLeaf=true; break;}
        }
        if (sigs.containsKey(n)) throw new ErrorSyntax(p, "sig \""+n+"\" is already declared in this module.");
        sigs.put(n,obj);
        return obj;
    }

    static Sig checkSig(SigAST oldS) throws Err {
        if (oldS.realSig!=null) return oldS.realSig;
        if (oldS.topo) throw new ErrorType("Sig "+oldS+" is involved in a cyclic inheritance.");
        oldS.topo=true;
        final Pos pos=oldS.pos;
        final Module u=oldS.realModule;
        final String name=oldS.name;
        if (u.params.containsKey(name)) throw new ErrorSyntax(pos, "A module cannot have a signature and a parameter with the same name: \""+name+"\"");
        if (u.funs.containsKey(name)) throw new ErrorSyntax(pos, "A module cannot have a signature and a function/predicate with the same name: \""+name+"\"");
        final String fullname = u.paths.contains("") ? "this/"+name : (u.paths.get(0)+"/"+name);
        final Sig s;
        if (oldS.subset)  {
            if (oldS.abs) throw new ErrorSyntax(pos, "Subset signature \""+name+"\" cannot be abstract.");
            ArrayList<Sig> parents = new ArrayList<Sig>();
            for(String n:oldS.parents) {
                Sig parent;
                if (n.equals("univ")) parent=UNIV;
                else if (n.equals("Int")) parent=SIGINT;
                else if (n.equals("seq/Int")) parent=SEQIDX;
                else if (n.equals("none")) parent=NONE;
                else {
                  Set<SigAST> anss=u._lookup_sigORparam(n);
                  if (anss.size()>1) throw new ErrorSyntax(pos, "Sig "+oldS+" tries to be a subset of \""+n+"\", but the name \""+n+"\" is ambiguous.");
                  if (anss.size()<1) throw new ErrorSyntax(pos, "Sig "+oldS+" tries to be a subset of a non-existent signature \""+n+"\"");
                  parent = checkSig(anss.iterator().next());
                }
                parents.add(parent);
            }
            s = new SubsetSig(pos, parents, fullname, oldS.lone, oldS.one, oldS.some);
        } else {
            Sig parent;
            String sup="univ";
            if (oldS.parents.size()==1) {sup=oldS.parents.get(0); if (sup==null || sup.length()==0) sup="univ";}
            if (sup.equals("univ")) parent=UNIV;
            else if (sup.equals("Int")) parent=SIGINT;
            else if (sup.equals("seq/Int")) parent=SEQIDX;
            else if (sup.equals("none")) parent=NONE;
            else {
                Set<SigAST> anss=u._lookup_sigORparam(sup);
                if (anss.size()>1) throw new ErrorSyntax(pos, "Sig "+oldS+" tries to extend \""+sup+"\", but that name is ambiguous.");
                if (anss.size()<1) throw new ErrorSyntax(pos, "Sig "+oldS+" tries to extend a non-existent signature \""+sup+"\"");
                parent = checkSig(anss.iterator().next());
            }
            if (!(parent instanceof PrimSig)) throw new ErrorSyntax(pos, "Sig "+oldS+" cannot extend a subset signature "+parent+"\".\nA signature can only extend a toplevel signature or a subsignature.");
            s = new PrimSig(pos, (PrimSig)parent, fullname, oldS.abs, oldS.lone, oldS.one, oldS.some, oldS.hint_isLeaf);
        }
        oldS.realSig=s;
        if (oldS.absPosition!=null) s.anno.put("abstract", oldS.absPosition);
        if (oldS.lonePosition!=null) s.anno.put("lone", oldS.lonePosition);
        if (oldS.onePosition!=null) s.anno.put("one", oldS.onePosition);
        if (oldS.somePosition!=null) s.anno.put("some", oldS.somePosition);
        if (oldS.extendsPosition!=null) s.anno.put("extends", oldS.extendsPosition);
        if (oldS.inPosition!=null) s.anno.put("in", oldS.inPosition);
        if (oldS.orderingPosition!=null) s.anno.put("ordering", oldS.orderingPosition);
        return s;
    }

    //=============================================================================================================//

    /** Caches an unmodifiable empty list of functions. */
    private static final SafeList<Func> empty_list_of_func = (new SafeList<Func>(0)).dup();

    private final List<FunAST> _funs = new ArrayList<FunAST>();
    private final Map<String,SafeList<Func>> funs = new LinkedHashMap<String,SafeList<Func>>();
    private static final Map<Func,Expr> fun_2_formula = new LinkedHashMap<Func,Expr>(); // TODO: static since Command lookup may cross Module boundaries

    void addFunc(Pos p, String n, Exp f, List<Decl> d, Exp t, Exp v) throws Err {
        d=new ArrayList<Decl>(d);
        if (f!=null) d.add(0, new Decl(null, Util.asList(new ExpName(f.span(), "this")), f));
        _funs.add(new FunAST(p, n, d, t, v));
    }

    JoinableList<Err> checkFunctionDecls(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        for(FunAST f: _funs) {
            /*
            if (this.params.containsKey(name)) throw new ErrorSyntax(pos,
                "Within the same file, a function/predicate cannot have the same name as a polymorphic type.");
            if (this.sigs.containsKey(name)) throw new ErrorSyntax(pos,
                "Within the same file, a function/predicate cannot have the same name as another signature.");
            */
            String fullname = (path.length()==0 ? "this/" : (path+"/")) + f.name;
            Context cx = new Context(this);
            cx.rootfun=true;
            String dup = f.args==null ? null : Decl.findDuplicateName(f.args);
            if (dup!=null) throw new ErrorSyntax(f.pos, "The parameter name \""+dup+"\" cannot appear more than once in this predicate/function declaration.");
            // Each PARAMETER can refer to earlier parameter in the same function, and any SIG or FIELD visible from here.
            // Each RETURNTYPE can refer to the parameters of the same function, and any SIG or FIELD visible from here.
            TempList<ExprVar> tmpvars = new TempList<ExprVar>();
            if (f.args!=null) {
              for(Decl d:f.args) {
                Expr val = d.expr.check(cx, warns).resolve_as_set(warns);
                errors = errors.join(val.errors);
                for(ExpName n: d.names) {
                    ExprVar v=ExprVar.make(n.span(), n.name, val);
                    cx.put(n.name, v);
                    tmpvars.add(v);
                    A4Reporter.getReporter().typecheck((f.returnType==null?"pred ":"fun ")+fullname+", Param "+n.name+": "+v.type+"\n");
                }
              }
            }
            Expr ret = null;
            if (f.returnType!=null) {
                ret = f.returnType.check(cx, warns).resolve_as_set(warns);
                errors = errors.join(ret.errors);
            }
            f.realFunc = new Func(f.pos, fullname, tmpvars.makeConst(), ret);
            SafeList<Func> list=funs.get(f.name);
            if (list==null) { list=new SafeList<Func>(); funs.put(f.name, list); }
            list.add(f.realFunc);
            A4Reporter.getReporter().typecheck(""+f.realFunc+", RETURN: "+f.realFunc.returnDecl.type+"\n");
        }
        return errors;
    }

    JoinableList<Err> checkFunctionBodies(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
        A4Reporter rep = A4Reporter.getReporter();
        for(FunAST f: _funs) {
            Func ff = f.realFunc;
            Expr disj = ExprConstant.TRUE;
            Context cx=new Context(this);
            Iterator<ExprVar> vv=ff.params.iterator();
            for(Decl d:f.args) {
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
            for(Decl d:f.args) for(ExpName n:d.names) cx.remove(n.name);
            if (!ff.isPred && newBody.type.hasTuple() && ff.returnDecl.type.hasTuple() && !newBody.type.intersects(ff.returnDecl.type))
                warns.add(new ErrorWarning(ff.getBody().span(),
                    "Function return value is disjoint from its return type.\n"
                    +"Function body has type "+ff.getBody().type+"\nbut the return type is "+ff.returnDecl.type));
            rep.typecheck(""+ff+", BODY:"+ff.getBody().type+"\n");
            //
            if (!ff.isPred) newBody=newBody.in(ff.returnDecl);
            if (ff.params.size()>0) newBody=ExprQuant.Op.SOME.make(null, null, ff.params, newBody.and(disj));
            fun_2_formula.put(ff, newBody);
        }
        return errors;
    }

    public Exp getFirstFunc() {
        if (_funs.size()==0) return null; else return _funs.get(0).body;
    }

    /** Typecheck if necessary, then return an unmodifiable list of all predicates/functions with that name. */
    public SafeList<Func> getFunc(String name) {
        SafeList<Func> answer = funs.get(name);
        return answer==null ? empty_list_of_func : answer.dup();
    }

    /** Typecheck if necessary, then return an unmodifiable list of all parameter-less predicates/functions. */
    public SafeList<Func> getAllFunc0() {
        SafeList<Func> ans = new SafeList<Func>();
        for(Map.Entry<String,SafeList<Func>> e:funs.entrySet()) {
            for(Func f:e.getValue()) if (f.params.size()==0) ans.add(f);
        }
        return ans.dup();
    }

    //=============================================================================================================//

    private final Map<String,Object> asserts = new LinkedHashMap<String,Object>();

    String addAssertion(Pos pos, String name, Exp value) throws Err {
        if (name==null || name.length()==0) name="assert#"+(1+asserts.size());
        if (name.indexOf('/')>=0) throw new ErrorSyntax(pos, "Assertion name \""+name+"\" cannot contain \'/\'");
        if (name.indexOf('@')>=0) throw new ErrorSyntax(pos, "Asserition name \""+name+"\" cannot contain \'@\'");
        if (asserts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same module, two assertions cannot have the same name.");
        asserts.put(name, value);
        return name;
    }

    JoinableList<Err> checkAssertions(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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

    /** If typecheck was successful, return the assertion with that name (it returns null if there's no match) */
    Expr getAssertion(String name) {
        Object ans = asserts.get(name);
        return (ans instanceof Expr) ? ((Expr)ans) : null;
    }

    //=============================================================================================================//

    private final Map<String,Object> facts = new LinkedHashMap<String,Object>();

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

    JoinableList<Err> checkFacts(JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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
            if (s.anno.get("orderingSIG") instanceof Sig) continue;
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

    //=============================================================================================================//

    final List<Pair<String,Command>> commands = new ArrayList<Pair<String,Command>>();

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

    private SafeList<Func> lookupFunctionOrPredicate(String name) {
        SafeList<Func> ans=new SafeList<Func>();
        for(Object x: lookupSigOrParameterOrFunctionOrPredicate(name,true)) if (x instanceof Func) ans.add((Func)x);
        return ans;
    }

    JoinableList<Err> checkCommands(JoinableList<Err> errors, List<ErrorWarning> warnings) throws Err {
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
                SafeList<Func> ee=getFunc(cname);
                if (ee.size()<1) ee=lookupFunctionOrPredicate(cname);
                if (ee.size()<1) throw new ErrorSyntax(cmd.pos, "The predicate/function \""+cname+"\" cannot be found.");
                if (ee.size()>1) throw new ErrorSyntax(cmd.pos, "There are more than 1 predicate/function with the name \""+cname+"\".");
                e=fun_2_formula.get(ee.get(0));
            }
            cmd = cmd.changeFormula(e);
            commands.set(i, new Pair<String,Command>(cname,cmd));
        }
        return errors;
    }

    public ConstList<Command> getAllCommands() {
        TempList<Command> ans = new TempList<Command>(commands.size());
        for(Pair<String,Command> e:commands) ans.add(e.b);
        return ans.makeConst();
    }

    //=============================================================================================================//

    private void _lookupNQsig_noparam (String name, Set<SigAST> ans) { // It ignores "params"
        SigAST x=sigs.get(name);
        if (x!=null) ans.add(x);
        for(Map.Entry<String,Module> i:opens.entrySet()) i.getValue()._lookupNQsig_noparam(name,ans);
    }

    private SigAST _lookupQsig_noparam (String name) { // It ignores "params"
        Module u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) return u.sigs.get(name);
            u=u.opens.get(name.substring(0,i));
            if (u==null) return null;
            name=name.substring(i+1,name.length());
        }
    }

    Set<SigAST> _lookup_sigORparam (String name) { // Will search "params" too, if at the CURRENT LEVEL
        SigAST s;
        Set<SigAST> ans=new LinkedHashSet<SigAST>();
        if (name.equals("seq/Int")) { ans.add(SEQIDXast); return ans; }
        if (name.equals("Int"))     { ans.add(SIGINTast); return ans; }
        if (name.equals("univ"))    { ans.add(UNIVast); return ans; }
        if (name.equals("none"))    { ans.add(NONEast); return ans; }
        if (name.indexOf('/')<0) {
            _lookupNQsig_noparam(name,ans); s=params.get(name); if (s!=null) ans.add(s);
            return ans;
        }
        if (name.startsWith("this/")) {
            String temp=name.substring(5);
            if (temp.indexOf('/')<0) { s=params.get(temp); if (s!=null) {ans.add(s); return ans;} }
        }
        s=_lookupQsig_noparam(name); if (s!=null) ans.add(s);
        return ans;
    }

    private static final SigAST UNIVast   = new SigAST(Pos.UNKNOWN, "univ",    "univ", true,  false,false,false,false, new ArrayList<String>(), new ArrayList<Decl>(), null, UNIV);
    private static final SigAST SIGINTast = new SigAST(Pos.UNKNOWN, "Int",     "Int",  false, false,false,false,false, Util.asList("univ"),     new ArrayList<Decl>(), null, SIGINT);
    private static final SigAST SEQIDXast = new SigAST(Pos.UNKNOWN, "seq/Int", "Int",  false, false,false,false,false, Util.asList("Int"),      new ArrayList<Decl>(), null, SEQIDX);
    private static final SigAST NONEast   = new SigAST(Pos.UNKNOWN, "none",    "none", false, false,false,false,false, new ArrayList<String>(), new ArrayList<Decl>(), null, NONE);

    /**
     * Look up a parameter, or a signature/function/predicate visible from this module.
     * @param name - the name which can either be fully-qualified (eg. "alias/alias/somename") or not
     * @param look_for_function_and_predicate - true if we want to include functions and predicates as well
     * @return an empty set if there is no match
     */
    public Set<Object> lookupSigOrParameterOrFunctionOrPredicate (String name, boolean look_for_function_and_predicate) {
        SafeList<Func> f;
        Set<Object> ans=new LinkedHashSet<Object>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) {
                for(Module u:world.lookupModuleAndSubmodules(p)) {
                    {SigAST ss=u.sigs.get(name); if (ss!=null) ans.add(ss.realSig);}
                    if (look_for_function_and_predicate) if ((f=u.funs.get(name))!=null) ans.addAll(f);
                }
            }
            SigAST ss=params.get(name); if (ss!=null) ans.add(ss.realSig);
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        {SigAST ss=params.get(name); if (ss!=null) ans.add(ss.realSig);}
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.path2module.get((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            {SigAST ss=u.sigs.get(name); if (ss!=null) ans.add(ss.realSig);}
            if (look_for_function_and_predicate) if ((f=u.funs.get(name))!=null) ans.addAll(f);
        }
        return ans;
    }

    //=============================================================================================================//

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
            for(Sig s:u.getAllSigs())
              for(Field f:s.getFields())
                if (f.label.equals(name))
                  ans.add(f);
        return ans;
    }

    //=============================================================================================================//

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
