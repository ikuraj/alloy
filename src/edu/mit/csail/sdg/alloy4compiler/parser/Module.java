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
import java.util.Collection;
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
import edu.mit.csail.sdg.alloy4.Env;
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

    /** Mutable; this class represents the current typechecking context. */
    static final class Context {
        /** The module that the current context is in. */
        private Module rootmodule=null;
        /** Nonnull if we are typechecking a field declaration or a sig appended facts. */
        SigAST rootsig=null;
        /** True if we are typechecking a field declaration. */
        private boolean rootfield=false;
        /** True if we are typechecking a function's parameter declarations or return declaration. */
        private boolean rootfun=false;
        /** This maps local names (eg. let/quantification variables and function parameters) to the objects they refer to. */
        private final Env<String,Expr> env=new Env<String,Expr>();
        /** Returns true if the name is in scope. */
        final boolean has(String name) {
            return env.has(name);
        }
        /** Returns the expression corresbonding to the given name, or returns null if the name is not in scope. */
        final Expr get(String name, Pos pos) {
            Expr ans = env.get(name);
            if (ans!=null) return ExprUnary.Op.NOOP.make(pos,ans); else return null;
        }
        /** Associates the given name with the given expression in the current lexical scope. */
        final void put(String name, Expr value) {
            env.put(name,value);
        }
        /** Removes the latest binding for the given name from the current lexical scope. */
        final void remove(String name) {
            env.remove(name);
        }
        /** Construct a new Context with an empty lexical scope. */
        Context(Module rootModule) {
            this.rootmodule=rootModule;
        }
        /** Resolve the given name to get a collection of Expr and Func objects. */
        public Collection<Object> resolve(Pos pos, String name) {
            Expr match = env.get(name);
            if (match!=null) { List<Object> ans=new ArrayList<Object>(); ans.add(match); return ans; }
            return rootmodule.populate(rootfield, rootsig, rootfun, pos, name, get("this",pos));
        }
    }

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
        private Module realModule=null;
        /** Constructs an Open object. */
        private Open(Pos pos, String alias, ConstList<String> args, String filename) {
            this.pos=pos; this.alias=alias; this.args=args; this.filename=filename;
        }
        /** Connect this OPEN statement to a module that it points to. */
        void connect(Module realModule) throws Err {
            if (this.realModule!=null && this.realModule!=realModule) throw new ErrorFatal("Internal error (import mismatch)");
            this.realModule=realModule;
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
        private FunAST(Pos pos, String name, List<Decl> params, Exp returnType, Exp body) {
            this.pos=pos; this.name=name; this.params=ConstList.make(params); this.returnType=returnType; this.body=body;
        }
        @Override public String toString() { return name; }
    }

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy signature; equals() uses object identity. */
    static final class SigAST {
        private boolean topo=false;             // This flag is set to "true" during resolving
        private final Module realModule;        // This value is set to its Module during resolving
        private Sig realSig=null;               // This value is set to its corresponding Sig during resolving
        private final List<SigAST> realParents; // This value is set to its corresponding Sig during resolving
        private boolean hint_isLeaf=false;
        private final Pos pos;
        private final String name,fullname;
        private final Pos abs,lone,one,some,subset;
        private final ConstList<ExpName> parents;
        private final ConstList<Decl> fields;
        private final Exp appendedFact;
        private Pos isOrdered=null;
        private SigAST(Pos pos, String fullname, String name, Pos abs, Pos lone, Pos one, Pos some, Pos subset,
            List<ExpName> parents, List<Decl> fields, Exp appendedFacts, Module realModule, Sig realSig) {
            this.pos=(pos==null ? Pos.UNKNOWN : pos);
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
            this.realParents=new ArrayList<SigAST>(this.parents.size());
        }
        private SigAST(String fullname, String name, List<ExpName> parents, Sig realSig) {
            this(null, fullname, name, null, null, null, null, null, parents, null, null, null, realSig);
        }
        @Override public String toString() { return fullname; }
    }

    /** The builtin sig "univ" */
    private static final SigAST UNIVast = new SigAST("univ", "univ", null, UNIV);

    /** The builtin sig "Int" */
    private static final SigAST SIGINTast = new SigAST("Int", "Int", Util.asList(new ExpName(null,"univ")), SIGINT);

    /** The builtin sig "seq/Int" */
    private static final SigAST SEQIDXast = new SigAST("seq/Int", "Int", Util.asList(new ExpName(null,"Int")), SEQIDX);

    /** The builtin sig "none" */
    private static final SigAST NONEast = new SigAST("none", "none", null, NONE);

    //============================================================================================================================//

    /** This field is used during a depth-first search of the dag-of-module(s) to mark which modules have been visited. */
    private Object visitedBy=null;

    /** The world that this Module belongs to. */
    private final Module world;

    /** The simplest path pointing to this Module; it is always equal to this.paths.get(0) */
    private final String path;

    /** The list of paths pointing to this Module; it is always nonempty and already sorted by Util.slashComparator */
    private final List<String> paths;

    /**
     * 1: has seen the "module" line
     * 2: has seen the "open" lines
     * 3: has seen the "sig/pred/fun/fact/assert/check/run" commands
     */
    private int status = 0;

    /** The position of the "MODULE" line at the top of the file; Pos.UNKNOWN if the line has not been parsed from the file yet. */
    private Pos modulePos = Pos.UNKNOWN;

    /** The text of the "MODULE" line at the top of the file; "unknown" if the line has not be parsed from the file yet. */
    private String moduleName = "unknown";

    /** Each param is mapped to its corresponding SigAST (or null if we have not resolved it). */
    private final Map<String,SigAST> params = new LinkedHashMap<String,SigAST>(); // Must be LinkedHashMap since the order matters

    /** Each alias is mapped to its corresponding "open" statement. */
    private final Map<String,Open> opens = new LinkedHashMap<String,Open>();

    /** Each sig name is mapped to its corresponding SigAST. */
    private final Map<String,SigAST> sigs = new LinkedHashMap<String,SigAST>();

    /** Each func name is mapped to a nonempty list of FunAST objects. */
    private final Map<String,SafeList<FunAST>> funcs = new LinkedHashMap<String,SafeList<FunAST>>();

    /** Each assertion name is mapped to either an untypechecked Exp, or a typechecked ExprVar with its value==the assertion. */
    private final Map<String,Object> asserts = new LinkedHashMap<String,Object>();

    /** Each fact name is mapped to either an untypechecked Exp, or a typechecked Expr. */
    private final Map<String,Object> facts = new LinkedHashMap<String,Object>();

    /** The list of (CommandName,Command) pairs; NOTE: duplicate command names are allowed. */
    private final List<Pair<String,Command>> commands = new ArrayList<Pair<String,Command>>();

    /**
     * Constructs a new Module object
     * @param world - the world that this Module belongs to (null if this is the beginning of a new World)
     * @param filename - the filename corresponding to this module
     * @param path - one of the path pointing to this module
     */
    Module(Module world, String filename, String path) throws Err {
        if (world==null) { if (path.length()>0) throw new ErrorAPI("Root module misparsed."); else world=this; }
        this.world=world;
        this.path=path;
        this.paths=new ArrayList<String>(1);
        this.paths.add(path);
        if (filename!=null && filename.length()>0) this.modulePos=new Pos(filename,1,1);
    }

    //============================================================================================================================//

    /** Return the untypechecked body of the first func/pred in this module; return null if there has not been any fun/pred. */
    Expr parseOneExpressionFromString(String input) throws Err, FileNotFoundException, IOException {
        Map<String,String> fc=new LinkedHashMap<String,String>();
        fc.put("", "run {\n"+input+"}"); // We prepend the line "run{"
        Module m = CompParser.alloy_parseStream(true, fc, null, -1, "", "");
        if (m.funcs.size()==0) throw new ErrorSyntax("The input does not correspond to an Alloy expression.");
        Exp body = m.funcs.entrySet().iterator().next().getValue().get(0).body;
        Context cx = new Context(this);
        ArrayList<ErrorWarning> warnings = new ArrayList<ErrorWarning>();
        Expr ans = body.check(cx, warnings);
        ans = ans.resolve(ans.type, warnings);
        if (ans.errors.size()>0) throw ans.errors.get(0);
        return ans;
    }

    /** Throw an exception if the name is already used, or has @ or /, or is univ/Int/none. */
    private void dup(Pos pos, String name, boolean checkFunctions) throws Err {
        if (name.length()==0)     throw new ErrorSyntax(pos, "Name cannot be empty");
        if (name.indexOf('@')>=0) throw new ErrorSyntax(pos, "Name cannot contain the \'@\' character");
        if (name.indexOf('/')>=0) throw new ErrorSyntax(pos, "Name cannot contain the \'/\' character");
        if (name.equals("univ"))  throw new ErrorSyntax(pos, "\'univ\' is a reserved keyword.");
        if (name.equals("Int"))   throw new ErrorSyntax(pos, "\'Int\' is a reserved keyword.");
        if (name.equals("none"))  throw new ErrorSyntax(pos, "\'none\' is a reserved keyword.");
        if (params.containsKey(name) || sigs.containsKey(name))
            throw new ErrorSyntax(pos, "\""+name+"\" is already the name of a sig/parameter in this module.");
        if (checkFunctions && funcs.containsKey(name))
            throw new ErrorSyntax(pos, "\""+name+"\" is already the name of a function/predicate in this module.");
        if (facts.containsKey(name))
            throw new ErrorSyntax(pos, "\""+name+"\" is already the name of a fact paragraph in this module.");
        if (asserts.containsKey(name))
            throw new ErrorSyntax(pos, "\""+name+"\" is already the name of an assertion in this module.");
    }

    /** Throw an exception if there are more than 1 match; return nonnull if only one match; return null if no match. */
    private Object unique (Pos pos, String name, List<Object> objs) throws Err {
        if (objs.size()==0) return null;
        if (objs.size()==1) return objs.get(0);
        StringBuilder msg = new StringBuilder("The name \"").append(name);
        msg.append("\" is ambiguous.\n" + "There are ").append(objs.size()).append(" choices:");
        for(int i=0; i<objs.size(); i++) {
            msg.append("\n\n#").append(i+1).append(": ");
            Object x=objs.get(i);
            if (x instanceof SigAST) {
                SigAST y=(SigAST)x; msg.append("sig ").append(y.fullname).append("\n"+"at ").append(y.pos.toShortString());
            }
            else if (x instanceof FunAST) {
                FunAST y=(FunAST)x;
                msg.append(y.returnType==null?"pred ":"fun ")
                   .append(y.realFunc.label).append("\n"+"at ").append(y.pos.toShortString());
            }
            else if (x instanceof ExprVar) {
                ExprVar y=(ExprVar)x; msg.append("assert ").append(y.label).append("\n"+"at ").append(y.pos.toShortString());
            }
        }
        throw new ErrorSyntax(pos, msg.toString());
    }

    /** Returns a list containing THIS MODULE and all modules reachable from this module. */
    private void getHelper(SafeList<Module> ans, Object key) {
        if (visitedBy==key) return;
        visitedBy=key;
        ans.add(this);
        for(Map.Entry<String,Open> i:opens.entrySet()) {Module m=i.getValue().realModule; if (m!=null) m.getHelper(ans,key);}
    }

    /** Return the list containing THIS MODULE and all modules reachable from this module. */
    public SafeList<Module> getAllReachableModules() {
        SafeList<Module> ans=new SafeList<Module>();
        getHelper(ans, new Object()); // The object must be new, since we need it to be a unique key
        return ans.dup();
    }

    /** Return the list containing UNIV, SIGINT, SEQIDX, NONE, and all sigs defined in this module or a reachable submodule. */
    public SafeList<Sig> getAllReachableSigs() {
        SafeList<Sig> x = new SafeList<Sig>();
        x.add(UNIV);
        x.add(SIGINT);
        x.add(SEQIDX);
        x.add(NONE);
        for(Module m:getAllReachableModules())
          for(Map.Entry<String,SigAST> s: m.sigs.entrySet())
            if (s.getValue().realSig!=null)
               x.add(s.getValue().realSig);
        return x.dup();
    }

    /** It looks up non-fully-qualified SigAST/FunAST/Assertion from the current module; it skips PARAMs. */
    private List<Object> getRawNQS (int r, String name) {
        // (r&1)!=0 => SigAST  (r&2) != 0 => ExprVar with expr is the value of an assertion    (r&4)!=0 => FunAST
        List<Object> ans=new ArrayList<Object>();
        for(Module m:getAllReachableModules()) {
            if ((r&1)!=0) { SigAST x=m.sigs.get(name); if (x!=null) ans.add(x); }
            if ((r&2)!=0) { Object x=m.asserts.get(name); if (x instanceof Expr) ans.add(x); }
            if ((r&4)!=0) { SafeList<FunAST> x=m.funcs.get(name); if (x!=null) ans.addAll(x); }
        }
        return ans;
    }

    /** It looks up fully-qualified SigAST/FunAST/Assertion from the current module; it skips PARAMs. */
    private List<Object> getRawQS (int r, String name) {
        // (r&1)!=0 => SigAST  (r&2) != 0 => ExprVar with expr is the value of an assertion    (r&4)!=0 => FunAST
        List<Object> ans=new ArrayList<Object>();
        Module u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) {
                if ((r&1)!=0) { SigAST x=u.sigs.get(name); if (x!=null) ans.add(x); }
                if ((r&2)!=0) { Object x=u.asserts.get(name); if (x instanceof Expr) ans.add(x); }
                if ((r&4)!=0) { SafeList<FunAST> x=u.funcs.get(name); if (x!=null) ans.addAll(x); }
                return ans;
            }
            String alias=name.substring(0,i);
            Open uu=u.opens.get(alias);
            if (uu==null || uu.realModule==null) return ans;
            u=uu.realModule;
            name=name.substring(i+1);
        }
    }

    /** Looks up a SigAST from the current module (and it will also search this.params) */
    private SigAST getRawSIG (Pos pos, String name) throws Err {
        List<Object> s;
        SigAST s2=null;
        if (name.equals("univ"))    return UNIVast;
        if (name.equals("Int"))     return SIGINTast;
        if (name.equals("seq/Int")) return SEQIDXast;
        if (name.equals("none"))    return NONEast;
        if (name.indexOf('/')<0) {
            s=getRawNQS(1, name);
            s2=params.get(name);
        } else {
            if (name.startsWith("this/")) { name=name.substring(5); s2=params.get(name); }
            s=getRawQS(1, name);
        }
        if (s2!=null && !s.contains(s2)) s.add(s2);
        return (SigAST) (unique(pos, name, s));
    }

    /** Returns a short description for this module. */
    @Override public String toString() {
        String answer=null;
        for(String x:paths) { if (answer==null) answer="module{"+x; else answer=answer+", "+x; }
        return answer+"}";
    }

    //============================================================================================================================//

    /** Returns a pointer to the root module in this world. */
    Module getRootModule() { return world; }

    /** Returns the text of the "MODULE" line at the top of the file; "unknown" if the line has not be parsed from the file yet. */
    String getModelName() { return moduleName; }

    /** Returns an unmodifiable copy of the current list of OPEN statements. */
    ConstList<Open> getOpens() {
        TempList<Open> ans = new TempList<Open>(opens.size());
        for(Map.Entry<String,Open> e: opens.entrySet()) ans.add(e.getValue());
        return ans.makeConst();
    }

    /** Add the "MODULE" declaration. */
    void addModelName(Pos pos, String moduleName, List<ExpName> list) throws Err {
        if (status>0) throw new ErrorSyntax(pos,
           "The \"module\" declaration must occur at the top,\n" + "and can occur at most once.");
        this.moduleName=moduleName;
        this.modulePos=pos;
        if (list!=null) for(ExpName expr: list) {
            String name=expr.name;
            dup(expr.span(), name, true);
            if (path.length()==0) addSig(null, expr.span(), name, null, null, null, null, null, null, null);
            else params.put(name, null);
        }
        this.status=1; // This line must be at the end, since "addSig" will otherwise bump the status value to 3
    }

    /** Add util/sequniv to the list of declarations. */
    void addSeq(Pos pos) throws Err {
        int oldStatus=status;
        status=0;
        try {
            addOpen(pos, new ExpName(pos,"util/sequniv"), null, new ExpName(pos,"seq"));
        } finally {
            status=oldStatus;
        }
    }

    /** Add an OPEN declaration. */
    void addOpen(Pos pos, ExpName name, List<ExpName> args, ExpName alias) throws Err {
        if (status>2) throw new ErrorSyntax(pos,
           "The \"open\" declaration must occur before any\n" + "sig/pred/fun/fact/assert/check/run command.");
        status=2;
        String as = (alias==null ? "" : alias.name);
        if (name.name.length()==0) throw new ErrorSyntax(name.span(), "The filename cannot be empty.");
        if (as.indexOf('@')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'@\' character");
        if (as.indexOf('/')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'/\' character");
        if (as.length()==0) {
            if (args!=null && args.size()!=0)
                throw new ErrorSyntax(pos,
                "The module being imported has parameters, so you must supply an alias using the AS keyword.");
            for(int i=0; i<name.name.length(); i++) {
                char c=name.name.charAt(i);
                if ((c>='a' && c<='z') || (c>='A' && c<='Z')) continue;
                if (i==0) throw new ErrorSyntax(pos, "This filename does not start with a-z or A-Z,\n"
                   + "so you must supply an alias using the AS keyword.");
                if (!(c>='0' && c<='9') && c!='_' && c!='\'' && c!='\"') throw new ErrorSyntax(pos, "Filename contains \'"
                   + c + "\' which is illegal in an alias,\n" + "so you must supply an alias using the AS keyword.");
            }
            as=name.name;
        }
        final TempList<String> newlist = new TempList<String>(args==null ? 0 : args.size());
        if (args!=null) for(int i=0; i<args.size(); i++) {
            ExpName arg=args.get(i);
            if (arg.name.length()==0)      throw new ErrorSyntax(arg.span(), "Argument cannot be empty.");
            if (arg.name.indexOf('@')>=0)  throw new ErrorSyntax(arg.span(), "Argument cannot contain the \'@\' chracter.");
            newlist.add(arg.name);
        }
        Open x=opens.get(as);
        if (x!=null) {
            // we allow this, especially because of util/sequniv
            if (x.args.equals(newlist.makeConst()) && x.filename.equals(name.name)) return;
            throw new ErrorSyntax(pos, "You cannot import two different modules\n" + "using the same alias.");
        }
        x=new Open(pos, as, newlist.makeConst(), name.name);
        opens.put(as,x);
    }

    /** Every param in every module will now point to a nonnull SigAST. */
    private static void resolveParams(A4Reporter rep, List<Module> modules) throws Err {
      while(true) {
         boolean chg=false;
         Open missing=null;
         String missingName="";
         for(Module mod:modules) for(Map.Entry<String,Open> entry:mod.opens.entrySet()) {
            Open open=entry.getValue();
            Module sub=open.realModule;
            if (open.args.size()!=sub.params.size())
                throw new ErrorSyntax(open.pos,
                    "You supplied "+open.args.size()+" arguments to the open statement, but the imported module requires "
                    +sub.params.size()+" arguments.");
            int i=0;
            for(Map.Entry<String,SigAST> p:sub.params.entrySet()) {
               SigAST old=p.getValue();
               String kn=p.getKey(), vn=open.args.get(i);
               i++;
               SigAST vv=mod.getRawSIG(open.pos, vn);
               if (vv==null) {if (old==null) {missing=open; missingName=vn;} continue;}
               if (old==vv) continue;
               if (old!=null) throw new ErrorFatal(open.pos, "Internal error (module re-instantiated with different arguments)");
               if (vv==Module.NONEast) throw new ErrorSyntax(open.pos, "You cannot use \"none\" as an instantiating argument.");
               chg=true;
               p.setValue(vv);
               // TODO: This detects for the Alloy3 behavior of util/ordering.als
               if (kn.equals("elem") && sub.sigs.size()==1
                  && vv!=Module.UNIVast && vv!=Module.SIGINTast && vv!=Module.SEQIDXast && vv!=Module.NONEast
                  && sub.modulePos.filename.toLowerCase(Locale.US).endsWith("util"+File.separatorChar+"ordering.als")) {
                     vv.isOrdered = open.pos;
                  }
               rep.parse("RESOLVE: "+(sub.path.length()==0?"this/":sub.path)+"/"+kn+" := "+vv+"\n");
            }
         }
         if (!chg && missing==null) return;
         if (!chg) throw new ErrorSyntax(missing.pos, "The signature name \""+missingName+"\" cannot be found.");
      }
    }

    /** Modules with same filename and instantiating arguments will be merged. */
    private static void resolveModules(A4Reporter rep, List<Module> modules) {
       // Before merging, the only pointers that go between Module objects are
       // (1) a module's "params" may point to a sig in another module
       // (2) a module's "imports" may point to another module
       // So when we find that two modules A and B should be merged,
       // we go through every module and replace "pointers into B" with equivalent "pointers into A".
       while(true) {
          boolean chg=false;
          for(int i=0; i<modules.size(); i++) {
             Module a=modules.get(i);
             for(int j=i+1; j<modules.size(); j++) {
                Module b=modules.get(j);
                if (!a.modulePos.filename.equals(b.modulePos.filename) || !a.params.equals(b.params)) continue;
                chg=true;
                rep.parse("MATCH FOUND ON "+a.modulePos.filename+"\n");
                if (i!=0 && Util.slashComparator.compare(a.path, b.path)>0) { a=b; b=modules.get(i); modules.set(i,a); }
                modules.remove(j);
                j--;
                a.paths.addAll(b.paths);
                Collections.sort(a.paths, Util.slashComparator);
                for(Module c:modules) {
                   for(Map.Entry<String,SigAST> p:c.params.entrySet())
                      { if (isin(p.getValue(), b.sigs)) p.setValue(a.sigs.get(p.getValue().name)); }
                   for(Map.Entry<String,Open> p:c.opens.entrySet())
                      { if (p.getValue().realModule==b) p.getValue().realModule=a; }
                }
             }
          }
          if (!chg) break;
       }
    }

    //============================================================================================================================//

    /** Add a sig declaration. */
    SigAST addSig(List<ExpName> hints, Pos pos, String name, Pos isAbstract, Pos isLone, Pos isOne, Pos isSome,
    List<ExpName> parents, List<Decl> fields, Exp fact) throws Err {
        status=3;
        dup(pos, name, true);
        String full = (path.length()==0) ? "this/"+name : path+"/"+name;
        Pos subset=null;
        if (parents!=null) {
            if (parents.get(0)==null) {ExpName parent=parents.get(1); parents=Util.asList(parent);}
            else for(ExpName p:parents) subset=p.span().merge(subset);
        }
        SigAST obj=new SigAST(pos,full,name, isAbstract,isLone,isOne,isSome,subset, parents, fields,fact,this,null);
        if (hints!=null) for(ExpName hint:hints) if (hint.name.equals("leaf")) {obj.hint_isLeaf=true; break;}
        sigs.put(name, obj);
        return obj;
    }

    /** The given SigAST will now point to a nonnull Sig. */
    private static Sig resolveSig(List<SigAST> sorted, SigAST oldS) throws Err {
        if (oldS.realSig != null) return oldS.realSig;
        final Pos pos = oldS.pos;
        final Module u = oldS.realModule;
        final String name = oldS.name;
        final String fullname = u.paths.contains("") ? "this/"+name : (u.paths.get(0)+"/"+name);
        if (oldS.topo) throw new ErrorType(pos, "Sig "+oldS+" is involved in a cyclic inheritance."); else oldS.topo=true;
        if (oldS.subset!=null)  {
            if (oldS.abs!=null) throw new ErrorSyntax(pos, "Subset signature \""+name+"\" cannot be abstract.");
            List<Sig> parents = new ArrayList<Sig>();
            for(ExpName n: oldS.parents) {
               SigAST parentAST = u.getRawSIG(n.span(), n.name);
               if (parentAST==null) throw new ErrorSyntax(n.span(), "The sig \""+n.name+"\" cannot be found.");
               oldS.realParents.add(parentAST);
               parents.add(resolveSig(sorted, parentAST));
            }
            oldS.realSig = new SubsetSig(pos, parents, fullname, oldS.subset, oldS.lone, oldS.one, oldS.some, oldS.isOrdered);
        } else {
            ExpName sup = null;
            if (oldS.parents.size()==1) {sup=oldS.parents.get(0); if (sup!=null && sup.name.length()==0) sup=null;}
            Pos suppos = sup==null ? Pos.UNKNOWN : sup.span();
            SigAST parentAST = sup==null ? UNIVast : u.getRawSIG(suppos, sup.name);
            if (parentAST==null) throw new ErrorSyntax(suppos, "The sig \""+sup.name+"\" cannot be found.");
            oldS.realParents.add(parentAST);
            Sig parent = resolveSig(sorted, parentAST);
            if (!(parent instanceof PrimSig)) throw new ErrorSyntax(suppos, "Cannot extend the subset signature \"" + parent
               + "\".\n" + "A signature can only extend a toplevel signature or a subsignature.");
            PrimSig p = (PrimSig)parent;
            oldS.realSig = new PrimSig(pos, p, fullname, oldS.abs, oldS.lone, oldS.one, oldS.some, oldS.isOrdered, oldS.hint_isLeaf);
        }
        sorted.add(oldS);
        return oldS.realSig;
    }

    /** Returns an unmodifiable list of all signatures defined inside this module. */
    public SafeList<Sig> getAllSigs() {
        SafeList<Sig> x = new SafeList<Sig>(sigs.size());
        for(Map.Entry<String,SigAST> e:sigs.entrySet()) x.add(e.getValue().realSig);
        return x.dup();
    }

    //============================================================================================================================//

    /** Add a FUN or PRED declaration. */
    void addFunc(Pos p, String n, Exp f, List<Decl> d, Exp t, Exp v) throws Err {
        status=3;
        dup(p, n, false);
        ExpName dup = Decl.findDuplicateName(d);
        if (dup!=null) throw new ErrorSyntax(dup.span(), "The parameter name \""+dup.name+"\" cannot appear more than once.");
        d=new ArrayList<Decl>(d);
        if (f!=null) d.add(0, new Decl(null, Util.asList(new ExpName(f.span(), "this")), f));
        FunAST ans = new FunAST(p, n, d, t, v);
        SafeList<FunAST> list = funcs.get(n);
        if (list==null) { list = new SafeList<FunAST>(); funcs.put(n, list); }
        list.add(ans);
    }

    /** Each FunAST will now point to a bodyless Func object. */
    private JoinableList<Err> resolveFuncDecls(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) {
        for(Map.Entry<String,SafeList<FunAST>> entry:funcs.entrySet()) for(FunAST f:entry.getValue()) {
            String fullname = (path.length()==0 ? "this/" : (path+"/")) + f.name;
            // Each PARAMETER can refer to earlier parameter in the same function, and any SIG or FIELD visible from here.
            // Each RETURNTYPE can refer to the parameters of the same function, and any SIG or FIELD visible from here.
            Context cx = new Context(this);
            cx.rootfun = true;
            TempList<ExprVar> tmpvars = new TempList<ExprVar>();
            boolean err=false;
            for(Decl d:f.params) {
                Expr val = d.expr.check(cx, warns).resolve_as_set(warns);
                if (!val.errors.isEmpty()) { err=true; errors = errors.join(val.errors); }
                for(ExpName n: d.names) {
                    ExprVar v = ExprVar.make(n.span(), n.name, val);
                    cx.put(n.name, v);
                    tmpvars.add(v);
                    rep.typecheck((f.returnType==null?"pred ":"fun ")+fullname+", Param "+n.name+": "+v.type+"\n");
                }
            }
            Expr ret = null;
            if (f.returnType!=null) {
                ret = f.returnType.check(cx, warns).resolve_as_set(warns);
                if (!ret.errors.isEmpty()) { err=true; errors=errors.join(ret.errors); }
            }
            if (err) continue;
            try {
                f.realFunc = new Func(f.pos, fullname, tmpvars.makeConst(), ret);
                rep.typecheck(""+f.realFunc+", RETURN: "+f.realFunc.returnDecl.type+"\n");
            } catch(Err ex) { errors = errors.append(ex); }
        }
        return errors;
    }

    /** Each Func's body will now be typechecked Expr object. */
    private JoinableList<Err> resolveFuncBodys(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) {
        for(Map.Entry<String,SafeList<FunAST>> entry:funcs.entrySet()) for(FunAST f:entry.getValue()) {
            Func ff = f.realFunc;
            Expr disj = null;
            Context cx = new Context(this);
            Iterator<ExprVar> vv=ff.params.iterator();
            for(Decl d:f.params) {
                List<Expr> disjvars = (d.disjoint!=null && d.names.size()>0) ? (new ArrayList<Expr>(d.names.size())) : null;
                for(ExpName n:d.names) {
                    ExprVar newvar=vv.next();
                    cx.put(n.name, newvar);
                    if (disjvars!=null) disjvars.add(newvar);
                }
                if (disjvars!=null) disj=ExprBuiltin.makeDISJOINT(d.disjoint, null, disjvars).and(disj);
            }
            Expr newBody = f.body.check(cx, warns);
            if (ff.isPred) newBody=newBody.resolve_as_formula(warns); else newBody=newBody.resolve_as_set(warns);
            errors = errors.join(newBody.errors);
            if (!newBody.errors.isEmpty()) continue;
            try { ff.setBody(newBody); } catch(Err er) {errors=errors.append(er); continue;}
            if (ff.returnDecl.type.hasTuple() && newBody.type.hasTuple() && !newBody.type.intersects(ff.returnDecl.type))
                warns.add(new ErrorWarning(ff.getBody().span(),
                    "Function return value is disjoint from its return type.\n"
                    +"Function body has type "+ff.getBody().type + "\n" + "but the return type is "+ff.returnDecl.type));
            rep.typecheck(ff.toString()+", BODY:"+ff.getBody().type+"\n");
            if (!ff.isPred) newBody=newBody.in(ff.returnDecl);
            if (ff.params.size()>0) newBody=ExprQuant.Op.SOME.make(null, null, ff.params, newBody.and(disj));
            if (newBody.errors.isEmpty()) f.realFormula=newBody; else errors=errors.join(newBody.errors);
        }
        return errors;
    }

    /** Return an unmodifiable list of all functions in this module. */
    public SafeList<Func> getAllFunc() {
        SafeList<Func> ans = new SafeList<Func>();
        for(Map.Entry<String,SafeList<FunAST>> e: funcs.entrySet()) for(FunAST func:e.getValue()) ans.add(func.realFunc);
        return ans.dup();
    }

    //============================================================================================================================//

    /** Add an ASSERT declaration. */
    String addAssertion(Pos pos, String name, Exp value) throws Err {
        status=3;
        if (name==null || name.length()==0) name="assert$"+(1+asserts.size());
        dup(pos, name, true);
        asserts.put(name, value);
        return name;
    }

    /** Each assertion name now points to a typechecked Expr rather than an untypechecked Exp. */
    private JoinableList<Err> resolveAssertions(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) {
        Context cx = new Context(this);
        for(Map.Entry<String,Object> e:asserts.entrySet()) {
            Object x = e.getValue();
            Expr expr = null;
            if (x instanceof Expr) expr=(Expr)x;
            if (x instanceof Exp) {
                expr=((Exp)x).check(cx, warns).resolve_as_formula(warns);
                if (expr.errors.isEmpty())
                   e.setValue(ExprVar.make(expr.span(), (path.length()==0?"this/":(path+"/"))+e.getKey(), expr));
            }
            if (expr.errors.size()>0) errors=errors.join(expr.errors);
            else rep.typecheck("Assertion " + e.getKey() + ": " + expr.type+"\n");
        }
        return errors;
    }

    /** Return an unmodifiable list of all assertions in this module. */
    public ConstList<Pair<String,Expr>> getAllAssertions() {
        TempList<Pair<String,Expr>> ans = new TempList<Pair<String,Expr>>(asserts.size());
        for(Map.Entry<String,Object> e:asserts.entrySet()) {
            Object x=e.getValue();
            if (x instanceof ExprVar) ans.add(new Pair<String,Expr>(e.getKey(), ((ExprVar)x).expr));
        }
        return ans.makeConst();
    }

    //============================================================================================================================//

    /** Add a FACT declaration. */
    void addFact(Pos pos, String name, Exp value) throws Err {
        status=3;
        if (name==null || name.length()==0) name="fact$"+(1+facts.size());
        dup(pos, name, true);
        facts.put(name,value);
    }

    /** Each fact name now points to a typechecked Expr rather than an untypechecked Exp; we'll also add the sig appended facts. */
    private JoinableList<Err> resolveFacts(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) {
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
            else rep.typecheck("Fact " + e.getKey() + ": " + expr.type+"\n");
        }
        for(Map.Entry<String,SigAST> e:sigs.entrySet()) {
            Sig s=e.getValue().realSig;
            Exp f=e.getValue().appendedFact;
            if (f==null) continue;
            Expr formula;
            cx.rootsig=e.getValue();
            if (s.isOne==null) {
                ExprVar THIS = s.oneOf("this");
                cx.put("this", THIS);
                formula = f.check(cx, warns).resolve_as_formula(warns).forAll(THIS);
            } else {
                cx.put("this", s);
                formula = f.check(cx, warns).resolve_as_formula(warns);
            }
            cx.remove("this");
            if (formula.errors.size()>0) { errors=errors.join(formula.errors); continue; }
            facts.put(s.toString()+"$fact", formula);
            rep.typecheck("Fact "+s+"$fact: " + formula.type+"\n");
        }
        return errors;
    }

    /** Return an unmodifiable list of all facts in this module. */
    public SafeList<Pair<String,Expr>> getAllFacts() {
        SafeList<Pair<String,Expr>> ans = new SafeList<Pair<String,Expr>>(facts.size());
        for(Map.Entry<String,Object> e:facts.entrySet()) {
            Object x=e.getValue();
            if (x instanceof Expr) ans.add(new Pair<String,Expr>(e.getKey(), (Expr)x));
        }
        return ans.dup();
    }

    //============================================================================================================================//

    /** Add a COMMAND declaration. */
    void addCommand(Pos p, String n, boolean c, int o, int b, int seq, int exp, List<Pair<Sig,Integer>> s, String label) throws Err {
        status=3;
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s)));
    }

    /** Add a COMMAND declaration. */
    void addCommand(Pos p, Exp e, boolean c, int o, int b, int seq, int exp, List<Pair<Sig,Integer>> s, String label) throws Err {
        status=3;
        String n;
        if (c) n=addAssertion(p,"",e); else addFunc(e.span(),n="run$"+(1+commands.size()),null,new ArrayList<Decl>(),null,e);
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s)));
    }

    /** Each command now points to a typechecked Expr. */
    private void resolveCommands() throws Err {
        for(int i=0; i<commands.size(); i++) {
            String cname=commands.get(i).a;
            Command cmd=commands.get(i).b;
            Expr e=null;
            if (cmd.check) {
                List<Object> m=getRawQS(2, cname); // We prefer assertion in the topmost module
                if (m.size()==0 && cname.indexOf('/')<0) m=getRawNQS(2, cname);
                if (m.size()>1) unique(cmd.pos, cname, m);
                if (m.size()<1) throw new ErrorSyntax(cmd.pos, "The assertion \""+cname+"\" cannot be found.");
                e=((ExprVar)(m.get(0))).expr;
            } else {
                List<Object> m=getRawQS(4, cname); // We prefer fun/pred in the topmost module
                if (m.size()==0 && cname.indexOf('/')<0) m=getRawNQS(4, cname);
                if (m.size()>1) unique(cmd.pos, cname, m);
                if (m.size()<1) throw new ErrorSyntax(cmd.pos, "The predicate/function \""+cname+"\" cannot be found.");
                e=((FunAST)(m.get(0))).realFormula;
            }
            TempList<Pair<Sig,Integer>> sc=new TempList<Pair<Sig,Integer>>(cmd.scope.size());
            for(Pair<Sig,Integer> et:cmd.scope) {
                SigAST s=getRawSIG(et.a.pos, et.a.label);
                if (s==null) throw new ErrorSyntax(et.a.pos, "The sig \""+et.a.label+"\" cannot be found.");
                sc.add(new Pair<Sig,Integer>(s.realSig, et.b));
            }
            commands.set(i, new Pair<String,Command>(cname, cmd.change(e, sc.makeConst())));
        }
    }

    /** Return an unmodifiable list of all commands in this module. */
    public SafeList<Command> getAllCommands() {
        SafeList<Command> ans = new SafeList<Command>(commands.size());
        for(Pair<String,Command> c:commands) ans.add(c.b);
        return ans.dup();
    }

    //============================================================================================================================//

    /** Returns true if exists some entry (a,b) in the map, such that b==value (using object identity as the comparison) */
    private static<K,V> boolean isin(V value, Map<K,V> map) {
        for(Map.Entry<K,V> e:map.entrySet()) if (e.getValue()==value) return true;
        return false;
    }

    //============================================================================================================================//

    /** This method resolves the entire world; NOTE: if it throws an exception, it may leave the world in an inconsistent state! */
    static Module resolveAll(final A4Reporter rep, final Module root) throws Err {
        List<Module> modules = new ArrayList<Module>(root.getAllReachableModules());
        resolveParams(rep, modules);
        resolveModules(rep, modules);
        JoinableList<Err> errors = new JoinableList<Err>();
        final List<ErrorWarning> warns = new ArrayList<ErrorWarning>();
        // Resolves SigAST -> Sig, and topologically sort the sigs into the "sorted" array
        List<SigAST> sorted = new ArrayList<SigAST>();
        sorted.add(UNIVast);
        sorted.add(SIGINTast);
        sorted.add(SEQIDXast);
        sorted.add(NONEast);
        for(final Module m:modules) for(final Map.Entry<String,SigAST> e:m.sigs.entrySet()) Module.resolveSig(sorted, e.getValue());
        // Add the fields to the sigs in topologically sorted order (since fields in subsigs are allowed to refer to parent's fields)
        for(final SigAST oldS:sorted) {
           // When typechecking each field:
           // * it is allowed to refer to earlier fields in the same SIG or in any visible ancestor sig
           // * it is allowed to refer to visible sigs
           // * it is NOT allowed to refer to any predicate or function
           // For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
           // then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
           final Sig s=oldS.realSig;
           final Module m=oldS.realModule;
           final Context cx=new Context(m);
           final ExpName dup=Decl.findDuplicateName(oldS.fields);
           if (dup!=null) throw new ErrorSyntax(dup.span(), "sig \""+s+"\" cannot have 2 fields named \""+dup.name+"\"");
           for(final Decl d:oldS.fields) {
              // The name "this" does matter, since the parser and the typechecker both refer to it as "this"
              final ExprVar THIS = s.oneOf("this");
              cx.rootfield=true;
              cx.rootsig=oldS;
              cx.put("this", THIS);
              Expr bound = d.expr.check(cx, warns).resolve_as_set(warns), disjA=null, disjF=ExprConstant.TRUE;
              cx.remove("this");
              for(final ExpName n:d.names) {
                 final Field f=s.addTrickyField(d.span(), n.name, THIS, bound);
                 rep.typecheck("Sig "+s+", Field "+f.label+": "+f.type+"\n");
                 if (d.disjoint==null) continue;
                 if (disjA==null) { disjA=f; continue; }
                 disjF=ExprBinary.Op.AND.make(d.disjoint, null, disjA.intersect(f).no(), disjF);
                 disjA=disjA.plus(f);
              }
              if (d.disjoint==null || disjF==ExprConstant.TRUE) continue;
              if (!disjF.errors.isEmpty()) { errors=errors.join(disjF.errors); continue; }
              String name = s.toString()+"$disjoint";
              m.facts.put(name, disjF);
              rep.typecheck("Fact " + name + ": " + disjF.type+"\n");
          }
        }
        if (!errors.isEmpty()) throw errors.get(0);
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
                     "Two overlapping signatures cannot have\n" + "two fields with the same name \""+field.label
                     +"\":\n\n1) one is in sig \""+field.sig+"\"\n"+field.pos
                     +"\n\n2) the other is in sig \""+field2.sig+"\"\n"+field2.pos);
               peers.add(field);
            }
          }
        }
        // Typecheck the function declarations
        for(Module x:modules) errors=x.resolveFuncDecls(rep, errors, warns);
        if (!errors.isEmpty()) throw errors.get(0);
        // Typecheck the function bodies, assertions, and facts (which can refer to function declarations)
        for(Module x:modules) {
            errors=x.resolveFuncBodys(rep,errors,warns);
            errors=x.resolveAssertions(rep,errors,warns);
            errors=x.resolveFacts(rep,errors,warns);
        }
        if (!errors.isEmpty()) throw errors.get(0);
        // Typecheck the run/check commands (which can refer to function bodies and assertions)
        root.resolveCommands();
        if (!errors.isEmpty()) throw errors.get(0);
        for(ErrorWarning w:warns) rep.warning(w);
        return root;
    }

    //============================================================================================================================//

    /** Look up a field from any visible sig (and returns an empty set if there is no match) */
    private Set<Field> lookupField(String name) {
        Set<Field> ans=new LinkedHashSet<Field>();
        for(Module m:getAllReachableModules())
          for(Map.Entry<String,SigAST> s:m.sigs.entrySet())
            for(Field f:s.getValue().realSig.getFields())
              if (f.label.equals(name))
                 ans.add(f);
        return ans;
    }

    /** Resolve the name based on the current context and this module. */
    public Collection<Object> populate(boolean rootfield, SigAST rootsig, boolean rootfun, Pos pos, String fullname, Expr THIS) {
        // Return object can be Func(with > 0 arguments) or Expr
        final String name = (fullname.charAt(0)=='@') ? fullname.substring(1) : fullname;
        boolean fun = (rootsig!=null && !rootfield) || (rootsig==null && !rootfun);
        List<Object> ans;
        if (name.equals("univ"))    { ans=new ArrayList<Object>(); ans.add(ExprUnary.Op.NOOP.make(pos, UNIV));   return ans; }
        if (name.equals("Int"))     { ans=new ArrayList<Object>(); ans.add(ExprUnary.Op.NOOP.make(pos, SIGINT)); return ans; }
        if (name.equals("seq/Int")) { ans=new ArrayList<Object>(); ans.add(ExprUnary.Op.NOOP.make(pos, SEQIDX)); return ans; }
        if (name.equals("none"))    { ans=new ArrayList<Object>(); ans.add(ExprUnary.Op.NOOP.make(pos, NONE));   return ans; }
        if (name.equals("iden"))    { ans=new ArrayList<Object>(); ans.add(ExprConstant.Op.IDEN.make(pos, 0));   return ans; }
        ans = name.indexOf('/')>=0 ? getRawQS(fun?5:1,name) : getRawNQS(fun?5:1,name);
        Object param = params.get(name); if (param!=null && !ans.contains(param)) ans.add(param);
        if (rootsig!=null) {
            // Within a field decl
            // (1) Can refer to any visible sig/param (but you cannot call any function or predicates)
            // (2) Can refer to field in this sig (defined earlier than you), and fields in any visible ancestor sig
            // Within an appended facts
            // (1) Can refer to any visible sig/param/func/predicate
            // (2) Can refer to any visible field (but if it is in this sig or a parent sig, we'll prepend "this." unless it has '@')
            for(Field f:lookupField(name)) {
                boolean isAncestor = rootsig.realSig.isSameOrDescendentOf(f.sig);
                if (isAncestor) { Expr ee=ExprUnary.Op.NOOP.make(pos,f); ans.add(fullname.charAt(0)=='@' ? ee : THIS.join(ee)); }
                else if (!rootfield) { Expr ee=ExprUnary.Op.NOOP.make(pos,f,f.weight+1,null); ans.add(ee); }
            }
        }
        else {
            // If within a function paramDecl/returnDecl, we cannot call, but can refer to anything else visible.
            // Else we can call, and can refer to anything visible.
            for(Field f:lookupField(name)) ans.add(ExprUnary.Op.NOOP.make(pos,f));
        }
        // Convert SigAST/FunAST/Field/Expr into Expr
        List<Object> realAns=new ArrayList<Object>();
        for(Object x:ans) {
            if (x instanceof SigAST) {
               SigAST y=(SigAST)x;
               realAns.add(ExprUnary.Op.NOOP.make(pos, y.realSig));
            }
            else if (x instanceof FunAST) {
               FunAST y=(FunAST)x;
               if (y.realFunc.params.isEmpty()) realAns.add(ExprCall.make(pos,null,y.realFunc,null,0)); else realAns.add(y.realFunc);
            }
            else if (x instanceof Field) {
               realAns.add(ExprUnary.Op.NOOP.make(pos, (Field)x));
            }
            else if (x instanceof Expr) {
               realAns.add(x);
            }
        }
        return realAns;
    }
}
