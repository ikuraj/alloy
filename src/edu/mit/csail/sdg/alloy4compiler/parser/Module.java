/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
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
import edu.mit.csail.sdg.alloy4.IdentitySet;
import edu.mit.csail.sdg.alloy4.JoinableList;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.Triple;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBad;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprBadCall;
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
        /** The level of macro substitution recursion. */
        public final int unrolls;
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
            this(rootModule, 20); // 20 is a reasonable threshold; deeper than this would likely be too big to solve anyway
        }
        /** Construct a new Context with an empty lexical scope. */
        Context(Module rootModule, int unrolls) {
            this.rootmodule=rootModule;
            this.unrolls=unrolls;
        }
        /** Resolve the given name to get a collection of Expr and Func objects. */
        public ConstList<Expr> resolve(Pos pos, final String name) {
            Expr match = env.get(name);
            int i = name.indexOf('/');
            if (i>=0) {
                String n=name;
                if (n.startsWith("this/")) n=n.substring(5);
                Module mod = rootmodule;
                while(true) {
                    i = n.indexOf('/');
                    if (i<0) {
                        Macro m = mod.macros.get(n);
                        if (m==null || (m.isPrivate!=null && mod!=rootmodule)) break; else return Util.asList(m.changePos(pos));
                    }
                    String alias = n.substring(0,i);
                    Open uu = mod.opens.get(alias);
                    if (uu==null || uu.realModule==null || uu.isPrivate) break;
                    n = n.substring(i+1);
                    mod = uu.realModule;
                }
            } else if (match==null) {
                List<Expr> choices=null;
                for(Module m: rootmodule.getAllNameableModules()) {
                    Macro mac = m.macros.get(name);
                    if (mac==null) continue;
                    if (choices==null) choices=new ArrayList<Expr>();
                    choices.add(mac.changePos(pos));
                }
                if (choices!=null) {
                    if (choices.size()>1) {
                        choices.clear();
                        choices.add(new ExprBad(pos, name, new ErrorType(pos, "There are multiple macros with the same name; please prepend the module alias in order to remove ambiguity.")));
                    }
                    if (choices.size()>0) return ConstList.make(choices);
                }
            }
            if (match==null) match = rootmodule.macros.get(name);
            if (match==null) match = rootmodule.globals.get(name);
            if (match instanceof Macro) return Util.asList(((Macro)match).changePos(pos));
            if (match!=null) {
                match=ExprUnary.Op.NOOP.make(pos,match);
                TempList<Expr> ans = new TempList<Expr>(1);
                ans.add(match);
                return ans.makeConst();
            }
            Expr th = env.get("this");
            if (th!=null) th = ExprUnary.Op.NOOP.make(pos, th);
            return rootmodule.populate(rootfield, rootsig, rootfun, pos, name, th);
        }
    }

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy module import statement; equals() uses object identity. */
    public static final class Open {
        /** The position in the original model where this "open" statement was declared; never null. */
        public final Pos pos;
        /** The alias for this open declaration; always a nonempty string. */
        public final String alias;
        /** The unmodifiable list of instantiating arguments. */
        public final ConstList<String> args;
        /** The relative filename for the file being imported, without the final ".als" part; always a nonempty string. */
        public final String filename;
        /** Whether this is a private open or not. */
        final boolean isPrivate;
        /** The actual Module object that it points to; null until we resolve it. */
        private Module realModule=null;
        /** Returns the actual Module object that it points to; null if we have not resolved it. */
        public Module getRealModule() { return realModule; }
        /** Constructs an Open object. */
        private Open(Pos pos, boolean isPrivate, String alias, ConstList<String> args, String filename) {
            this.pos=pos; this.isPrivate=isPrivate; this.alias=alias; this.args=args; this.filename=filename;
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
        /** Whether it is private.           */ private final Pos isPrivate;
        /** The original module.             */ private final Module realModule;
        /** The original position.           */ private final Pos pos;
        /** The short name.                  */ private final String name;
        /** The parameters.                  */ private final ConstList<Decl> params;
        /** The return type.                 */ private final Exp returnType;
        /** The body.                        */ private final Exp body;
        private FunAST(Pos pos, Pos isPrivate, Module realModule, String name, List<Decl> params, Exp returnType, Exp body) {
            this.pos=pos;
            this.isPrivate=isPrivate;
            this.realModule=realModule;
            this.name=name; this.params=ConstList.make(params); this.returnType=returnType; this.body=body;
        }
        @Override public String toString() { return name; }
    }

    //============================================================================================================================//

    /** Mutable; this class represents an untypechecked Alloy signature; equals() uses object identity. */
    static final class SigAST {
        private boolean topo=false;             // This flag is set to "true" during resolving
        private final Pos isPrivate;
        private final Module realModule;        // This value is set to its Module during resolving
        private Sig realSig=null;               // This value is set to its corresponding Sig during resolving
        private final List<SigAST> realParents; // This value is set to its corresponding Sig during resolving
        private boolean hint_isLeaf=false;
        private final Pos pos;
        private final String name,fullname;
        private final Pos abs,lone,one,some,subsig,subset;
        private final ConstList<ExpName> parents;
        private final ConstList<Decl> fields;
        private final Exp appendedFact;
        private Pos isOrdered=null;
        private SigAST(Pos pos, Pos isPrivate, String fullname, String name, Pos abs, Pos lone, Pos one, Pos some, Pos subsig, Pos subset,
            List<ExpName> parents, List<Decl> fields, Exp appendedFacts, Module realModule, Sig realSig) {
            this.pos=(pos==null ? Pos.UNKNOWN : pos);
            this.isPrivate=isPrivate;
            this.fullname=fullname;
            this.name=name;
            this.abs=abs;
            this.lone=lone;
            this.one=one;
            this.some=some;
            this.subsig=subsig;
            this.subset=subset;
            this.parents=ConstList.make(parents);
            this.fields=ConstList.make(fields);
            this.appendedFact=appendedFacts;
            this.realModule=realModule;
            this.realSig=realSig;
            this.realParents=new ArrayList<SigAST>(this.parents.size());
        }
        private SigAST(String fullname, String name, List<ExpName> parents, Sig realSig) {
            this(null, null, fullname, name, null, null, null, null, null, null, parents, null, null, null, realSig);
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

    /** Whether we have seen a name containing a dollar sign or not. */
    public boolean seenDollar = false;

    /** Each param is mapped to its corresponding SigAST (or null if we have not resolved it). */
    private final Map<String,SigAST> params = new LinkedHashMap<String,SigAST>(); // Must be LinkedHashMap since the order matters

    /** Each alias is mapped to its corresponding "open" statement. */
    private final Map<String,Open> opens = new LinkedHashMap<String,Open>();

    /** Each sig name is mapped to its corresponding SigAST. */
    private final Map<String,SigAST> sigs = new LinkedHashMap<String,SigAST>();

    /** The list of sigs in this module whose scope shall be deemed "exact" */
    private final IdentitySet<SigAST> exactSigs = new IdentitySet<SigAST>();

    /** The list of params in this module whose scope shall be deemed "exact" */
    private final List<String> exactParams = new ArrayList<String>();

    /** Each func name is mapped to a nonempty list of FunAST objects. */
    private final Map<String,SafeList<FunAST>> funcs = new LinkedHashMap<String,SafeList<FunAST>>();

    /** Each macro name is mapped to a MacroAST object. */
    private final Map<String,Macro> macros = new LinkedHashMap<String,Macro>();

    /** Each assertion name is mapped to either an untypechecked Exp, or a typechecked ExprVar with its value==the assertion. */
    private final Map<String,Object> asserts = new LinkedHashMap<String,Object>();

    /** Each fact name is mapped to either an untypechecked Exp, or a typechecked Expr. */
    private final Map<String,Object> facts = new LinkedHashMap<String,Object>();

    /** The list of (CommandName,Command,Expr) triples; NOTE: duplicate command names are allowed. */
    private final List<Triple<String,Command,Expr>> commands = new ArrayList<Triple<String,Command,Expr>>();

    /** This stores a set of global values; given a unresolved name, we query this map first before all else. */
    private final Map<String,Expr> globals = new LinkedHashMap<String,Expr>();

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
        Module m = CompParser.alloy_parseStream(new ArrayList<Object>(), null, fc, null, -1, "", "");
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
    private void getHelper(int level, SafeList<Module> ans, Object key) {
        if (visitedBy==key) return;
        visitedBy=key;
        ans.add(this);
        for(Map.Entry<String,Open> i:opens.entrySet()) {
            if (level>0 && i.getValue().isPrivate) continue;
            Module m=i.getValue().realModule;
            if (m!=null) m.getHelper(level<0 ? (-1) : (level+1), ans, key);
        }
    }

    /** Return the list containing THIS MODULE and all modules reachable from this module. */
    public SafeList<Module> getAllReachableModules() {
        SafeList<Module> ans=new SafeList<Module>();
        getHelper(-1, ans, new Object()); // The object must be new, since we need it to be a unique key
        return ans.dup();
    }

    /** Return the list containing THIS MODULE and all modules nameable from this module. */
    private SafeList<Module> getAllNameableModules() {
        SafeList<Module> ans=new SafeList<Module>();
        getHelper(0, ans, new Object()); // The object must be new, since we need it to be a unique key
        return ans.dup();
    }

    /** Return the list containing UNIV, SIGINT, SEQIDX, NONE, and all sigs defined in this module or a reachable submodule. */
    public ConstList<Sig> getAllReachableSigs() {
        TempList<Sig> x = new TempList<Sig>();
        x.add(UNIV);
        x.add(SIGINT);
        x.add(SEQIDX);
        x.add(NONE);
        for(Module m:getAllReachableModules())
          for(Map.Entry<String,SigAST> s: m.sigs.entrySet())
            if (s.getValue().realSig!=null)
               x.add(s.getValue().realSig);
        return x.makeConst();
    }

    /** Lookup non-fully-qualified SigAST/FunAST/Assertion from the current module; it skips PARAMs. */
    private List<Object> getRawNQS (Module start, int r, String name) {
        // (r&1)!=0 => SigAST  (r&2) != 0 => ExprVar whose expr is the value of an assertion    (r&4)!=0 => FunAST
        List<Object> ans=new ArrayList<Object>();
        for(Module m:getAllNameableModules()) {
            if ((r&1)!=0) { SigAST x=m.sigs.get(name); if (x!=null) if (m==start || x.isPrivate==null) ans.add(x); }
            if ((r&2)!=0) { Object x=m.asserts.get(name); if (x instanceof Expr) ans.add(x); }
            if ((r&4)!=0) { SafeList<FunAST> x=m.funcs.get(name); if (x!=null) for(FunAST y:x) if (m==start || y.isPrivate==null) ans.add(y); }
        }
        return ans;
    }

    /** Lookup a fully-qualified SigAST/FunAST/Assertion from the current module; it skips PARAMs. */
    private List<Object> getRawQS (int r, String name) {
        // (r&1)!=0 => SigAST  (r&2) != 0 => ExprVar whose expr is the value of an assertion    (r&4)!=0 => FunAST
        List<Object> ans=new ArrayList<Object>();
        Module u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        for(int level=0; ;level++) {
            int i=name.indexOf('/');
            if (i<0) {
                if ((r&1)!=0) { SigAST x=u.sigs.get(name); if (x!=null) if (level==0 || x.isPrivate==null) ans.add(x); }
                if ((r&2)!=0) { Object x=u.asserts.get(name); if (x instanceof Expr) ans.add(x); }
                if ((r&4)!=0) { SafeList<FunAST> x=u.funcs.get(name); if (x!=null) for(FunAST y:x) if (level==0 || y.isPrivate==null) ans.add(y); }
                if (ans.size()==0) return u.getRawNQS(this,r,name); // If nothing at this module, then do a non-qualified search from this module
                return ans;
            }
            String alias=name.substring(0,i);
            Open uu=u.opens.get(alias);
            if (uu==null || uu.realModule==null) return ans; // may happen during the initial "module"
            if (level>0 && uu.isPrivate) return ans; // that means the module is imported privately
            u=uu.realModule;
            name=name.substring(i+1);
        }
    }

    /** Lookup a SigAST from the current module (and it will also search this.params) */
    private SigAST getRawSIG (Pos pos, String name) throws Err {
        List<Object> s;
        SigAST s2=null;
        if (name.equals("sig$") || name.equals("field$")) if (world!=null) {
            s2 = world.sigs.get(name);
            if (s2!=null) return s2;
        }
        if (name.equals("univ"))    return UNIVast;
        if (name.equals("Int"))     return SIGINTast;
        if (name.equals("seq/Int")) return SEQIDXast;
        if (name.equals("none"))    return NONEast;
        if (name.indexOf('/')<0) {
            s=getRawNQS(this, 1, name);
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
    public Module getRootModule() { return world; }

    /** Returns the text of the "MODULE" line at the top of the file; "unknown" if the line has not be parsed from the file yet. */
    public String getModelName() { return moduleName; }

    /** Returns an unmodifiable copy of the current list of OPEN statements. */
    public ConstList<Open> getOpens() {
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
        boolean nextIsExact = false;
        if (list!=null) for(ExpName expr: list) {
            if (expr==null) { nextIsExact=true; continue; }
            String name=expr.name;
            dup(expr.span(), name, true);
            if (path.length()==0) {
                SigAST newSig = addSig(null, expr.span(), name, null, null, null, null, null, null, null, null, null);
                if (nextIsExact) exactSigs.add(newSig);
            } else {
                params.put(name, null);
                if (nextIsExact) exactParams.add(name);
            }
            nextIsExact=false;
        }
        this.status=1; // This line must be at the end, since "addSig" will otherwise bump the status value to 3
    }

    /** Add util/sequniv to the list of declarations. */
    void addSeq(Pos pos) throws Err {
        int oldStatus=status;
        status=0;
        try {
            addOpen(pos, null, new ExpName(pos,"util/sequniv"), null, new ExpName(pos,"seq"));
        } finally {
            status=oldStatus;
        }
    }

    /** Add an OPEN declaration. */
    void addOpen(Pos pos, Pos isPrivate, ExpName name, List<ExpName> args, ExpName alias) throws Err {
        if (status>2) throw new ErrorSyntax(pos,
           "The \"open\" declaration must occur before any\n" + "sig/pred/fun/fact/assert/check/run command.");
        status=2;
        String as = (alias==null ? "" : alias.name);
        if (name.name.length()==0) throw new ErrorSyntax(name.span(), "The filename cannot be empty.");
        if (as.indexOf('@')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'@\' character");
        if (as.indexOf('/')>=0) throw new ErrorSyntax(alias.span(), "Alias must not contain the \'/\' character");
        if (as.length()==0) {
            as="open$"+(1+opens.size());
            if (args==null || args.size()==0) {
              for(int i=0; ; i++) {
                if (i>=name.name.length()) { as=name.name; break; }
                char c=name.name.charAt(i);
                if ((c>='a' && c<='z') || (c>='A' && c<='Z')) continue;
                if (i==0) break;
                if (!(c>='0' && c<='9') && c!='_' && c!='\'' && c!='\"') break;
              }
            }
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
        x=new Open(pos, isPrivate!=null, as, newlist.makeConst(), name.name);
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
               // FIXTHIS: This detects for the Alloy3 behavior of util/ordering.als
               String sn=sub.modulePos.filename.toLowerCase(Locale.US).replace('\\', '/');
               if (kn.equals("elem") && sub.sigs.size()==1 && sn.endsWith("/util/ordering.als") &&
                  vv!=Module.UNIVast && vv!=Module.SIGINTast && vv!=Module.SEQIDXast && vv!=Module.NONEast)
                     vv.isOrdered = open.pos;
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
    SigAST addSig(List<ExpName> hints, Pos pos, String name, Pos isAbstract, Pos isLone, Pos isOne, Pos isSome, Pos isPrivate,
    ExpName par, List<ExpName> parents, List<Decl> fields, Exp fact) throws Err {
        pos = pos.merge(isAbstract).merge(isLone).merge(isOne).merge(isSome);
        status=3;
        dup(pos, name, true);
        String full = (path.length()==0) ? "this/"+name : path+"/"+name;
        Pos subset=null, subsig=null;
        if (par!=null) {
            if (par.name.charAt(0)=='e') { subsig=par.span().merge(parents.get(0).span()); }
            else { subset=par.span(); for(ExpName p:parents) subset=p.span().merge(subset); }
        }
        SigAST obj=new SigAST(pos, isPrivate, full, name, isAbstract,isLone,isOne,isSome,subsig,subset, parents, fields,fact,this,null);
        if (hints!=null) for(ExpName hint:hints) if (hint.name.equals("leaf")) {obj.hint_isLeaf=true; break;}
        sigs.put(name, obj);
        return obj;
    }

    /** Add an enumeration. */
    void addEnum(Pos pos, Pos priv, ExpName name, List<ExpName> parents, List<ExpName> atoms, Pos closingBracket) throws Err {
        ExpName LEAF = new ExpName(null,"leaf");
        ExpName EXTENDS = new ExpName(null, "extends");
        ExpName THIS = new ExpName(null, "this/"+name);
        List<ExpName> THESE = Arrays.asList(THIS);
        if (atoms==null || atoms.size()==0) throw new ErrorSyntax(pos, "Enumeration must contain at least one name.");
        if (parents!=null) parents = new ArrayList<ExpName>(parents);
        ExpName inOrExtend = (parents!=null && parents.size()>0) ? parents.remove(parents.size()-1) : null;
        if (inOrExtend!=null && inOrExtend.name.charAt(0)=='i') throw new ErrorSyntax(pos, "Enumeration signatures cannot derive from a subset signature.");
        addSig(Arrays.asList(LEAF), name.pos, name.name, name.pos, null, null, null, priv, inOrExtend, parents, null, null);
        for(ExpName a:atoms) addSig(null, a.pos, a.name, null, null, a.pos, null, priv, EXTENDS, THESE, null, null);
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
            oldS.realSig = new SubsetSig(pos, parents, fullname, oldS.subset, oldS.lone, oldS.one, oldS.some, oldS.isOrdered, oldS.isPrivate, null);
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
            oldS.realSig = new PrimSig(pos, p, fullname, oldS.abs, oldS.lone, oldS.one, oldS.some, oldS.subsig, oldS.isOrdered, oldS.isPrivate, null, oldS.hint_isLeaf);
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

    /** Add a MACRO declaration. */
    void addMacro(Pos p, Pos isPrivate, String n, List<ExpName> decls, Exp v) throws Err {
        //if (1==1) throw new ErrorSyntax(p, "LET declaration is allowed only inside a toplevel paragraph.");
        ConstList<ExpName> ds = ConstList.make(decls);
        status=3;
        dup(p, n, true);
        for(int i=0; i<ds.size(); i++) for(int j=i+1; j<ds.size(); j++)
          if (ds.get(i).name.equals(ds.get(j).name))
             throw new ErrorSyntax(ds.get(j).span(), "The parameter name \""+ds.get(j).name+"\" cannot appear more than once.");
        Macro ans = new Macro(p, isPrivate, this, n, ds, v);
        Macro old = macros.put(n, ans);
        if (old!=null) { macros.put(n, old); throw new ErrorSyntax(p, "You cannot declare more than one macro with the same name \""+n+"\" in the same file."); }
     }

    /** Add a FUN or PRED declaration. */
    void addFunc(Pos p, Pos isPrivate, String n, Exp f, List<Decl> decls, Exp t, Exp v) throws Err {
        if (decls==null) decls=new ArrayList<Decl>(); else decls=new ArrayList<Decl>(decls);
        if (f!=null) decls.add(0, new Decl(null, null, null, Util.asList(new ExpName(f.span(), "this")), f));
        for(Decl d:decls) {
            if (d.isPrivate!=null) {
                ExpName name = d.names.get(0);
                throw new ErrorSyntax(d.isPrivate.merge(name.pos), "Function parameter \""+name.name+"\" is always private already.");
            }
            if (d.disjoint2!=null) {
                ExpName name = d.names.get(d.names.size()-1);
                throw new ErrorSyntax(d.disjoint2.merge(name.pos), "Function parameter \""+name.name+"\" cannot be bound to a 'disjoint' expression.");
            }
        }
        status=3;
        dup(p, n, false);
        ExpName dup = Decl.findDuplicateName(decls);
        if (dup!=null) throw new ErrorSyntax(dup.span(), "The parameter name \""+dup.name+"\" cannot appear more than once.");
        FunAST ans = new FunAST(p, isPrivate, this, n, decls, t, v);
        SafeList<FunAST> list = funcs.get(n);
        if (list==null) { list = new SafeList<FunAST>(); funcs.put(n, list); }
        list.add(ans);
    }

    /** Each FunAST will now point to a bodyless Func object. */
    private JoinableList<Err> resolveFuncDecls(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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
                f.realFunc = new Func(f.pos, f.isPrivate, fullname, tmpvars.makeConst(), ret);
                rep.typecheck(""+f.realFunc+", RETURN: "+f.realFunc.returnDecl.type+"\n");
            } catch(Err ex) { errors = errors.append(ex); }
        }
        return errors;
    }

    /** Each Func's body will now be typechecked Expr object. */
    private JoinableList<Err> resolveFuncBodys(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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
        asserts.put(name, new ExpUnary(value.span().merge(pos), ExprUnary.Op.NOOP, value));
        return name;
    }

    /** Each assertion name now points to a typechecked Expr rather than an untypechecked Exp. */
    private JoinableList<Err> resolveAssertions(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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
        facts.put(name, new ExpUnary(value.span().merge(pos), ExprUnary.Op.NOOP, value));
    }

    /** Each fact name now points to a typechecked Expr rather than an untypechecked Exp; we'll also add the sig appended facts. */
    private JoinableList<Err> resolveFacts(A4Reporter rep, JoinableList<Err> errors, List<ErrorWarning> warns) throws Err {
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
                ExprVar THIS = ExprVar.make(null, "this", s.oneOf());
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
    void addCommand(Pos p, String n, boolean c, int o, int b, int seq, int exp, List<Pair<Sig,Integer>> s, ExpName label) throws Err {
        if (label!=null) p=Pos.UNKNOWN.merge(p).merge(label.pos);
        status=3;
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        String labelName = (label==null || label.name.length()==0) ? n : label.name;
        commands.add(new Triple<String,Command,Expr>(n, new Command(p, labelName, c, o, b, seq, exp, s), ExprConstant.TRUE));
    }

    /** Add a COMMAND declaration. */
    void addCommand(Pos p, Exp e, boolean c, int o, int b, int seq, int exp, List<Pair<Sig,Integer>> s, ExpName label) throws Err {
        if (label!=null) p=Pos.UNKNOWN.merge(p).merge(label.pos);
        status=3;
        String n;
        if (c) n=addAssertion(p,"check$"+(1+commands.size()),e);
           else addFunc(e.span().merge(p), Pos.UNKNOWN, n="run$"+(1+commands.size()), null, new ArrayList<Decl>(), null, e);
        String labelName = (label==null || label.name.length()==0) ? n : label.name;
        commands.add(new Triple<String,Command,Expr>(n, new Command(e.span().merge(p), labelName, c, o, b, seq, exp, s), ExprConstant.TRUE));
    }

    /** Each command now points to a typechecked Expr. */
    private void resolveCommands() throws Err {
        Sig[] exactSigs = new Sig[this.exactSigs.size()];
        int i=0;
        for(SigAST s: this.exactSigs) { exactSigs[i]=s.realSig; i++; }
        for(i=0; i<commands.size(); i++) {
            String cname=commands.get(i).a;
            Command cmd=commands.get(i).b;
            Expr e=null;
            if (cmd.check) {
                List<Object> m=getRawQS(2, cname); // We prefer assertion in the topmost module
                if (m.size()==0 && cname.indexOf('/')<0) m=getRawNQS(this, 2, cname);
                if (m.size()>1) unique(cmd.pos, cname, m);
                if (m.size()<1) throw new ErrorSyntax(cmd.pos, "The assertion \""+cname+"\" cannot be found.");
                e = ((ExprVar)(m.get(0))).expr.not();
            } else {
                List<Object> m=getRawQS(4, cname); // We prefer fun/pred in the topmost module
                if (m.size()==0 && cname.indexOf('/')<0) m=getRawNQS(this, 4, cname);
                if (m.size()>1) unique(cmd.pos, cname, m);
                if (m.size()<1) throw new ErrorSyntax(cmd.pos, "The predicate/function \""+cname+"\" cannot be found.");
                e = ((FunAST)(m.get(0))).realFormula;
            }
            TempList<Pair<Sig,Integer>> sc=new TempList<Pair<Sig,Integer>>(cmd.scope.size());
            for(Pair<Sig,Integer> et:cmd.scope) {
                SigAST s=getRawSIG(et.a.pos, et.a.label);
                if (s==null) throw new ErrorSyntax(et.a.pos, "The sig \""+et.a.label+"\" cannot be found.");
                sc.add(new Pair<Sig,Integer>(s.realSig, et.b));
            }
            cmd = cmd.make(sc.makeConst());
            cmd = cmd.make(exactSigs);
            commands.set(i, new Triple<String,Command,Expr>(cname, cmd, e));
        }
    }

    /** Return an unmodifiable list of all commands in this module. */
    public ConstList<Command> getAllCommands() {
        TempList<Command> ans = new TempList<Command>(commands.size());
        for(Triple<String,Command,Expr> c:commands) ans.add(c.b);
        return ans.makeConst();
    }

    /** Return an unmodifiable list of all commands (and each command's associated formula) in this module. */
    public ConstList<Pair<Command,Expr>> getAllCommandsWithFormulas() {
        TempList<Pair<Command,Expr>> ans = new TempList<Pair<Command,Expr>>(commands.size());
        for(Triple<String,Command,Expr> c:commands) ans.add(new Pair<Command,Expr>(c.b, c.c));
        return ans.makeConst();
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
        List<Module> modules = root.getAllReachableModules().makeCopy();
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
        for(final Module m:modules) for(final Map.Entry<String,SigAST> e:m.sigs.entrySet()) resolveSig(sorted, e.getValue());
        // Add the fields to the sigs in topologically sorted order (since fields in subsigs are allowed to refer to parent's fields)
        for(final SigAST oldS:sorted) {
           // When typechecking each field:
           // * it is allowed to refer to earlier fields in the same SIG or in any visible ancestor sig
           // * it is allowed to refer to visible sigs
           // * it is NOT allowed to refer to any predicate or function
           // For example, if A.als opens B.als, and B/SIGX extends A/SIGY,
           // then B/SIGX's fields cannot refer to A/SIGY, nor any fields in A/SIGY)
           final Sig s = oldS.realSig;
           final Module m = oldS.realModule;
           final Context cx = new Context(m);
           final ExpName dup = Decl.findDuplicateName(oldS.fields);
           if (dup!=null) throw new ErrorSyntax(dup.span(), "sig \""+s+"\" cannot have 2 fields named \""+dup.name+"\"");
           List<Field> disjoint2 = new ArrayList<Field>();
           for(final Decl d:oldS.fields) {
              // The name "this" does matter, since the parser and the typechecker both refer to it as "this"
              final ExprVar THIS = ExprVar.make(null, "this", s.oneOf());
              cx.rootfield = true;
              cx.rootsig = oldS;
              cx.put("this", THIS);
              Expr bound = d.expr.check(cx, warns).resolve_as_set(warns), disjA=null, disjF=ExprConstant.TRUE;
              cx.remove("this");
              for(final ExpName n:d.names) {
                 final Field f = s.addTrickyField(d.span(), d.isPrivate, null, n.name, THIS, bound);
                 rep.typecheck("Sig "+s+", Field "+f.label+": "+f.type+"\n");
                 if (d.disjoint2!=null) disjoint2.add(f);
                 if (d.disjoint==null) continue;
                 if (disjA==null) { disjA=f; continue; }
                 disjF = ExprBinary.Op.AND.make(d.disjoint, null, disjA.intersect(f).no(), disjF);
                 disjA = disjA.plus(f);
              }
              if (d.disjoint==null || disjF==ExprConstant.TRUE) continue;
              if (!disjF.errors.isEmpty()) { errors=errors.join(disjF.errors); continue; }
              String name = s + "$disjoint";
              m.facts.put(name, disjF);
              rep.typecheck("Fact " + name + ": " + disjF.type+"\n");
           }
           if (s.isOne==null && disjoint2.size()>0) {
               Expr formula = null;
               ExprVar THIS = ExprVar.make(null, "this", s.oneOf());
               ExprVar THAT = ExprVar.make(null, "that", s.oneOf());
               for(Field f:disjoint2) formula = THIS.join(f).intersect(THAT.join(f)).no().and(formula);
               formula = THIS.equal(THAT).not().implies(formula).forAll(THIS).forAll(THAT);
               m.facts.put(s + "$disj", formula);
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
        // Now, add the meta sigs and fields if needed
        if (root.seenDollar) {
            boolean hasMetaSig=false, hasMetaField=false;
            ExpName EXTENDS = new ExpName(null, "extends");
            ExpName THIS = new ExpName(null, "univ");
            List<ExpName> THESE = Arrays.asList(THIS);
            SigAST metasig   = root.addSig(null, Pos.UNKNOWN, "sig$", Pos.UNKNOWN, null, null, null, null, EXTENDS, THESE, null, null);
            SigAST metafield = root.addSig(null, Pos.UNKNOWN, "field$", Pos.UNKNOWN, null, null, null, null, EXTENDS, THESE, null, null);
            metasig.topo = true;
            metasig.realParents.add(UNIVast);
            metasig.realSig = new PrimSig(Pos.UNKNOWN, UNIV, "this/sig$", Pos.UNKNOWN, null, null, null, null, null, null, Pos.UNKNOWN, false);
            metafield.topo = true;
            metafield.realParents.add(UNIVast);
            metafield.realSig = new PrimSig(Pos.UNKNOWN, UNIV, "this/field$", Pos.UNKNOWN, null, null, null, null, null, null, Pos.UNKNOWN, false);
            PrimSig metaSig = (PrimSig)(metasig.realSig);      sorted.add(metasig);
            PrimSig metaField = (PrimSig)(metafield.realSig);  sorted.add(metafield);
            for(Module m:modules) for(SigAST sig: new ArrayList<SigAST>(m.sigs.values())) if (m!=root || (sig!=metasig && sig!=metafield)) {
                Sig s = sig.realSig;
                String slab = sig.name;
                SigAST ast = m.addSig(null, Pos.UNKNOWN, slab+"$", null, null, Pos.UNKNOWN, null, s.isPrivate, EXTENDS, THESE, null, null);
                ast.topo=true;
                ast.realParents.add(metasig);
                ast.realSig = new PrimSig(Pos.UNKNOWN, metaSig, m.paths.contains("") ? "this/"+ast.name : (m.paths.get(0)+"/"+ast.name), null, null, ast.one, null, null, null, ast.isPrivate, Pos.UNKNOWN, false);
                sorted.add(ast);
                hasMetaSig=true;
                for(Field field: s.getFields()) {
                    ast = m.addSig(null, Pos.UNKNOWN, slab+"$"+field.label, null, null, Pos.UNKNOWN, null, field.isPrivate, EXTENDS, THESE, null, null);
                    ast.topo=true;
                    ast.realParents.add(metafield);
                    ast.realSig = new PrimSig(Pos.UNKNOWN, metaField, m.paths.contains("") ? "this/"+ast.name : (m.paths.get(0)+"/"+ast.name), null, null, ast.one, null, null, null, ast.isPrivate, Pos.UNKNOWN, false);
                    sorted.add(ast);
                    hasMetaField=true;
                }
            }
            if (hasMetaSig==false) root.facts.put("sig$fact", metaSig.no().and(metaField.no()));
            else if (hasMetaField==false) root.facts.put("sig$fact", metaField.no());
        }
        // Typecheck the function declarations
        for(Module x:modules) errors=x.resolveFuncDecls(rep, errors, warns);
        if (!errors.isEmpty()) throw errors.get(0);
        // Typecheck the function bodies, assertions, and facts (which can refer to function declarations)
        for(Module x:modules) {
            errors=x.resolveFuncBodys(rep,errors,warns);
            errors=x.resolveAssertions(rep,errors,warns);
            errors=x.resolveFacts(rep,errors,warns);
            // also, we can collect up all the exact sigs and add them to the root module's list of exact sigs
            if (x!=root) for(SigAST s:x.exactSigs) root.exactSigs.add(s);
            for(String n:x.exactParams) { SigAST sig=x.params.get(n); if (sig!=null) root.exactSigs.add(sig); }
        }
        if (!errors.isEmpty()) throw errors.get(0);
        // Typecheck the run/check commands (which can refer to function bodies and assertions)
        root.resolveCommands();
        if (!errors.isEmpty()) throw errors.get(0);
        for(ErrorWarning w:warns) rep.warning(w);
        for(SigAST s: root.exactSigs) rep.debug("Forced to be exact: "+s+"\n");
        return root;
    }

    //============================================================================================================================//

    /** Add a global expression; if the name already exists, it is removed first. */
    public void addGlobal(String name, Expr value) {
        globals.put(name, value);
    }

    /** Resolve the name based on the current context and this module. */
    private ConstList<Expr> populate(boolean rootfield, SigAST rootsig, boolean rootfun, Pos pos, String fullname, Expr THIS) {
        // Return object can be Func(with > 0 arguments) or Expr
        final String name = (fullname.charAt(0)=='@') ? fullname.substring(1) : fullname;
        boolean fun = (rootsig!=null && !rootfield) || (rootsig==null && !rootfun);
        if (name.equals("univ"))    { Expr a = ExprUnary.Op.NOOP.make(pos, UNIV);   return ConstList.make(1,a); }
        if (name.equals("Int"))     { Expr a = ExprUnary.Op.NOOP.make(pos, SIGINT); return ConstList.make(1,a); }
        if (name.equals("seq/Int")) { Expr a = ExprUnary.Op.NOOP.make(pos, SEQIDX); return ConstList.make(1,a); }
        if (name.equals("none"))    { Expr a = ExprUnary.Op.NOOP.make(pos, NONE);   return ConstList.make(1,a); }
        if (name.equals("iden"))    { Expr a = ExprConstant.Op.IDEN.make(pos, 0);   return ConstList.make(1,a); }
        if (name.equals("sig$") || name.equals("field$")) if (world!=null) {
            SigAST s = world.sigs.get(name);
            if (s!=null) { Expr s2 = ExprUnary.Op.NOOP.make(pos, s.realSig); return ConstList.make(1, s2); }
        }
        final TempList<Expr> ans1 = new TempList<Expr>();
        final List<Object> ans = name.indexOf('/')>=0 ? getRawQS(fun?5:1, name) : getRawNQS(this, fun?5:1, name);
        final SigAST param = params.get(name); if (param!=null && !ans.contains(param)) ans.add(param);
        for(Object x: ans) {
            if (x instanceof SigAST) {
                SigAST y=(SigAST)x;
                ans1.add(ExprUnary.Op.NOOP.make(pos, y.realSig, null, y.realModule==this?0:1000)); // penalty of 1000
            }
            else if (x instanceof FunAST) {
                FunAST y=(FunAST)x;
                Func f = y.realFunc;
                int fn = f.params.size();
                int penalty = (y.realModule==this ? 0 : 1000); // penalty of 1000
                if (fn>0 && rootsig!=null && THIS!=null && THIS.type.hasArity(1) && f.params.get(0).type.intersects(THIS.type)) {
                    // If we're inside a sig, and there is a unary variable bound to "this",
                    // we should consider it as a possible FIRST ARGUMENT of a fun/pred call
                    ConstList<Expr> t = Util.asList(THIS);
                    ans1.add(fn==1 ? ExprCall.make(pos,null,f,t,1+penalty) : ExprBadCall.make(pos,null,f,t,1+penalty)); // penalty of 1
                }
                ans1.add(fn==0 ? ExprCall.make(pos,null,f,null,penalty) : ExprBadCall.make(pos,null,f,null,penalty));
            }
        }
        // Within a field decl
        // (1) Can refer to any visible sig/param (but you cannot call any function or predicates)
        // (2) Can refer to field in this sig (defined earlier than you), and fields in any visible ancestor sig
        // Within an appended facts
        // (1) Can refer to any visible sig/param/func/predicate
        // (2) Can refer to any visible field (but if it is in this sig or a parent sig, we'll prepend "this." unless it has '@')
        // Within a function paramDecl/returnDecl
        // (1) Cannot call
        // (2) But can refer to anything else visible.
        // All else: we can call, and can refer to anything visible.
        for(Module m: getAllNameableModules()) {
          for(Map.Entry<String,SigAST> s:m.sigs.entrySet()) if (m==this || s.getValue().isPrivate==null) {
            int fi=(-1), fn=s.getValue().realSig.getFields().size(); // fn is the number of fields that are typechecked so far
            for(Decl d: s.getValue().fields) {
              for(ExpName label: d.names) {
                fi++;
                if (fi<fn && (m==this || d.isPrivate==null) && label.name.equals(name)) {
                   Field f=s.getValue().realSig.getFields().get(fi);
                   Expr x=null;
                   int penalty = (s.getValue().realModule==this ? 0 : 1000); // penalty of 1000
                   if (rootsig==null)
                      { x=ExprUnary.Op.NOOP.make(pos,f,null,penalty); }
                   else if (rootsig.realSig.isSameOrDescendentOf(f.sig))
                      { x=ExprUnary.Op.NOOP.make(pos,f,null,penalty); if (fullname.charAt(0)!='@') x=THIS.join(x); }
                   else if (!rootfield)
                      { x=ExprUnary.Op.NOOP.make(pos, f, null, 1+penalty); } // penalty of 1
                   if (x!=null) ans1.add(x);
                }
              }
            }
          }
        }
        return ans1.makeConst();
    }
}
