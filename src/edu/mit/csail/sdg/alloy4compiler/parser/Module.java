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

import java.util.Collection;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorType;
import edu.mit.csail.sdg.alloy4.ErrorAPI;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4.ConstList.TempList;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprCall;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

/** Mutable; this class represents one Alloy module; equals() uses object identity. */

public final class Module {

    /** The position of the "MODULE" line at the top of the file; never null. */
    public final Pos pos;

    /** The world that this Module belongs to. */
    public final World world;

    /** The simplest path pointing to this Module; it is always equal to this.paths.get(0) */
    public final String path;

    /** The unmodifiable list of paths pointing to this Module; it is always already sorted by Util.slashComparator */
    public final ConstList<String> paths;

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
     * @param paths - the list of paths pointing to this module (it must be pre-sorted using Util.slashComparator)
     */
    Module(World world, Pos pos, List<String> paths) throws Err {
        if (world==null) throw new ErrorAPI(pos, "Module() constructor cannot be called with null World.");
        if (paths.size()==0) throw new ErrorAPI(pos, "Module must have at least 1 path pointing to it.");
        this.world=world;
        this.pos=(pos==null ? Pos.UNKNOWN : pos);
        this.paths=ConstList.make(paths);
        this.path=this.paths.get(0);
    }

    //=============================================================================================================//

    /** This stores the list of run/check commands in the order they were inserted. */
    private final SafeList<Command> commands=new SafeList<Command>();

    /** Add a new command; if this module is not the main module, then this request is silently ignored and returns null. */
    public Command addCommand(Command c) {
        if (path.length()!=0) return null;
        commands.add(c);
        return c;
    }

    /** Return an unmodifiable list of commands in this module. */
    public SafeList<Command> getAllCommands() { return commands.dup(); }

    //=============================================================================================================//

    /**
     * This maps each module parameter to the Sig object that it refers to.
     * <p> Each Sig object could have been defined in any module.
     * <p> Each name cannot be empty, and cannot have "/" or "@".
     */
    private final Map<String,Sig> params=new LinkedHashMap<String,Sig>();

    /**
     * Add a new parameter to a module
     *
     * @param name - the name of the parameter
     * @param target - the sig that this name will refer to
     *
     * @throws ErrorSyntax  if the module already has a parameter, sig, or function/predicate with that name
     *
     * @throws ErrorSyntax if this is the main module
     * @throws ErrorSyntax if the name is "", or if the name contains '/' or '@' or '[' or ']'
     * @throws ErrorSyntax if the module already has a signature or parameter or function/predicate with that name
     */
    public void addParameter(String name, Sig target) throws Err {
        Pos p = this.pos;
        if (target==null) throw new ErrorAPI(p,
            "addParam() encountered a NullPointerException");
        if (target==NONE) throw new ErrorSyntax(p,
            "You cannot instantiate a module using \"none\"");
        if (path.length()==0) throw new ErrorSyntax(p,
            "The main module cannot have parameters.");
        if (name.length()==0) throw new ErrorSyntax(p,
            "Module parameter name cannot be empty.");
        if (name.indexOf('/')>=0) throw new ErrorSyntax(p,
            "Module parameter name \""+name+"\" cannot contain \'/\'");
        if (name.indexOf('@')>=0) throw new ErrorSyntax(p,
            "Module parameter name \""+name+"\" cannot contain \'@\'");
        if (name.indexOf('[')>=0) throw new ErrorSyntax(p,
            "Module parameter name \""+name+"\" cannot contain \'[\'");
        if (name.indexOf(']')>=0) throw new ErrorSyntax(p,
            "Module parameter name \""+name+"\" cannot contain \']\'");
        if (params.containsKey(name)) throw new ErrorSyntax(p,
            "A module cannot have two parameters with the same name: \""+name+"\"");
        if (sigs.containsKey(name)) throw new ErrorSyntax(p,
            "A module cannot have a parameter and a signature with the same name: \""+name+"\"");
        if (funs.containsKey(name)) throw new ErrorSyntax(p,
            "A module cannot have a parameter and a function/predicate with the same name: \""+name+"\"");
        this.params.put(name, target);
    }

    /**
     * Look up a parameter.
     * @param name - the name
     * @return null if the name contains '/', or if there is no such parameter in this module
     */
    public Sig getParameter(String name) { return params.get(name); }

    //=============================================================================================================//

    /**
     * This maps each signature declared in this file to its Sig object.
     * <p> Each Sig object must be originally and solely declared within this module.
     * <p> Each name cannot be empty, and cannot have "/" or "@".
     */
    private final Map<String,Sig> sigs=new LinkedHashMap<String,Sig>();

    /** This stores the same values as this.sigs, but as a list. */
    private final SafeList<Sig> list_of_all_sigs=new SafeList<Sig>();

    /** Returns a unmodifiable list of Sig objects declared originally and solely within this module. */
    public SafeList<Sig> getAllSigs() { return list_of_all_sigs.dup(); }

    /**
     * Create then return a new Sig in this module.
     * @param pos - the position in the original source file
     * @param name - the name of the Sig
     * @param isAbstract - true if the Sig is abstract
     * @param lone - true if the Sig has the "lone" multiplicity
     * @param one - true if the Sig has the "one" multiplicity
     * @param some - true if the Sig has the "some" multiplicity
     *
     * @throws ErrorSyntax  if the module already has a parameter, sig, or function/predicate with that name
     *
     * @throws ErrorType    if isAbstract and isSubset are both true
     * @throws ErrorAPI     if the signature name, or at least one of the field name is empty
     * @throws ErrorSyntax  if the signature name, or at least one of the field name contains '/' or '@'
     * @throws ErrorSyntax  if the signature name contains '[' or ']'
     * @throws ErrorSyntax  if the signature has two or more multiplicities
     * @throws ErrorSyntax  if more than one field has the same name
     */
    public Sig addSig (Pos pos, String name, PrimSig parent,
            boolean isAbstract, boolean lone, boolean one, boolean some,
            boolean isLeaf) throws Err {
        if (params.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have a signature and a parameter with the same name: \""+name+"\"");
        if (sigs.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have two signatures with the same name: \""+name+"\"");
        if (funs.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have a signature and a function/predicate with the same name: \""+name+"\"");
        Sig x=world.makeSIG(pos, this, parent, name, isAbstract, lone, one, some, isLeaf);
        sigs.put(name, x);
        list_of_all_sigs.add(x);
        return x;
    }
    public Sig addSubsetSig (Pos pos, String name, Collection<Sig> parents, boolean isAbstract, boolean lone, boolean one, boolean some) throws Err {
        if (params.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have a signature and a parameter with the same name: \""+name+"\"");
        if (sigs.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have two signatures with the same name: \""+name+"\"");
        if (funs.containsKey(name)) throw new ErrorSyntax(pos,
                "A module cannot have a signature and a function/predicate with the same name: \""+name+"\"");
        Sig x=world.makeSUBSETSIG(pos, this, parents, name, isAbstract, lone, one, some, false);
        sigs.put(name, x);
        list_of_all_sigs.add(x);
        return x;
    }

    /**
     * Create and return a new Sig in this module.
     * @param name - the name of the Sig
     * @param isAbstract - true if the Sig is abstract
     * @param lone - true if the Sig has the "lone" multiplicity
     * @param one - true if the Sig has the "one" multiplicity
     * @param some - true if the Sig has the "some" multiplicity
     *
     * @throws ErrorSyntax  if the module already has a parameter, sig, or function/predicate with that name
     * @throws ErrorAPI     if the signature name is empty
     * @throws ErrorSyntax  if the signature name contains '/' or '@'
     * @throws ErrorSyntax  if the signature name contains '[' or ']'
     * @throws ErrorSyntax  if the signature has two or more multiplicities
     */
    public PrimSig addSig (String name, PrimSig parent, boolean isAbstract, boolean lone, boolean one, boolean some) throws Err {
        if (params.containsKey(name)) throw new ErrorSyntax(pos,
            "A module cannot have a signature and a parameter with the same name: \""+name+"\"");
        if (sigs.containsKey(name)) throw new ErrorSyntax(pos,
            "A module cannot have two signatures with the same name: \""+name+"\"");
        if (funs.containsKey(name)) throw new ErrorSyntax(pos,
            "A module cannot have a signature and a function/predicate with the same name: \""+name+"\"");
        Sig x=world.makeSIG(null, this, parent, name, isAbstract, lone, one, some, false);
        sigs.put(name, x);
        list_of_all_sigs.add(x);
        return (PrimSig)x;
    }

    //=============================================================================================================//

    /** This lists all the facts defined inside this file. */
    private SafeList<Pair<String,Expr>> facts = new SafeList<Pair<String,Expr>>();

    /**
     * Add a new fact.
     * <p> Note: the name does not have to be unique; it is okay for 2 facts to have the same name.
     *
     * @param name - if name==null or name=="", we will automatically pick a new name for it.
     * @param formula - the new fact
     *
     * @throws ErrorType if formula fails to be typechecked
     */
    public void addFact (String name, Expr formula) throws Err {
        if (name==null || name.length()==0) name="fact#"+(1+facts.size());
        //
        A4Reporter.getReporter().typecheck("Fact "+name+": "+formula.type+"\n");
        //
        facts.add(new Pair<String,Expr>(name,formula));
    }

    /** Typecheck if necessary, then return an unmodifiable list of all facts in this module. */
    public SafeList<Pair<String,Expr>> getAllFacts() throws Err {
        return facts.dup();
    }

    //=============================================================================================================//

    /** This lists all the asseritions defined inside this file. */
    private final Map<String,Expr> assertions=new LinkedHashMap<String,Expr>();

    /**
     * Add a new assertion.
     *
     * @param name - if name==null or name=="", this method pick a new unique name for it, and return the new name.
     * @param formula - the new assertion
     *
     * @throws ErrorType if formula fails to be typechecked
     * @throws ErrorSyntax if the name contains '/' or '@'
     * @throws ErrorSyntax if this module already has an assertion with the same name
     */
    public String addAssertion (String name, Expr formula) throws Err {
        if (name==null || name.length()==0) name="assert#"+(1+assertions.size());
        if (name.indexOf('/')>=0) throw new ErrorSyntax(formula.span(), "Assertion name \""+name+"\" cannot contain \'/\'");
        if (name.indexOf('@')>=0) throw new ErrorSyntax(formula.span(), "Asserition name \""+name+"\" cannot contain \'@\'");
        if (assertions.containsKey(name)) throw new ErrorSyntax(formula.span(), "Within the same module, two assertions cannot have the same name.");
        //
        A4Reporter.getReporter().typecheck("Assertion "+name+": "+formula.type+"\n");
        //
        assertions.put(name,formula);
        return name;
    }

    /** Typecheck if necessary, then return the assertion with that name (it returns null if there's no match) */
    public Expr getAssertion(String name) throws Err {
        return assertions.get(name);
    }

    /** Typecheck if necessary, then returns an unmodifiable list of all assertions in this module. */
    public ConstList<Pair<String,Expr>> getAllAssertions() throws Err {
        TempList<Pair<String,Expr>> ans = new TempList<Pair<String,Expr>>(assertions.size());
        for(Map.Entry<String,Expr> e:assertions.entrySet()) {
            ans.add(new Pair<String,Expr>(e.getKey(), e.getValue()));
        }
        return ans.makeConst();
    }

    //=============================================================================================================//

    /** This lists all the functions and predicates defined in this module. */
    private final Map<String,SafeList<Func>> funs=new LinkedHashMap<String,SafeList<Func>>();

    /** This stores the list of all parameter-less functions and predicates defined in this module. */
    private final SafeList<Func> list_of_func0=new SafeList<Func>();

    /** This stores the list of all functions and predicates defined in this module. */
    private final SafeList<Func> list_of_func=new SafeList<Func>();

    /**
     * Constructs a new predicate/function.
     *
     * @param pos - the original position in the file
     * @param name - the name of the predicate/function (it cannot be "", and cannot contain '@' or '/')
     * @param params - the list of parameters (can be null or even an empty list)
     * @param returnType - the return type (null if this is a predicate rather than a function)
     *
     * @throws ErrorSyntax if the module already has a parameter or signature with that name
     *
     * @throws ErrorSyntax if the name is "", or if the name contains '@' or '/'
     * @throws ErrorSyntax if the list of parameters contain duplicate names
     * @throws ErrorSyntax if at least one of the parameter declaration contains a predicate/function call
     * @throws ErrorSyntax if this function's  return type  declaration contains a predicate/function call
     * @throws ErrorAPI    if this is called by the FuncN constructor, but the list of parameter is empty
     */
    public Func addFun(Pos pos, String name, List<ExprVar> params, Expr returnType) throws Err {
        if (this.params.containsKey(name)) throw new ErrorSyntax(pos,
            "Within the same file, a function/predicate cannot have the same name as a polymorphic type.");
        if (this.sigs.containsKey(name)) throw new ErrorSyntax(pos,
            "Within the same file, a function/predicate cannot have the same name as another signature.");
        String fullname = (path.length()==0 ? "this/" : path+"/") + name;
        Func func=new Func(pos, fullname, params, returnType);
        SafeList<Func> list=funs.get(name);
        if (list==null) { list=new SafeList<Func>(); funs.put(name, list); }
        list.add(func);
        list_of_func.add(func);
        if (func.params.size()==0) list_of_func0.add(func);
        return func;
    }

    /** Caches an unmodifiable empty list of functions. */
    private static final SafeList<Func> empty_list_of_func = (new SafeList<Func>(0)).dup();

    /** Typecheck if necessary, then return an unmodifiable list of all predicates/functions with that name. */
    public SafeList<Func> getFunc(String name) throws Err {
        SafeList<Func> answer = funs.get(name);
        return answer==null ? empty_list_of_func : answer.dup();
    }

    /** Typecheck if necessary, then return an unmodifiable list of all parameter-less predicates/functions. */
    public SafeList<Func> getAllFunc0() throws Err {
        return list_of_func0.dup();
    }

    /** Typecheck if necessary, then return an unmodifiable list of all predicates/functions. */
    public SafeList<Func> getAllFunc() throws Err {
        return list_of_func.dup();
    }

    //=============================================================================================================//

    /**
     * Look up an assertion visible from this module.
     * @param name - the name which can either be fully-qualified (eg. "alias/alias/somename") or not
     * @return an empty set if there is no match
     */
    public Set<Expr> lookupAssertion(String name) {
        Expr s;
        Set<Expr> ans=new LinkedHashSet<Expr>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) {
                for(Module u:world.lookupModuleAndSubmodules(p)) {
                    if ((s=u.assertions.get(name))!=null) ans.add(s);
                }
            }
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.lookupModule((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            if ((s=u.assertions.get(name))!=null) ans.add(s);
        }
        return ans;
    }

    /**
     * Look up a parameter, or a signature/function/predicate visible from this module.
     * @param name - the name which can either be fully-qualified (eg. "alias/alias/somename") or not
     * @param look_for_function_and_predicate - true if we want to include functions and predicates as well
     * @return an empty set if there is no match
     */
    public Set<Object> lookupSigOrParameterOrFunctionOrPredicate
    (String name, boolean look_for_function_and_predicate) {
        Sig s;
        SafeList<Func> f;
        Set<Object> ans=new LinkedHashSet<Object>();
        if (name.length()==0 || name.charAt(0)=='/' || name.charAt(name.length()-1)=='/') return ans; // Illegal name
        if (name.indexOf('/')<0) {
            for(String p:paths) {
                for(Module u:world.lookupModuleAndSubmodules(p)) {
                    if ((s=u.sigs.get(name))!=null) ans.add(s);
                    if (look_for_function_and_predicate) if ((f=u.funs.get(name))!=null) ans.addAll(f);
                }
            }
            s=params.get(name); if (s!=null) ans.add(s);
            return ans;
        }
        if (name.startsWith("this/")) name=name.substring(5);
        s=params.get(name); if (s!=null) ans.add(s);
        int i=name.lastIndexOf('/');
        Module u=(i<0) ? this : world.lookupModule((path.length()==0?"":path+"/")+name.substring(0,i));
        if (u!=null) {
            if (i>=0) name=name.substring(i+1);
            if ((s=u.sigs.get(name))!=null) ans.add(s);
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
            //if (ans2!=null) throw new ErrorSyntax(current.pos, "This signature's \""
            //        +name+"\" field conflicts with a parent signature's field with the same name.");
        }
        if (current instanceof SubsetSig) for(Sig p:((SubsetSig)current).parents) {
            Field ans2=lookupField(origin, p, name);
            if (ans==null) ans=ans2;
            //else if (ans2!=null) throw new ErrorSyntax(current.pos, "This signature's \""
            //        +name+"\" field conflicts with a parent signature's field with the same name.");
        }
        return ans;
    }

    /** Look up field "name" from any visible sig (and returns an empty set if there is no match) */
    private Set<Field> lookupField(String name) {
        Set<Field> ans=new LinkedHashSet<Field>();
        for(String p:paths)
          for(Module u:world.lookupModuleAndSubmodules(p))
            for(Sig s:u.list_of_all_sigs)
              for(Field f:s.getFields())
                if (f.label.equals(name))
                  ans.add(f);
        return ans;
    }

    //=============================================================================================================//

    public SafeList<Func> lookupFunctionOrPredicate(String name) {
        SafeList<Func> ans=new SafeList<Func>();
        for(Object x: lookupSigOrParameterOrFunctionOrPredicate(name,true)) {
            if (x instanceof Func)
                ans.add((Func)x);
        }
        return ans;
    }

    //=============================================================================================================//

    /** Resolve the name based on the current context. */
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
                realAns.add(ExprCall.make(pos, (Func)x, null, 0));
            else if (x instanceof Func || x instanceof Expr)
                realAns.add(x);
        }
        return realAns;
    }
}
