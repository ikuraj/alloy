package edu.mit.csail.sdg.alloy4.core;

import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import edu.mit.csail.sdg.alloy4.util.ErrorInternal;
import edu.mit.csail.sdg.alloy4.util.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.util.Pair;
import edu.mit.csail.sdg.alloy4.util.Pos;

/**
 * Mutable; this class represents one instantiation of an Alloy module.
 *
 * @author Felix Chang
 */

public final class Unit {

    public Unit(String prefix) { aliases.add(prefix); }

    /** The list of aliases that point to this Unit; it contains "" if it's the outermost module. */
    public final List<String> aliases=new ArrayList<String>();

    /** The position of the "MODULE" line at the top of the file. */
    public Pos pos=null;

    /**
     * List of all the PARAMETRIC PARAMETERS used in instantiating this file. Each name cannot have "/" or "@".
     * This must be a LinkedHashMap because we depend on the iterator returning them in the original order.
     */
    public final Map<String,ParaSig> params=new LinkedHashMap<String,ParaSig>();

    public void makeModule(Pos pos, List<ExprName> list) {
        this.pos=pos;
        for(ExprName expr:list) {
            String name=expr.name;
            if (params.containsKey(name))
                throw new ErrorSyntax(pos, "You cannot use the same name for more than 1 instantiating parameter!");
            if (sigs.containsKey(name))
                throw new ErrorSyntax(pos, "Within the same file, a signature and a polymorphic parameter cannot have the same name!");
            if (!aliases.contains(""))
                params.put(name, null);
            else
                makeSig(pos, name, false, false, false, false, null, null, new ArrayList<VarDecl>(), null);
        }
    }

    /**
     * This lists all the SIGS defined inside this file.
     * The NAME must not contain any "/" or "@"
     */
    public final Map<String,ParaSig> sigs=new LinkedHashMap<String,ParaSig>();

    public void makeSig(Pos p,String n,boolean fa,boolean fl,boolean fo,boolean fs,List<String> i,String e,List<VarDecl> d,Expr f) {
        ParaSig x=new ParaSig(p,aliases.get(0),n,fa,fl,fo,fs,i,e,d,f);
        if (asserts.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another assertion!");
        if (facts.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another fact!");
        if (funs.containsKey(n)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another function/predicate!");
        if (sigs.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another signature!");
        if (params.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as a polymorphic type!");
        sigs.put(x.name, x);
    }

    private int anonymous_id=0;

    /**
     * This lists all the FACTS defined inside this file.
     * The NAME must not contain any "/" or "@"
     */
    public final Map<String,ParaFact> facts=new LinkedHashMap<String,ParaFact>();

    public void makeFact(Pos p,String n,Expr v) {
        if (n==null || n.length()==0) n="*"+(++anonymous_id)+"*";
        ParaFact x=new ParaFact(p,aliases.get(0),n,v);
        if (asserts.containsKey(n)) throw x.syntaxError("Within the same file, a fact cannot have the same name as another assertion!");
        if (facts.containsKey(n)) throw x.syntaxError("Within the same file, a fact cannot have the same name as another fact!");
        if (funs.containsKey(n)) throw x.syntaxError("Within the same file, a fact cannot have the same name as another function/predicate!");
        if (sigs.containsKey(n)) throw x.syntaxError("Within the same file, a fact cannot have the same name as another signature!");
        if (params.containsKey(n)) throw x.syntaxError("Within the same file, a fact cannot have the same name as a polymorphic type!");
        facts.put(n,x);
    }

    // This lists all the ASSERTS defined inside this file.
    // The NAME must not contain any "/" or "@"
    public final Map<String,ParaAssert> asserts=new LinkedHashMap<String,ParaAssert>();
    public String makeAssert(Pos p,String n,Expr v) {
        if (n==null || n.length()==0) n="*"+(++anonymous_id)+"*";
        ParaAssert x=new ParaAssert(p,aliases.get(0),n,v);
        if (asserts.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another assertion!");
        if (facts.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another fact!");
        if (funs.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another function/predicate!");
        if (sigs.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another signature!");
        if (params.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as a polymorphic type!");
        asserts.put(n,x);
        return n;
    }

    // This lists all the FUNCTIONS defined inside this file.
    // The NAME must not contain any "/" or "@"
    public final Map<String,List<ParaFun>> funs=new LinkedHashMap<String,List<ParaFun>>();
    public void makeFun(Pos p,String n,ExprName f,List<VarDecl> d,Expr t,Expr v) {
        List<ParaFun> list=funs.get(n);
        if (list==null) list=new ArrayList<ParaFun>();
        //
        d=new ArrayList<VarDecl>(d);
        if (f!=null) d.add(0, new VarDecl(p, "this", ExprUnary.Op.ONEMULT.make(p,f)));
        //
        ParaFun x=new ParaFun(p, aliases.get(0), n, d, t, v);
        if (asserts.containsKey(x.name)) throw x.syntaxError("Within the same file, a function/predicate cannot have the same name as another assertion!");
        if (facts.containsKey(x.name)) throw x.syntaxError("Within the same file, a function/predicate cannot have the same name as another fact!");
        if (sigs.containsKey(x.name)) throw x.syntaxError("Within the same file, a function/predicate cannot have the same name as another signature!");
        if (params.containsKey(x.name)) throw x.syntaxError("Within the same file, a function/predicate cannot have the same name as a polymorphic type!");
        list.add(x);
        funs.put(x.name,list);
    }

    // This maps between all the INCLUDED MODULES, and their ALIAS.
    // The ALIAS must not contain any "/" or "@"
    public final Map<String,Unit> opens=new LinkedHashMap<String,Unit>();
    public final Map<String,ParaOpen> opencmds=new LinkedHashMap<String,ParaOpen>();
    public void makeOpen(Pos p, String n, List<ExprName> l, String a) {
        ParaOpen x=new ParaOpen(p,aliases.get(0),a,l,n);
        if (opencmds.containsKey(x.name)) throw x.syntaxError("You cannot import more than 1 module using the same alias!");
        opencmds.put(x.name, x);
    }

    // This stores the list of RUN/CHECK commands, in the order they appear in the file.
    public final List<ParaRuncheck> runchecks=new ArrayList<ParaRuncheck>();

    public void makeRuncheck(Pos p,String n,boolean c,int o,int exp,Map<String,Integer> s, String label, List<ExprName> opts) {
        if (!aliases.contains("")) return;
        runchecks.add(new ParaRuncheck(p, aliases.get(0), n, c, o, exp, s, label, opts));
    }

    public void makeRuncheck(Pos p,Expr e,boolean c,int o,int exp,Map<String,Integer> s, String label, List<ExprName> opts) {
        if (!aliases.contains("")) return;
        String n;
        if (c) n=makeAssert(p,"",e); else { n="*"+(++anonymous_id)+"*"; makeFun(p,n,null,new ArrayList<VarDecl>(),null,e); }
        runchecks.add(new ParaRuncheck(p, aliases.get(0), n, c, o, exp, s, label, opts));
    }

    /*======================================================================*/

    /**
     * Return the Unit corresponding to the "path" parameter relative to this Unit (and returns null if not found).
     *
     * <p/>  For example, if this unit is "a/b", then
     * <br/> 1) lookupPath("c/d") and lookupPath("this/c/d") will both return the Unit known as "a/b/c/d"
     * <br/> 2) lookupPath("") and lookupPath("this") will both return the Unit known as "a/b"
     */
    public Unit lookupPath(String path) {
        Unit u=this;
        if (path.startsWith("this/")) path=path.substring(5);
        if (path.length()==0 || path.equals("this")) return this;
        while(true) {
            int i=path.indexOf('/');
            if (i<0) return u.opens.get(path);
            u=u.opens.get(path.substring(0,i));
            if (u==null) return null;
            path=path.substring(i+1,path.length());
        }
    }

    /*======================================================================*/

    private void lookupNQsig_noparam (String name, Set<Object> ans) { // It ignores "params"
        Para x=sigs.get(name);
        if (x!=null) ans.add(x);
        for(Map.Entry<String,Unit> i:opens.entrySet()) i.getValue().lookupNQsig_noparam(name,ans);
    }

    private Para lookupQsig_noparam (String name) { // It ignores "params"
        Unit u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) return u.sigs.get(name);
            u=u.opens.get(name.substring(0,i));
            if (u==null) return null;
            name=name.substring(i+1,name.length());
        }
    }

    public Set<Object> lookup_sigORparam (String name) { // Will search "params" too, if at the CURRENT LEVEL
        Para s;
        Set<Object> ans=new LinkedHashSet<Object>();
        if (name.indexOf('/')<0) {
            if (name.equals("Int")) { ans.add(ParaSig.SIGINT); return ans; }
            if (name.equals("univ")) { ans.add(ParaSig.UNIV); return ans; }
            if (name.equals("none")) { ans.add(ParaSig.NONE); return ans; }
            lookupNQsig_noparam(name,ans); s=params.get(name); if (s!=null) ans.add(s);
            return ans;
        }
        if (name.startsWith("this/")) {
            String temp=name.substring(5);
            if (temp.indexOf('/')<0) { s=params.get(temp); if (s!=null) {ans.add(s); return ans;} }
        }
        s=lookupQsig_noparam(name); if (s!=null) ans.add(s);
        return ans;
    }

    /*======================================================================*/

    private boolean canSee(List<String> a,List<String> b) {
        for(String aname: a)
            for(String bname: b)
                if (aname.equals(bname) || bname.startsWith(aname+"/")) return true;
        return false;
    }

    // Used by populate()
    private Pair<ParaSig,Field> lookup_Field(ParaSig origin, ParaSig s, String n) {
        Pair<ParaSig,Field> ans=null;
        if (canSee(origin.aliases, s.aliases)) for(Field f:s.fields) if (f.name.equals(n)) ans=new Pair<ParaSig,Field>(s,f);
        for(ParaSig p:s.sups()) {
            Pair<ParaSig,Field> ans2=lookup_Field(origin,p,n);
            if (ans==null) ans=ans2;
            else if (ans2!=null) throw s.syntaxError("This signature's \""+n+"\" field conflicts with a parent signature's field with the same name!");
        }
        if (s.sup()!=null && s.sup()!=ParaSig.UNIV) {
            Pair<ParaSig,Field> ans2=lookup_Field(origin,s.sup(),n);
            if (ans==null) ans=ans2;
            else if (ans2!=null) throw s.syntaxError("This signature's \""+n+"\" field conflicts with a parent signature's field with the same name!");
        }
        return ans;
    }

    // Used by populate()
    private Pair<ParaSig,Field> lookup_Field(ParaSig s, String n, String me) {
        // Looks up "n" from this SIG or any visible ancestor SIG.
        // But will return null if ((n and me are both fields in s) && (n==me, or n comes after me))
        int ii=0;
        int ni=(-1);
        int mi=(-1);
        for(VarDecl d:s.decls) for(String str:d.names) {
            if (str.equals(n)) ni=ii;
            if (str.equals(me)) mi=ii;
            ii++;
        }
        if (mi>=0 && ni>=mi) return null; else return lookup_Field(s,s,n);
    }

    // Looks up "n" from any visible SIG
    private Set<Pair<ParaSig,Field>> lookup_Field(String name, Set<Pair<ParaSig,Field>> ans) {
        if (ans==null) ans=new LinkedHashSet<Pair<ParaSig,Field>>();
        for(Map.Entry<String,ParaSig> e:sigs.entrySet())
            for(Field f:e.getValue().fields)
                if (f.name.equals(name)) ans.add(new Pair<ParaSig,Field>(e.getValue(), f));
        for (Map.Entry<String,Unit> e:opens.entrySet()) e.getValue().lookup_Field(name,ans);
        return ans;
    }

    /*======================================================================*/

    // Used by lookup_SigParamFunPred, which is only used by populate()
    private void lookupNQfunpred(String name, Set<Object> ans) {
        List<ParaFun> x=funs.get(name);
        if (x!=null) ans.addAll(x);
        for (Map.Entry<String,Unit> i:opens.entrySet()) i.getValue().lookupNQfunpred(name,ans);
    }

    // Used by lookup_SigParamFunPred, which is only used by populate()
    private void lookupQfunpred(String name, Set<Object> ans) {
        Unit u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) { List<ParaFun> x=u.funs.get(name); if (x!=null) ans.addAll(x); return; }
            u=u.opens.get(name.substring(0,i));
            if (u==null) return;
            name=name.substring(i+1,name.length());
        }
    }

    /*======================================================================*/

    // Used by populate()
    private Set<Object> lookup_SigParamFunPred (Pos pos, String name) {
        Set<Object> ans=lookup_sigORparam(name);
        Set<Object> reply=new LinkedHashSet<Object>();
        if (name.indexOf('/')>=0) lookupQfunpred(name,ans); else lookupNQfunpred(name,ans);
        for(Object z:ans) {
            if (z instanceof ParaSig) {
                reply.add(new ExprName(pos, ((ParaSig)z).fullname, z, ((ParaSig)z).type));
            } else if (z instanceof ParaFun) {
                ParaFun f=(ParaFun)z;
                if (f.getArgCount()==0) reply.add(new ExprName(pos, name, f, (f.getType()==null?Type.FORMULA:f.getType().type)));
                else reply.add(z);
            } else reply.add(z);
        }
        return reply;
    }

    /*======================================================================*/

    public Set<Object> populate(Field rootfield, ParaSig rootsig, ParaFun rootfun, Pos pos, String fullname) {
        // Return object can be ParaFun(with >0 arg) or Expr
        Set<Object> ans=new LinkedHashSet<Object>();
        Set<Object> ans2=null;
        final String name=(fullname.charAt(0)=='@') ? fullname.substring(1) : fullname;
        if (name.equals("this") && rootsig!=null) {
            ans.add(new ExprName(pos, "this", null, rootsig.type));
        }
        else if (rootfield!=null) {
            ans2=this.lookup_sigORparam(name);
            Pair<ParaSig,Field> y2=this.lookup_Field(rootsig, name, rootfield.name);
            if (y2!=null) {
                if (fullname.charAt(0)=='@') ans2.add(y2.b); else {
                    ExprName l=new ExprName(pos, "this", null, y2.a.type);
                    ExprName r=new ExprName(pos, y2.b.fullname, y2.b, y2.b.fulltype);
                    ans.add(new ExprJoin(pos, l, r, y2.b.halftype));
                }
            }
        }
        else if (rootsig!=null) {
            ans2=this.lookup_SigParamFunPred(pos,name);
            Pair<ParaSig,Field> y22=this.lookup_Field(rootsig,rootsig,name);
            for(Pair<ParaSig,Field> y2:this.lookup_Field(name,null)) {
                if (y22!=null && y2.a==y22.a && y2.b==y22.b) {
                    if (fullname.charAt(0)=='@') ans2.add(y22.b); else {
                        ExprName l=new ExprName(pos, "this", null, y22.a.type);
                        ExprName r=new ExprName(pos, y22.b.fullname, y22.b, y22.b.fulltype);
                        ans.add(new ExprJoin(pos, l, r, y22.b.halftype));
                    }
                }
                else if (y22==null) ans2.add(y2.b);
            }
        }
        else {
            if (rootfun!=null) ans2=this.lookup_sigORparam(name); else ans2=this.lookup_SigParamFunPred(pos,name);
            for(Pair<ParaSig,Field> y2:this.lookup_Field(name,null)) ans2.add(y2.b);
        }
        if (ans2!=null) for(Object x:ans2) {
            if (x instanceof ParaSig) {
                ans.add(new ExprName(pos, ((ParaSig)x).fullname, x, ((ParaSig)x).type));
            } else if (x instanceof Field) {
                Field s=(Field)x;
                ans.add(new ExprName(pos, s.fullname, x, s.fulltype));
            } else if (x instanceof Type) {
                ans.add(new ExprName(pos, name, null, (Type)x));
            } else if (x instanceof ParaFun) {
                ParaFun f=(ParaFun)x;
                if (f.getArgCount()==0) ans.add(new ExprName(pos, name, f, (f.getType()==null?Type.FORMULA:f.getType().type)));
                else ans.add(x);
            } else if (x instanceof Expr) {
                ans.add(x);
            } else throw new ErrorInternal(pos,null,"populuate() encountered unknown object "+x);
        }
        return ans;
    }

}
