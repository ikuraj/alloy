package edu.mit.csail.sdg.alloy4;

import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;

public final class Unit { // Represents 1 instantiation of an ALS file

  public Unit(String prefix) { aliases.add(prefix); }

  // The "MODULE" line at the top of the file.
  public Pos pos;
  public String name="";

  // The list of aliases (from root) that point to this Unit. Contains "" if it's the outermost module.
  public final List<String> aliases=new ArrayList<String>();

  // List of all the PARAMETRIC PARAMETERS used in instantiating this file. Each name cannot have "/" or "@".
  // This must be a LinkedHashMap because we depend on the iterator returning them in the original order
  public final Map<String,ParaSig> params=new LinkedHashMap<String,ParaSig>();

  public void makeModule(Pos p,String n,List<ExprName> l) {
	pos=p;
	name=n;
    for(ExprName x:l) {
      String y=x.name;
      if (params.containsKey(y)) throw new ErrorSyntax(p,"You cannot use the same name for more than 1 instantiating parameter!");
      if (sigs.containsKey(y)) throw new ErrorSyntax(p,"Within the same file, a signature and a polymorphic parameter cannot have the same name!");
      if (!aliases.contains("")) params.put(y, null);
      else makeSig(p, y, false, false, false, false, null, null, new ArrayList<VarDecl>(), null);
    }
  }

  // This lists all the SIGS defined inside this file.
  // The NAME must not contain any "/" or "@"
  public final Map<String,ParaSig> sigs=new LinkedHashMap<String,ParaSig>();
  public void makeSig(Pos p,String n,boolean fa,boolean fl,boolean fo,boolean fs,List<ExprName> i,ExprName e,List<VarDecl> d,Expr f) {
    ParaSig x=new ParaSig(p,aliases.get(0),n,fa,fl,fo,fs,i,e,d,f);
    if (asserts.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another assertion!");
    if (facts.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another fact!");
    if (funs.containsKey(n)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another function/predicate!");
    if (sigs.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as another signature!");
    if (params.containsKey(x.name)) throw x.syntaxError("Within the same file, a signature cannot have the same name as a polymorphic type!");
    sigs.put(x.name, x);
  }

  private int anonymous_factassert_id=0;

  // This lists all the FACTS defined inside this file.
  // The NAME must not contain any "/" or "@"
  public final Map<String,ParaFact> facts=new LinkedHashMap<String,ParaFact>();
  public void makeFact(Pos p,String n,Expr v) {
    if (n==null || n.length()==0) n="*"+(++anonymous_factassert_id)+"*";
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
  public void makeAssert(Pos p,String n,Expr v) {
    if (n==null || n.length()==0) n="*"+(++anonymous_factassert_id)+"*";
    ParaAssert x=new ParaAssert(p,aliases.get(0),n,v);
    if (asserts.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another assertion!");
    if (facts.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another fact!");
    if (funs.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another function/predicate!");
    if (sigs.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as another signature!");
    if (params.containsKey(n)) throw x.syntaxError("Within the same file, an assertion cannot have the same name as a polymorphic type!");
    asserts.put(n,x);
  }

  // This lists all the FUNCTIONS defined inside this file.
  // The NAME must not contain any "/" or "@"
  public final Map<String,List<ParaFun>> funs=new LinkedHashMap<String,List<ParaFun>>();
  public void makeFun(Pos p,String n,Expr f,List<VarDecl> d,Expr t,Expr v) {
    List<ParaFun> list=funs.get(n);
    if (list==null) list=new ArrayList<ParaFun>();
    //
    d=new ArrayList<VarDecl>(d);
    if (f!=null) d.add(0,new VarDecl("this",ExprUnary.Op.ONEMULT.make(p,f)));
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
    ParaOpen x=new ParaOpen(p,aliases.get(0),n,l,a);
    if (opencmds.containsKey(x.as)) throw x.syntaxError("You cannot import more than 1 module using the same alias!");
    opencmds.put(x.as, x);
  }

  // This stores the list of RUN/CHECK commands, in the order they appear in the file.
  public final List<ParaRuncheck> runchecks=new ArrayList<ParaRuncheck>();
  public void makeRuncheck(Pos p,String n,boolean c,int o,int exp,Map<String,Integer> s,Set<String> exa) {
    if (!aliases.contains("")) return;
    runchecks.add(new ParaRuncheck(p, aliases.get(0), n, c, o, exp, s, exa));
  }

  private void lookupNQsig_noparam(String name,Set<Object> ans) { // It ignores "params"
    Para x=sigs.get(name);
    if (x!=null) ans.add(x);
    for(Map.Entry<String,Unit> i:opens.entrySet()) i.getValue().lookupNQsig_noparam(name,ans);
  }

  private Para lookupQsig_noparam(String name) { // It ignores "params"
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

  public Set<Object> lookup_sigORparam(String name) { // Will search "params" too, if at the CURRENT LEVEL
    Para s;
    Set<Object> ans=new LinkedHashSet<Object>();
    if (name.indexOf('/')<0) {
      if (name.equals("Int")) { ans.add(ParaSig.SIGINT); return ans; }
      if (name.equals("univ")) { ans.add(ParaSig.UNIV); return ans; }
      if (name.equals("none")) { ans.add(ParaSig.NONE); return ans; }
      if (name.equals("iden")) return ans;
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

  private void lookupNQfunpred(String name,Set<Object> ans) {
    List<ParaFun> x=funs.get(name);
    if (x!=null) ans.addAll(x);
    for (Map.Entry<String,Unit> i:opens.entrySet()) i.getValue().lookupNQfunpred(name,ans);
  }

  private void lookupQfunpred(String name,Set<Object> ans) {
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

  public Set<Object> lookup_SigParamFunPred (String name) {
    Set<Object> ans=lookup_sigORparam(name);
    if (name.indexOf('/')>=0) lookupQfunpred(name,ans); else lookupNQfunpred(name,ans);
    return ans;
  }

  private boolean canSee(List<String> a,List<String> b) {
    for(String aname: a)
      for(String bname: b)
        if (aname.equals(bname) || bname.startsWith(aname+"/")) return true;
    return false;
  }

  private ParaSig.Field lookup_Field_helper(ParaSig origin,ParaSig s,String n) {
    ParaSig.Field ans=null;
    if (canSee(origin.aliases(), s.aliases())) for(ParaSig.Field f:s.fields) if (f.name.equals(n)) ans=f;
    for(ParaSig p:s.sups()) {
      ParaSig.Field ans2=lookup_Field_helper(origin,p,n);
      if (ans==null) ans=ans2; else if (ans2!=null) throw s.syntaxError("This signature's \""+n+"\" field conflicts with a parent signature's field with the same name!");
    }
    if (s.sup!=null) {
      ParaSig.Field ans2=lookup_Field_helper(origin,s.sup,n);
      if (ans==null) ans=ans2; else if (ans2!=null) throw s.syntaxError("This signature's \""+n+"\" field conflicts with a parent signature's field with the same name!");
    }
    return ans;
  }

  public ParaSig.Field lookup_Field(ParaSig s,String n) { // Looks up "n" from this SIG or any visible ancestor SIG
    if (n.charAt(0)=='@') n=n.substring(1);
    return lookup_Field_helper(s,s,n);
  }

  public ParaSig.Field lookup_Field(ParaSig s,String n,String me) {
    // Looks up "n" from this SIG or any visible ancestor SIG.
    // But will return null if ((n and me are both fields in s) && (n==me, or n comes after me))
    int ii=0;
    int ni=(-1); if (n.charAt(0)=='@') n=n.substring(1);
    int mi=(-1); if (me.charAt(0)=='@') me=me.substring(1);
    for(FieldDecl d:s.decls) for(int i=0; i<d.size(); i++) {
      String str=d.get(i);
      if (str.equals(n)) ni=ii;
      if (str.equals(me)) mi=ii;
      ii++;
    }
    if (mi>=0 && ni>=mi) return null; else return lookup_Field_helper(s,s,n);
  }

  private void lookup_Field_helper(String name,Set<Object> ans) {
    for(Map.Entry<String,ParaSig> e:sigs.entrySet())
     for(ParaSig.Field f:e.getValue().fields)
      if (f.name.equals(name)) ans.add(f);
    for (Map.Entry<String,Unit> e:opens.entrySet()) e.getValue().lookup_Field_helper(name,ans);
  }

  public Set<Object> lookup_Field(String n) { // Looks up "n" from any visible SIG
    Set<Object> ans=new LinkedHashSet<Object>();
    if (n.charAt(0)=='@') n=n.substring(1);
    lookup_Field_helper(n,ans);
    return ans;
  }

  public Unit lookupPath(String name) {
    Unit u=this;
    if (name.startsWith("this/")) name=name.substring(5);
    if (name.equals("this")) return this;
    if (name.length()==0) return this;
    while(true) {
      int i=name.indexOf('/');
      if (i<0) return u.opens.get(name);
      u=u.opens.get(name.substring(0,i));
      if (u==null) return null;
      name=name.substring(i+1,name.length());
    }
  }
}
