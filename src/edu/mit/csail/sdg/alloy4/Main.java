package edu.mit.csail.sdg.alloy4;

import java.util.Set;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.io.File;

public final class Main {

  public static Type zzz=null;

  // For debuging/error-reporting purposes
  public static void debug(String s) { System.out.println(s); System.out.flush(); }
  public static void debug2(String s) { /*System.out.println(s); System.out.flush();*/ }
  public static RuntimeException syntaxError(String s) { return new ErrorSyntax(null,s); }
  public static RuntimeException typeError(String s) { return new ErrorType(null,null,s); }
  public static RuntimeException internalError(String s) { return new ErrorInternal(null,null,s); }

  public static boolean code=true;
  public static void main(String[] args) {
	if (args.length==0) {
      System.out.print("% "); System.out.flush();
      readall("");
    }
    else for(String a:args) {
      if (args.length>1) code=false;
      System.out.printf("%n===================================================================%nROOT=%s%n",a);
      readall(a);
    }
  }

/************************************************************************************************************
First, recursively parse each file into a Unit object, and return a list of "Unit" objects.
The FIRST Unit object must be the primary unit.
We throw an exception if there is a cycle in the IMPORT graph
************************************************************************************************************/

  public static void readall(String name) {
    // First, read all the Unit objects
    ArrayList<Unit> units=new ArrayList<Unit>();
    ArrayList<String> thispath=new ArrayList<String>();
    readall_helper(name,"",units,thispath);
    // Here we fill in the "params" field in each Unit object.
    while(true) {
      boolean chg=false;
      ParaOpen missing=null;
      for(Unit u:units) {
         for(Map.Entry<String, ParaOpen> f:u.opencmds.entrySet()) {
            Unit uu=u.opens.get(f.getKey());
            int j=uu.params.size();
            if (f.getValue().list.size() != j) throw new ErrorSyntax(u.pos, "To import the \""+uu.pos.filename+"\" module, you must provide exactly "+j+" parameters!");
            int i=0;
            for(Map.Entry<String,ParaSig> pp:uu.params.entrySet()) {
               String kn=pp.getKey();
               String vn=f.getValue().list.get(i);
               i++;
               ParaSig old=pp.getValue();
               Set<Object> v=u.lookup_sigORparam(vn);
               if (v.size()<1) {if (old==null) missing=f.getValue(); continue;}
               if (v.size()>1) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because the signature named \""+vn+"\" is ambiguous");
               ParaSig vv=(ParaSig)(v.iterator().next());
               if (old==vv) continue;
               if (old!=null) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because it is being imported more than once, with different arguments!");
               chg=true;
               uu.params.put(kn,vv);
               if (!code) debug("  RESOLVE: "+f.getKey()+"/"+kn+" := "+vv.fullname);
            }
         }
      }
      if (chg==false) {
         if (missing!=null) throw missing.syntaxError("Failed to import the module, because one of the instantiating signature cannot be found");
         break;
      }
    }
    // Then we merge units that have same FILENAME and same INSTANTIATING PARAMETERS
    while(mergeunits(units)) {}
    fillSig(units);
  }

  public static Unit readall_helper(String name,String prefix,ArrayList<Unit> units,ArrayList<String> thispath) {
    // Figure out the exact filename
    File f=new File(name);
    if (name.length()>0) {
      if (!f.exists()) f=new File("/zweb/zweb/work/alloy/models.new/"+name+".als");
      if (!f.exists()) f=new File("/zweb/zweb/work/alloy4/"+name+".als");
      if (!f.exists()) throw syntaxError("The module \""+name+"\" cannot be found!");
    }
    // Add the filename into a ArrayList, so that we can detect cycles in the module import graph
    // How? I'll argue that (filename appears > 1 time along a chain) <=> (infinite loop in the import graph)
    // => As you descend down the chain via OPEN, if you see the same FILE twice, then
    //    you will go into an infinite loop (since, regardless of the instantiating parameter,
    //    that file will attempt to OPEN the exact same set of files. leading back to itself, etc. etc.)
    // <= If there is an infinite loop, that means there is at least 1 infinite chain of OPEN (from root).
    //    Since the number of files is finite, at least 1 filename will be repeated.
    if (thispath.contains(f.getPath())) throw syntaxError("Circular dependency in module import!");
    thispath.add(f.getPath());
    // No cycle detected so far. So now we parse the file.
    Unit u=AlloyParser.alloy_parseFile(f.getPath(),prefix);
    units.add(u);
    // The returned Unit object is fully-filled-in except
    // * Unit.{opens,params}
    // * Sig.{type,sup,sups,subs}
    // * Field.halftype, Field.Full.fulltype, Expr*.type, and ExprName.resolved
    // Also, there will not be any ExprCall. Only ExprJoin.
    for(Map.Entry<String, ParaOpen> opens:u.opencmds.entrySet()) {
       // Here, we recursively open the included files (to fill out the "Unit.opens" field)
       ParaOpen y=opens.getValue();
       Unit uu=readall_helper(y.name, prefix.length()==0 ? y.as : prefix+"/"+y.as, units, thispath);
       if (y.list.size() != uu.params.size()) throw y.syntaxError("You supplied "+y.list.size()+" arguments to the import statement, but the imported module requires "+uu.params.size()+" arguments!");
       u.opens.put(y.as, uu);
    }
    thispath.remove(thispath.size()-1); // Remove this file from the CYCLE DETECTION LIST.
    return u;
  }

  private static<V> boolean isin(V x,Map<String,V> y) {
    for(Map.Entry<String,V> e:y.entrySet()) if (e.getValue()==x) return true;
    return false;
  }

  private static final Comparator<String> aliasComparator = new Comparator<String>() {
    public final int compare(String a,String b) {
      int alen=a.length();
      int blen=b.length();
      if (alen<blen) return -1;
      if (alen>blen) return 1;
      return a.compareTo(b);
    }
  };

  private static boolean mergeunits(ArrayList<Unit> units) {
    // Before merging, the only pointers that go between Unit objects are
    // (1) a unit's "params" may point to a sig in another unit
    // (2) a unit's "opens" may point to another unit
    // So when we find that two units A and B should be merged,
    // we iterate through every unit (except B), and replace
    // pointers into B with pointers into A.
    for(int i=0; i<units.size(); i++) {
      Unit a=units.get(i);
      for(int j=i+1; j<units.size(); j++) {
        Unit b=units.get(j);
        if (a.pos.filename.equals(b.pos.filename) && a.params.equals(b.params)) {
           if (!code) System.out.println("MATCH FOUND ON "+a.pos.filename);
           a.aliases.addAll(b.aliases);
           Collections.sort(a.aliases, aliasComparator);
           Map<String,ParaSig> asigs=new LinkedHashMap<String,ParaSig>(a.sigs);
           for(Map.Entry<String,ParaSig> p:a.sigs.entrySet())
        	{
        	 p.getValue().aliases=new ArrayList<String>(a.aliases);
        	}
           for(Unit c:units) if (c!=b) {
             for(Map.Entry<String,ParaSig> p:c.params.entrySet()) {
               if (isin(p.getValue(),asigs)) p.setValue(a.sigs.get(p.getValue().name));
               if (isin(p.getValue(),b.sigs)) p.setValue(a.sigs.get(p.getValue().name));
             }
             for(Map.Entry<String,Unit> p:c.opens.entrySet()) {
               if (p.getValue()==b) p.setValue(a);
             }
           }
           units.remove(j);
           return true;
        }
      }
    }
    return false;
  }

/************************************************************************************************************
For each toplevelsig and subsig: fill in the "sup", "subs", and "type" fields.
For each subsetsig: fill in its "sups" and "type" fields.
And we return a sorted list of all sigs (where if A extends or is subset of B, then B will preceed A)
=> We complain if a SIG tries to extend a SUBSETSIG.
=> We also complain if there is a cycle in SIG relationship.
************************************************************************************************************/

  private static void tsort(ParaSig x, LinkedHashMap<ParaSig,Boolean> status, ArrayList<ParaSig> list) {
    // Performs a topological sort
    status.put(x, Boolean.FALSE);
    ParaSig y=x.sup;
    List<ParaSig> yy=x.sups;
    if (y!=null) {
       Boolean v=status.get(y);
       if (v==null) tsort(y,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+y.fullname+"\"");
    }
    if (yy!=null) for(ParaSig yyy:yy) {
       y=yyy;
       Boolean v=status.get(y);
       if (v==null) tsort(y,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+y.fullname+"\"");
    }
    status.put(x, Boolean.TRUE);
    list.add(x);
  }

  public static void fillSig(ArrayList<Unit> units) {
    ArrayList<ParaSig> sigs=new ArrayList<ParaSig>();
    for(Unit u:units) {
      for(Map.Entry<String,ParaSig> si:u.sigs.entrySet()) {
        ParaSig s=si.getValue();
        sigs.add(s);
        if (s.ext!=null) {
           Set<Object> ans=u.lookup_sigORparam(s.ext);
           if (ans.size()>1) throw new ErrorSyntax(u.pos, "Sig \""+s.fullname+"\" tries to extend \""+s.ext+"\", but the name \""+s.ext+"\" is ambiguous.");
           if (ans.size()<1) throw new ErrorSyntax(u.pos, "Sig \""+s.fullname+"\" tries to extend a non-existent signature \""+s.ext+"\"");
           ParaSig parent=(ParaSig)(ans.iterator().next());
           if (parent.subset) throw new ErrorSyntax(u.pos, "Sig \""+s.fullname+"\" cannot extend a subset signature \""+parent.fullname+"\"! A signature can only extend a toplevel signature or a subsignature.");
           s.ext=parent.fullname;
           s.sup=parent;
           parent.subs.add(s);
        }
        if (s.subset) {
           List<String> newin=new ArrayList<String>();
           for(String i:s.in()) {
             Set<Object> ans=u.lookup_sigORparam(i);
             if (ans.size()>1) throw new ErrorSyntax(u.pos, "Sig \""+s.fullname+"\" tries to be a subset of \""+i+"\", but the name \""+i+"\" is ambiguous.");
             if (ans.size()<1) throw new ErrorSyntax(u.pos, "Sig \""+s.fullname+"\" tries to be a subset of a non-existent signature \""+i+"\"");
             ParaSig parent=(ParaSig)(ans.iterator().next());
             s.sups.add(parent);
             newin.add(parent.fullname);
           }
           s.in=newin;
        }
      }
    }
    // Now we perform a topological sort on the sigs
    LinkedHashMap<ParaSig,Boolean> status=new LinkedHashMap<ParaSig,Boolean>(); // NONE=NONE FALSE=VISITING TRUE=VISITED
    ArrayList<ParaSig> list=new ArrayList<ParaSig>();
    for(ParaSig y:sigs) {
       Boolean v=status.get(y);
       if (v==null) tsort(y,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+y.fullname+"\"");
    }
    for(int i=0; i<list.size(); i++) {
       ParaSig y=list.get(i);
       Type t=null;
       if (y.subset) for(ParaSig z:y.sups) {
         if (t==null) t=z.type; else t=t.union(z.type);
       }
       if (t==null) t=Type.make(y);
       y.type=t;
    }
    // Now we fill in the Type field of all the ParaSig objects.
    // Since the SIGS are topologically sorted, whenever we process a sig,
    // we know the Type values of its ancestors sigs are already computed.
    typecheck(units,list);
  }

/************************************************************************************************************
************************************************************************************************************/

  public static void typecheck(ArrayList<Unit> units, List<ParaSig> sigs) {
    VisitorTypechecker tc=new VisitorTypechecker();
    for(int i=0; i<sigs.size(); i++) {
      ParaSig s=sigs.get(i);
      Unit u=units.get(0).lookupPath(s.path); // s.parent();
      sigs.set(i, tc.accept(s,u));
    }
    for(Unit u:units) {
      for(Map.Entry<String,List<ParaFun>> funi:u.funs.entrySet()) {
        List<ParaFun> funs=funi.getValue();
        for(int i=0; i<funs.size(); i++) {
          tc=new VisitorTypechecker();
          ParaFun f=funs.get(i);
          funs.set(i, tc.accept(f,u));
        }
      }
    }
    tc=new VisitorTypechecker();
    for(int ui=0; ui<units.size(); ui++) {
      Unit u=units.get(ui);
      u=tc.accept(u);
      units.set(ui, u);
    }
    finalDesugar(units,sigs);
  }

/************************************************************************************************************
After the earlier TYPECHECK PHASE, we know
1) There are no more CallOrJoin (they're all turned into a combination of Call, Join, and Name)
2) ExprCall is always a call to a pred/fun with 1 or more arguments. And there are no excess arguments.
3) ExprName is always a SIG, FIELD, FILEDFULL, 0-argument PRED/FUN, or a LET/QUANT/FUNCPARAMETER binding.
Now we perform the final desugarings...
************************************************************************************************************/

  private static Expr addOne(Expr x) {
    if (x instanceof ExprUnary) {
       ExprUnary y=(ExprUnary)x;
       if (y.op==ExprUnary.Op.SETMULT || y.op==ExprUnary.Op.ONEMULT || y.op==ExprUnary.Op.LONEMULT || y.op==ExprUnary.Op.SOMEMULT) return x;
    }
    if (x.type.isInt || x.type.isBool || x.type.arity()!=1) return x;
    Expr xx=ExprUnary.Op.ONEMULT.make(x.pos,x,x.type); return xx;
  }

  private static void finalDesugar(ArrayList<Unit> units, List<ParaSig> sigs)  {
    // 1. Turn "UnitSigField" into "this . UnitSigFieldFull".
    //    The only places this could have been is in field declarations and signature appended facts.
    final VisitDesugar desugar1=new VisitDesugar(units.get(0)) {
      @Override public Expr accept(ExprName x) {
        Object re = x.object;
        if (re instanceof ParaSig.Field) {
           ParaSig.Field f = (ParaSig.Field)re;
           ParaSig rts=f.parent();
           ExprName l=new ExprName(x.pos, "this", rts.type, rts.type);
           ExprName r=new ExprName(x.pos, f.full.fullname, f.full, f.full.fulltype);
           ExprJoin y=new ExprJoin(x.pos,l,r,x.type);
           return y;
        }
        return x;
      }
    };
    for(ParaSig s:sigs) {
      List<FieldDecl> newdecl=new ArrayList<FieldDecl>();
      for(FieldDecl d:s.decls) newdecl.add(new FieldDecl(d, d.value.accept(desugar1), s.type));
      s.decls=newdecl;
      if (s.appendedFacts!=null) s.appendedFacts=s.appendedFacts.accept(desugar1);
    }
    // 2. Turn "a: b" (where b is unary and has no multiplicity marking) into "a: one b"
    // After this, we can uniformly treat "a:b" as "a:set b".
    final VisitDesugar desugar2=new VisitDesugar() {
      @Override public Expr accept(ExprQuant x) {
        List<VarDecl> list=new ArrayList<VarDecl>();
        for(VarDecl d:x.list) {
           list.add(new VarDecl(d,addOne(d.value.accept(this))));
        }
        Expr sub=x.sub.accept(this);
        Expr ans=x.op.make(x.pos, list, sub, x.type); return ans;
      }
    };
    for(Unit u:units) {
      for(Map.Entry<String,List<ParaFun>> e:u.funs.entrySet()) for(int xi=0; xi<e.getValue().size(); xi++) {
    	ParaFun x=e.getValue().get(xi);
        List<VarDecl> newdecls=new ArrayList<VarDecl>();
        for(VarDecl d:x.decls) {
          newdecls.add(new VarDecl(d, addOne(d.value.accept(desugar2))));
        }
        Expr type = (x.type==null) ? null : addOne(x.type.accept(desugar2));
        Expr value = x.value.accept(desugar2);
        x.decls=newdecls; x.type=type; x.value=value;
      }
      for(Map.Entry<String,ParaSig> e:u.sigs.entrySet()) {
        ParaSig x=e.getValue();
        List<FieldDecl> newdecls=new ArrayList<FieldDecl>();
        for(FieldDecl d:x.decls) newdecls.add(new FieldDecl(d, addOne(d.value.accept(desugar2)), x.type));
        x.decls=newdecls;
        if (x.appendedFacts!=null) x.appendedFacts=x.appendedFacts.accept(desugar2);
      }
      for(Map.Entry<String,ParaAssert> e:u.asserts.entrySet()) {
        ParaAssert x=e.getValue();
        x.value=x.value.accept(desugar2);
      }
      for(Map.Entry<String,ParaFact> e:u.facts.entrySet()) {
        ParaFact x=e.getValue();
        x.value=x.value.accept(desugar2);
      }
    }
    // 3. Turn SIG APPENDED FACTS into standalone facts (and set Sig.appendedfacts:=null)
    //    Turn FIELD DECLS CONSTRAINTS into standalone facts (and remove every entry in Sig.decls)
    VisitQuery hasThis=new VisitQuery() {
      @Override public Object accept(ExprName x) { if (x.name.equals("this")) return x; return null; }
    };
    for(Unit u:units) for(Map.Entry<String,ParaSig> e:u.sigs.entrySet()) {
      ParaSig s=e.getValue();
      // zzz ALLOY3 compatiblity hack below
      if (s.pos!=null && s.pos.filename!=null && s.pos.filename.endsWith("util/ordering.als") && s.name.equals("Ord")) continue;
      // zzz ALLOY3 compatiblity hack above
      Expr temp=s.appendedFacts; s.appendedFacts=null;
      int f=0;
      for(FieldDecl d:s.decls) {
        boolean noThis=!hasThis.query(d.value);
        for(int n=0; n<d.size(); n++) {
          ParaSig.Field x00=s.fields.get(f); f++;
          Expr x22=new ExprName(d.value.pos, x00.full.fullname, x00.full, x00.full.fulltype);
          Expr x5=new ExprName(s.pos, s.fullname, s, s.type);
          if (noThis && d.value.mult==0)
             { u.makeFact(d.value.pos, "", x22.in(x5.product(d.value))); continue; }
          if (noThis && d.value.isSetOf1ary())
             { u.makeFact(d.value.pos, "", x22.in(x5.product(d.value.getUnarySub()))); continue; }
          for(int i=x22.type.arity(); i>1; i--) x5=x5.product(Expr.univ(x5.pos));
          u.makeFact(d.value.pos, "", x22.in(x5));
          ExprName x11=new ExprName(d.value.pos, "this", s.type, s.type);
          Expr x33=x11.join(x22);
          Expr x44=x33.in(d.value);
          if (temp==null) temp=x44; else temp=temp.and(x44);
        }
      }
      if (temp!=null) {
         Expr x1=new ExprName(s.pos, s.fullname, s, s.type);
         Expr x2=ExprUnary.Op.ONEMULT.make(s.pos, x1, s.type);
         VarDecl x3=new VarDecl("this",x2);
         List<VarDecl> x4=new ArrayList<VarDecl>(1); x4.add(x3);
         Expr x5=ExprQuant.Op.ALL.make(s.pos,x4,temp,Type.FORMULA);
         u.makeFact(x5.pos, "", x5);
      }
    }
    if (code) { VisitorEval c=new VisitorEval(units); c.codegen(sigs); }
  }


/************************************************************************************************************
All Done!
************************************************************************************************************/
}
