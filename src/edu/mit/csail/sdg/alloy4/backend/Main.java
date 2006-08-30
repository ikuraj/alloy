package edu.mit.csail.sdg.alloy4.backend;

import java.util.Set;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.Reader;

import edu.mit.csail.sdg.alloy4.core.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.core.Logger;
import edu.mit.csail.sdg.alloy4.core.ParaOpen;
import edu.mit.csail.sdg.alloy4.core.ParaSig;
import edu.mit.csail.sdg.alloy4.core.Type;
import edu.mit.csail.sdg.alloy4.core.Unit;
import edu.mit.csail.sdg.alloy4.core.VisitTypechecker;
import edu.mit.csail.sdg.alloy4.frontend.AlloyParser;

public final class Main {

    public static void main(String[] args) throws FileNotFoundException {
        Logger log=new FileLogger(".alloy.tmp");
        if (args.length<=1) run(-1,args,log); else run(-2,args,log);
        log.close();
    }

    public enum Result { SAT, UNSAT, TRIVIALLY_SAT, TRIVIALLY_UNSAT };

    public static List<Result> run(int code, Reader i, Logger log) {
        Logger blank=new Logger();
        ArrayList<Unit> units=readall(i);
        fillParams(units,blank);
        ArrayList<ParaSig> sigs=fillSig(units);
        new VisitTypechecker(blank).check(units,sigs);
        if (code>=(-1)) { VisitEval c=new VisitEval(code,log,units); return c.codegen(sigs); }
        return new ArrayList<Result>();
    }

    public static void run(int code, String[] args, Logger log) throws FileNotFoundException {
        ArrayList<Unit> units;
        ArrayList<ParaSig> sigs;
        if (args.length==0) {
            System.out.print("% ");
            System.out.flush();
            units=readall("");
            fillParams(units,log);
            sigs=fillSig(units);
            new VisitTypechecker(log).check(units,sigs);
            if (code>=(-1)) { VisitEval c=new VisitEval(code,log,units); c.codegen(sigs); }
        }
        else for(String a:args) {
            log.log("\n\nMain file = "+a+"\n");
            units=readall(a);
            fillParams(units,log);
            sigs=fillSig(units);
            new VisitTypechecker(log).check(units,sigs);
            if (code>=(-1)) { VisitEval c=new VisitEval(code,log,units); c.codegen(sigs); }
        }
    }

    private static RuntimeException syntaxError(String s) { return new ErrorSyntax(null,s); }

    /************************************************************************************************************
     First, recursively parse each file into a Unit object, and return a list of "Unit" objects.
     The FIRST Unit object must be the primary unit.
     We throw an exception if there is a cycle in the IMPORT graph
     ************************************************************************************************************/

    private static void fillParams(ArrayList<Unit> units, Logger log) {
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
                        ParaSig old=pp.getValue();
                        String vn=f.getValue().list.get(i); i++;
                        Set<Object> v=u.lookup_sigORparam(vn);
                        if (v.size()<1) {if (old==null) missing=f.getValue(); continue;}
                        if (v.size()>1) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because the signature named \""+vn+"\" is ambiguous");
                        ParaSig vv=(ParaSig)(v.iterator().next());
                        if (old==vv) continue;
                        if (old!=null) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because it is being imported more than once, with different arguments!");
                        if (vv==ParaSig.NONE) throw new ErrorSyntax(u.pos, "Failed to import the \""+uu.pos.filename+"\" module, because you cannot use \"none\" as an instantiating argument!");
                        chg=true;
                        uu.params.put(kn,vv);
                        log.log("RESOLVE: "+f.getKey()+"/"+kn+" := "+vv.fullname+"\n");
                    }
                }
            }
            if (chg==false) {
                if (missing!=null) throw missing.syntaxError("Failed to import the module, because one of the instantiating signature cannot be found");
                break;
            }
        }
        while(mergeunits(units,log)) {}
    }

    private static ArrayList<Unit> readall(String name) {
        ArrayList<Unit> units=new ArrayList<Unit>();
        ArrayList<String> thispath=new ArrayList<String>();
        readall_helper(name,"",units,thispath);
        return units;
    }

    private static ArrayList<Unit> readall (Reader i) {
        ArrayList<Unit> units=new ArrayList<Unit>();
        ArrayList<String> thispath=new ArrayList<String>();
        Unit u=AlloyParser.alloy_parseStream(i);
        units.add(u);
        for(Map.Entry<String, ParaOpen> opens:u.opencmds.entrySet()) {
            // Here, we recursively open the included files (to fill out the "Unit.opens" field)
            ParaOpen y=opens.getValue();
            Unit uu=readall_helper(y.file, y.name, units, thispath);
            if (y.list.size() != uu.params.size()) throw y.syntaxError("You supplied "+y.list.size()+" arguments to the import statement, but the imported module requires "+uu.params.size()+" arguments!");
            u.opens.put(y.name, uu);
        }
        return units;
    }

    private static Unit readall_helper(String name,String prefix,ArrayList<Unit> units,ArrayList<String> thispath) {
        // Figure out the exact filename
        File f=new File(name);
        if (name.length()>0) {
            if (!f.exists()) f=new File("models/"+name+".als");
            if (!f.exists()) f=new File("/zweb/zweb/work/alloy/models.new/"+name+".als");
            if (!f.exists()) f=new File("/zweb/zweb/work/alloy4/"+name+".als");
            if (!f.exists()) f=new File("/zweb/zweb/work/alloy4/mondex.aug24/"+name+".als");
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
            Unit uu=readall_helper(y.file, prefix.length()==0 ? y.name : prefix+"/"+y.name, units, thispath);
            if (y.list.size() != uu.params.size()) throw y.syntaxError("You supplied "+y.list.size()+" arguments to the import statement, but the imported module requires "+uu.params.size()+" arguments!");
            u.opens.put(y.name, uu);
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

    private static boolean mergeunits(ArrayList<Unit> units, Logger log) {
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
                    log.log("MATCH FOUND ON "+a.pos.filename+"\n");
                    a.aliases.addAll(b.aliases);
                    Collections.sort(a.aliases, aliasComparator);
                    Map<String,ParaSig> asigs=new LinkedHashMap<String,ParaSig>(a.sigs);
                    for(Map.Entry<String,ParaSig> p:a.sigs.entrySet())
                        p.getValue().aliases=new ArrayList<String>(a.aliases);
                    for(Unit c:units) if (c!=b) {
                        for(Map.Entry<String,ParaSig> p:c.params.entrySet()) {
                            if (isin(p.getValue(),asigs)) p.setValue(a.sigs.get(p.getValue().name));
                            if (isin(p.getValue(),b.sigs)) p.setValue(a.sigs.get(p.getValue().name));
                        }
                        for(Map.Entry<String,Unit> p:c.opens.entrySet()) if (p.getValue()==b) p.setValue(a);
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
        ParaSig y=x.sup();
        if (y!=null) {
            Boolean v=status.get(y);
            if (v==null) tsort(y,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+y.fullname+"\"");
        }
        for(ParaSig yy:x.sups()) {
            Boolean v=status.get(yy);
            if (v==null) tsort(yy,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+yy.fullname+"\"");
        }
        status.put(x, Boolean.TRUE);
        list.add(x);
    }

    private static ArrayList<ParaSig> fillSig(ArrayList<Unit> units) {
        ArrayList<ParaSig> sigs=new ArrayList<ParaSig>();
        for(Unit u:units) {
            for(Map.Entry<String,ParaSig> si:u.sigs.entrySet()) {
                ParaSig s=si.getValue();
                sigs.add(s);
                s.resolveSup(u);
                s.resolveSups(u);
            }
        }
        // Now we perform a topological sort on the sigs
        LinkedHashMap<ParaSig,Boolean> status=new LinkedHashMap<ParaSig,Boolean>(); // NONE=NONE FALSE=VISITING TRUE=VISITED
        ArrayList<ParaSig> list=new ArrayList<ParaSig>();
        for(ParaSig y:sigs) {
            Boolean v=status.get(y);
            if (v==null) tsort(y,status,list); else if (v==Boolean.FALSE) throw new ErrorSyntax(null,"Circular extension detected, involving the signature named \""+y.fullname+"\"");
        }
        for(ParaSig y:list) if (y.sup()!=null) y.sup().subs.add(y);
        // Now we fill in the Type field of all the ParaSig objects.
        // Since the SIGS are topologically sorted, whenever we process a sig,
        // we know the Type values of its ancestors sigs are already computed.
        for(int i=0; i<list.size(); i++) {
            ParaSig y=list.get(i);
            Type t=null;
            if (y.subset) for(ParaSig z:y.sups()) {
                if (t==null) t=z.type; else t=t.union(z.type);
            }
            if (t==null) t=Type.make(y);
            y.type=t;
        }
        return list;
    }

}
