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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprConstant;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;

final class CompModule { // Comparison is by identity

    //=============================================================================================================//

    String moduleName="unknown";
    Pos pos=Pos.UNKNOWN;
    final World world;
    final String path;
    List<String> paths=new ArrayList<String>();
    final Map<String,SigAST> params=new LinkedHashMap<String,SigAST>();
    final List<String> paramNames=new ArrayList<String>();
    Module topoModule=null; // This flag is set to its corresponding Module during topological sort

    CompModule(World world, String path) {
        this.world=world;
        this.path=path;
        paths.add(path);
    }

    void makeModule(Pos pos, String moduleName, List<ExpName> list) throws Err {
        this.moduleName=moduleName;
        this.pos=pos;
        for(ExpName expr:list) {
            String name=expr.name;
            paramNames.add(name);
            if (params.containsKey(name))
                throw new ErrorSyntax(pos, "You cannot use the same name for more than 1 instantiating parameter.");
            params.put(name, null);
            if (path.length()==0)
                addSig(null, pos, name, false, false, false, false, null, null, new ArrayList<Decl>(), null);
        }
        if (path.length()==0) {params.clear(); paramNames.clear();}
    }

    final Map<String,CompModule> opens=new LinkedHashMap<String,CompModule>();

    final Map<String,CompOpen> opencmds=new LinkedHashMap<String,CompOpen>();

    void addOpen(Pos pos, String name, List<ExpName> args, String alias) throws Err {
        CompOpen x=new CompOpen(pos, alias, args, name);
        CompOpen y=opencmds.get(x.alias);
        // Special case, especially needed for auto-import of "util/sequniv"
        if (y!=null && x.alias.equals(y.alias) && x.args.equals(y.args) && x.filename.equals(y.filename)) return;
        if (opencmds.containsKey(x.alias))
            throw new ErrorSyntax(pos, "You cannot import more than 1 module using the same alias.");
        opencmds.put(x.alias, x);
    }

    //=============================================================================================================//

    final List<FunAST> funs = new ArrayList<FunAST>();

    void addFunc(Pos p, String n, Exp f, List<Decl> d, Exp t, Exp v) throws Err {
        d=new ArrayList<Decl>(d);
        if (f!=null) d.add(0, new Decl(null, Util.asList(new ExpName(f.span(), "this")), f));
        funs.add(new FunAST(p, n, d, t, v));
    }

    static final class FunAST {
        final Pos pos;
        final String name;
        final List<Decl> args;
        final Exp returnType;
        final Exp body;
        FunAST(Pos p, String n, List<Decl> a, Exp r, Exp b) {
            pos=p; name=n; args=a; returnType=r; body=b;
        }
    }

    //=============================================================================================================//

    final Map<String,SigAST> sigs = new LinkedHashMap<String,SigAST>();

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

    static final class SigAST {
        // Comparison must be by identity
        boolean topo=false; // This flag is set to "true" during topological sort
        CompModule topoParent=null; // This value is set to its CompModule during topological sort
        Sig topoSig=null; // This value is set to its corresponding Sig during topological sort
        boolean hint_isLeaf=false;
        final Pos pos;
        final String name;
        final String fullname;
        final boolean abs,lone,one,some,subset;
        final List<String> parents;
        final List<Decl> fields;
        final Exp appendedFact;
        Pos orderingPosition;
        Pos absPosition, lonePosition, onePosition, somePosition, extendsPosition, inPosition;
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
            this.parents=parents;
            this.fields=fields;
            this.appendedFact=appendedFacts;
            this.topoSig=topoSig;
        }
        @Override public String toString() { return fullname; }
    }

    //=============================================================================================================//

    final Map<String,Exp> facts = new LinkedHashMap<String,Exp>();

    void addFact(Pos pos, String name, Exp value) throws Err {
        if (name==null || name.length()==0) name="fact#"+(1+facts.size());
        if (facts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same file, a fact cannot have the same name as another fact.");
        facts.put(name,value);
    }

    //=============================================================================================================//

    final Map<String,Exp> asserts=new LinkedHashMap<String,Exp>();

    String addAssertion(Pos pos, String name, Exp value) throws Err {
        if (name==null || name.length()==0) name="assert#"+(1+asserts.size());
        if (asserts.containsKey(name)) throw new ErrorSyntax(pos, "Within the same file, an assertion cannot have the same name as another assertion.");
        asserts.put(name, value);
        return name;
    }

    //=============================================================================================================//

    final List<Pair<String,Command>> commands=new ArrayList<Pair<String,Command>>();

    void addCommand(Pos p,String n,boolean c,int o,int b,int seq,int exp,Map<String,Integer> s, String label, List<ExpName> opts) throws Err {
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        List<String> options=new ArrayList<String>(opts.size());
        for(ExpName opt:opts) options.add(opt.name);
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s, options)));
    }

    void addCommand(Pos p,Exp e,boolean c,int o,int b,int seq,int exp,Map<String,Integer> s, String label, List<ExpName> opts) throws Err {
        String n;
        if (c) n=addAssertion(p,"",e); else addFunc(e.span(),n="run#"+(1+commands.size()),null,new ArrayList<Decl>(),null,e);
        if (n.length()==0) throw new ErrorSyntax(p, "Predicate/assertion name cannot be empty.");
        if (n.indexOf('@')>=0) throw new ErrorSyntax(p, "Predicate/assertion name cannot contain \'@\'");
        List<String> options=new ArrayList<String>(opts.size());
        for(ExpName opt:opts) options.add(opt.name);
        if (label==null || label.length()==0) label=n;
        commands.add(new Pair<String,Command>(n, new Command(p, label, ExprConstant.TRUE, c, o, b, seq, exp, s, options)));
    }

    //=============================================================================================================//

    private void lookupNQsig_noparam (String name, Set<SigAST> ans) { // It ignores "params"
        SigAST x=sigs.get(name);
        if (x!=null) ans.add(x);
        for(Map.Entry<String,CompModule> i:opens.entrySet()) i.getValue().lookupNQsig_noparam(name,ans);
    }

    private SigAST lookupQsig_noparam (String name) { // It ignores "params"
        CompModule u=this;
        if (name.startsWith("this/")) name=name.substring(5);
        while(true) {
            int i=name.indexOf('/');
            if (i<0) return u.sigs.get(name);
            u=u.opens.get(name.substring(0,i));
            if (u==null) return null;
            name=name.substring(i+1,name.length());
        }
    }

    Set<SigAST> lookup_sigORparam (String name) { // Will search "params" too, if at the CURRENT LEVEL
        SigAST s;
        Set<SigAST> ans=new LinkedHashSet<SigAST>();
        if (name.equals("seq/Int")) { ans.add(SEQIDXast); return ans; }
        if (name.equals("Int"))     { ans.add(SIGINTast); return ans; }
        if (name.equals("univ"))    { ans.add(UNIVast); return ans; }
        if (name.equals("none"))    { ans.add(NONEast); return ans; }
        if (name.indexOf('/')<0) {
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

    static final SigAST UNIVast   = new SigAST(Pos.UNKNOWN, "univ",    "univ", true,  false,false,false,false, new ArrayList<String>(), new ArrayList<Decl>(), null, UNIV);
    static final SigAST SIGINTast = new SigAST(Pos.UNKNOWN, "Int",     "Int",  false, false,false,false,false, Util.asList("univ"),     new ArrayList<Decl>(), null, SIGINT);
    static final SigAST SEQIDXast = new SigAST(Pos.UNKNOWN, "seq/Int", "Int",  false, false,false,false,false, Util.asList("Int"),      new ArrayList<Decl>(), null, SEQIDX);
    static final SigAST NONEast   = new SigAST(Pos.UNKNOWN, "none",    "none", false, false,false,false,false, new ArrayList<String>(), new ArrayList<Decl>(), null, NONE);
}
