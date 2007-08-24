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

package edu.mit.csail.sdg.alloy4compiler.translator;

import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.NONE;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SEQIDX;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.SIGINT;
import static edu.mit.csail.sdg.alloy4compiler.ast.Sig.UNIV;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import kodkod.ast.Expression;
import kodkod.ast.Relation;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import nanoxml_2_2_3.XMLElement;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.UniqueNameGenerator;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprVar;
import edu.mit.csail.sdg.alloy4compiler.ast.Func;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Type;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;

/** This helper class contains helper routines for reading an A4Solution object from an XML file. */

public final class A4SolutionReader {

    /** The filename-to-content cache. */
    private final Map<String,String> fc = new LinkedHashMap<String,String>();

    /** The root of the XML document. */
    private XMLElement xml = null;

    /** This stores the root module parsed from the Alloy model. */
    private Module root = null;

    /** This stores the list of sigs. */
    private ConstList<Sig> sigs = null;

    //private PrimSig make(String signame, Map<String,String> name2parent, Map<String,PrimSig> name2sig) throws Err {
    //    if (signame==null || signame.length()==0 || signame.equals(UNIV.label)) return UNIV;
    //    if (signame.equals(SIGINT.label)) return SIGINT;
    //    if (signame.equals(SEQIDX.label)) return SEQIDX;
    //    if (signame.equals(NONE.label)) return NONE;
    //    PrimSig ans = name2sig.get(signame);
    //    if (ans!=null) return ans;
    //    PrimSig parent = make(name2parent.get(signame), name2parent, name2sig);
    //    ans = new PrimSig(null, parent, signame, false, false, false, false, false);
    //    name2sig.put(signame, ans);
    //    root.addSig(ans);
    //    return ans;
    //}

    /** Step1: parse the original Alloy model. */
    private void parseXML(String filename) throws Err {
        XMLElement x=null;
        try {
            FileInputStream fis=null;
            InputStreamReader reader=null;
            try {
                fis = new FileInputStream(filename);
                reader = new InputStreamReader(fis,"UTF-8");
                x = new XMLElement(new Hashtable(),true,false);
                x.parseFromReader(reader);
            } finally {
                Util.close(reader);
                Util.close(fis);
            }
        } catch(Throwable ex) {
            throw new ErrorFatal("Cannot read or parse the XML file: "+filename);
        }
        if (!x.is("alloy")) throw new ErrorSyntax("The XML file's root node must be <alloy>.");
        for(XMLElement sub: x.getChildren("source")) {
            String name = sub.getAttribute("filename");
            String content = sub.getAttribute("content");
            fc.put(name, content);
        }
        for(XMLElement sub: x.getChildren()) if (sub.is("instance")) { xml=sub; break; }
        if (xml==null) throw new ErrorSyntax("The XML file must contain an <instance> element.");
        try {
            if (fc.size()>0) {
                root = CompUtil.parseEverything_fromFile(fc, xml.getAttribute("filename"));
            } else {
                throw new ErrorSyntax("The original source files were not embedded in the saved instance file.");
                // root = new Module();
                // Map<String,String> ext = new LinkedHashMap<String,String>();
                // Map<String,PrimSig> make = new LinkedHashMap<String,PrimSig>();
                // for(XMLElement sub:xml.getChildren()) if (sub.is("sig")) {
                //     String name = sub.getAttribute("name");
                //     String parent = sub.getAttribute("extends");
                //     ext.put(name, parent==null ? "" : parent);
                // }
                // for(Map.Entry<String,String> e:ext.entrySet()) make(e.getKey(), ext, make);
            }
        } catch(Throwable ex) {
            throw new ErrorFatal("The original source files failed to be reconstructed.");
        }
        sigs = ConstList.make(root.getAllReachableSigs());
    }


    //============================================================================================================================//

    /** This maps each Sig and Field to a Kodkod expression. */
    private final Map<Object,Expression> a2k = new LinkedHashMap<Object,Expression>();

    /** Step2: construct all sigs and fields. */
    private void makeSigsAndFields() {
        Expression u = Relation.INTS.union(BoundsComputer.SEQ_SEQIDX);
        for(Sig s:sigs) if (!s.builtin) {
            Relation r = Relation.unary(s.label);
            u = u.union(r);
            a2k.put(s, r);
            for(Field f:s.getFields()) a2k.put(f, Relation.nary(s.label+"."+f.label, f.type.arity()));
        }
        a2k.put(UNIV, u);
        a2k.put(SIGINT, Relation.INTS);
        a2k.put(SEQIDX, BoundsComputer.SEQ_SEQIDX);
        a2k.put(NONE, Relation.NONE);
    }

    //============================================================================================================================//

    /** This stores the resulting Kodkod instance. */
    private Instance inst = null;

    /** Step3: construct the list of all atoms, then make the instance. */
    private void makeInstance(int bitwidth) throws Err {
        if (bitwidth<1 || bitwidth>30) throw new ErrorSyntax("Bitwidth of "+bitwidth+" is not allowed.");
        final int min=0-(1<<(bitwidth-1));
        final int max=(1<<(bitwidth-1))-1;
        LinkedHashSet<String> atoms = new LinkedHashSet<String>();
        for(XMLElement sub: xml.getChildren()) {
            if (sub.is("sig") || sub.is("set")) {
                for(XMLElement atom:sub.getChildren()) if (atom.is("atom")) atoms.add(atom.getAttribute("name"));
            }
        }
        for(int i=min; i<=max; i++) atoms.add(""+i);
        Universe u = new Universe(atoms);
        TupleFactory f = u.factory();
        inst = new Instance(u);
        for(Map.Entry<Object,Expression> e: a2k.entrySet()) {
            Expression r = e.getValue();
            if (r!=Relation.UNIV && r!=Relation.INTS && r!=Relation.NONE && r instanceof Relation)
               inst.add((Relation)r, f.noneOf(r.arity()));
        }
        TupleSet next = f.noneOf(2);
        for(int i=min; i<=max; i++) {
            Tuple t=f.tuple(""+i);
            inst.add(i, f.range(t,t));
            if (i+1<=max) next.add(t.product(f.tuple(""+(i+1))));
        }
        inst.add(BoundsComputer.SIGINT_NEXT, next);
        inst.add(BoundsComputer.SIGINT_MAX, f.range(f.tuple(""+max), f.tuple(""+max)));
        inst.add(BoundsComputer.SIGINT_ZERO, f.range(f.tuple("0"), f.tuple("0")));
        inst.add(BoundsComputer.SIGINT_MIN, f.range(f.tuple(""+min), f.tuple(""+min)));
    }

    //============================================================================================================================//

    /** This maps the "sig name" in the XML file to its corresponding Sig object. */
    private final Map<String,Sig> name2sig = new LinkedHashMap<String,Sig>();

    /** This maps each atom to the most specific sig. */
    private final Map<String,PrimSig> atom2sig = new LinkedHashMap<String,PrimSig>();

    /** This maps each skolem name to its tupleset. */
    private final Map<String,TupleSet> skolems = new LinkedHashMap<String,TupleSet>();

    /** This allows us to choose unique names for each skolem value. */
    private final UniqueNameGenerator un = new UniqueNameGenerator();

    /** Step4: process all "sig" and "set" elements in the XML file. */
    private void processSigAndSet() throws Err {
        TupleFactory tf = inst.universe().factory();
        again:
        for(XMLElement sub:xml.getChildren()) if (sub.is("sig") || sub.is("set")) {
           // Parse the tuple set
           TupleSet ts = tf.noneOf(1);
           for(XMLElement atom:sub.getChildren()) if (atom.is("atom")) {
              String atomname = atom.getAttribute("name");
              Tuple tuple = tf.tuple(atomname);
              ts.add(tuple);
           }
           // If it's one of the PrimSig or SubsetSig, then read its atoms and add them to the instance
           String name=sub.getAttribute("name"), sname="this/"+name;
           for(Sig s:sigs) if (s.label.equals(name) || s.label.equals(sname)) {
              name2sig.put(name,s);
              if (s==UNIV || s==SIGINT || s==NONE) continue again;
              if (s!=SEQIDX && s instanceof PrimSig) for(Tuple tp:ts) {
                 String atom = (String) tp.atom(0);
                 PrimSig oldsig = atom2sig.get(atom);
                 if (oldsig==null || s.isSameOrDescendentOf(oldsig)) atom2sig.put(atom,(PrimSig)s);
              }
              Relation r = (Relation) (a2k.get(s));
              ts.addAll(inst.tuples(r));
              inst.add(r, ts);
              continue again;
           }
           // Otherwise, that means it is a skolem set
           while(name.length()>0 && name.charAt(0)=='$') name=name.substring(1);
           if (name.length()==0) name="x"; // Any default would do
           skolems.put(un.make("$"+name), ts);
        }
    }

    //============================================================================================================================//

    /** Step5: process all "field" elements in the XML file. */
    private void processField() {
        TupleFactory tf=inst.universe().factory();
        again:
        for(XMLElement sub:xml.getChildren()) if (sub.is("field")) {
           // Parse the type
           String name=sub.getAttribute("name");
           Type type=Type.EMPTY;
           for(XMLElement t:sub.getChildren()) if (t.is("type")) for(XMLElement s:t.getChildren()) if (s.is("sig")) {
               Sig sg = name2sig.get(s.getAttribute("name"));
               if (sg == null) continue again; // This field contains nonexistent sig!
               if (type == Type.EMPTY) type=sg.type; else type=type.product(sg.type);
           }
           // Parse the tuple set
           TupleSet ts = tf.noneOf(type.arity());
           for(XMLElement t:sub.getChildren()) if (t.is("tuple")) {
              Tuple tp = null;
              for(XMLElement s:t.getChildren()) if (s.is("atom")) {
                 Tuple tp2 = tf.tuple(s.getAttribute("name"));
                 if (tp==null) tp=tp2; else tp=tp.product(tp2);
              }
              ts.add(tp);
           }
           // If it's one of the Field, then read its tuples and add them to the instance
           for(Sig s:sigs) for(Field f:s.getFields()) if (f.label.equals(name) && f.type.firstColumnOverlaps(type)) {
               Relation r = (Relation) (a2k.get(f));
               ts.addAll(inst.tuples(r));
               inst.add(r, ts);
               continue again;
           }
           // Otherwise, that means it is a skolem relation
           while(name.length()>0 && name.charAt(0)=='$') name=name.substring(1);
           if (name.length()==0) name="x"; // Any default would do
           skolems.put(un.make("$"+name), ts);
        }
    }

    //============================================================================================================================//

    /** Step6: add the atoms and skolems so that they can be referred to by the evaluator. */
    private void addSkolems() throws Err {
        TupleFactory tf = inst.universe().factory();
        List<ExprVar> empty = new ArrayList<ExprVar>();
        for(Object atom: inst.universe()) {
            String n = (String)atom;
            PrimSig ret = atom2sig.get(n);
            if (ret!=null) {
                Func func = new Func(null, n, empty, ret);
                root.addGlobal(n, func.call());
                Relation r = Relation.unary(n);
                inst.add(r, tf.range(tf.tuple(n), tf.tuple(n)));
                a2k.put(func, r);
            }
        }
        again:
        for(Map.Entry<String,TupleSet> s:skolems.entrySet()) {
            int a = s.getValue().arity();
            String n = s.getKey();
            Expr ret = NONE;
            while(ret.type.arity()<a) ret=ret.product(NONE);
            for(Tuple tp: s.getValue()) {
                Expr one = null;
                for(int i=0; i<a; i++) {
                    PrimSig sig = atom2sig.get(tp.atom(i));
                    if (sig==null) continue again;
                    if (one==null) one=sig; else one=one.product(sig);
                }
                if (ret.type.hasNoTuple()) ret=one; else ret=ret.plus(one);
            }
            Func func = new Func(null, n, empty, ret);
            root.addGlobal(n, func.call());
            Relation r = Relation.nary(n,a);
            inst.add(r, s.getValue());
            a2k.put(func, r);
        }
    }

    //============================================================================================================================//

    /** This stores the resulting A4Solution object. */
    private A4Solution sol = null;

    /** Parse the XML element into an AlloyInstance if possible. */
    private A4SolutionReader(String filename) throws Err {
        parseXML(filename);
        makeSigsAndFields();
        int bitwidth = Integer.parseInt(xml.getAttribute("bitwidth"));
        makeInstance(bitwidth);
        processSigAndSet();
        processField();
        addSkolems();
        String command = xml.getAttribute("command");
        filename = xml.getAttribute("filename");
        sol = new A4Solution(sigs, ConstMap.make(a2k), filename, fc, command, null, null, null, bitwidth, inst, null, null, null);
    }

    /** Parse the XML element into an AlloyInstance. */
    public static Pair<Module,A4Solution> read(String filename) throws Err {
        try {
            A4SolutionReader x = new A4SolutionReader(filename);
            return new Pair<Module,A4Solution>(x.root, x.sol);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw ((Err)ex);
            throw new ErrorFatal("Fatal error occured: "+ex, ex);
        }
    }
}
