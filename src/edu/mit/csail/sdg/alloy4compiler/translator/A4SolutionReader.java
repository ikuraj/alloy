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
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
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
import edu.mit.csail.sdg.alloy4.ConstMap;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4.ConstMap.TempMap;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.Field;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.PrimSig;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig.SubsetSig;
import edu.mit.csail.sdg.alloy4compiler.parser.CompUtil;
import edu.mit.csail.sdg.alloy4compiler.parser.Module;

/**
 * This helper class contains helper routines for reading an A4Solution object from an XML file.
 */

public final class A4SolutionReader {

    /** Use the XML library to parse the given file into an XMLElement object. */
    private static XMLElement readElement(File file) throws IOException {
        FileInputStream fis=null;
        InputStreamReader reader=null;
        try {
            fis = new FileInputStream(file);
            reader = new InputStreamReader(fis,"UTF-8");
            XMLElement xml = new XMLElement(new Hashtable(),true,false);
            xml.parseFromReader(reader);
            return xml;
        } finally {
            Util.close(reader);
            Util.close(fis);
        }
    }

    private static void getField(Sig s, String fieldName, Map<String,TupleSet> cache, TupleSet ans) {
        if (s instanceof PrimSig) {
            String sn=s.label;
            if (sn.startsWith("this/")) sn=sn.substring(5);
            TupleSet tmp=cache.remove(sn+"["+fieldName+"]");
            if (tmp!=null && tmp.arity()==ans.arity()) ans.addAll(tmp);
            return;
        }
        for(Sig c: ((SubsetSig)s).parents) getField(c, fieldName, cache, ans);
    }

    /** Parse the XML element into an AlloyInstance if possible. */
    private static A4Solution parseInstance(final ConstMap<String,String> fc, final XMLElement x) throws Err {
        final String filename = x.getAttribute("filename");
        final String command = x.getAttribute("command");
        Module world;
        try {
            world = CompUtil.parseEverything_fromFile(new LinkedHashMap<String,String>(fc), filename);
        } catch(Throwable ex) {
            throw new RuntimeException("The original source files failed to be reconstructed.");
        }
        LinkedHashSet<String> atoms=new LinkedHashSet<String>();
        LinkedHashMap<String,List<String>> sig2atoms=new LinkedHashMap<String,List<String>>(); // The list may contain duplicates
        LinkedHashMap<String,List<String>> set2atoms=new LinkedHashMap<String,List<String>>(); // The list may contain duplicates
        for(XMLElement sub:x.getChildren()) {
            boolean isSig=sub.is("sig");
            if (isSig || sub.is("set")) {
                String name=sub.getAttribute("name");
                if (name.indexOf('/')<0 && !name.equals("univ") && !name.equals("Int")) name="this/"+name;
                List<String> array = isSig ? sig2atoms.get(name) : set2atoms.get(name);
                if (array==null) array = new ArrayList<String>();
                for(XMLElement atom:sub.getChildren("atom")) { array.add(atom.getAttribute("name")); }
                atoms.addAll(array);
                if (isSig) sig2atoms.put(name,array); else set2atoms.put(name,array);
            }
        }
        Universe u=new Universe(atoms);
        TupleFactory tf=u.factory();
        Instance i=new Instance(u);
        LinkedHashMap<Object,Expression> obj2expr = new LinkedHashMap<Object,Expression>();
        obj2expr.put(UNIV, Relation.UNIV);
        obj2expr.put(SIGINT, Relation.INTS);
        obj2expr.put(NONE, Relation.NONE);
        for(Sig s:world.getAllReachableSigs()) if (s!=NONE && s!=UNIV) {
            List<String> atms = (s instanceof PrimSig) ? sig2atoms.remove(s.label) : set2atoms.remove(s.label);
            TupleSet ts=tf.noneOf(1);
            if (atms!=null) for(String a:atms) {
                Tuple ta=tf.tuple(a);
                if (s==SIGINT) i.add(Integer.parseInt(a), tf.range(ta,ta));
                ts.add(ta);
            }
            if (s!=SIGINT) {
                Relation r = (s==SEQIDX) ? BoundsComputer.SEQ_SEQIDX : Relation.unary(s.label);
                i.add(r,ts);
                obj2expr.put(s,r);
            }
        }
        LinkedHashMap<String,TupleSet> field2expr = new LinkedHashMap<String,TupleSet>();
        for(XMLElement sub:x.getChildren("field")) {
            int arity=0;
            String fn=null;
            for(XMLElement t:sub.getChildren()) if (t.is("type")) {
                for(XMLElement s:t.getChildren()) if (s.is("sig")) {
                    arity++;
                    if (arity==1) fn=s.getAttribute("name")+"["+sub.getAttribute("name")+"]";
                }
            }
            if (arity<1 || fn==null) continue;
            TupleSet ts=field2expr.get(fn);
            if (ts==null) ts=tf.noneOf(arity);
            if (ts.arity()!=arity) continue; // If we encounter an illegal value, we skip it.
            for(XMLElement t:sub.getChildren()) if (t.is("tuple")) {
                Tuple tuple=null;
                for(XMLElement a:t.getChildren()) if (a.is("atom")) {
                    Tuple temp=tf.tuple(a.getAttribute("name"));
                    if (tuple==null) tuple=temp; else tuple=tuple.product(temp);
                }
                if (tuple!=null && tuple.arity()==arity) ts.add(tuple);
            }
            field2expr.put(fn, ts);
        }
        for(Sig s:world.getAllReachableSigs()) if (!s.builtin) {
            for(Field f:s.getFields()) {
                TupleSet ts=tf.noneOf(f.type.arity());
                getField(s, f.label, field2expr, ts);
                Relation fr=Relation.nary(s.label+" <: "+f.label, ts.arity());
                obj2expr.put(f, fr);
                i.add(fr, ts);
            }
        }
        // Bound SIGINT_{NEXT,MAX,ZERO,MIN}
        final int bitwidth=Integer.parseInt(x.getAttribute("bitwidth"));
        if (bitwidth<1 || bitwidth>30) throw new RuntimeException("Bitwidth of "+bitwidth+" is not allowed.");
        int min=0-(1<<(bitwidth-1));
        int max=(1<<(bitwidth-1))-1;
        TupleSet next=tf.noneOf(2);
        for(int h=min; h<max; h++) next.add(tf.tuple(""+h).product(tf.tuple(""+(h+1))));
        i.add(BoundsComputer.SIGINT_NEXT, next);
        i.add(BoundsComputer.SIGINT_MAX, tf.range(tf.tuple(""+max), tf.tuple(""+max)));
        i.add(BoundsComputer.SIGINT_ZERO, tf.range(tf.tuple("0"), tf.tuple("0")));
        i.add(BoundsComputer.SIGINT_MIN, tf.range(tf.tuple(""+min), tf.tuple(""+min)));
        // Add each atom so that you can refer to them in the evaluator
        for(String a:atoms) if (a.indexOf('$')>=0) {
            if (a.startsWith("this/")) a=a.substring(5);
            TupleSet ts=tf.noneOf(1);
            ts.add(tf.tuple(a));
            Relation r=Relation.unary(a);
            obj2expr.put(a,r);
            i.add(r, ts);
        }
        // Add the skolem sets
        for(Map.Entry<String,List<String>> e: set2atoms.entrySet()) {
            TupleSet ts=tf.noneOf(1);
            for(String a: e.getValue()) ts.add(tf.tuple(a));
            String n=e.getKey();
            if (n.startsWith("this/")) n=n.substring(5);
            if (n.length()==0 || n.charAt(0)!='$') n="$"+n;
            Relation r=Relation.unary(n);
            obj2expr.put(n, r);
            i.add(r, ts);
        }
        // Add the skolem relations
        for(Map.Entry<String,TupleSet> e: field2expr.entrySet()) {
            String n=e.getKey();
            if (n.startsWith("this/")) n=n.substring(5);
            if (n.length()==0 || n.charAt(0)!='$') n="$"+n;
            Relation r=Relation.nary(n, e.getValue().arity());
            obj2expr.put(n, r);
            i.add(r, e.getValue());
        }
        // Done
        return new A4Solution(world, obj2expr, filename, fc, command, null, null, null, bitwidth, i, null, null, null);
    }


    /**
     * Parse the file into an AlloyInstance if possible.
     * @throws ErrorFatal - if an error occurred in reading of the XML file.
     * @throws ErrorSyntax - if there is a syntax error in the XML file.
     */
    public static A4Solution readXML(final String file) throws Err {
        final XMLElement xml;
        try {
            xml=readElement(new File(file));
        } catch(IOException ex) {
            throw new ErrorFatal("Cannot read or parse the XML file: "+file);
        }
        if (!xml.is("alloy")) throw new ErrorSyntax("The XML file's root node must be <alloy>.");
        A4Solution instance = null;
        TempMap<String,String> fc = new TempMap<String,String>();
        for(XMLElement sub: xml.getChildren()) if (sub.is("source")) {
            String name = sub.getAttribute("filename");
            String content = sub.getAttribute("content");
            fc.put(name,content);
        }
        if (fc.size()==0) throw new RuntimeException("The original source files were not embedded in the saved instance file.");
        ConstMap<String,String> cfc = fc.makeConst();
        for(XMLElement sub: xml.getChildren("instance")) { instance=parseInstance(cfc, sub); break; }
        if (instance==null) throw new ErrorSyntax("The XML file does not have an <instance> element.");
        return instance;
    }
}
