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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4viz;

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
import java.util.Set;
import java.util.TreeSet;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Util;
import nanoxml_2_2_3.XMLElement;

/**
 * This utility class parses an XML file into an AlloyInstance object.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */

public final class StaticInstanceReader {

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private StaticInstanceReader() { }

    /** Use the XML library to parse the file into an XMLElement object. */
    private static XMLElement readElement(File file) {
        FileInputStream fis=null;
        InputStreamReader reader=null;
        try {
            fis = new FileInputStream(file);
            reader = new InputStreamReader(fis,"UTF-8");
            XMLElement xml = new XMLElement(new Hashtable(),true,false);
            xml.parseFromReader(reader);
            return xml;
        } catch(IOException ex) {
            throw new RuntimeException("I/O error: "+ex);
        } finally {
            Util.close(reader);
            Util.close(fis);
        }
    }

    /**
     * Parse the file into an AlloyInstance if possible.
     * @throws ErrorFatal - if an error occurred in reading of the XML file.
     * @throws ErrorSyntax - if there is a syntax error in the XML file.
     */
    public static AlloyInstance parseInstance(File file) throws ErrorFatal, ErrorSyntax {
        XMLElement xml=readElement(file);
        if (!xml.is("alloy")) throw new ErrorSyntax("The XML file's root node must be <alloy>.");
        AlloyInstance instance=null;
        String kinput="", koutput="";
        for(XMLElement sub: xml.getChildren()) {
            if (sub.is("kinput")) kinput = sub.getAttribute("value");
            else if (sub.is("koutput")) koutput = sub.getAttribute("value");
        }
        for(XMLElement sub: xml.getChildren("instance")) { instance = parseInstance(sub, kinput, koutput); break; }
        if (instance==null) throw new ErrorSyntax("The XML file does not have an <instance> element.");
        return instance;
    }

    /**
     * Parse the XML element into an AlloyInstance if possible
     * @param x - the XML element labeled "instance"
     * @param kinput - the kodkod input we want to include with the AlloyInstance object
     * @param koutput - the kodkod output we want to include with the AlloyInstance object
     */
    private static AlloyInstance parseInstance(XMLElement x, String kinput, String koutput) {
        boolean isMetamodel = x.getAttribute("isMetamodel").length()>0;
        String filename = x.getAttribute("filename");
        String commandname = x.getAttribute("command");
        // Generate "types"
        Map<String,AlloyType> types=new LinkedHashMap<String,AlloyType>();
        for(XMLElement sub:x.getChildren("sig")) {
            String name=sub.getAttribute("name");
            if (name.length()==0) throw new RuntimeException("<sig> name cannot be empty.");
            AlloyType type=new AlloyType(name,
                    sub.getAttribute("isOne").length()>0,
                    sub.getAttribute("isAbstract").length()>0,
                    sub.getAttribute("isBuiltin").length()>0,
                    sub.getAttribute("isOrdered").length()>0);
            if (types.put(name,type)!=null)
                throw new RuntimeException("<sig name=\""+name+"\"> appeared more than once.");
        }
        types.put("univ", AlloyType.UNIV);
        // Generate the extends relationship and all the atoms
        Map<AlloyType,AlloyType> ts = parseTypeStructure(x, types);
        Map<String,AlloyAtom> atomname2atom = parseAllAtoms(x, types, ts);
        // Generate "sets" and "atom2sets"
        Map<AlloyAtom,Set<AlloySet>> atom2sets = new LinkedHashMap<AlloyAtom,Set<AlloySet>>();
        for(Map.Entry<String,AlloyAtom> e:atomname2atom.entrySet()) {
            // The following is needed since atom2sets's KeySet is considered the universe of all atoms
            atom2sets.put(e.getValue(), new LinkedHashSet<AlloySet>());
        }
        Set<AlloySet> sets=new LinkedHashSet<AlloySet>();
        for(XMLElement sub:x.getChildren("set")) {
            String name=sub.getAttribute("name");
            if (name.length()==0) throw new RuntimeException("<set> name cannot be empty.");
            String typename=sub.getAttribute("type");
            if (typename.length()==0) typename="univ";
            AlloyType type=types.get(typename);
            if (type==null) throw new RuntimeException("<set name=\""+name+"\"> cannot be a subset of a nonexisting type \""+type.getName()+"\"");
            Set<AlloyAtom> atoms=parseAlloyAtomS(sub, atomname2atom);
            AlloySet set=new AlloySet(name,type);
            sets.add(set);
            for(AlloyAtom atom:atoms) {
                Set<AlloySet> temp=atom2sets.get(atom);
                if (temp==null) { temp=new LinkedHashSet<AlloySet>(); atom2sets.put(atom,temp); }
                temp.add(set);
            }
        }
        // Generate "rels" and "rel2tuples"
        Map<AlloyRelation,Set<AlloyTuple>> rel2tuples = new LinkedHashMap<AlloyRelation,Set<AlloyTuple>>();
        Set<AlloyRelation> rels=new LinkedHashSet<AlloyRelation>();
        for(XMLElement sub:x.getChildren("field")) {
            String name=sub.getAttribute("name");
            if (name.length()==0) throw new RuntimeException("<field> name cannot be empty.");
            if (sub.getChildren().isEmpty())
                throw new RuntimeException("<field name=\""+name+"\"> must declare its type.");
            XMLElement sub1=sub.getChildren().get(0);
            if (!sub1.is("type"))
                throw new RuntimeException("<field name=\""+name+"\"> must have <type> as its first subnode.");
            AlloyRelation r=parseAlloyRelation(name, types, sub1);
            rels.add(r);
            Set<AlloyTuple> tuples=parseAlloyTupleS(sub, atomname2atom);
            Set<AlloyTuple> temp=rel2tuples.get(r);
            if (temp==null) { temp=new LinkedHashSet<AlloyTuple>(); rel2tuples.put(r,temp); }
            temp.addAll(tuples);
        }
        AlloyModel model = new AlloyModel(types.values(), sets, rels, ts);
        return new AlloyInstance(filename, commandname, kinput, koutput, model, atom2sets, rel2tuples, isMetamodel);
    }

    /**
     * Generate all the AlloyAtom objects.
     * @param xml - the XML node
     * @param ts - the "extends" relationship computed from parseTypeStructure()
     */
    private static Map<String,AlloyAtom> parseAllAtoms(XMLElement xml, Map<String,AlloyType> types, Map<AlloyType,AlloyType> ts) {
        Map<String,AlloyType> atom2type=new LinkedHashMap<String,AlloyType>();
        Map<AlloyType,Set<String>> type2atoms=new LinkedHashMap<AlloyType,Set<String>>();
        // Compute the atom2type and type2atom maps
        for(XMLElement node:xml.getChildren("sig")) {
            AlloyType sig = types.get(node.getAttribute("name")); // We already know this will not be null
            if (!type2atoms.containsKey(sig))
                type2atoms.put(sig, new TreeSet<String>()); // Must be LinkedHashSet since we want atoms in order
            for(XMLElement atom:node.getChildren("atom")) {
                String name=atom.getAttribute("name");
                if (name.length()==0) throw new RuntimeException("<atom> name cannot be empty.");
                AlloyType type=atom2type.get(name);
                if (type==null || isSubtype(ts,sig,type)) {
                    atom2type.put(name, sig);
                    if (type!=null) { Set<String> set=type2atoms.get(type); if (set!=null) set.remove(name); }
                    type2atoms.get(sig).add(name);
                }
            }
        }
        // Now that we know the most specific type that an atom belongs to, we can create the AlloyAtom objects
        Map<String,AlloyAtom> ans=new LinkedHashMap<String,AlloyAtom>();
        for(Map.Entry<AlloyType,Set<String>> e:type2atoms.entrySet()) {
            AlloyType type=e.getKey();
            if (type.getName().equals("Int")) {
                // Special handling for Int atoms
                int n=e.getValue().size();
                for(String atom:e.getValue()) ans.put(atom, new AlloyAtom(type, ((n==1)?Integer.MAX_VALUE:parseInt(atom)), atom));
            } else {
                int n=e.getValue().size(), i=0;
                for(String atom:e.getValue()) {
                    ans.put(atom, new AlloyAtom(type, ((n==1)?Integer.MAX_VALUE:i), atom));
                    i++;
                }
            }
        }
        return ans;
    }

    /** Parses the String to get an integer, and throw an appropriate exception if it is not parsable. */
    private static int parseInt(String text) {
        int n;
        try {n=Integer.parseInt(text);}
        catch(NumberFormatException ex) {throw new RuntimeException("\""+text+"\" is not a valid integer.");}
        return n;
    }

    /**
     * Generate the extends relation: "If A extends B, and B is not univ, then (A,B) will be in the answer".
     * <p>  We guarantee the following:
     * <br> (1) reply.keySet() is always equal or subset of the set of "sig" declarations we see
     * <br> (2) reply.valueSet() is always equal or subset of the set of "sig" declarations we see
     * <br> (3) "univ" is never in the keySet nor valueSet
     * <br> (4) null is never in the keySet nor valueSet
     * <br> (5) there is no cycle in this relation
     * <br> We throw an exception if a sig tries to extend an undeclared sig
     * <br> We throw an exception if there is a cycle in the extends relationship
     */
    private static Map<AlloyType,AlloyType> parseTypeStructure (XMLElement xml, Map<String,AlloyType> allTypes) {
        Map<AlloyType,AlloyType> ts = new LinkedHashMap<AlloyType,AlloyType>();
        for(XMLElement sig:xml.getChildren("sig")) {
            String name = sig.getAttribute("name");
            String ext = sig.getAttribute("extends");
            if (name.equals("univ")) continue; // "univ" must not be in the keyset
            if (ext.length()==0) ext="univ";
            AlloyType sup=allTypes.get(ext);
            if (sup==null) throw new RuntimeException("<sig name=\""+name+"\"> tries to extend an undeclared sig \""+ext+"\"");
            AlloyType me=allTypes.get(name);
            if (me==null) throw new RuntimeException("<sig name=\""+name+"\"> does not exist");
            ts.put(me, sup);
        }
        for(Map.Entry<AlloyType,AlloyType> e:ts.entrySet()) {
            AlloyType me=e.getKey();
            if (AlloyModel.isCycle(ts,me)) throw new RuntimeException(
                    "sig \""+me.getName()+"\" has an infinite cycle in its extends relation.");
        }
        return ts;
    }

    /** Parses XML to generate an AlloyRelation object. */
    private static AlloyRelation parseAlloyRelation(String relName, Map<String,AlloyType> types, XMLElement xml) {
        // parses one or more <sig name=".."/>
        List<AlloyType> list=new ArrayList<AlloyType>();
        for(XMLElement type:xml.getChildren("sig")) {
            String name=type.getAttribute("name");
            if (name.length()==0) throw new RuntimeException("<sig> name cannot be empty.");
            AlloyType t = types.get(name);
            if (t==null) throw new RuntimeException("<field> cannot reference a non-existing sig \""+name+"\"");
            list.add(t);
        }
        if (list.size()<2) throw new RuntimeException("<type> must contain at least two <sig> subnode.");
        return new AlloyRelation(relName, list);
    }

    /** Parses XML to generate a set of AlloyTuple objects. */
    private static Set<AlloyTuple> parseAlloyTupleS(XMLElement xml, Map<String,AlloyAtom> atomname2atom) {
        // parses zero or more <tuple>..</tuple>
        Set<AlloyTuple> ans=new LinkedHashSet<AlloyTuple>();
        for(XMLElement node:xml.getChildren("tuple")) ans.add(parseAlloyTuple(node,atomname2atom));
        return ans;
    }

    /** Parses XML to generate an AlloyTuple object. */
    private static AlloyTuple parseAlloyTuple(XMLElement xml, Map<String,AlloyAtom> atomname2atom) {
        // parses one or more <atom name=".."/>
        List<AlloyAtom> ans=new ArrayList<AlloyAtom>();
        for(XMLElement node:xml.getChildren("atom")) {
            String name=node.getAttribute("name");
            AlloyAtom atom=atomname2atom.get(name);
            if (atom==null) throw new RuntimeException("<atom> "+name+" is undeclared!");
            ans.add(atom);
        }
        if (ans.size()<2) throw new RuntimeException("<tuple> must contain two or more <atom>.");
        return new AlloyTuple(ans);
    }

    /** Parses XML to generate a set of AlloyAtom objects. */
    private static Set<AlloyAtom> parseAlloyAtomS(XMLElement xml, Map<String,AlloyAtom> atomname2atom) {
        // parses zero or more <atom name=".."/>
        Set<AlloyAtom> ans = new LinkedHashSet<AlloyAtom>();
        for(XMLElement node:xml.getChildren("atom")) {
            String name=node.getAttribute("name");
            AlloyAtom atom=atomname2atom.get(name);
            if (atom==null) throw new RuntimeException("<atom> "+name+" is undeclared!");
            ans.add(atom);
        }
        return ans;
    }

    /**
     * Returns true if "subType" is a direct or indirect subsig of "superType".
     * <br> If subType==null or superType==null, it always returns false.
     */
    private static boolean isSubtype(Map<AlloyType,AlloyType> map, AlloyType subType, AlloyType superType) {
        if (superType==null) return false;
        if (superType.equals(AlloyType.UNIV) && subType!=null && !subType.equals(superType)) return true;
        while(subType!=null) {
            subType = map.get(subType); // We call map.get() before equal(), since we want isSubtype(A,A) to be false
            if (superType.equals(subType)) return true;
        }
        return false;
    }
}
