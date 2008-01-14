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

package edu.mit.csail.sdg.alloy4viz;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import nanoxml_2_2_3.XMLElement;
import nanoxml_2_2_3.XMLParseException;
import edu.mit.csail.sdg.alloy4.Util;

/**
 * This utility class contains methods to read and write VizState customizations.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class StaticThemeReaderWriter {

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private StaticThemeReaderWriter() { }

    /** Read the XML file and merge its settings into an existing VizState object. */
    public static void readAlloy(String filename, VizState theme) throws IOException {
        File file=new File(filename);
        FileInputStream fis=null;
        InputStreamReader reader=null;
        try {
            fis = new FileInputStream(file);
            reader = new InputStreamReader(fis,"UTF-8");
            XMLElement elem = new XMLElement(new Hashtable<Object,Object>(), true, false);
            try {
                elem.parseFromReader(reader);
            } catch (XMLParseException e) {
                throw new IOException("The file \""+file.getPath()+"\" is not a valid XML file.");
            }
            try {
                for(XMLElement sub: elem.getChildren("view")) parseView(sub,theme);
            } catch(Throwable e) {
                throw new IOException("An error occurred in reading or parsing the file \""+file.getPath()+"\"");
            }
        } finally {
            Util.close(reader);
            Util.close(fis);
        }
    }

    /** Write the VizState's customizations into a new file (which will be overwritten if it exists). */
    public static void writeAlloy(String filename, VizState theme) throws IOException {
        PrintWriter bw = new PrintWriter(filename,"UTF-8");
        bw.write("<?xml version=\"1.0\"?>\n<alloy>\n\n");
        if (theme!=null) {
            try {
                writeView(bw, theme);
            } catch(IOException ex) {
                Util.close(bw);
                throw new IOException("Error writing to the file \""+filename+"\"");
            }
        }
        bw.write("\n</alloy>\n");
        if (!Util.close(bw)) throw new IOException("Error writing to the file \""+filename+"\"");
    }

    /*============================================================================================*/

    /** Does nothing if the element is malformed. */
    private static void parseView(XMLElement x, VizState now) {
        /*
         * <view orientation=".." nodetheme=".." edgetheme=".." useOriginalAtomNames="yes/no" fontsize="12">
         *   <projection> .. </projection>
         *   <defaultnode../>
         *   <defaultedge../>
         *   0 or more NODE or EDGE
         * </view>
         */
        if (!"view".equals(x.getName())) return;
        if (has(x,"useOriginalAtomNames")) now.useOriginalName(getbool(x,"useOriginalAtomNames"));
        if (has(x,"fontsize")) now.setFontSize(getint(x,"fontsize"));
        if (has(x,"orientation")) now.setOrientation(parseDotOrientation(x));
        if (has(x,"nodetheme")) now.setNodePalette(parseDotPalette(x,"nodetheme"));
        if (has(x,"edgetheme")) now.setEdgePalette(parseDotPalette(x,"edgetheme"));
        for(XMLElement xml:x.getChildren()) {
            if (xml.is("projection")) {
                now.deprojectAll();
                for(AlloyType t:parseProjectionList(now,xml)) now.project(t);
            }
            else if (xml.is("defaultnode")) parseNodeViz(xml, now, null);
            else if (xml.is("defaultedge")) parseEdgeViz(xml, now, null);
            else if (xml.is("node")) {
                for(XMLElement sub:xml.getChildren("type")) {
                    AlloyType t=parseAlloyType(now,sub); if (t!=null) parseNodeViz(xml, now, t);
                }
                for(XMLElement sub:xml.getChildren("set")) {
                    AlloySet s=parseAlloySet(now,sub); if (s!=null) parseNodeViz(xml, now, s);
                }
            }
            else if (xml.is("edge")) {
                for(XMLElement sub:xml.getChildren("relation")) {
                    AlloyRelation r=parseAlloyRelation(now,sub); if (r!=null) parseEdgeViz(xml, now, r);
                }
            }
        }
    }

    /*============================================================================================*/

    /** Writes nothing if the argument is null. */
    private static void writeView(PrintWriter out, VizState view) throws IOException {
        if (view==null) return;
        VizState defaultView=new VizState(view.getOriginalInstance());
        out.write("<view");
        writeDotOrientation(out, view.getOrientation(), defaultView.getOrientation());
        writeDotPalette(out, "nodetheme", view.getNodePalette(), defaultView.getNodePalette());
        writeDotPalette(out, "edgetheme", view.getEdgePalette(), defaultView.getEdgePalette());
        if (view.useOriginalName()!=defaultView.useOriginalName()) {
            out.write(" useOriginalAtomNames=\"");
            out.write(view.useOriginalName() ? "yes" : "no");
            out.write("\"");
        }
        if (view.getFontSize()!=defaultView.getFontSize()) {
            out.write(" fontsize=\""+view.getFontSize()+"\"");
        }
        out.write(">\n");
        if (view.getProjectedTypes().size()>0) writeProjectionList(out, view.getProjectedTypes());
        out.write("\n<defaultnode" + writeNodeViz(view, defaultView, null));
        out.write("/>\n\n<defaultedge" + writeEdgeViz(view, defaultView, null));
        out.write("/>\n");
        // === nodes ===
        Set<AlloyNodeElement> types = new TreeSet<AlloyNodeElement>();
        types.addAll(view.getOriginalModel().getTypes());
        types.addAll(view.getCurrentModel().getTypes());
        types.addAll(view.getOriginalModel().getSets());
        types.addAll(view.getCurrentModel().getSets());
        Map<String,Set<AlloyNodeElement>> viz2node=new TreeMap<String,Set<AlloyNodeElement>>();
        for(AlloyNodeElement t:types) {
            String str=writeNodeViz(view,defaultView,t);
            //if (str.length()==0) continue;
            Set<AlloyNodeElement> nodes=viz2node.get(str);
            if (nodes==null) viz2node.put(str, nodes=new TreeSet<AlloyNodeElement>());
            nodes.add(t);
        }
        for(Map.Entry<String,Set<AlloyNodeElement>> e:viz2node.entrySet()) {
            out.write("\n<node"+e.getKey()+">\n");
            for(AlloyNodeElement ts:e.getValue()) {
                if (ts instanceof AlloyType) writeAlloyType(out,(AlloyType)ts);
                else if (ts instanceof AlloySet) writeAlloySet(out,(AlloySet)ts);
            }
            out.write("</node>\n");
        }
        // === edges ===
        Set<AlloyRelation> rels = new TreeSet<AlloyRelation>();
        rels.addAll(view.getOriginalModel().getRelations());
        rels.addAll(view.getCurrentModel().getRelations());
        Map<String,Set<AlloyRelation>> viz2edge=new TreeMap<String,Set<AlloyRelation>>();
        for(AlloyRelation r:rels) {
            String str=writeEdgeViz(view,defaultView,r);
            if (str.length()==0) continue;
            Set<AlloyRelation> edges=viz2edge.get(str);
            if (edges==null) viz2edge.put(str, edges=new TreeSet<AlloyRelation>());
            edges.add(r);
        }
        for(Map.Entry<String,Set<AlloyRelation>> e:viz2edge.entrySet()) {
            out.write("\n<edge"+e.getKey()+">\n");
            for(AlloyRelation r:e.getValue()) writeAlloyRelation(out,r);
            out.write("</edge>\n");
        }
        // === done ===
        out.write("\n</view>\n");
    }

    /*============================================================================================*/

    /** Writes nothing if the argument is null. */
    public static String dumpView(VizState view) throws IOException {
        StringWriter sw=new StringWriter();
        PrintWriter out=new PrintWriter(sw);
        out.write("#defaultnode" + saveNodeViz(view,null) + "\tprojected=\"no\"\n");
        Set<AlloyElement> types = new TreeSet<AlloyElement>();
        types.addAll(view.getOriginalModel().getTypes());
        types.addAll(view.getCurrentModel().getTypes());
        types.addAll(view.getOriginalModel().getSets());
        types.addAll(view.getCurrentModel().getSets());
        for(AlloyElement x:types) {
            if (x instanceof AlloyType) {
                AlloyType t=(AlloyType)x;
                saveAlloyType(out,t);
                out.write(saveNodeViz(view,t));
                if (view.getProjectedTypes().contains(t)) out.write("\tprojected=\"yes\""); else out.write("\tprojected=\"no\"");
                out.write("\n");
            }
            else if (x instanceof AlloySet) {
                AlloySet s=(AlloySet)x;
                saveAlloySet(out,s);
                out.write(saveNodeViz(view,s));
                out.write("\tprojected=\"no\"\n");
            }
        }
        out.write("#defaultedge" + saveEdgeViz(view,null) + "\n");
        types.clear();
        types.addAll(view.getOriginalModel().getRelations());
        types.addAll(view.getCurrentModel().getRelations());
        for(AlloyElement x:types) {
            if (x instanceof AlloyRelation) {
                AlloyRelation r=(AlloyRelation)x;
                saveAlloyRelation(out,r);
                out.write(saveEdgeViz(view,r));
                out.write("\n");
            }
        }
        if (out.checkError()) throw new IOException("Output Failure");
        return sw.toString();
    }

    /*============================================================================================*/

    /** Return null if the element is malformed. */
    private static AlloyType parseAlloyType(VizState now, XMLElement x) {
        /* class AlloyType implements AlloyNodeElement {
         *      String name;
         * }
         * <type name="the type name"/>
         */
        if (!x.is("type")) return null;
        String name=x.getAttribute("name");
        if (name.length()==0) return null; else return now.getOriginalModel().hasType(name);
    }

    /** Writes nothing if the argument is null. */
    private static void writeAlloyType(PrintWriter out, AlloyType x) throws IOException {
        if (x!=null) Util.encodeXMLs(out, "   <type name=\"", x.getName(), "\"/>\n");
    }

    /** Writes nothing if the argument is null. */
    private static void saveAlloyType(PrintWriter out, AlloyType x) throws IOException {
        if (x!=null) Util.encodeXMLs(out, "sig(", x.getName(), ")");
    }

    /*============================================================================================*/

    /** Return null if the element is malformed. */
    private static AlloySet parseAlloySet(VizState now, XMLElement x) {
        /* class AlloySet implements AlloyNodeElement {
         *   String name;
         *   AlloyType type;
         * }
         * <set name="name" type="name"/>
         */
        if (!x.is("set")) return null;
        String name=x.getAttribute("name"), type=x.getAttribute("type");
        if (name.length()==0 || type.length()==0) return null;
        AlloyType t=now.getOriginalModel().hasType(type);
        if (t==null) return null; else return now.getOriginalModel().hasSet(name, t);
    }

    /** Writes nothing if the argument is null. */
    private static void writeAlloySet(PrintWriter out, AlloySet x) throws IOException {
        if (x!=null) Util.encodeXMLs(out,"   <set name=\"",x.getName(),"\" type=\"",x.getType().getName(),"\"/>\n");
    }

    /** Writes nothing if the argument is null. */
    private static void saveAlloySet(PrintWriter out, AlloySet x) throws IOException {
        if (x!=null) Util.encodeXMLs(out, "set(", x.getName(), ".", x.getType().getName(), ")");
    }

    /*============================================================================================*/

    /** Return null if the element is malformed. */
    private static AlloyRelation parseAlloyRelation(VizState now, XMLElement x) {
        /*
         * <relation name="name">
         *   2 or more <type name=".."/>
         * </relation>
         */
        List<AlloyType> ans=new ArrayList<AlloyType>();
        if (!x.is("relation")) return null;
        String name=x.getAttribute("name");
        if (name.length()==0) return null;
        for(XMLElement sub:x.getChildren("type")) {
            String typename=sub.getAttribute("name");
            if (typename.length()==0) return null;
            AlloyType t = now.getOriginalModel().hasType(typename);
            if (t==null) return null;
            ans.add(t);
        }
        if (ans.size()<2) return null; else return now.getOriginalModel().hasRelation(name, ans);
    }

    /** Writes nothing if the argument is null. */
    private static void writeAlloyRelation(PrintWriter out, AlloyRelation x) throws IOException {
        if (x==null) return;
        Util.encodeXMLs(out, "   <relation name=\"", x.getName(), "\">");
        for(AlloyType t:x.getTypes()) Util.encodeXMLs(out, " <type name=\"", t.getName(), "\"/>");
        out.write(" </relation>\n");
    }

    /** Writes nothing if the argument is null. */
    private static void saveAlloyRelation(PrintWriter out, AlloyRelation x) throws IOException {
        if (x==null) return;
        Util.encodeXMLs(out, "rel(", x.getName());
        for(AlloyType t:x.getTypes()) Util.encodeXMLs(out, ".", t.getName());
        out.write(")");
    }

    /*============================================================================================*/

    /** Always returns a nonnull (though possibly empty) set of AlloyType. */
    private static Set<AlloyType> parseProjectionList(VizState now, XMLElement x) {
        /*
        * <projection>
        *   0 or more <type name=".."/>
        * </projection>
        */
        Set<AlloyType> ans=new TreeSet<AlloyType>();
        if (x.is("projection")) for(XMLElement sub:x.getChildren("type")) {
            String name=sub.getAttribute("name");
            if (name.length()==0) continue;
            AlloyType t = now.getOriginalModel().hasType(name);
            if (t!=null) ans.add(t);
        }
        return ans;
    }

    /** Writes an empty Projection tag if the argument is null or empty */
    private static void writeProjectionList(PrintWriter out, Set<AlloyType> types) throws IOException {
        if (types==null || types.size()==0) { out.write("\n<projection/>\n"); return; }
        out.write("\n<projection>");
        for(AlloyType t:types) Util.encodeXMLs(out, " <type name=\"", t.getName(), "\"/>");
        out.write(" </projection>\n");
    }

    /*============================================================================================*/

    /** Do nothing if the element is malformed; note: x can be null. */
    private static void parseNodeViz(XMLElement xml, VizState view, AlloyNodeElement x) {
        /*
         * <node visible="inherit/yes/no"  label=".."  color=".."  shape=".."  style=".."
         * samerank="inherit/yes/no"  showlabel="inherit/yes/no"  showinattr="inherit/yes/no"
         * hideunconnected="inherit/yes/no" nubmeratoms="inherit/yes/no">
         *      zero or more SET or TYPE
         * </node>
         *
         * Each attribute, if omitted, means "no change".
         * Note: BOOLEAN is tristate.
         */
        if (has(xml,"visible"))  view.nodeVisible (x, getbool(xml, "visible"));
        if (has(xml,"samerank")) view.nodeSameRank(x, getbool(xml, "samerank"));
        if (has(xml,"hideunconnected")) view.hideUnconnected(x, getbool(xml, "hideunconnected"));
        if (x==null || x instanceof AlloySet) {
            AlloySet s=(AlloySet)x;
            if (has(xml,"showlabel"))  view.showAsLabel(s, getbool(xml, "showlabel"));
            if (has(xml,"showinattr")) view.showAsAttr (s, getbool(xml, "showinattr"));
        }
        if (x==null || x instanceof AlloyType) {
            AlloyType t=(AlloyType)x;
            if (has(xml,"numberatoms"))     view.number         (t, getbool(xml, "numberatoms"));
        }
        if (has(xml,"style")) view.nodeStyle(x, parseDotStyle(xml));
        if (has(xml,"color")) view.nodeColor(x, parseDotColor(xml));
        if (has(xml,"shape")) view.shape    (x, parseDotShape(xml));
        if (has(xml,"label")) view.label    (x, xml.getAttribute("label"));
    }

    /** Returns the String representation of an AlloyNodeElement's settings. */
    private static String writeNodeViz(VizState view, VizState defaultView, AlloyNodeElement x) throws IOException {
        StringWriter sw=new StringWriter();
        PrintWriter out=new PrintWriter(sw);
        writeBool(out, "visible",   view.nodeVisible(x),   defaultView.nodeVisible(x));
        writeBool(out, "samerank",  view.nodeSameRank(x),  defaultView.nodeSameRank(x));
        writeBool(out, "hideunconnected", view.hideUnconnected(x), defaultView.hideUnconnected(x));
        if (x==null || x instanceof AlloySet) {
            AlloySet s=(AlloySet)x;
            writeBool(out, "showlabel",  view.showAsLabel(s), defaultView.showAsLabel(s));
            writeBool(out, "showinattr", view.showAsAttr(s),  defaultView.showAsAttr(s));
        }
        if (x==null || x instanceof AlloyType) {
            AlloyType t=(AlloyType)x;
            writeBool(out, "numberatoms",     view.number(t),          defaultView.number(t));
        }
        writeDotStyle(out, view.nodeStyle(x), defaultView.nodeStyle(x));
        writeDotShape(out, view.shape(x),     defaultView.shape(x));
        writeDotColor(out, view.nodeColor(x), defaultView.nodeColor(x));
        if (x!=null && !view.label(x).equals(defaultView.label(x)))
            Util.encodeXMLs(out, " label=\"", view.label(x), "\"");
        if (out.checkError()) throw new IOException("PrintWriter IO Exception!");
        return sw.toString();
    }

    /** Returns the String representation of an AlloyNodeElement's settings. */
    private static String saveNodeViz(VizState view, AlloyNodeElement x) throws IOException {
        AlloyModel am = view.getCurrentModel();
        AlloySet s = (x instanceof AlloySet) ? ((AlloySet)x) : null;
        AlloyType t = (x instanceof AlloyType) ? ((AlloyType)x): null;
        StringWriter sw=new StringWriter();
        PrintWriter out=new PrintWriter(sw);
        saveBool(out, "visible",   view.nodeVisible(x,am));
        saveBool(out, "samerank",  view.nodeSameRank(x,am));
        saveBool(out, "hideunconnected", view.hideUnconnected(x,am));
        saveBool(out, "showlabel",  view.showAsLabel(s,am));
        saveBool(out, "showinattr", view.showAsAttr(s,am));
        saveBool(out, "numberatoms", view.number(t,am));
        saveDotStyle(out, view.nodeStyle(x,am));
        saveDotShape(out, view.shape(x,am));
        saveDotColor(out, view.nodeColor(x,am));
        Util.encodeXMLs(out, "\tlabel=\"", x==null ? "" : view.label(x), "\"");
        if (out.checkError()) throw new IOException("PrintWriter IO Exception!");
        return sw.toString();
    }

    /*============================================================================================*/

    /** Do nothing if the element is malformed; note: x can be null. */
    private static void parseEdgeViz(XMLElement xml, VizState view, AlloyRelation x) {
        /*
         * <edge visible="inherit/yes/no"  label=".."  color=".."  style=".."  weight=".."  constraint=".."
         * attribute="inherit/yes/no", samerank="inherit/yes/no"  merge="inherit/yes/no" layout="inherit/yes/no">
         *     zero or more RELATION
         * </edge>
         *
         * Each attribute, if omitted, means "no change".
         * Note: BOOLEAN is tristate.
         */
        if (has(xml,"visible"))    view.edgeVisible (x, getbool(xml,"visible"));
        if (has(xml,"attribute"))  view.attribute   (x, getbool(xml,"attribute"));
        if (has(xml,"samerank"))   view.edgeSameRank(x, getbool(xml,"samerank"));
        if (has(xml,"merge"))      view.mergeArrows (x, getbool(xml,"merge"));
        if (has(xml,"layout"))     view.layoutBack  (x, getbool(xml,"layout"));
        if (has(xml,"constraint")) view.constraint  (x, getbool(xml,"constraint"));
        if (has(xml,"weight"))     view.weight      (x, getint (xml,"weight"));
        if (has(xml,"style"))      view.edgeStyle   (x, parseDotStyle(xml));
        if (has(xml,"color"))      view.edgeColor   (x, parseDotColor(xml));
        if (has(xml,"label"))      view.label       (x, xml.getAttribute("label"));
    }

    /** Returns the String representation of an AlloyRelation's settings. */
    private static String writeEdgeViz(VizState view, VizState defaultView, AlloyRelation x) throws IOException {
        StringWriter sw=new StringWriter();
        PrintWriter out=new PrintWriter(sw);
        writeDotColor(out, view.edgeColor(x), defaultView.edgeColor(x));
        writeDotStyle(out, view.edgeStyle(x), defaultView.edgeStyle(x));
        writeBool(out, "samerank",  view.edgeSameRank(x), defaultView.edgeSameRank(x));
        writeBool(out, "visible",   view.edgeVisible(x),  defaultView.edgeVisible(x));
        writeBool(out, "merge",     view.mergeArrows(x),  defaultView.mergeArrows(x));
        writeBool(out, "layout",    view.layoutBack(x),   defaultView.layoutBack(x));
        writeBool(out, "attribute", view.attribute(x),    defaultView.attribute(x));
        writeBool(out, "constraint",view.constraint(x),   defaultView.constraint(x));
        if (view.weight(x) != defaultView.weight(x))         out.write(" weight=\"" + view.weight(x) + "\"");
        if (x!=null && !view.label(x).equals(defaultView.label(x)))
            Util.encodeXMLs(out, " label=\"", view.label(x), "\"");
        if (out.checkError()) throw new IOException("PrintWriter IO Exception!");
        return sw.toString();
    }

    /** Returns the String representation of an AlloyRelation's settings. */
    private static String saveEdgeViz(VizState view, AlloyRelation x) throws IOException {
        AlloyModel am=view.getCurrentModel();
        StringWriter sw=new StringWriter();
        PrintWriter out=new PrintWriter(sw);
        saveDotColor(out, view.edgeColor(x,am));
        saveDotStyle(out, view.edgeStyle(x,am));
        saveBool(out, "samerank",  view.edgeSameRank(x,am));
        saveBool(out, "visible",   view.edgeVisible(x,am));
        saveBool(out, "merge",     view.mergeArrows(x,am));
        saveBool(out, "layout",    view.layoutBack(x,am));
        saveBool(out, "attribute", view.attribute(x,am));
        saveBool(out, "constraint",view.constraint(x,am));
        out.write("\tweight=\"" + view.weight(x) + "\"");
        Util.encodeXMLs(out, "\tlabel=\"", x==null ? "" : view.label(x), "\"");
        if (out.checkError()) throw new IOException("PrintWriter IO Exception!");
        return sw.toString();
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static DotPalette parseDotPalette(XMLElement x, String key) {
        return DotPalette.valueOf(x.getAttribute(key));
    }

    /** Writes nothing if value==defaultValue. */
    private static void writeDotPalette(PrintWriter out, String key, DotPalette value, DotPalette defaultValue) throws IOException {
        if (value!=defaultValue) Util.encodeXMLs(out, " "+key+"=\"", value==null?"inherit":value.toString(), "\"");
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static DotOrientation parseDotOrientation(XMLElement x) {
        return DotOrientation.valueOf(x.getAttribute("orientation"));
    }

    /** Writes nothing if value==defaultValue. */
    private static void writeDotOrientation(PrintWriter out, DotOrientation value, DotOrientation defaultValue) throws IOException {
        if (value!=defaultValue) Util.encodeXMLs(out," orientation=\"",value==null?"inherit":value.toString(), "\"");
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static DotColor parseDotColor(XMLElement x) { return DotColor.valueOf(x.getAttribute("color")); }

    /** Writes nothing if value==defaultValue. */
    private static void writeDotColor(PrintWriter out, DotColor value, DotColor defaultValue) throws IOException {
        if (value!=defaultValue) Util.encodeXMLs(out, " color=\"", value==null?"inherit":value.toString(), "\"");
    }

    /** Writes regardless. */
    private static void saveDotColor(PrintWriter out, DotColor value) throws IOException {
        Util.encodeXMLs(out, "\tcolor=\"", value.toString(), "\"");
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static DotShape parseDotShape(XMLElement x) { return DotShape.valueOf(x.getAttribute("shape")); }

    /** Writes nothing if value==defaultValue. */
    private static void writeDotShape(PrintWriter out, DotShape value, DotShape defaultValue) throws IOException {
        if (value!=defaultValue) Util.encodeXMLs(out, " shape=\"", value==null?"inherit":value.toString(), "\"");
    }

    /** Writes regardless. */
    private static void saveDotShape(PrintWriter out, DotShape value) throws IOException {
        Util.encodeXMLs(out, "\tshape=\"", value.toString(), "\"");
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static DotStyle parseDotStyle(XMLElement x) { return DotStyle.valueOf(x.getAttribute("style")); }

    /** Writes nothing if value==defaultValue. */
    private static void writeDotStyle(PrintWriter out, DotStyle value, DotStyle defaultValue) throws IOException {
        if (value!=defaultValue) Util.encodeXMLs(out, " style=\"", value==null?"inherit":value.toString(), "\"");
    }

    /** Writes regardless. */
    private static void saveDotStyle(PrintWriter out, DotStyle value) throws IOException {
        Util.encodeXMLs(out, "\tstyle=\"", value.toString(), "\"");
    }

    /*============================================================================================*/

    /** Returns null if the attribute doesn't exist, or is malformed. */
    private static Boolean getbool(XMLElement x, String attr) {
        String value=x.getAttribute(attr);
        if (value.equalsIgnoreCase("yes") || value.equalsIgnoreCase("true")) return Boolean.TRUE;
        if (value.equalsIgnoreCase("no") || value.equalsIgnoreCase("false")) return Boolean.FALSE;
        return null;
    }

    /** Writes nothing if the value is equal to the default value. */
    private static void writeBool(PrintWriter out, String key, Boolean value, Boolean defaultValue) throws IOException {
        if (value==null && defaultValue==null) return;
        if (value!=null && defaultValue!=null && value.booleanValue()==defaultValue.booleanValue()) return;
        out.write(' ');
        out.write(key);
        if (value==null) out.write("=\"inherit\""); else out.write(value ? "=\"yes\"":"=\"no\"");
    }

    /** Writes regardless. */
    private static void saveBool(PrintWriter out, String key, boolean value) throws IOException {
        out.write('\t');
        out.write(key);
        out.write(value ? "=\"yes\"":"=\"no\"");
    }

    /*============================================================================================*/

    /** Returns true if the XML element has the given attribute. */
    private static boolean has(XMLElement x, String attr) {
        return x.getAttribute(attr,null)!=null;
    }

    /** Returns 0 if the attribute doesn't exist, or is malformed. */
    private static int getint(XMLElement x, String attr) {
        String value=x.getAttribute(attr);
        int i;
        try {
            i=Integer.parseInt(value);
        } catch(NumberFormatException ex) {
            i=0;
        }
        return i;
    }
}
