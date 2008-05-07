/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4viz;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.IOException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.TreeSet;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import edu.mit.csail.sdg.alloy4.MailBug;
import edu.mit.csail.sdg.alloy4.OurUtil;
import edu.mit.csail.sdg.alloy4.Pair;

/**
 * Mutable; this stores an unprojected model as well as the current theme customization.
 *
 * <p><b>Thread Safety:</b> Can be called only by the AWT event thread.
 */

public final class VizState {

    /**
     * Construct a new VizState (with default theme settings) for the given instance; if world!=null, it is the root of the AST.
     */
    public VizState(AlloyInstance originalInstance) {
        this.originalInstance=originalInstance;
        this.currentModel=originalInstance.model;
        resetTheme();
        loadInstance(originalInstance);
    }

    /** Make a copy of an existing VizState object. */
    public VizState(VizState old) {
        originalInstance=old.originalInstance;
        currentModel=old.currentModel;
        projectedTypes=new TreeSet<AlloyType>(old.projectedTypes);
        useOriginalNames=old.useOriginalNames;
        hidePrivate=old.hidePrivate;
        fontSize=old.fontSize;
        orientation=old.orientation;
        nodePalette=old.nodePalette;
        edgePalette=old.edgePalette;
        nodeColor=new LinkedHashMap<AlloyNodeElement,DotColor>(old.nodeColor);
        nodeStyle=new LinkedHashMap<AlloyNodeElement,DotStyle>(old.nodeStyle);
        nodeVisible=new LinkedHashMap<AlloyNodeElement,Boolean>(old.nodeVisible);
        nodeSameRank=new LinkedHashMap<AlloyNodeElement,Boolean>(old.nodeSameRank);
        label=new LinkedHashMap<AlloyElement,String>(old.label);
        number=new LinkedHashMap<AlloyType,Boolean>(old.number);
        hideUnconnected=new LinkedHashMap<AlloyNodeElement,Boolean>(old.hideUnconnected);
        showAsAttr=new LinkedHashMap<AlloySet,Boolean>(old.showAsAttr);
        showAsLabel=new LinkedHashMap<AlloySet,Boolean>(old.showAsLabel);
        shape=new LinkedHashMap<AlloyNodeElement,DotShape>(old.shape);
        weight=new LinkedHashMap<AlloyRelation,Integer>(old.weight);
        attribute=new LinkedHashMap<AlloyRelation,Boolean>(old.attribute);
        mergeArrows=new LinkedHashMap<AlloyRelation,Boolean>(old.mergeArrows);
        constraint=new LinkedHashMap<AlloyRelation,Boolean>(old.constraint);
        layoutBack=new LinkedHashMap<AlloyRelation,Boolean>(old.layoutBack);
        edgeColor=new LinkedHashMap<AlloyRelation,DotColor>(old.edgeColor);
        edgeStyle=new LinkedHashMap<AlloyRelation,DotStyle>(old.edgeStyle);
        edgeVisible=new LinkedHashMap<AlloyRelation,Boolean>(old.edgeVisible);
        edgeSameRank=new LinkedHashMap<AlloyRelation,Boolean>(old.edgeSameRank);
        changedSinceLastSave=false;
    }

    /** Clears the current theme. */
    public void resetTheme() {
        currentModel = originalInstance.model;
        projectedTypes.clear();
        useOriginalNames = false;
        hidePrivate = true;
        fontSize = 12;
        orientation = DotOrientation.getDefault();
        nodePalette = DotPalette.getDefault();
        edgePalette = DotPalette.getDefault();
        nodeColor.clear(); nodeColor.put(null, DotColor.WHITE);
        nodeStyle.clear(); nodeStyle.put(null, DotStyle.getDefault());
        nodeVisible.clear(); nodeVisible.put(null, true);
        nodeSameRank.clear(); nodeSameRank.put(null, false);
        label.clear(); label.put(null, "");
        number.clear(); number.put(null, true);
        hideUnconnected.clear(); hideUnconnected.put(null, false);
        showAsAttr.clear(); showAsAttr.put(null, false);
        showAsLabel.clear(); showAsLabel.put(null, true);
        shape.clear(); shape.put(null, DotShape.getDefault());
        weight.clear(); weight.put(null, 0);
        attribute.clear(); attribute.put(null, false);
        mergeArrows.clear(); mergeArrows.put(null, true);
        constraint.clear(); constraint.put(null, true);
        layoutBack.clear(); layoutBack.put(null, false);
        edgeColor.clear(); edgeColor.put(null, DotColor.MAGIC);
        edgeStyle.clear(); edgeStyle.put(null, DotStyle.getDefault());
        edgeVisible.clear(); edgeVisible.put(null, true);
        edgeSameRank.clear(); edgeSameRank.put(null, false);
        // Provide some nice defaults for "Int" and "seq/Int" type
        AlloyType sigint=AlloyType.INT;
        label.put(sigint,"");
        number.put(sigint,true);
        hideUnconnected.put(sigint,true);
        AlloyType seqidx=AlloyType.SEQINT;
        label.put(seqidx,"");
        number.put(seqidx,true);
        hideUnconnected.put(seqidx,true);
        // Provide some nice defaults for meta model stuff
        AlloyType set=AlloyType.SET;
        AlloyRelation ext=AlloyRelation.EXTENDS, in=AlloyRelation.IN;
        shape.put(null,DotShape.BOX); nodeColor.put(null,DotColor.YELLOW); nodeStyle.put(null,DotStyle.SOLID);
        shape.put(set,DotShape.ELLIPSE); nodeColor.put(set,DotColor.BLUE); label.put(set,"");
        edgeColor.put(ext,DotColor.BLACK); weight.put(ext,100); layoutBack.put(ext,true);
        edgeColor.put(in,DotColor.BLACK); weight.put(in,100); layoutBack.put(in,true);
        // Done
        cache.clear();
        changedSinceLastSave=false;
    }

    /**
     * Load a new instance into this VizState object (the input argument is treated as a new unprojected instance);
     * if world!=null, it is the root of the AST
     */
    public void loadInstance(AlloyInstance unprojectedInstance) {
        this.originalInstance=unprojectedInstance;
        for (AlloyType t:getProjectedTypes()) if (!unprojectedInstance.model.hasType(t)) projectedTypes.remove(t);
        currentModel = StaticProjector.project(unprojectedInstance.model, projectedTypes);
        cache.clear();
    }

    /**
     * Erase the current theme customizations and then load it from a file.
     * @throws IOException - if an error occurred
     */
    public void loadPaletteXML(String filename) throws IOException {
        resetTheme();
        StaticThemeReaderWriter.readAlloy(filename,this);
        cache.clear();
        changedSinceLastSave=false;
    }

    /**
     * Saves the current theme to a file (which will be overwritten if it exists already).
     * @throws IOException - if an error occurred
     */
    public void savePaletteXML(String filename) throws IOException {
        StaticThemeReaderWriter.writeAlloy(filename,this);
        changedSinceLastSave=false;
    }

    /** Caches previously generated graphs. */
    private LinkedHashMap<AlloyProjection,Pair<String,JPanel>> cache=new LinkedHashMap<AlloyProjection,Pair<String,JPanel>>();

    /** Generate a VizGraphPanel for a given projection choice, using the current settings. */
    public Pair<String,JPanel> getGraph(AlloyProjection projectionChoice) {
        Pair<String,JPanel> ans=cache.get(projectionChoice);
        if (ans!=null) return ans;
        AlloyInstance inst = originalInstance;
        DotGraph graph = StaticGraphMaker.produceGraph(inst, this, projectionChoice);
        try {
            ans=graph.visualize();
            cache.put(projectionChoice,ans);
        } catch(Throwable ex) {
            String msg = "An error has occurred: "+ex+"\n\nStackTrace:\n"+MailBug.dump(ex)+"\nRaw Dot:\n\n"+graph.write();
            JTextArea message = OurUtil.textarea(msg, 0, 0);
            JScrollPane scroll = OurUtil.scrollpane(message);
            ans=new Pair<String,JPanel>("",new JPanel());
            ans.b.setLayout(new BorderLayout());
            ans.b.add(scroll, BorderLayout.CENTER);
            ans.b.setBackground(Color.WHITE);
        }
        ans.b.setBorder(null);
        return ans;
    }

    /** True if the theme has been modified since last save. */
    private boolean changedSinceLastSave=false;

    /** True if the theme has been modified since last save. */
    public boolean changedSinceLastSave() { return changedSinceLastSave; }

    /** Sets the "changed since last save" flag, then flush any cached generated graphs. */
    private void change() { changedSinceLastSave=true; cache.clear(); }

    /** If oldValue is different from newValue, then sets the "changed since last save" flag and flush the cache. */
    private void changeIf(Object oldValue, Object newValue) {
        if (oldValue==null) { if (newValue==null) return; } else { if (oldValue.equals(newValue)) return; }
        change();
    }

    /*============================================================================================*/

    /**
     * If x is an AlloyType, x is not univ, then return its parent (which could be univ);
     * If x is an AlloySet, then return x's type;
     * All else, return null.
     */
    private AlloyType parent(AlloyNodeElement x, AlloyModel model) {
        if (x instanceof AlloySet) return ((AlloySet)x).getType();
        if (x instanceof AlloyType) return model.getSuperType((AlloyType)x);
        return null;
    }

    /*============================================================================================*/

    /** The original unprojected instance. */
    private AlloyInstance originalInstance;

    /** Returns the original unprojected model. */
    public AlloyInstance getOriginalInstance() { return originalInstance; }

    /** Returns the original unprojected model. */
    public AlloyModel getOriginalModel() { return originalInstance.model; }

    /*============================================================================================*/

    /** The current (possibly projected) model. */
    private AlloyModel currentModel;

    /** Returns the current (possibly projected) model. */
    public AlloyModel getCurrentModel() { return currentModel; }

    /*============================================================================================*/

    /** The set of types we are currently projecting over. */
    private Set<AlloyType> projectedTypes = new TreeSet<AlloyType>();

    /** Gets an unmodifiable copy of the set of types we are currently projecting over. */
    public Set<AlloyType> getProjectedTypes() {
        return Collections.unmodifiableSet(new TreeSet<AlloyType>(projectedTypes));
    }

    /** Returns true iff the type is not univ, and it is a toplevel type. */
    public boolean canProject(final AlloyType type) {
        return isTopLevel(type);
    }

    /** Returns true iff the type is not univ, and it is a toplevel type. */
    public boolean isTopLevel(final AlloyType type) {
        return AlloyType.UNIV.equals(originalInstance.model.getSuperType(type));
    }

    /** Adds type to the list of projected types if it's a toplevel type. */
    public void project(AlloyType type) {
        if (canProject(type)) if (projectedTypes.add(type)) {
            currentModel = StaticProjector.project(originalInstance.model, projectedTypes);
            change();
        }
    }

    /** Removes type from the list of projected types if it is currently projected. */
    public void deproject(AlloyType type) {
        if (projectedTypes.remove(type)) {
            currentModel = StaticProjector.project(originalInstance.model, projectedTypes);
            change();
        }
    }

    /** Removes every entry from the list of projected types. */
    public void deprojectAll() {
        if (projectedTypes.size()>0) {
            projectedTypes.clear();
            currentModel = StaticProjector.project(originalInstance.model, projectedTypes);
            change();
        }
    }

    /*============================================================================================*/

    /** Whether to use the original atom names. */
    private boolean useOriginalNames = false;

    /** Returns whether we will use original atom names. */
    public boolean useOriginalName() { return useOriginalNames; }

    /** Sets whether we will use original atom names or not. */
    public void useOriginalName(Boolean newValue) {
        if (newValue!=null && useOriginalNames!=newValue) { change(); useOriginalNames=newValue; }
    }

    /*============================================================================================*/

    /** Whether to hide private sigs/fields/relations. */
    private boolean hidePrivate = false;

    /** Returns whether we will hide private sigs/fields/relations. */
    public boolean hidePrivate() { return hidePrivate; }

    /** Sets whether we will hide private sigs/fields/relations. */
    public void hidePrivate(Boolean newValue) {
        if (newValue!=null && hidePrivate!=newValue) { change(); hidePrivate=newValue; }
    }

    /*============================================================================================*/

    /** The graph's font size. */
    private int fontSize = 12;

    /** Returns the font size. */
    public int getFontSize() { return fontSize; }

    /** Sets the font size. */
    public void setFontSize(int n) { if (fontSize!=n && fontSize>0) { change(); fontSize=n; } }

    /*============================================================================================*/

    /** The graph orientation. */
    private DotOrientation orientation = DotOrientation.getDefault();

    /** Gets the graph orientation. */
    public DotOrientation getOrientation() { return orientation; }

    /** Sets the graph orientation. */
    public void setOrientation(DotOrientation x) {
        if (orientation!=x && x!=null) {change(); orientation=x;}
    }

    /*============================================================================================*/

    /** The default node palette. */
    private DotPalette nodePalette = DotPalette.getDefault();

    /** Gets the default node palette. */
    public DotPalette getNodePalette() { return nodePalette; }

    /** Sets the default node palette. */
    public void setNodePalette(DotPalette x) {
        if (nodePalette!=x && x!=null) {change(); nodePalette=x;}
    }

    /*============================================================================================*/

    /** The default edge palette. */
    private DotPalette edgePalette = DotPalette.getDefault();

    /** Gets the default edge palette. */
    public DotPalette getEdgePalette() { return edgePalette; }

    /** Sets the default edge palette. */
    public void setEdgePalette(DotPalette x) {
        if (edgePalette!=x && x!=null) {change(); edgePalette=x;}
    }

    /*============================================================================================*/

    // An important invariant to maintain: every map here must map null to a nonnull value.
    private LinkedHashMap<AlloyNodeElement,DotColor> nodeColor = new LinkedHashMap<AlloyNodeElement,DotColor>();
    private LinkedHashMap<AlloyNodeElement,DotStyle> nodeStyle = new LinkedHashMap<AlloyNodeElement,DotStyle>();
    private LinkedHashMap<AlloyNodeElement,Boolean> nodeVisible = new LinkedHashMap<AlloyNodeElement,Boolean>();
    private LinkedHashMap<AlloyNodeElement,Boolean> nodeSameRank = new LinkedHashMap<AlloyNodeElement,Boolean>();
    private LinkedHashMap<AlloyElement,String> label = new LinkedHashMap<AlloyElement,String>();
    private LinkedHashMap<AlloyType,Boolean> number = new LinkedHashMap<AlloyType,Boolean>();
    private LinkedHashMap<AlloyNodeElement,Boolean> hideUnconnected = new LinkedHashMap<AlloyNodeElement,Boolean>();
    private LinkedHashMap<AlloySet,Boolean> showAsAttr = new LinkedHashMap<AlloySet,Boolean>();
    private LinkedHashMap<AlloySet,Boolean> showAsLabel = new LinkedHashMap<AlloySet,Boolean>();
    private LinkedHashMap<AlloyNodeElement,DotShape> shape = new LinkedHashMap<AlloyNodeElement,DotShape>();
    private LinkedHashMap<AlloyRelation,Integer> weight = new LinkedHashMap<AlloyRelation,Integer>();
    private LinkedHashMap<AlloyRelation,Boolean> attribute = new LinkedHashMap<AlloyRelation,Boolean>();
    private LinkedHashMap<AlloyRelation,Boolean> mergeArrows = new LinkedHashMap<AlloyRelation,Boolean>();
    private LinkedHashMap<AlloyRelation,Boolean> constraint = new LinkedHashMap<AlloyRelation,Boolean>();
    private LinkedHashMap<AlloyRelation,Boolean> layoutBack = new LinkedHashMap<AlloyRelation,Boolean>();
    private LinkedHashMap<AlloyRelation,DotColor> edgeColor = new LinkedHashMap<AlloyRelation,DotColor>();
    private LinkedHashMap<AlloyRelation,DotStyle> edgeStyle = new LinkedHashMap<AlloyRelation,DotStyle>();
    private LinkedHashMap<AlloyRelation,Boolean> edgeVisible = new LinkedHashMap<AlloyRelation,Boolean>();
    private LinkedHashMap<AlloyRelation,Boolean> edgeSameRank = new LinkedHashMap<AlloyRelation,Boolean>();

    // Reads the value for that type/set/relation.
    // If x==null, then we guarantee the return value is nonnull
    // If x!=null, then it may return null (which means "inherited")
    // (Note: "label" and "weight" will never return null)
    public DotColor nodeColor       (AlloyNodeElement x)  { return nodeColor.get(x); }
    public DotStyle nodeStyle       (AlloyNodeElement x)  { return nodeStyle.get(x); }
    public Boolean  nodeVisible     (AlloyNodeElement x)  { return nodeVisible.get(x); }
    public Boolean  nodeSameRank    (AlloyNodeElement x)  { return nodeSameRank.get(x); }
    public String   label           (AlloyElement x)      { String ans=label.get(x); if (ans==null) ans=x.getName().trim(); return ans; }
    public Boolean  number          (AlloyType x)         { return number.get(x); }
    public Boolean  hideUnconnected (AlloyNodeElement x)  { return hideUnconnected.get(x); }
    public Boolean  showAsAttr      (AlloySet x)          { return showAsAttr.get(x); }
    public Boolean  showAsLabel     (AlloySet x)          { return showAsLabel.get(x); }
    public DotShape shape           (AlloyNodeElement x)  { return shape.get(x); }
    public int      weight          (AlloyRelation x)     { Integer ans=weight.get(x); if (ans==null) ans=0; return ans; }
    public Boolean  attribute       (AlloyRelation x)     { return attribute.get(x); }
    public Boolean  mergeArrows     (AlloyRelation x)     { return mergeArrows.get(x); }
    public Boolean  constraint      (AlloyRelation x)     { return constraint.get(x); }
    public Boolean  layoutBack      (AlloyRelation x)     { return layoutBack.get(x); }
    public DotColor edgeColor       (AlloyRelation x)     { return edgeColor.get(x); }
    public DotStyle edgeStyle       (AlloyRelation x)     { return edgeStyle.get(x); }
    public Boolean  edgeVisible     (AlloyRelation x)     { return edgeVisible.get(x); }
    public Boolean  edgeSameRank    (AlloyRelation x)     { return edgeSameRank.get(x); }

    // Reads the value for that atom based on an existing AlloyInstance; return value is never null.
    public DotColor nodeColor   (AlloyAtom a, AlloyInstance i) { for(AlloySet s:i.atom2sets(a)) {DotColor v=nodeColor(s); if (v!=null) return v;} return nodeColor  (a.getType(), i.model); }
    public DotStyle nodeStyle   (AlloyAtom a, AlloyInstance i) { for(AlloySet s:i.atom2sets(a)) {DotStyle v=nodeStyle(s); if (v!=null) return v;} return nodeStyle  (a.getType(), i.model); }
    public DotShape shape       (AlloyAtom a, AlloyInstance i) { for(AlloySet s:i.atom2sets(a)) {DotShape v=shape(s);     if (v!=null) return v;} return shape      (a.getType(), i.model); }
    public boolean  nodeVisible (AlloyAtom a, AlloyInstance i) {
        // If it's in 1 or more set, then TRUE if at least one of them is TRUE.
        // If it's in 0 set, then travel up the chain of AlloyType and return the first non-null value.
        if (i.atom2sets(a).size()>0) {
          for(AlloySet s:i.atom2sets(a)) if (nodeVisible(s, i.model)) return true;
          return false;
        }
        return nodeVisible(a.getType(), i.model);
    }

    // Reads the value for that type/set/relation; return value is never null.
    public boolean  nodeVisible     (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { Boolean v=nodeVisible.get(x);     if (v!=null) return v; } }
    public boolean  nodeSameRank    (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { Boolean v=nodeSameRank.get(x);    if (v!=null) return v; } }
    public DotColor nodeColor       (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { DotColor v=nodeColor.get(x);      if (v!=null) return v; } }
    public DotStyle nodeStyle       (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { DotStyle v=nodeStyle.get(x);      if (v!=null) return v; } }
    public DotShape shape           (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { DotShape v=shape.get(x);          if (v!=null) return v; } }
    public boolean  number          (AlloyType x,        AlloyModel m) { for(;;x=parent(x,m)) { Boolean v=number.get(x);          if (v!=null) return v; } }
    public boolean  hideUnconnected (AlloyNodeElement x, AlloyModel m) { for(;;x=parent(x,m)) { Boolean v=hideUnconnected.get(x); if (v!=null) return v; } }
    public boolean  showAsAttr      (AlloySet x,         AlloyModel m) { Boolean v=showAsAttr.get(x);   return (v!=null) ? v : showAsAttr.get(null);  }
    public boolean  showAsLabel     (AlloySet x,         AlloyModel m) { Boolean v=showAsLabel.get(x);  return (v!=null) ? v : showAsLabel.get(null); }
    public boolean  attribute       (AlloyRelation x,    AlloyModel m) { Boolean v=attribute.get(x);    return (v!=null) ? v : attribute.get(null);  }
    public boolean  mergeArrows     (AlloyRelation x,    AlloyModel m) { Boolean v=mergeArrows.get(x);  return (v!=null) ? v : mergeArrows.get(null); }
    public boolean  constraint      (AlloyRelation x,    AlloyModel m) { Boolean v=constraint.get(x);   return (v!=null) ? v : constraint.get(null); }
    public boolean  layoutBack      (AlloyRelation x,    AlloyModel m) { Boolean v=layoutBack.get(x);   return (v!=null) ? v : layoutBack.get(null); }
    public DotColor edgeColor       (AlloyRelation x,    AlloyModel m) { DotColor v=edgeColor.get(x);   return (v!=null) ? v : edgeColor.get(null); }
    public DotStyle edgeStyle       (AlloyRelation x,    AlloyModel m) { DotStyle v=edgeStyle.get(x);   return (v!=null) ? v : edgeStyle.get(null); }
    public boolean  edgeVisible     (AlloyRelation x,    AlloyModel m) { Boolean v=edgeVisible.get(x);  return (v!=null) ? v : edgeVisible.get(null); }
    public boolean  edgeSameRank    (AlloyRelation x,    AlloyModel m) { Boolean v=edgeSameRank.get(x); return (v!=null) ? v : edgeSameRank.get(null); }

    // Sets the value for that type/set/relation; v can be null (which means "inherit")
    public void nodeColor       (AlloyNodeElement x, DotColor v) { if (v==null && x==null) v=DotColor.WHITE;         changeIf(nodeColor      .put(x,v), v); }
    public void nodeStyle       (AlloyNodeElement x, DotStyle v) { if (v==null && x==null) v=DotStyle.getDefault();  changeIf(nodeStyle      .put(x,v), v); }
    public void nodeVisible     (AlloyNodeElement x, Boolean  v) { if (v==null && x==null) v=true;                   changeIf(nodeVisible    .put(x,v), v); }
    public void nodeSameRank    (AlloyNodeElement x, Boolean  v) { if (v==null && x==null) v=false;                  changeIf(nodeSameRank   .put(x,v), v); }
    public void label           (AlloyElement x,     String   v) { if (v==null && x==null) v=""; if (x!=null && x.getName().equals(v)) v=null; changeIf(label.put(x,v), v); }
    public void number          (AlloyType x,        Boolean  v) { if (v==null && x==null) v=true;                   changeIf(number         .put(x,v), v); }
    public void hideUnconnected (AlloyNodeElement x, Boolean  v) { if (v==null && x==null) v=false;                  changeIf(hideUnconnected.put(x,v), v); }
    public void showAsAttr      (AlloySet x,         Boolean  v) { if (v==null && x==null) v=false;                  changeIf(showAsAttr     .put(x,v), v); }
    public void showAsLabel     (AlloySet x,         Boolean  v) { if (v==null && x==null) v=true;                   changeIf(showAsLabel    .put(x,v), v); }
    public void shape           (AlloyNodeElement x, DotShape v) { if (v==null && x==null) v=DotShape.getDefault();  changeIf(shape          .put(x,v), v); }
    public void weight          (AlloyRelation x,    int      v) { if (v<0) v=0; changeIf(weight.put(x,v), v); }
    public void attribute       (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=false;                  changeIf(attribute   .put(x,v), v); }
    public void mergeArrows     (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=true;                   changeIf(mergeArrows .put(x,v), v); }
    public void constraint      (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=true;                   changeIf(constraint  .put(x,v), v); }
    public void layoutBack      (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=false;                  changeIf(layoutBack  .put(x,v), v); }
    public void edgeColor       (AlloyRelation x,    DotColor v) { if (v==null && x==null) v=DotColor.BLACK;         changeIf(edgeColor   .put(x,v), v); }
    public void edgeStyle       (AlloyRelation x,    DotStyle v) { if (v==null && x==null) v=DotStyle.getDefault();  changeIf(edgeStyle   .put(x,v), v); }
    public void edgeVisible     (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=true;                   changeIf(edgeVisible .put(x,v), v); }
    public void edgeSameRank    (AlloyRelation x,    Boolean  v) { if (v==null && x==null) v=false;                  changeIf(edgeSameRank.put(x,v), v); }
}
