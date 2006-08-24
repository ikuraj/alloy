/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/**
 * VizState is essentially the link between the gui package and the alloyviz
 * package.  It provides methods for the gui to update the view, etc.  It
 * also control most of the interactions between different classes of alloyviz.
 */
package kodviz.alloyviz;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ext.nanoxml.XMLParseException;

import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotOrientation;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;
import kodviz.util.Dbg;
import kodviz.xml.PaletteXMLParseException;

public class VizState {

    private Model _originalModel, _unprojectedModel, _currentModel;
    private VizInstance _instance;
    private View _view;
    private ViewPalette _palette;
    private File _modelFile;

    /* CustVar -> Set[AlloySet/AlloyRelation] */
    private Map _custVarsToElts;
    private Map _eltsToCustVars;
    private Map _custEltsToOrig;

    private Set vizStateListeners;

    /* for determining inherited settings */
    private Map _nodeEltToViz;
    private Map _relToViz;

    // maps model names to whether or not the palette associated with
    // that model has changed.
    private static Map _modelsToPalChanged = new HashMap();

    /**
     * model_ is the original model and will be preserved even when a new model
     * is generated (the "current model") for projections
     */
    public VizState(Model model_, VizInstance inst_) {
        this(model_, inst_, null);
    }

    @SuppressWarnings("unchecked")
    public VizState(Model model_, VizInstance inst_, ViewPalette pal_) {
        if (pal_ == null) {
            _palette = new ViewPalette(model_.getName());
            _view = new View(model_.getName());
            _palette.addView(_view);
        } else {
            _palette = pal_;
            _view = (View)_palette.getCurrentView(); // loads in first view in palette
        }
        _originalModel = model_.copy();
        setUnprojectedModel(model_);
        setCurrentModel(model_);
        setInstance(inst_);

        _custVarsToElts = new HashMap();
        _eltsToCustVars = new HashMap();
        loadCustVars();

        if (_currentModel != null) {
            applyProjection();
            matchView(_currentModel);
            recomputeInheritedSettings();
        }
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.FALSE);

        vizStateListeners = new HashSet();

    }

    /**
     * changes the unprojected model
     */
    public void setUnprojectedModel(Model model_) {
        _unprojectedModel = model_;

        //setCurrentModel(model_);
    }

    public void setCurrentModel(Model model_) {
        _currentModel = model_;
        matchView(model_);
        // do somethin' to the gui?
    }

    public void setInstance(VizInstance inst_) {
        _instance = inst_;
        // do somethin to gui
    }

    /**
     * use with caution
     */
    public VizInstance getInstance() {
        return _instance;
    }

    /**
     * gets the current view
     */
    public View getView() {
        return _view;
    }

    private void loadCustVars() {

        //  clean up the instance first of any custom vars
        for (Iterator cvs = _custVarsToElts.keySet().iterator(); cvs.hasNext();) {
            CustVar var = (CustVar)cvs.next();
            Iterator eltSetIter = ((Set)_custVarsToElts.get(var)).iterator();
            Object elt = eltSetIter.next();
            if (elt instanceof AlloySet) {
                _instance.removeSet((AlloySet)elt, var.getModuleName());
                while (eltSetIter.hasNext()) {
                    _instance.removeSet((AlloySet)eltSetIter.next(), var.getModuleName());
                }
            } else {
                _instance.removeRelation((AlloyRelation)elt, var.getModuleName());
                while (eltSetIter.hasNext()) {
                    _instance.removeRelation((AlloyRelation)eltSetIter.next(), var.getModuleName());
                }
            }
        }

        // why don't we need to clean up model?  i haven't figured it out yet
        // but it works.  should think about it sometime.

        // now clear _custVarsToElts map
        _custVarsToElts.clear();
        _eltsToCustVars.clear();

        // loads this view's cust vars
        for (Iterator i = _currentModel.getModules().iterator(); i.hasNext();) {
            ((AlloyModule)i.next()).getName();
        }

    }

    @SuppressWarnings("unchecked")
    public void setView(View view_) {
        _view = view_;
        _palette.setCurrentView(view_);

        loadCustVars();

        if (_currentModel != null) {
            matchView(_currentModel);
        }
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);

        fireViewsChanged();
    }

    /**
     * gets the most recently selected view for the palette.
     */
    public View getPaletteCurrentView() {
        return _palette.getCurrentView();
    }

    // returns current palette
    public ViewPalette getPalette() {
        return _palette;
    }

    @SuppressWarnings("unchecked")
    public void flushView() {
        _view.getModelView().getVizMap().flushCache();
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    @SuppressWarnings("unchecked")
    public void updateNodeView(AlloyNodeElement elt_, NodeViz viz_, boolean recordChange) {
        _view.getModelView().getVizMap().addNodeMapping(elt_, viz_);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.valueOf(recordChange));
    }

    @SuppressWarnings("unchecked")
    public void updateEdgeView(AlloyRelation rel_, EdgeViz viz_, boolean recordChange) {
        _view.getModelView().getVizMap().addEdgeMapping(rel_, viz_);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.valueOf(recordChange));
    }

    /**
     * matches a model and a view, moving matching visualizations into the relevant
     * part of the VizMap and the rest into the cache.
     *
     * now it also fills in any unspecified views for a node/edge that's in the
     * current model.
     */
    @SuppressWarnings("unchecked")
    public void matchView(Model model_) {

        VizMap map = _view.getModelView().getVizMap();
        map.mergeCacheRelevant();
        List types = model_.getTypes();
        List sets = model_.getSets();
        List rels = model_.getRelations();

        // using these lists (as opposed to caching in the two loops) will eliminate
        // concurrent modification errors
        List nodesToCache = new ArrayList();
        List edgesToCache = new ArrayList();

        for (Iterator nodeIter = map.getNodes().iterator(); nodeIter.hasNext();) {
            AlloyNodeElement elt = (AlloyNodeElement)nodeIter.next();
            if (!(types.contains(elt) || sets.contains(elt))) {
                nodesToCache.add(elt);
            }
        }

        for (Iterator edgeIter = map.getEdges().iterator(); edgeIter.hasNext();) {
            AlloyRelation rel = (AlloyRelation)edgeIter.next();
            if (!(rels.contains(rel))) {
                edgesToCache.add(rel);
            }
        }

        for (Iterator nodeIter = nodesToCache.iterator(); nodeIter.hasNext();) {
            map.cacheNodeMapping((AlloyNodeElement)nodeIter.next());
        }

        for (Iterator edgeIter = edgesToCache.iterator(); edgeIter.hasNext();) {
            map.cacheEdgeMapping((AlloyRelation)edgeIter.next());
        }

        //
        // need to take care of bogus projected types as well
        //

        ProjectionFrame pf = _view.getModelView().getProjectionFrame();
        for (Iterator typeIter = pf.getProjectedTypes().iterator(); typeIter.hasNext();) {
            AlloyType type = (AlloyType)typeIter.next();
            if (!_unprojectedModel.getTypes().contains(type)) {
                pf.deproject(type);
            }
        }

        // "fills in holes"
        for (Iterator typeIter = _currentModel.getTypes().iterator(); typeIter.hasNext();) {
            AlloyType next = (AlloyType)typeIter.next();
            if (getNodeViz(next) == null) {
                // if this is Int, clear the label
                if (next.getName().equals("Int")) {
                    updateNodeView(
                        next,
                        new NodeViz(null, "", null, null, null, null, null, Boolean.TRUE, null, null, null),
                        false);
                } else {

                    updateNodeView(
                        next,
                        new NodeViz(
                            null,
                            next.getName(),
                            null,
                            null,
                            null,
                            null,
                            null,
                            Boolean.TRUE,
                            null,
                            null,
                            null),
                        false);
                }
            }
        }
        for (Iterator setIter = _currentModel.getSets().iterator(); setIter.hasNext();) {
            AlloySet next = (AlloySet)setIter.next();
            if (getNodeViz(next) == null) {
                updateNodeView(
                    next,
                    new NodeViz(
                        null,
                        next.getName(),
                        null,
                        null,
                        null,
                        null,
                        null,
                        Boolean.TRUE,
                        null,
                        null,
                        null),
                    false);
            }
        }

        for (Iterator relIter = _currentModel.getRelations().iterator(); relIter.hasNext();) {
            AlloyRelation next = (AlloyRelation)relIter.next();
            if (getEdgeViz(next) == null) {

                updateEdgeView(
                    next,
                    new EdgeViz(null, next.getName(), null, null, 0, null, null, null, Boolean.TRUE, null),
                    false);
            }
        }
    }

    /**
     * resets current view
     * the Projection frame is cleared, as are all custom variables
     */
    @SuppressWarnings("unchecked")
    public void resetView() {
        // clean up the instance first of any custom vars
        for (Iterator cvs = _custVarsToElts.keySet().iterator(); cvs.hasNext();) {
            CustVar var = (CustVar)cvs.next();
            Iterator eltSetIter = ((Set)_custVarsToElts.get(var)).iterator();
            Object elt = eltSetIter.next();
            if (elt instanceof AlloySet) {
                _instance.removeSet((AlloySet)elt, var.getModuleName());
                while (eltSetIter.hasNext()) {
                    _instance.removeSet((AlloySet)eltSetIter.next(), var.getModuleName());
                }
            } else {
                _instance.removeRelation((AlloyRelation)elt, var.getModuleName());
                while (eltSetIter.hasNext()) {
                    _instance.removeRelation((AlloyRelation)eltSetIter.next(), var.getModuleName());
                }
            }
        }

        // now clear _custVarsToElts map
        _custVarsToElts.clear();

        // clear projection Frame, custom vars, and vizmap
        _view.getModelView().reset();

        // though vizmap is cleared, you still need to explicitly set
        // settings for the remaining elts
        for (Iterator typeIter = _originalModel.getTypes().iterator(); typeIter.hasNext();) {
            AlloyType next = (AlloyType)typeIter.next();
            updateNodeView(
                next,
                new NodeViz(
                    null,
                    next.getName(),
                    null,
                    null,
                    null,
                    null,
                    null,
                    Boolean.TRUE,
                    null,
                    null,
                    null),
                false);
        }
        for (Iterator setIter = _originalModel.getSets().iterator(); setIter.hasNext();) {
            AlloySet next = (AlloySet)setIter.next();
            updateNodeView(
                next,
                new NodeViz(
                    null,
                    next.getName(),
                    null,
                    null,
                    null,
                    null,
                    null,
                    Boolean.TRUE,
                    null,
                    null,
                    null),
                false);
        }
        for (Iterator relIter = _originalModel.getRelations().iterator(); relIter.hasNext();) {
            AlloyRelation next = (AlloyRelation)relIter.next();
            updateEdgeView(
                next,
                new EdgeViz(null, next.getName(), null, null, 0, null, null, null, Boolean.TRUE, null),
                false);
        }

        // Reset General stuff
        _view.getGeneralView().resetView();

        // reset the model to the original, untinted version
        _currentModel = _originalModel.copy();
        _unprojectedModel = _currentModel;

        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    /**
     * returns the current View's NodeViz stored for elt if any
     * otherwise returns null
     */
    public NodeViz getNodeViz(AlloyNodeElement elt) {
        return _view.getModelView().getVizMap().getNodeViz(elt);
    }

    /**
     * returns the current View's EdgeViz stored for elt if any
     * otherwise returns null
     */
    public EdgeViz getEdgeViz(AlloyRelation rel) {
        return _view.getModelView().getVizMap().getEdgeViz(rel);
    }

    /*
     * IO STUFF
     */
    public void loadView(File f_) {
        try {
            _view = View.load(f_);
            if (_currentModel != null) {
                matchView(_currentModel);
            }
        } catch (Exception e) {
            Dbg.fatal("Error loading view", e);
        }
    }

    public void saveView(File f_) {
        try {
            _view.save(f_);
        } catch (Exception e) {
            Dbg.fatal("Error saving view", e);
        }
    }

    public void setPalette(ViewPalette pal_) {
        _palette = pal_;
        setView(_palette.getCurrentView());
    }
    
    // jbaek added
    /**
     * Loads a palette from the given file <code>f_</code>. If it is null,
     * an empty palette will be loaded.
     * @param f_ the file that contains the palette XML to be loaded
     * @throws IncomptaiblePaletteException if the version is not compatible
     * @throws PaletteXMLParseException if the file is not a valid palette XML
     *  file
     * @throws Exception if the file does not exist
     */
    public void loadPaletteXML(File f_) throws IncompatiblePaletteException,
    	PaletteXMLParseException, XMLParseException, Exception {
    	if (f_ == null) {
    		_palette = new ViewPalette(getModelName());
    		_view = new View(getModelName());
            _palette.addView(_view);
    	} else 	
    		_palette = ViewPalette.loadXML(f_, this);
    	setView(_palette.getCurrentView());
    }
    
    public void loadPalette(File f_) throws IncompatiblePaletteException, Exception {
        try {
            _palette = ViewPalette.load(f_);
            setView(_palette.getCurrentView());
        } catch (IncompatiblePaletteException ipe) {
            throw ipe;
        } catch (Exception e) {
            throw e;
        }
    }
    
    //jbaek added
    /**
     * Saves the current palette as an XML file. If the file already contains
     * palette XML data, the file will be overwritten only as much as needed
     * (unrelated signature names, etc, will be kept.)
     * @param f_ the file to save onto
     * @throws Exception if a parsing error is encountered
     */
    public void savePaletteXML(File f_) throws Exception {
    	_palette.saveXML(f_, this); // jbaek modified
    }
    
    @SuppressWarnings("unchecked")
    public void savePalette(File f_) {
        try {
            _palette.save(f_);
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.FALSE);
        } catch (Exception e) {
            Dbg.fatal("Error saving palette", e);
        }
    }

    /*
     * PALETTE STUFF
     */

    /*
    public ViewPalette getViewPalette() {
    return _palette;
    }*/

    public List getAllViews() {
        return _palette.getAllViews();
    }

    @SuppressWarnings("unchecked")
    public void addView(String name) {
        View temp;
        if (_view != null) {
            temp = _view.copy();
            temp.setName(name);
        } else {
            temp = new View(name);
        }
        _palette.addView(temp);
        setView(temp);
        // new view inherits settings from current view
        //resetView();
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);

        fireViewsChanged();
    }

    @SuppressWarnings("unchecked")
    public void removeView(View view) {
        _palette.removeView(view);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);

        fireViewsChanged();
    }

    /**
     * Figures out which types in a module are projected and which aren't.
     * Only returns top-level types (i.e. those with no supertypes)
     *
     * Pass in empty, modifiable lists for projected and unprojected.
     */
    @SuppressWarnings("unchecked")
    public void classifyTypesInModule(AlloyModule mod, List projected, List unprojected) {
        ProjectionFrame pf = _view.getModelView().getProjectionFrame();
        for (Iterator types = mod.getTypes().iterator(); types.hasNext();) {
            AlloyType type = (AlloyType)types.next();
            // skip univ (null supertype) or non-top-level subtypes
            if (_originalModel.getSuperForType(type) == null || 
                !_originalModel.getSuperForType(type).getName().equals("univ")) {                
                continue;
            }
            if (pf.isProjected(type)) {
                projected.add(type);
            } else {
                unprojected.add(type);
            }
        }
    }

    /**
     * Modifies the projection frame.  Projects on a certain AlloyType in the
     * model, with no specific current atom set.  (note, though, that as soon
     * as a GraphPanel is created for the diagrams, a current atom, namely,
     * the first one in the sorted list, will be picked and loaded for each
     * projected type.
     */
    @SuppressWarnings("unchecked")
    public void projectOn(AlloyType type) {
        _view.getModelView().getProjectionFrame().projectOn(type);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    /**
     * See note for projectOn.  If the type indicated is not currently projected,
     * nothing is done.
     */
    @SuppressWarnings("unchecked")
    public void deproject(AlloyType type) {
        _view.getModelView().getProjectionFrame().deproject(type);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    /**
     * Clears all projections.  Useful for initialization procedures in gui.
     */
    @SuppressWarnings("unchecked")
    public void deprojectAll() {
        ProjectionFrame pf = _view.getModelView().getProjectionFrame();
        for (Iterator types = pf.getProjectedTypes().iterator(); types.hasNext();) {
            pf.deproject((AlloyType)types.next());
        }
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    /**
     * Returns the truly original model (before projections and any custom
     * variable definitions)
     */
    public Model getOriginalModel() {
        return _originalModel;
    }

    /**
     * Gets the unprojected model
     */
    public Model getUnprojectedModel() {
        return _unprojectedModel;
    }

    /**
     * Gets the current model.  May or may not be the same as the original, depending
     * on the ProjectionFrame.
     */
    public Model getCurrentModel() {
        return _currentModel;
    }

    /**
     * Applies the (possibly changed) ProjectionFrame to the unprojected model and
     * sets it to the new model.
     */
    public void applyProjection() {
        ModelProjector modelProjector =
            new ModelProjector(getUnprojectedModel(), _view.getModelView().getProjectionFrame());
        _custEltsToOrig = modelProjector.getCustEltConversionMap();
        setCurrentModel(modelProjector.getProjectedModel());
    }

    // lotsa mutators for generalview

    @SuppressWarnings("unchecked")
    public void setUniversalNodeViz(NodeViz viz) {
        _view.getGeneralView().setGeneralNodeViz(viz);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    @SuppressWarnings("unchecked")
    public void setUniversalEdgeViz(EdgeViz viz) {
        _view.getGeneralView().setGeneralEdgeViz(viz);
        _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
    }

    // NOTE:
    // isLoading means if this change occurred during initialization
    // if so then don't record it as a user-made change

    @SuppressWarnings("unchecked")
    public void setFontName(String name, boolean isLoading) {
        _view.getGeneralView().setGeneralFontName(name);
        if (!isLoading) {
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
        }
    }

    @SuppressWarnings("unchecked")
    public void setFontSize(int size, boolean isLoading) {
        _view.getGeneralView().setGeneralFontSize(size);
        if (!isLoading) {
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
        }
    }

    @SuppressWarnings("unchecked")
    public void setOrientation(DotOrientation orient, boolean isLoading) {
        _view.getGeneralView().setGeneralOrientation(orient);
        if (!isLoading) {
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
        }
    }

    @SuppressWarnings("unchecked")
    public void setNodePalette(DotPalette dp, boolean isLoading) {
        _view.getGeneralView().setNodePalette(dp);
        if (!isLoading) {
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
        }
    }

    @SuppressWarnings("unchecked")
    public void setEdgePalette(DotPalette dp, boolean isLoading) {
        _view.getGeneralView().setEdgePalette(dp);
        if (!isLoading) {
            _modelsToPalChanged.put(_currentModel.getName(), Boolean.TRUE);
        }
    }

    // lotsa accessors for general view

    public NodeViz getUniversalNodeViz() {
        return _view.getGeneralView().getGeneralNodeViz();
    }

    public EdgeViz getUniversalEdgeViz() {
        return _view.getGeneralView().getGeneralEdgeViz();
    }

    public String getFontName() {
        return _view.getGeneralView().getGeneralFontName();
    }

    public int getFontSize() {
        return _view.getGeneralView().getGeneralFontSize();
    }

    public DotOrientation getOrientation() {
        return _view.getGeneralView().getGeneralOrientation();
    }

    public DotPalette getNodePalette() {
        return _view.getGeneralView().getNodePalette();
    }

    public DotPalette getEdgePalette() {
        return _view.getGeneralView().getEdgePalette();
    }

    //
    // filename stuff
    //

    /**
     * sets the filename (including path) of the model
     */
    public void setModelFile(File f) {
        _modelFile = f;
    }

    public File getModelFile() {
        return _modelFile;
    }

    public boolean paletteChanged() {
        return ((Boolean)_modelsToPalChanged.get(_currentModel.getName())).booleanValue();
    }

    @SuppressWarnings("unchecked")
    public void setPaletteAsUnchanged() { // jbaek added
    	_modelsToPalChanged.put(_currentModel.getName(), Boolean.FALSE);
    }
    
    public String getModelName() {
        return _unprojectedModel.getName();
    }

    //
    // eval (custom vars) stuff
    //

    public Set getCustVars(String moduleName) {
        return _view.getModelView().getCustVars(moduleName);
    }

    /**
     * @deprecated
     */
    public String addCustVar(CustVar var) {
    	return null;
    }
    
    /**
     * @deprecated
     */
    public String validateCustVar(CustVar var) {
        return null;
    }

    /**
     * if custvar got partitioned (because it is of a UnionType), removing any one
     * part (i.e. set or relation) will remove the complementary parts as well
     * (i.e. the whole cust var)
     *
     * @param eltToRemove AlloySet or AlloyRelation corresponding to custvar to be removed
     */
    public void removeCustVar(Object eltToRemove) {
        Object origElt = _custEltsToOrig.get(eltToRemove);
        CustVar var = getCustForElt(origElt);
        _view.getModelView().removeCustVar(var);

        for (Iterator eltIter = ((Set)_custVarsToElts.get(var)).iterator(); eltIter.hasNext();) {
            // clear everything

            Object elt = eltIter.next();

            //origElt = _custEltsToOrig.get(elt);

            /*if (origElt != null) {
                // this elt got flattened during projection and so has an
                // orig. elt
                if (origElt instanceof AlloySet) {
                    _instance.removeSet((AlloySet)origElt, var.getModuleName());
                    _unprojectedModel.getModuleByName(var.getModuleName()).removeSet((AlloySet)origElt);
                } else {
                    _instance.removeRelation((AlloyRelation)origElt, var.getModuleName());
                    _unprojectedModel.getModuleByName(var.getModuleName()).removeRelation(
                        (AlloyRelation)origElt);
                }
            } else {*/
            // shouldn't need to look at origElt, because custVarsToElts contains
            // the elts in their original forms

            if (elt instanceof AlloySet) {
                _instance.removeSet((AlloySet)elt, var.getModuleName());
                _unprojectedModel.getModuleByName(var.getModuleName()).removeSet((AlloySet)elt);
            } else {
                _instance.removeRelation((AlloyRelation)elt, var.getModuleName());
                _unprojectedModel.getModuleByName(var.getModuleName()).removeRelation((AlloyRelation)elt);
            }

            //_eltsToCustVars.remove(origElt);
            _eltsToCustVars.remove(elt);
        }
        _custVarsToElts.remove(var);
    }

    @SuppressWarnings("unchecked")
    public void addCustEltMapping(CustVar cv, Object elt) {
        Set eltSet = (Set)_custVarsToElts.get(cv);
        if (eltSet == null) {
            eltSet = new HashSet();
            _custVarsToElts.put(cv, eltSet);
        }
        eltSet.add(elt);
        _eltsToCustVars.put(elt, cv);
    }

    public Object getEltForCust(CustVar cv) {
        return _custVarsToElts.get(cv);
    }

    public CustVar getCustForElt(Object elt) {
        return (CustVar)_eltsToCustVars.get(elt);
    }
    
    public Object getOrigEltForCustomizedElt(Object custElt) {
        return _custEltsToOrig.get(custElt);        
    }

    /*
    public Set getCustNodes(String moduleName) {
        return (Set)_custNodesMap.get(moduleName);
    }
    
    
    public Set getCustEdges(String moduleName) {
        return (Set)_custEdgesMap.get(moduleName);
    }
    
    public void addCustNode(String moduleName, AlloyNodeElement n) {
    
        if (!n.isCustom()) {
            throw new IllegalArgumentException("node must be custom");
        }
        Set nodes = (Set)_custNodesMap.get(moduleName);
    
        if (nodes == null) {
            nodes = new HashSet();
            _custNodesMap.put(moduleName, nodes);
        }
        nodes.add(n);
    }
    
    public void addCustEdge(String moduleName, AlloyRelation e) {
    
        if (!e.isCustom()) {
            throw new IllegalArgumentException("edge must be custom");
        }
        Set edges = (Set)_custEdgesMap.get(moduleName);
    
        if (edges == null) {
            edges = new HashSet();
            _custEdgesMap.put(moduleName, edges);
        }
        edges.add(e);
    }
    
    
    public void clearCustElements(String moduleName) {
        Set nodes = (Set)_custNodesMap.get(moduleName);
        Set edges = (Set)_custEdgesMap.get(moduleName);
    
        if (nodes != null) {
            nodes.clear();
        }
        if (edges != null) {
            edges.clear();
        }
    }*/

    @SuppressWarnings("unchecked")
    public void addVizStateListener(VizStateListener l) {
        vizStateListeners.add(l);
    }

    public void removeVizStateListener(VizStateListener l) {
        vizStateListeners.remove(l);
    }

    private void fireViewsChanged() {
        Iterator it = vizStateListeners.iterator();
        while (it.hasNext()) {
            VizStateListener l = (VizStateListener)it.next();
            l.viewsChanged(new VizStateEvent(VizState.this));
        }
    }

    /**
     * recomputes each type, set, and rel's inherited viz settings
     */
    @SuppressWarnings("unchecked")
    public void recomputeInheritedSettings() {
        _nodeEltToViz = new HashMap();
        _relToViz = new HashMap();

        // figure out settings for types first by traversing up the
        // typestructure

        LinkedList queue = new LinkedList(_currentModel.getTypes());

        while (!queue.isEmpty()) {

            AlloyType curType = (AlloyType)queue.removeFirst();

            AlloyType superType = _currentModel.getSuperForType(curType);

            if (superType == null) {
                // top-level type, compare with universal settings
                _nodeEltToViz.put(curType, getUniversalNodeViz());
            } else if (_nodeEltToViz.get(superType) != null) {
                // supertype setting already resolved -- change the settings of the
                // fields that are set to something other than null (inherit) in the
                // current type
                _nodeEltToViz.put(
                    curType,
                    resolveNodeVizDifference(
                        (NodeViz)_nodeEltToViz.get(superType),
                        _view.getModelView().getVizMap().getNodeViz(superType)));

            } else {
                // put at the end of queue and come back to it later
                queue.addLast(curType);
            }

        }

        // for sets, just indicate the universal setting -- why not indicate its type's
        // setting?  well, if the set is on a union type (not supported in viz yet), then
        // even from a conceptual perceptive it's hard to say which type to use

        for (Iterator setIter = _currentModel.getSets().iterator(); setIter.hasNext();) {
            AlloySet curSet = (AlloySet)setIter.next();
            _nodeEltToViz.put(curSet, getUniversalNodeViz());
        }

        // for relations, just resolve with universal edge viz
        for (Iterator relIter = _currentModel.getRelations().iterator(); relIter.hasNext();) {
            AlloyRelation curRel = (AlloyRelation)relIter.next();
            _relToViz.put(curRel, getUniversalEdgeViz());
        }

    }

    @SuppressWarnings("unchecked")
    private NodeViz resolveNodeVizDifference(NodeViz superViz, NodeViz curViz) {

        Map resolved = new HashMap();

        List fields = new ArrayList();

        fields.add(VizResolver.VISIBLE);
        fields.add(VizResolver.COLOR);
        fields.add(VizResolver.SHAPE);
        fields.add(VizResolver.STYLE);
        fields.add(VizResolver.SAME_RANK);
        fields.add(VizResolver.SHOW_LABEL);
        fields.add(VizResolver.SELECTED);
        fields.add(VizResolver.ATTRIBUTE);
        fields.add(VizResolver.HIDE_UNCONNECTED);
        fields.add(VizResolver.NUMBER_ATOMS);

        for (Iterator fieldIter = fields.iterator(); fieldIter.hasNext();) {
            String curField = (String)fieldIter.next();
            Object curSetting = VizResolver.lookupNodeSetting(curViz, curField);
            if (curSetting == null) {
                // use supertype setting
                resolved.put(curField, VizResolver.lookupNodeSetting(superViz, curField));
            } else {
                resolved.put(curField, curSetting);
            }
        }

        return new NodeViz(
            (Boolean)resolved.get(VizResolver.VISIBLE),
            "",
            (DotColor)resolved.get(VizResolver.COLOR),
            (DotShape)resolved.get(VizResolver.SHAPE),
            (DotStyle)resolved.get(VizResolver.STYLE),
            (Boolean)resolved.get(VizResolver.SAME_RANK),
            (Boolean)resolved.get(VizResolver.SHOW_LABEL),
            (Boolean)resolved.get(VizResolver.SELECTED),
            (Boolean)resolved.get(VizResolver.ATTRIBUTE),
            (Boolean)resolved.get(VizResolver.HIDE_UNCONNECTED),
            (Boolean)resolved.get(VizResolver.NUMBER_ATOMS));

    }

    @SuppressWarnings("unchecked")
    /*
    private EdgeViz resolveEdgeVizDifference(EdgeViz superViz, EdgeViz curViz) {

        Map resolved = new HashMap();

        List fields = new ArrayList();

        fields.add(VizResolver.VISIBLE);
        fields.add(VizResolver.COLOR);
        fields.add(VizResolver.STYLE);
        fields.add(VizResolver.ATTRIBUTE);
        fields.add(VizResolver.SAME_RANK);
        fields.add(VizResolver.MERGE);
        fields.add(VizResolver.SELECTED);
        fields.add(VizResolver.LAYOUT_BACK);

        for (Iterator fieldIter = fields.iterator(); fieldIter.hasNext();) {
            String curField = (String)fieldIter.next();
            Object curSetting = VizResolver.lookupEdgeSetting(curViz, curField);
            if (curSetting == null) {
                // use supertype setting
                resolved.put(curField, VizResolver.lookupEdgeSetting(superViz, curField));
            } else {
                resolved.put(curField, curSetting);
            }
        }

        return new EdgeViz(
            (Boolean)resolved.get(VizResolver.VISIBLE),
            "",
            (DotColor)resolved.get(VizResolver.COLOR),
            (DotStyle)resolved.get(VizResolver.STYLE),
            0,
            (Boolean)resolved.get(VizResolver.ATTRIBUTE),
            (Boolean)resolved.get(VizResolver.SAME_RANK),
            (Boolean)resolved.get(VizResolver.MERGE),
            (Boolean)resolved.get(VizResolver.SELECTED),
            (Boolean)resolved.get(VizResolver.LAYOUT_BACK));

    }
    */

    public NodeViz getInheritedNodeViz(AlloyNodeElement elt) {
        return (NodeViz)_nodeEltToViz.get(elt);
    }

    public EdgeViz getInheritedEdgeViz(AlloyRelation rel) {
        return (EdgeViz)_relToViz.get(rel);
    }

}
