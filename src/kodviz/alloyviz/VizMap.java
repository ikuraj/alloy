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
package kodviz.alloyviz;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * VizMap provides a mapping from the alloy concepts of sets, types, and
 * elements to their visual display properties, some of which are dot-specific.
 * The mappings are divided between a current, active map and a cached,
 * irrelevant map.  As an invariant, the cached map must be emptied and
 * merged with the active map before saving occurs.
 */
public class VizMap implements Serializable {

	private static final long serialVersionUID = 1L;

	Map relevantNodes, relevantEdges, cachedNodes, cachedEdges;

    //
    // CONSTRUCTOR
    //

    /**
     * Basic constructor
     */
    public VizMap() {

        relevantNodes = new HashMap();
        relevantEdges = new HashMap();
        cachedNodes = new HashMap();
        cachedEdges = new HashMap();

    }

    public void reset() {
        relevantNodes.clear();
        relevantEdges.clear();
        cachedNodes.clear();
        cachedEdges.clear();
    }

    @SuppressWarnings("unchecked")
    public VizMap copy() {
        VizMap vm = new VizMap();
        for (Iterator nodeIter = this.relevantNodes.keySet().iterator(); nodeIter.hasNext();) {
            AlloyNodeElement next = (AlloyNodeElement)nodeIter.next();
            NodeViz viz = (NodeViz)this.relevantNodes.get(next);
            vm.relevantNodes.put(next, viz.copy());
        }
        for (Iterator nodeIter = this.cachedNodes.keySet().iterator(); nodeIter.hasNext();) {
            AlloyNodeElement next = (AlloyNodeElement)nodeIter.next();
            NodeViz viz = (NodeViz)this.cachedNodes.get(next);
            vm.cachedNodes.put(next, viz.copy());
        }
        for (Iterator edgeIter = this.relevantEdges.keySet().iterator(); edgeIter.hasNext();) {
            AlloyRelation next = (AlloyRelation)edgeIter.next();
            EdgeViz viz = (EdgeViz)this.relevantEdges.get(next);
            vm.relevantEdges.put(next, viz.copy());
        }
        for (Iterator edgeIter = this.cachedEdges.keySet().iterator(); edgeIter.hasNext();) {
            AlloyRelation next = (AlloyRelation)edgeIter.next();
            EdgeViz viz = (EdgeViz)this.cachedEdges.get(next);
            vm.cachedEdges.put(next, viz.copy());
        }
        return vm;
    }

    //
    // MUTATORS
    //

    /**
     * Flushes (clears) all data from the cache.
     */
    public void flushCache() {
        cachedNodes.clear();
        cachedEdges.clear();
    }

    /**
     * Adds a mapping from an AlloyNodeElement to a node visualization.  The
     * mapping will be added into the active map.  If a mapping containing the
     * same key is already present, it will be overwritten.
     *
     * @param elt the AlloyNodeElement to map from
     * @param viz the NodeViz associated with type
     */
    @SuppressWarnings("unchecked")
    public void addNodeMapping(AlloyNodeElement elt, NodeViz viz) {
        relevantNodes.put(elt, viz);
    }

    /**
     * Adds a mapping from an AlloyRelation to an edge visualization.  The
     * mapping will be added into the active map.  If a mapping containing
     * the same key is already present, it will be overwritten.
     *
     * @param rel the AlloyRelation to map from
     * @param viz the EdgeViz associated with rel
     */
    @SuppressWarnings("unchecked")
    public void addEdgeMapping(AlloyRelation rel, EdgeViz viz) {
        relevantEdges.put(rel, viz);
    }

    /**
     * Adds a mapping from an AlloyNodeElement to a node visualization.  The
     * mapping will be added into the cached map.  If a mapping containing the
     * same key is already present, it will be overwritten.
     *
     * @param elt the AlloyNodeElement to map from
     * @param viz the NodeViz associated with type
     */
    @SuppressWarnings("unchecked")
    public void addCachedNodeMapping(AlloyNodeElement elt, NodeViz viz) {
        cachedNodes.put(elt, viz);
    }

    /**
     * Adds a mapping from an AlloyRelation to an edge visualization.  The
     * mapping will be added into the cached map.  If a mapping containing
     * the same key is already present, it will be overwritten.
     *
     * @param rel the AlloyRelation to map from
     * @param viz the EdgeViz associated with rel
     */
    @SuppressWarnings("unchecked")
    public void addCachedEdgeMapping(AlloyRelation rel, EdgeViz viz) {
        cachedEdges.put(rel, viz);
    }

    /**
     * Puts all cached mappings into the active map and clears cache.
     * There should not be any conflicting mappings among the two maps.
     * If such a conflict is found (as evident when the same key
     * appears in both maps with different values), the setting in the 
     * active map will be overwritten.
     *
     * (used to prepare for saving)
     */
    @SuppressWarnings("unchecked")
    public void mergeCacheRelevant() {
        relevantNodes.putAll(cachedNodes);
        relevantEdges.putAll(cachedEdges);
        flushCache();
    }

    /**
     * Moves a specific AlloyNodeElement mapping from the cache to the active
     * map.  If there is no cached mapping originating from the specified
     * AlloyNodeElement, then does nothing.
     *
     * @param elt the AlloyNodeElement whose mapping should be moved from the
     *            cache to the active map
     */
    @SuppressWarnings("unchecked")
    public void activateNodeMapping(AlloyNodeElement elt) {
        if (cachedNodes.containsKey(elt)) {
            relevantNodes.put(elt, cachedNodes.get(elt));
            cachedNodes.remove(elt);
        }
    }

    /**
     * Moves a specific AlloyRelation mapping from the cache to the active map.
     * If there is no cached mapping originating from the specified
     * AlloyRelation, then does nothing.
     *
     * @param rel the AlloyRelation whose mapping should be moved from the cache
     *            to the active map
     */
    @SuppressWarnings("unchecked")
    public void activateEdgeMapping(AlloyRelation rel) {
        if (cachedEdges.containsKey(rel)) {
            relevantEdges.put(rel, cachedEdges.get(rel));
            cachedEdges.remove(rel);
        }
    }

    /**
     * Moves a specific AlloyNodeElement mapping from the active map to the
     * cache.  If there is no mapping in the relevant map originating from the
     * specified AlloyNodeElement, then does nothing.
     *
     * @param elt the AlloyNodeElement whose mapping should be moved from the
     *            active map to the cache
     */
    @SuppressWarnings("unchecked")
    public void cacheNodeMapping(AlloyNodeElement elt) {
        if (relevantNodes.containsKey(elt)) {
            cachedNodes.put(elt, relevantNodes.get(elt));
            relevantNodes.remove(elt);
        }
    }

    /**
     * Moves a specific AlloyRelation mapping from the active map to the cache.
     * If there is no mapping in the relevant map originating from the specified
     * AlloyRelation, then does nothing.
     *
     * @param rel the AlloyRelation whose mapping should be moved from the
     *            active map to the cache
     */
    @SuppressWarnings("unchecked")
    public void cacheEdgeMapping(AlloyRelation rel) {
        if (relevantEdges.containsKey(rel)) {
            cachedEdges.put(rel, relevantEdges.get(rel));
            relevantEdges.remove(rel);
        }
    }

    /**
     * Putting this method here tells the Java serializer to use the return
     * value of this method when serializing.  Whether or not this is
     * an acceptable way of preserving our invariant is questionable.
     */
    private Object writeReplace() throws ObjectStreamException {
        this.mergeCacheRelevant();
        return this;
    }

    //
    // ACCESSORS
    //

    /**
     * Gets the visualization object associated with a particular
     * AlloyNodeElement from the active/relevant map.  If no such visualization
     * is found, returns null.
     *
     * @param elt the AlloyNodeElement for which a visualization object is to be
     *            returned
     * @return the NodeViz object associated with elt in the active map
     */
    public NodeViz getNodeViz(AlloyNodeElement elt) {
        return (NodeViz)relevantNodes.get(elt);
    }

    /**
     * Gets the visualization object associated with a particular AlloyRelation
     * from the active/relevant map.  If no such visualization is found, returns
     * null.
     *
     * @param rel the AlloyRelation for which a visualization object is to be
     *            returned
     * @return the EdgeViz object associated with rel in the active map
     */
    public EdgeViz getEdgeViz(AlloyRelation rel) {
        return (EdgeViz)relevantEdges.get(rel);
    }

    /**
     * Gets the visualization object associated with a particular
     * AlloyNodeElement from the cached map.  If no such visualization is found,
     * returns null.
     *
     * @param elt the AlloyNodeElement for which a NodeViz is to be returned
     * @return the NodeViz object associated with elt in the active map
     */
    public NodeViz getCachedNodeViz(AlloyNodeElement elt) {
        return (NodeViz)cachedNodes.get(elt);
    }

    /**
     * Gets the visualization object associated with a particular AlloyRelation
     * from the cached map.  If no such visualization is found, returns null.
     *
     * @param rel the AlloyRelation for which an EdgeViz is to be returned
     * @return the EdgeViz object associated with elt in the active map
     */
    public EdgeViz getCachedEdgeViz(AlloyRelation rel) {
        return (EdgeViz)cachedEdges.get(rel);
    }

    /**
     * Checks if the active map contains a mapping from the specified
     * AlloyNodeElement
     *
     * @param elt the AlloyNodeElement to look for
     * @return true if such a mapping exists. false otherwise
     */
    public boolean containsNode(AlloyNodeElement elt) {
        return relevantNodes.containsKey(elt);
    }

    /**
     * Checks if the active map contains a mapping from the specified
     * AlloyRelation
     *
     * @param rel the AlloyRelation to look for
     * @return true if such a mapping exists. false otherwise
     */
    public boolean containsEdge(AlloyRelation rel) {
        return relevantEdges.containsKey(rel);
    }

    /**
     * Checks if the cached map contains a mapping from the specified
     * AlloyNodeElement.
     *
     * @param elt the AlloyNodeElement to look for
     * @return true if such a mapping exists. false otherwise
     */
    public boolean cacheContainsNode(AlloyNodeElement elt) {
        return cachedNodes.containsKey(elt);
    }

    /**
     * Checks if the cached map contains a mapping from the specified
     * AlloyRelation.
     *
     * @param rel the AlloyRelation to look for
     * @return true if such a mapping exists. false otherwise
     */
    public boolean cacheContainsEdge(AlloyRelation rel) {
        return cachedEdges.containsKey(rel);
    }

    /**
     * Returns an unmodifiable Set of all elements for which a mapping is stored
     * in the active map.
     *
     * @return the set of nodes and/or edges with a mapping in the active map
     */
    @SuppressWarnings("unchecked")
    public Set getElements() {
        Set returnSet = new HashSet();
        returnSet.addAll(relevantNodes.keySet());
        returnSet.addAll(relevantEdges.keySet());
        return Collections.unmodifiableSet(returnSet);
    }

    @SuppressWarnings("unchecked")
    public Set getNodes() {
        return Collections.unmodifiableSet(relevantNodes.keySet());
    }

    @SuppressWarnings("unchecked")
    public Set getEdges() {
        return Collections.unmodifiableSet(relevantEdges.keySet());
    }

    /**
     * Returns an unmodifiable Set of all elements for which a mapping is
     * cached.
     */
    @SuppressWarnings("unchecked")
    public Set getCachedElements() {
        Set returnSet = new HashSet();
        returnSet.addAll(cachedNodes.keySet());
        returnSet.addAll(cachedEdges.keySet());
        return Collections.unmodifiableSet(returnSet);
    }

    /**
     * Returns a String representation of this VizMap.
     */
    public String toString() {
        return "VizMap:\nRelevent Nodes:\n"
            + relevantNodes.toString()
            + "\nRelevant Edges:\n"
            + relevantEdges.toString()
            + "\nCached Nodes:\n"
            + cachedNodes.toString()
            + "\nCached Edges:\n"
            + cachedEdges.toString();
    }
}
