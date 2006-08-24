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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import kodviz.util.Dbg;

import kodviz.dotviz.DotArrowHead;
import kodviz.dotviz.DotDirection;
import kodviz.dotviz.DotStyle;
import kodviz.graph.Cartoon;
import kodviz.graph.Edge;
import kodviz.graph.Graph;
import kodviz.graph.Indexer;
import kodviz.graph.Node;


/**
 * Cartoonist is the class responsible for taking the View and Instance, and
 * producing a Cartoon, for use with the Visualizer.
 */
public class Cartoonist {

    private VizInstance _originalInstance, _projectedInstance;
    private GeneralView _generalView;
    private InstanceProjectionFrame _ipf;
    private VizResolver _vizResolver;

    // _nodeAttributes is used to store the *set* labels that get added to the 
    // label of the node.
    // _nodeAttrForRels is used to store the *set* labels that get added to 
    // a *relations's* attribute on a *node*.
    // e.g. I have a relation pet with tuple Man_1 -> Dog_1.  I want to show    
    // pet as an attribute.
    // note: values of _nodeAttributes don't include the atom's name (given by 
    // type's settings) while those of _nodeAttrForRels do.  Finally remember
    // that _nodeAttrForRel is a map from Node->StringBuffer, not Node->List  
    private Map _nodeAttrForRel;
    private Map _nodeAttributes;
    private Set _unconnectedNodes;

    // Used to keep pointers to the Node object associated with each AlloyAtom
    // once it is created.
    private Map _atomsToNodes = new HashMap();
    // Used to keep pointers to the Edge object associated with each AlloyTuple
    // once it is created.
    //private Map _tuplesToEdges = new HashMap();

    // Used to keep track of indices while iterating through the projected types
    //private Map _typesToIndices = new HashMap();

    private Map _nodesToAtoms = new HashMap();

    /**
     * Produces a Cartoon from the given Instance and View.
     */
    public Cartoon produceCartoon(VizInstance instance_, View view_) {
        reset();

        _originalInstance = instance_;
        _generalView = view_.getGeneralView();
        _ipf = view_.getModelView().getProjectionFrame().instantiate(instance_);

        Cartoon cartoon = new Cartoon();
        Graph graph;
        Set nodes;
        Set edges;
        Set ranks;

        // First, set general settings
        setGeneralSettings(cartoon);

        // For each possible set of indices that are projected on, create an instance
        Map atomsToIndexers = iterateThroughProjectedTypes();
        for (Iterator projections = atomsToIndexers.keySet().iterator(); projections.hasNext();) {
            _nodeAttributes = new HashMap();
            _nodeAttrForRel = new HashMap();
            _unconnectedNodes = new HashSet();
            Set projectedAtoms = (Set)projections.next();
            if (projectedAtoms.contains(null)) {
                //System.out.println("one or more projected types have no atoms");
                List indices = (List)atomsToIndexers.get(projectedAtoms);
                //System.out.println("putting: " + indices);
                cartoon.assocGraphWithIndices(indices, null);
            } else {
                List indices = (List)atomsToIndexers.get(projectedAtoms);
                for (Iterator atomsIter = projectedAtoms.iterator(); atomsIter.hasNext();) {
                    AlloyAtom atom = (AlloyAtom)atomsIter.next();
                    _ipf.setCurrentAtom(getTopLevelType(atom.getType()), atom);
                }
                InstanceProjector ip = new InstanceProjector(_originalInstance, _ipf);
                _projectedInstance = ip.getProjectedInstance();
                _vizResolver = new VizResolver(_projectedInstance, view_);
                // Now, create Nodes for each AlloyAtom.
                nodes = createNodes();

                // Next, do the edges.
                edges = createEdges();

                // Finally, the ranks.
                ranks = createRanks();

                for (Iterator toRemove = _unconnectedNodes.iterator(); toRemove.hasNext();) {
                    Node node = (Node)toRemove.next();
                    AlloyAtom atom = (AlloyAtom)_nodesToAtoms.get(node);
                    if (_vizResolver.getSettingsForAtom(atom).hideUnconnected().booleanValue()) {
                        nodes.remove(node);
                    }
                }

                graph = new Graph(nodes, edges, ranks);

                cartoon.assocGraphWithIndices(indices, graph);
            }

        }
        return cartoon;
    }

    private AlloyType getTopLevelType(AlloyType type) {
        // skip univ, unless the atom itself is really of type univ
        // (this should not be possible, however)

        if (type.getName().equals("univ")) {
            return type;
        }

        AlloyType top = type;
        while (top != null) {
            AlloyType newTop = _originalInstance.getSuperForType(top);
            if (newTop == null || newTop.getName().equals("univ")) {
                break;
            } else {
                top = newTop;
            }
        }

        return top;
    }

    /**
     * Returns the projected model.  This method returns valid values only after produceCartoon
     * has been called on a valid ProjectionFrame.  Otherwise (empty ProjectionFrame and/or
     * produceCartoon not yet called), null is returned.
     */
    public Model getProjectedModel() {
        if (_projectedInstance != null) {
            return _projectedInstance.getModel();
        } else {
            return null;
        }
    }

    //
    // GENERAL STUFF
    //

    /**
     * Resets the Cartoonist so it is ready to produce a fresh Cartoon.
     */
    private void reset() {
        _atomsToNodes.clear();
        //_tuplesToEdges.clear();
        _nodesToAtoms.clear();
    }

    /**
     * Set the general settings for the cartoon
     */
    private void setGeneralSettings(Cartoon c) {
        c.setOrientation(_generalView.getGeneralOrientation());
        c.setFontName(_generalView.getGeneralFontName());
        c.setFontSize(_generalView.getGeneralFontSize());
    }

    //
    // NODES
    //

    /**
     * Create the set of Nodes that will appear in the Cartoon.
     */
    @SuppressWarnings("unchecked")
    private Set createNodes() {
        Set nodes = new HashSet();

        for (Iterator atoms = _projectedInstance.getAllAtoms().iterator(); atoms.hasNext();) {
            AlloyAtom atom = (AlloyAtom)atoms.next();
            NodeViz atomSettings = _vizResolver.getSettingsForAtom(atom);

            // Now that we have the settings, create a Node and add it to the
            // set of Nodes
            Node n = createNode(atom, atomSettings);
            nodes.add(n);
            _atomsToNodes.put(atom, n);
            _nodesToAtoms.put(n, atom);
        }
        _unconnectedNodes = new HashSet(nodes);
        return nodes;
    }

    /**
     * Create a Node from an AlloyAtom and a NodeViz.
     */
    @SuppressWarnings("unchecked")
    private Node createNode(AlloyAtom atom, NodeViz settings) {
        int suffix = atom.getIndex();

        String label = _vizResolver.createNodeLabel(atom, suffix, settings.numberAtoms());
        StringBuffer setsLabel = _vizResolver.createSetsLabel(atom);
        StringBuffer setsLabelForRel = _vizResolver.createSetsLabelForRel(atom);

        String id = atom.getType().getName() + "_" + suffix;

        DotStyle style;
        if (settings.isVisible().booleanValue()) {
            style = settings.getStyle();
        } else {
            style = DotStyle.INVIS;
        }

        Node temp = new Node(id, label, settings.getShape(), settings.getColor(), style);

        if (setsLabel.length() != 0) {
            List labels = (List)_nodeAttributes.get(temp);
            if (labels == null) {
                labels = new LinkedList();
                _nodeAttributes.put(temp, labels);
            }
            // and add this newly created attribute label
            /*
            if (!label.equals("")) {
                labels.add(new StringBuffer(setsLabel));
            }
            else {*/
            labels.add(setsLabel);
            //}
        }

        //System.out.println("setsLabel: " + setsLabel);
        //System.out.println("setsLabelForRel: " + setsLabelForRel);

        // add this newly created attribute label
        // differs from the _nodeAttributes map in that here 
        // automatically put in the atom name, too
        if (!label.equals("")) {
            //System.out.println("yay "+setsLabelForRel.toString()+setsLabelForRel==null);
            if (setsLabelForRel.length() != 0) {
                _nodeAttrForRel.put(temp, new StringBuffer(label + " " + setsLabelForRel));
            } else {
                _nodeAttrForRel.put(temp, new StringBuffer(label));
            }
        } else {
            _nodeAttrForRel.put(temp, setsLabelForRel);
        }

        return temp;
    }

    //
    // EDGES
    //

    /**
     * Create the set of edges that will appear in a graph.
     */
    @SuppressWarnings("unchecked")
    private Set createEdges() {
        Set edges = new HashSet();
        //EdgeViz defaultEdgeViz =
        _generalView.getGeneralEdgeViz();

        Map attrLabels = new HashMap();
        // maps each relation to maps of starting nodes-> +ending nodes LABELS

        // Iterate through all of the relations in the instance...
        for (Iterator relations = _projectedInstance.getAllRelations().iterator(); relations.hasNext();) {
            AlloyRelation rel = (AlloyRelation)relations.next();

            // Get their visualization settings            
            EdgeViz relSettings = _vizResolver.getSettingsForRelation(rel);
            //System.out.println(rel);
            //System.out.println(relSettings.getLabel());

            // First, if this relation is an attribute, add it to the list of
            // attributes for its node of origin.  attribute labels take the
            // form:
            //
            // <relation name>: <node label> (<set label>, <set label>, ...)
            //
            // So if Person_0 likes Person_1, and Person_1 is a member of the
            // sets Adult and Worker, the attribute label under Person_0 would
            // look like this:
            //
            // likes: Person_1 (Adult, Worker)

            Map startToEndNodes = (Map)attrLabels.get(rel); // a new map for each relation
            if (startToEndNodes == null) {
                startToEndNodes = new HashMap();
                attrLabels.put(rel, startToEndNodes);
            }

            if (relSettings.isAttribute().booleanValue()) {
                for (Iterator tuples = _projectedInstance.getTuplesInRelation(rel).iterator();
                    tuples.hasNext();
                    ) {
                    AlloyTuple tuple = (AlloyTuple)tuples.next();
                    Node start = (Node)_atomsToNodes.get(tuple.getStart());

                    Dbg.chk(start);

                    List endNodes = (List)startToEndNodes.get(start);
                    if (endNodes == null) {
                        endNodes = new LinkedList();
                        startToEndNodes.put(start, endNodes);
                    }

                    StringBuffer label;

                    if (tuple.getArity() > 2) {
                        StringBuffer nodeListLabel = new StringBuffer();
                        List nodes = tuple.getAtoms();
                        nodeListLabel.append(_nodeAttrForRel.get(_atomsToNodes.get((AlloyAtom)nodes.get(1))));
                        //((Node)_atomsToNodes.get((AlloyAtom)nodes.get(1))).getLabel());                            
                        for (int i = 2; i < nodes.size(); i++) {
                            nodeListLabel.append("->");
                            nodeListLabel.append(
                                _nodeAttrForRel.get(_atomsToNodes.get((AlloyAtom)nodes.get(i))));
                            //((Node)_atomsToNodes.get((AlloyAtom)nodes.get(i))).getLabel());
                        }
                        label = nodeListLabel;
                    } else {
                        // I have no clue what all the \\\\n's are for.  Just c&p'ing previous code.
                        label = (StringBuffer)_nodeAttrForRel.get(_atomsToNodes.get(tuple.getEnd()));
                        /*new StringBuffer(
                            ((Node)_atomsToNodes.get(tuple.getEnd())).getLabel().replaceAll(
                                "\\\\n",
                                " "));*/
                    }
                    endNodes.add(label);

                    /*
                    StringBuffer attrLabel = new StringBuffer();
                    // First get the relation nameendNodes.add();
                    
                    String relLabel = relSettings.getLabel();
                    if (relLabel==null) {
                    relLabel = "";
                    }
                    
                    
                    attrLabel.append("\\n" + relLabel + ": ");
                    // Then the node label
                    String nodeLabel=((Node)_atomsToNodes.get(tuple.getEnd())).getLabel().replaceAll("\\\\n",
                    									     " ");
                    attrLabel.append(nodeLabel);
                    // Then get the list of attribute labels for this node
                    List labels = (List)nodeAttributes.get(start);
                    if (labels == null) {
                    labels = new LinkedList();
                    nodeAttributes.put(start, labels);
                    }
                    // and add this newly created attribute label
                    labels.add(attrLabel);
                    */
                }
            }
            // We only need to create the edges if they're going to be visible
            if (relSettings.isVisible().booleanValue()) {
                // If we're not merging bidirectional arrows, simply create an edge for each tuple.
                if (!relSettings.mergeArrows().booleanValue()) {
                    for (Iterator tuples = _projectedInstance.getTuplesInRelation(rel).iterator();
                        tuples.hasNext();
                        ) {
                        AlloyTuple tuple = (AlloyTuple)tuples.next();
                        Node start = (Node)_atomsToNodes.get(tuple.getStart());
                        Node end = (Node)_atomsToNodes.get(tuple.getEnd());

                        Dbg.chk(start);
                        Dbg.chk(end);
                        // Only create the edge if the starting and ending nodes
                        // are visible
                        if (start.getStyle() != DotStyle.INVIS && end.getStyle() != DotStyle.INVIS) {

                            Edge e = createEdge(start, end, relSettings, false, tuple);

                            edges.add(e);
                            //_tuplesToEdges.put(tuple, e);
                        }
                    }
                }
                // Otherwise, find bidirectional arrows and only create one edge for
                // each pair.
                else {
                    Set tuples = _projectedInstance.getTuplesInRelation(rel);
                    //Set bidirectional = new HashSet();
                    Set ignore = new HashSet();
                    
                    for (Iterator tuplesIter = tuples.iterator(); tuplesIter.hasNext();) {
                        AlloyTuple tuple = (AlloyTuple)tuplesIter.next();

                        if (ignore.contains(tuple)) {
                            continue;
                        }

                        AlloyTuple reverseTuple = tuple.reversed();

                        Node start = (Node)_atomsToNodes.get(tuple.getStart());
                        Node end = (Node)_atomsToNodes.get(tuple.getEnd());

                        Dbg.chk(start);
                        Dbg.chk(end);

                        // Only create the edge if the starting and ending nodes
                        // are visible
                        if (start.getStyle() != DotStyle.INVIS && end.getStyle() != DotStyle.INVIS) {

                            Edge e;

                            // check if reversed tuple is in the same relation, and if this r
                            // relation was a self edge, in which case we do a single arrow.
                            if (tuples.contains(reverseTuple) && !reverseTuple.equals(tuple)) {
                                ignore.add(reverseTuple);
                                // create a <--> edge
                                e = createEdge(start, end, relSettings, true, tuple);
                            } else {
                                // create a --> edge
                                e = createEdge(start, end, relSettings, false, tuple);
                            }
                            edges.add(e);
                            //_tuplesToEdges.put(tuple, e);

                        }
                    }
                }
            }
        }

        // put attrLabels info in nodeAttr
        for (Iterator rels = attrLabels.keySet().iterator(); rels.hasNext();) {
            AlloyRelation rel = (AlloyRelation)rels.next();
            EdgeViz relSettings = _vizResolver.getSettingsForRelation(rel);
            Map startToEndNodes = (Map)attrLabels.get(rel);
            for (Iterator startIter = startToEndNodes.keySet().iterator(); startIter.hasNext();) {
                Node start = (Node)startIter.next();
                List nodeLabels = (List)startToEndNodes.get(start);

                //
                // tack all the labels together
                //

                StringBuffer label = new StringBuffer();

                // unless there's no label for the relation, put at the front "name: "
                if (!relSettings.getLabel().equals("")) {
                    label.append(relSettings.getLabel());
                    label.append(": ");
                }

                Iterator lblIter = nodeLabels.iterator();
                if (lblIter.hasNext()) {
                    label.append((StringBuffer)lblIter.next());

                    while (lblIter.hasNext()) {
                        StringBuffer lbl = (StringBuffer)lblIter.next();
                        label.append(", ");
                        label.append(lbl);
                    }
                }

                List labels = (List)_nodeAttributes.get(start);
                if (labels == null) {
                    labels = new LinkedList();
                    _nodeAttributes.put(start, labels);
                }

                labels.add(label);
            }
        }

        // Finally, take all of those stored attribute labels, and put them on the Nodes.
        for (Iterator nodes = _nodeAttributes.keySet().iterator(); nodes.hasNext();) {
            Node n = (Node)nodes.next();
            List attrLbls = (List)_nodeAttributes.get(n);
            StringBuffer label = new StringBuffer();
            for (Iterator labels = attrLbls.iterator(); labels.hasNext();) {
                label.append((StringBuffer)labels.next());
                if (labels.hasNext())
                    label.append("\\n"); // add newlines until the last one
            }

            // add newline before first rel attribute if previous parts non-empty
            if (!n.getLabel().equals("")) {
                n.setLabel(n.getLabel() + "\\n" + label);
            } else {
                n.setLabel(label.toString());
            }

        }
        return edges;
    }

    /**
     * Create an Edge given the starting and ending Nodes, the EdgeViz, and
     * whether or not it is bidirectional (and to be laid out backwards).
     *
     * you need an AlloyTuple to do indexing.
     */
    private Edge createEdge(
        Node start,
        Node end,
        EdgeViz settings,
        boolean bidirectional,
        AlloyTuple tuple) {
        DotDirection dir;

        boolean layoutBack = settings.layoutBack().booleanValue();

        String label = settings.getLabel();
        if (label == null) {
            label = "";
            System.out.println("null edge label?");
        } else if (label.equals("")) {
            //System.out.println("blank edge label?");
        }

        if (tuple.getArity() > 2) {
            StringBuffer nodeListLabel = new StringBuffer();
            List nodes = tuple.getAtoms();
            nodeListLabel.append(((Node)_atomsToNodes.get((AlloyAtom)nodes.get(1))).getLabel());
            for (int i = 2; i < nodes.size() - 1; i++) {
                nodeListLabel.append(", ");
                nodeListLabel.append(((Node)_atomsToNodes.get((AlloyAtom)nodes.get(i))).getLabel());
            }
            if (label.equals("")) {
                label = nodeListLabel.toString();
            } else {
                label = label + "[" + nodeListLabel.toString() + "]";
            }
        }

        if (bidirectional) {
            dir = DotDirection.BOTH;
        } else {
            dir = layoutBack ? DotDirection.BACK : DotDirection.FORWARD;
        }

        _unconnectedNodes.remove(start);
        _unconnectedNodes.remove(end);

        if (layoutBack) {
            return new Edge(
                end,
                start,
                label,
                settings.getStyle(),
                settings.getColor(),
                dir,
                DotArrowHead.NORMAL,
                settings.getWeight(),
                label);
        }
        return new Edge(
            start,
            end,
            label,
            settings.getStyle(),
            settings.getColor(),
            dir,
            DotArrowHead.NORMAL,
            settings.getWeight(),
            label);
    }

    //
    // RANKS
    //

    /**
     * Creates the set of sets of atoms that should be placed on the same rank.
     */
    @SuppressWarnings("unchecked")
    private Set createRanks() {
        Set ranks = new HashSet();

        ranks.addAll(rankTypes());
        ranks.addAll(rankSets());
        ranks.addAll(rankEdges());

        return ranks;
    }

    /**
     * Generate a set of sets of Nodes that should be ranked together based on
     * their type.
     */
    @SuppressWarnings("unchecked")
    private Set rankTypes() {
        Set retSet = new HashSet();

        // Go through all the AlloyTypes to see if they are marked Same Rank
        List types = _projectedInstance.getAllTypes();
        for (Iterator typesIter = types.iterator(); typesIter.hasNext();) {
            AlloyType type = (AlloyType)typesIter.next();
            if (_vizResolver.getTypeRank(type)) {
                // If so, get all of the atoms of that type, and put their
                // nodes on the same rank.
                Set atoms = _projectedInstance.getAtomsOfType(type);
                Set nodes = new HashSet();
                for (Iterator atomsIter = atoms.iterator(); atomsIter.hasNext();) {
                    AlloyAtom atom = (AlloyAtom)atomsIter.next();
                    Node node = (Node)_atomsToNodes.get(atom);
                    // Only add the node if it was successfully created
                    // AND it is in connected if hide unconnected is on
                    boolean hideUnconn =
                        _vizResolver.getSettingsForAtom(atom).hideUnconnected().booleanValue();
                    if (node != null && (!hideUnconn || !_unconnectedNodes.contains(node))) {
                        nodes.add(node);
                    }
                }
                retSet.add(nodes);
            }
        }
        return retSet;
    }

    /**
     * Generate a set of sets of Nodes that should be ranked together based on
     * their set.
     */
    @SuppressWarnings("unchecked")
    private Set rankSets() {
        Set retSet = new HashSet();

        // Go through all the AlloySets to see if they are marked Same Rank
        List sets = _projectedInstance.getAllSets();
        for (Iterator setsIter = sets.iterator(); setsIter.hasNext();) {
            AlloySet set = (AlloySet)setsIter.next();
            if (_vizResolver.getSetRank(set)) {
                // If so, get all atoms in that set, and put their nodes on
                // the same rank.
                Set atoms = _projectedInstance.getAtomsInSet(set);
                Set nodes = new HashSet();
                for (Iterator atomsIter = atoms.iterator(); atomsIter.hasNext();) {
                    AlloyAtom atom = (AlloyAtom)atomsIter.next();
                    Node node = (Node)_atomsToNodes.get(atom);
                    // Only add the node if it was successfully created
                    boolean hideUnconn =
                        _vizResolver.getSettingsForAtom(atom).hideUnconnected().booleanValue();
                    if (node != null && (!hideUnconn || !_unconnectedNodes.contains(node))) {
                        nodes.add(node);
                    }
                }
                retSet.add(nodes);
            }
        }
        return retSet;
    }

    /**
     * Generate a set of sets of Nodes that should be ranked together based on
     * the Edges they are connected by.
     */
    @SuppressWarnings("unchecked")
    private Set rankEdges() {
        Set retSet = new HashSet();

        // Go through all of the AlloyRelations to see if they are marked Same
        // Rank.
        List rels = _projectedInstance.getAllRelations();
        for (Iterator relsIter = rels.iterator(); relsIter.hasNext();) {
            AlloyRelation rel = (AlloyRelation)relsIter.next();
            if (_vizResolver.getRelationRank(rel)) {
                // If so, get all tuples in that relation, and put their
                // starting and ending node<s on the the same rank.
                Set tuples = _projectedInstance.getTuplesInRelation(rel);
                for (Iterator tuplesIter = tuples.iterator(); tuplesIter.hasNext();) {
                    AlloyTuple tuple = (AlloyTuple)tuplesIter.next();
                    Set nodes = new HashSet();

                    Node fromNode = (Node)_atomsToNodes.get(tuple.getStart());
                    Node toNode = (Node)_atomsToNodes.get(tuple.getEnd());

                    if (fromNode == null || toNode == null) {
                        continue;
                    }

                    if (fromNode.getStyle() == DotStyle.INVIS || toNode.getStyle() == DotStyle.INVIS) {
                        continue;
                    }

                    nodes.add(fromNode);
                    nodes.add(toNode);
                    retSet.add(nodes);
                }

            }
        }
        return retSet;
    }

    /**
     * Go through all of the projected types and for every possible combination,
     * create a mapping between a Set of AlloyAtoms and a List of Indexers.
     *
     * note: indexers generated contain only the projected types, in sorted order,
     *       starting at 0.
     */
    @SuppressWarnings("unchecked")
    private Map iterateThroughProjectedTypes() {
        List projectedTypes = new ArrayList(_ipf.getProjectedTypes());
        SortedMap typesToIndices = new TreeMap();
        Map atomsToIndexers = new HashMap();
        Collections.sort(projectedTypes);
        // Set up a map of all of the projected types, starting at index 0
        for (Iterator typesIter = projectedTypes.iterator(); typesIter.hasNext();) {
            AlloyType type = (AlloyType)typesIter.next();
            List atoms = new ArrayList(_originalInstance.getAtomsOfType(type));
            // if the type has no atoms at all, then use the flag -1 so we know 
            // later on
            typesToIndices.put(type, atoms.isEmpty() ? new Integer(-1) : new Integer(0));
        }
        // Create a set of atoms and list of indexers for every possible
        // combination of atoms

        do {
            //int i = 0;
            Set atomSet = new HashSet();
            List indexerList = new LinkedList();
            for (Iterator typesIter = projectedTypes.iterator(); typesIter.hasNext();) {
                AlloyType type = (AlloyType)typesIter.next();
                List atoms = new ArrayList(_originalInstance.getAtomsOfType(type));
                Collections.sort(atoms);
                int index = ((Integer)typesToIndices.get(type)).intValue();
                if (!atoms.isEmpty()) {
                    atomSet.add(atoms.get(index));
                } else {
                    atomSet.add(null);
                }
                indexerList.add(new Indexer(type.getName(), index));
                //System.out.println(i);
                //i++;
            }
            atomsToIndexers.put(atomSet, indexerList);
            increment(typesToIndices);
        } while (!done(typesToIndices));

        return atomsToIndexers;
    }

    /**
     * Take the map of AlloyTypes to indices, and incrementit by one, in such a
     * way that all possible combinations of atoms will be generated before any
     * repeats.
     */
    @SuppressWarnings("unchecked")
    private void increment(SortedMap typesToIndices) {
        if (typesToIndices.size() > 0) {
            AlloyType type = (AlloyType)typesToIndices.lastKey();
            int maxValue = _originalInstance.getAtomsOfType(type).size() - 1;
            int currentValue = ((Integer)typesToIndices.get(type)).intValue();
            if (currentValue == maxValue || maxValue == -1) {
                typesToIndices.put(type, new Integer(0));
                increment(typesToIndices.headMap(type));
            } else {
                typesToIndices.put(type, new Integer(currentValue + 1));
            }
        }
    }

    /**
     * Tests if we are done iterating through all possible combinations of atoms
     * for the projected types
     */
    private boolean done(Map typesToIndices) {
        for (Iterator typesIter = typesToIndices.keySet().iterator(); typesIter.hasNext();) {
            AlloyType type = (AlloyType)typesIter.next();
            if (((Integer)typesToIndices.get(type)).intValue() != 0) {
                return false;
            }
        }
        return true;
    }
}
