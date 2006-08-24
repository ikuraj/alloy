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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kodviz.util.Dbg;

import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;


/**
 * The VizResolver works as a sort of assistant to the Cartoonist.  Given an
 * atom or a relation, it can return the NodeViz or EdgeViz object that should
 * be used to create the associated Node or Edge.  The VizResolver takes care of
 * all conflicts and inheritance to return a usable NodeViz or EdgeViz.
 */
public class VizResolver {

    private VizInstance _instance;
    private View _view;
    private Model _model;

    private VizMap _vizMap;
    private NodeViz _defaultNodeViz;
    private EdgeViz _defaultEdgeViz;

    public static final String VISIBLE = "visible";
    public static final String COLOR = "color";
    public static final String SHAPE = "shape";
    public static final String STYLE = "style";
    public static final String LABEL = "label";
    public static final String ATTRIBUTE = "attribute";
    public static final String MERGE = "merge arrows";
    public static final String SAME_RANK = "same rank";
    public static final String LAYOUT_BACK = "layout back";
    public static final String HIDE_UNCONNECTED = "hide unconnected";
    public static final String NUMBER_ATOMS = "number atoms";

    public static final String SHOW_LABEL = "show label";
    public static final String SELECTED = "selected";

    VizResolver(VizInstance instance_, View view_) {
        _instance = instance_;
        _view = view_;
        _model = _instance.getModel();

        _vizMap = _view.getModelView().getVizMap();
        _defaultNodeViz = _view.getGeneralView().getGeneralNodeViz();
        _defaultEdgeViz = _view.getGeneralView().getGeneralEdgeViz();
    }

    /*
     * Create the NodeViz object to represent the settings for the given atom.
     */
    @SuppressWarnings("unchecked")
    NodeViz getSettingsForAtom(AlloyAtom atom) {

        Map selections = new HashMap();

        List fields = new ArrayList(4);
        fields.add(VISIBLE);
        fields.add(COLOR);
        fields.add(SHAPE);
        fields.add(STYLE);

        // First look through all of the sets that this atom is a member of.
        for (Iterator atomSets = _instance.getSetsForAtom(atom).iterator(); atomSets.hasNext();) {
            AlloySet set = (AlloySet)atomSets.next();

            //System.out.println(set);
            // If there is a setting for this set...
            if (_vizMap.containsNode(set)) {
                NodeViz setSettings = _vizMap.getNodeViz(set);

                // Go through all the fields and try to apply the setting
                for (Iterator fieldsIter = fields.iterator(); fieldsIter.hasNext();) {
                    String field = (String)fieldsIter.next();
                    Object setting = lookupNodeSetting(setSettings, field);
                    // If there is a setting for this field, put it in the
                    // selections map
                    if (setting != null) {
                        Object oldValue = selections.put(field, setting);
                        if (oldValue != null) {
                            // A conflict has occurred.
                            //
                            // If it is a boolean option, and one set says turn
                            // it on, and the other says turn it off, we're
                            // going to turn it on.
                            if (Boolean.TRUE.equals(oldValue)) {
                                selections.put(field, oldValue);
                            }
                        }
                    }
                }
            }
        }

        // hide unconnected is a type notion so add here to insure that the setting
        // didn't get messed up by sets
        fields.add(HIDE_UNCONNECTED);
        // same for numberAtoms
        fields.add(NUMBER_ATOMS);

        // Now go through the unset fields and try to apply the type settings.
        // NEW: now try to traverse up the type hierarchy for this info
        AlloyType type = atom.getType();

        while (type != null) {
            if (_vizMap.containsNode(type)) {
                NodeViz typeSettings = _vizMap.getNodeViz(type);
                for (Iterator fieldsIter = fields.iterator(); fieldsIter.hasNext();) {
                    String field = (String)fieldsIter.next();
                    if (selections.get(field) == null) {
                        selections.put(field, lookupNodeSetting(typeSettings, field));
                    }
                }
            }
            type = _model.getSuperForType(type);
        }

        // Finally, apply the default settings to anything that is still unset
        for (Iterator fieldsIter = fields.iterator(); fieldsIter.hasNext();) {
            String field = (String)fieldsIter.next();
            if (selections.get(field) == null) {
                selections.put(field, lookupNodeSetting(_defaultNodeViz, field));
            }
        }

        // Now, create a NodeViz from the gathered information and return
        // it. (The label and same rank and show in attr are irrelevant for this method.
        // They are taken care of elsewhere.)
        return new NodeViz(
            (Boolean)selections.get(VISIBLE),
            null,
            (DotColor)selections.get(COLOR),
            (DotShape)selections.get(SHAPE),
            (DotStyle)selections.get(STYLE),
            null,
            null,
            null,
            null,
            (Boolean)selections.get(HIDE_UNCONNECTED),
            (Boolean)selections.get(NUMBER_ATOMS));
    }

    /*
     * Returns the setting represented by the specified String in the given
     * NodeViz
     */
    public static Object lookupNodeSetting(NodeViz viz, String field) {



        if (field.equals(VISIBLE)) {
            return viz.isVisible();
        }
        if (field.equals(COLOR)) {
            return viz.getColor();
        }
        if (field.equals(SHAPE)) {
            return viz.getShape();
        }
        if (field.equals(STYLE)) {
            return viz.getStyle();
        }
        if (field.equals(SAME_RANK)) {
            return viz.isSameRank();
        }
        if (field.equals(ATTRIBUTE)) {
            return viz.showInAttr();
        }
        if (field.equals(HIDE_UNCONNECTED)) {
            return viz.hideUnconnected();
        }
        if (field.equals(NUMBER_ATOMS)) {
            return viz.numberAtoms();
        }
        if (field.equals(SHOW_LABEL)) {
            return viz.showLabel();
        }
        if (field.equals(SELECTED)) {
            return new Boolean(viz.isSelected());
        }
        Dbg.fail(field + " is not a valid field for a node");
        return null;
    }

    /*
     * Creates a label for a given AlloyAtom
     */
    String createNodeLabel(AlloyAtom atom, int suffix, Boolean numberAtoms) {
        StringBuffer label = new StringBuffer();

        // First, put the primary label (of the form TypeLabel_#), only if
        // numberAtoms is true
        if (numberAtoms == null) {
            // this should always have been resolved before this point
            throw new NullPointerException("numberAtoms was null");
        } else {
            label.append(createTypeLabel(atom, suffix, numberAtoms.booleanValue()));
        }

        // Next, put the secondary label (of the form (SetLabel, SetLabel, SetLabel, ...))
        /* do this later in the Cartoonist so that the set label doesn't get shown as part of an index
        StringBuffer setsLabel = createSetsLabel(atom);
        if (setsLabel.length() != 0) {
            label.append("\\n" + setsLabel);
            }*/
        return label.toString();
    }

    /*
     * Creates the primary label for a Node, based on the label for its type.
     */
    private String createTypeLabel(AlloyAtom atom, int suffix, boolean appendSuffix) {
        AlloyType type = atom.getType();
        NodeViz typeSetting = _vizMap.getNodeViz(type);
        // if typeSetting is null, there is no mapping at all for the AlloyType,
        // so the type label should be empty.
        if (typeSetting == null) {
            return "";
        }
        String typeLabel = typeSetting.getLabel();
        // if typeLabel is null, the user explicitly requested no label, so the
        // type label should be empty.
        if (typeLabel == null /*|| typeLabel.equals("") don't want this anymore */) {
            return "";
        }
        // Otherwise, use the user-specified label and a suffix to make it
        // unique.
        if (appendSuffix) {
            typeLabel = typeLabel /*+ "_" */+ suffix;
        }
        return typeLabel;
    }

    /*
     * Creates the secondary label for a Node, based on the labels for its sets.
     */
    StringBuffer createSetsLabel(AlloyAtom atom) {
        StringBuffer setsLabel = new StringBuffer();
        boolean showByDefault = _defaultNodeViz.showLabel().booleanValue();
        for (Iterator sets = _instance.getSetsForAtom(atom).iterator(); sets.hasNext();) {
            AlloySet set = (AlloySet)sets.next();
            NodeViz setViz = _vizMap.getNodeViz(set);
            if (setViz != null) {
                if (setViz.showLabel() == null) {
                    if (showByDefault) {
                        String setLabel = setViz.getLabel();
                        if (setLabel != null && setLabel.length() != 0) {
                            setsLabel.append(setLabel + ", ");
                        }
                    } else {
                        // do nothing
                    }
                } else if (setViz.showLabel().booleanValue()) {
                    String setLabel = setViz.getLabel();
                    if (setLabel != null && setLabel.length() != 0) {
                        setsLabel.append(setLabel + ", ");
                    }
                }
            }
        }
        int length = setsLabel.length();
        if (length != 0) {
            NodeViz typeViz = _vizMap.getNodeViz(atom.getType());
            if (!typeViz.getLabel().equals("")) {
                // put parens around enumerated list
                setsLabel.insert(0, "(");
                setsLabel.replace(length - 1, length + 1, ")");
            } else {
                // don't put parens if the type label is empty
                setsLabel.delete(length - 2, length);
            }
        }
        return setsLabel;
    }

    /**
     * Almost the same as createSetsLabel, except it only includes the labels of
     * those sets that have a true value for their showInAttribute field.
     */
    StringBuffer createSetsLabelForRel(AlloyAtom atom) {
        StringBuffer setsLabel = new StringBuffer();
        boolean showByDefault = _defaultNodeViz.showInAttr().booleanValue();
        for (Iterator sets = _instance.getSetsForAtom(atom).iterator(); sets.hasNext();) {
            AlloySet set = (AlloySet)sets.next();
            NodeViz setViz = _vizMap.getNodeViz(set);
            if (setViz != null) {
                // only append if the showInAttr property is true
                if (setViz.showInAttr() == null) {
                    if (showByDefault) {
                        String setLabel = setViz.getLabel();
                        if (setLabel != null && setLabel.length() != 0) {
                            setsLabel.append(setLabel + ", ");
                        }
                    } else {
                        // do nothing
                    }
                } else if (setViz.showInAttr().booleanValue()) {
                    String setLabel = setViz.getLabel();
                    if (setLabel != null && setLabel.length() != 0) {
                        setsLabel.append(setLabel + ", ");
                    }
                }
            }

        }
        //System.out.println(setsLabel);
        int length = setsLabel.length();
        if (length != 0) {
            NodeViz typeViz = _vizMap.getNodeViz(atom.getType());
            if (!typeViz.getLabel().equals("")) {
                // put parens around enumerated list
                setsLabel.insert(0, "(");
                setsLabel.replace(length - 1, length + 1, ")");
            } else {
                // don't put parens if the type label is empty
                setsLabel.delete(length - 2, length);
            }
        }
        return setsLabel;
    }

    /*
     * Returns an Object representing the setting for the specified field of the
     * specified type, inheriting as necessary from superclasses.
     */
    private Object inheritNodeSetting(AlloyType type, String field) {
        // If type is null, we've hit the universal type
        if (type == null) {
            return lookupNodeSetting(_defaultNodeViz, field);
        }
        NodeViz typeViz = _vizMap.getNodeViz(type);
        // If there is a NodeViz for this type...
        if (typeViz != null) {
            Object setting = lookupNodeSetting(typeViz, field);
            if (setting != null) {
                return setting;
            }
        }
        return inheritNodeSetting(_instance.getSuperForType(type), field);
    }

    /*
     * Returns true if atoms that are members of the specified type should be
     * placed on the same rank.
     */
    boolean getTypeRank(AlloyType type) {
        return ((Boolean)inheritNodeSetting(type, SAME_RANK)).booleanValue();
    }

    /*
     * Returns true if atoms that are a part of the specified set should be
     * placed on the same rank.
     */
    boolean getSetRank(AlloySet set) {
        NodeViz viz = _vizMap.getNodeViz(set);
        // if there is a setting, return that
        if (viz != null) {
            Boolean sameRank = viz.isSameRank();
            if (sameRank != null) {
                return sameRank.booleanValue();
            }
        }
        // if there is no setting for this set, use the default
        return _defaultNodeViz.isSameRank().booleanValue();
    }

    //
    // EDGES
    //

    /*
     * Create an EdgeViz object to represent the settings for the given
     * AlloyRelation.
     */
    @SuppressWarnings("unchecked")
    EdgeViz getSettingsForRelation(AlloyRelation rel) {
        Map selections = new HashMap();
        int weight = -1;

        List fields = new ArrayList(6);
        fields.add(VISIBLE);
        fields.add(LABEL);
        fields.add(COLOR);
        fields.add(STYLE);
        fields.add(ATTRIBUTE);
        fields.add(MERGE);
        fields.add(LAYOUT_BACK);

        // First go through the EdgeViz for this relation.
        EdgeViz relViz = _vizMap.getEdgeViz(rel);
        if (relViz != null) {
            weight = relViz.getWeight();
            // Go through all of the fields and apply the setting from relViz
            for (Iterator fieldsIter = fields.iterator(); fieldsIter.hasNext();) {
                String field = (String)fieldsIter.next();
                selections.put(field, lookupEdgeSetting(relViz, field));
            }
        } else {
            Dbg.fatal("relViz wasn't in map for this rel! " + rel);
        }

        // Now go through all of the unset fields and apply the default settings.
        for (Iterator fieldsIter = fields.iterator(); fieldsIter.hasNext();) {
            String field = (String)fieldsIter.next();
            if (selections.get(field) == null) {
                selections.put(field, lookupEdgeSetting(_defaultEdgeViz, field));
            }
        }
        if (weight == -1) {
            weight = _defaultEdgeViz.getWeight();
        }
        // Now, create an EdgeViz from the gathered information and return
        // it. (Same rank is irrelevant for this method.  It is taken care of
        // elsewhere.)

        return new EdgeViz(
            (Boolean)selections.get(VISIBLE),
            (String)selections.get(LABEL),
            (DotColor)selections.get(COLOR),
            (DotStyle)selections.get(STYLE),
            weight,
            (Boolean)selections.get(ATTRIBUTE),
            null,
            (Boolean)selections.get(MERGE),
            null,
            (Boolean)selections.get(LAYOUT_BACK));

    }

    public static Object lookupEdgeSetting(EdgeViz viz, String field) {
        if (field.equals(VISIBLE)) {
            return viz.isVisible();
        }
        if (field.equals(LABEL)) {
            return viz.getLabel();
        }
        if (field.equals(COLOR)) {
            return viz.getColor();
        }
        if (field.equals(STYLE)) {
            return viz.getStyle();
        }
        if (field.equals(ATTRIBUTE)) {
            return viz.isAttribute();
        }
        if (field.equals(MERGE)) {
            return viz.mergeArrows();
        }
        if (field.equals(LAYOUT_BACK)) {
            return viz.layoutBack();
        }
        if (field.equals(SAME_RANK)) {
            return viz.isSameRank();
        }
        if (field.equals(SELECTED)) {
            return new Boolean(viz.isSelected());
        }
        Dbg.fail(field + " is not a valid field for a edge");
        return null;
    }

    /*
     * Returns true if atoms that are connected by the specified relation should
     * be placed on the same rank.
     */
    boolean getRelationRank(AlloyRelation rel) {
        EdgeViz viz = _vizMap.getEdgeViz(rel);
        // if there is a setting, return that
        if (viz != null) {
            Boolean sameRank = viz.isSameRank();
            if (sameRank != null) {
                return sameRank.booleanValue();
            }
        }
        // if there is no setting for this relation, use the default
        return _defaultEdgeViz.isSameRank().booleanValue();
    }
}
