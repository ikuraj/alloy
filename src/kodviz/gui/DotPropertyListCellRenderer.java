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

package kodviz.gui;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import kodviz.dotviz.DotProperty;


class DotPropertyListCellRenderer extends JLabel implements ListCellRenderer {

    /*
    static final int TYPE=1;
    static final int SET=2;
    static final int RELATION=3;
    */

	private static final long serialVersionUID = 1L;

	private int _eltType, _index;

    private ModulePanel _modPanel;
    private String _fieldType;

    /**
     * use one of TYPE, SET, RELATION for eltType
     */

    public DotPropertyListCellRenderer() {
        this(ModulePanel.TYPE, 0, "",null); // arbituarily chosen.
    }

    public DotPropertyListCellRenderer(
        int eltType_,
        int index_,
        String fieldType_,
        ModulePanel modPanel_
        ) {
        super();
        _modPanel = modPanel_;
        _eltType = eltType_;
        _index = index_;
        _fieldType = fieldType_;

        setOpaque(true);
    }

    public Component getListCellRendererComponent(
        JList list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus) {

        if (value == null) {
            if (_modPanel != null) {
                // not general layout panel, get inherited value                 
                setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
                switch (_eltType) {
                    case ModulePanel.TYPE :
                        setText("Inherit");
                        setIcon(
                            ((DotProperty)_modPanel.getInheritedNodeProperty(_index, _fieldType))
                                .getIcon());
                        break;
                    case ModulePanel.SET :
                        setText("None");
                        setIcon(
                            ((DotProperty)_modPanel.getInheritedNodeProperty(_index, _fieldType))
                                .getIcon());
                        break;
                    case ModulePanel.RELATION :
                        setText("Default");
                        setIcon(
                            ((DotProperty)_modPanel.getInheritedEdgeProperty(_index, _fieldType))
                                .getIcon());
                        break;
                    default :
                        // assertion failure?
                }
            } else {
                // no inherited values in general layout panel -- use a null icon
                // (it'll never happen anyway)

                setIcon(null);
                switch (_eltType) {
                    case ModulePanel.TYPE :
                        setText("Inherit");
                        break;
                    case ModulePanel.SET :
                        setText("None");
                        break;
                    case ModulePanel.RELATION :
                        setText("Default");
                        break;
                    default :
                        // assertion failure?
                }

            }
        } else {
            // so we changed our minds, and now colors are just matte borders rather 
            // than icons, so here's a simple hack to take care of it :).
            /*if (value instanceof DotColor) {
             String colorName = ((DotColor)value).getDisplayedText();
             Color colorValue = GrappaColor.getColor(colorName,Color.black);
             //setHorizontalAlignment(SwingConstants.CENTER);
             setText(((DotProperty)value).getDisplayedText());
             setBorder(BorderFactory.createMatteBorder(0,15,0,0,colorValue));
            }
            else {*/
            setIcon(((DotProperty)value).getIcon());
            setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0));
            setText(((DotProperty)value).getDisplayedText());
            //}
            //setHorizontalAlignment(SwingConstants.CENTER);

        }
        if (isSelected) {
            setBackground(list.getSelectionBackground());
            setForeground(list.getSelectionForeground());
        } else {
            setBackground(list.getBackground());
            setForeground(list.getForeground());
        }
        return this;
    }
}
