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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import kodviz.alloyviz.VizResolver;


/**
 * package protected class for generating the "visibility" panel that indicates
 * if an edge/node is to be shown in the graph....
 */
@SuppressWarnings("serial")
class NodeHeaderPanel extends JPanel {

    private int _index;
    private ModulePanel _parent;
    private boolean _isType;

    //private JRadioButton buttonYes;
    //private JRadioButton buttonNo;
    //private JRadioButton buttonIn;

    private TristateCheckBox displayTCB;

    private static final String TYPE_LABEL = "Show";
    private static final String SET_LABEL = "Show";

    private static final String TTYES1 = "Display members as nodes";

    private static final String TTYES2 = "Include members of set as nodes";


    private static final String EDIT_BTN_LBL = "Edit var";
    private static final String REMOVE_BTN_LBL = "Remove var";

    NodeHeaderPanel(ModulePanel parent_, int index_, boolean isType_, boolean isCustom) {
        _parent = parent_;
        _index = index_;
        _isType = isType_;

        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        if (_isType) {
            displayTCB =
                new TristateCheckBox(TYPE_LABEL, true, _index, VizResolver.VISIBLE, parent_);
            displayTCB.setLabelToolTipText(TTYES1);
        } else {
            displayTCB =
                new TristateCheckBox(SET_LABEL, true, _index, VizResolver.SHOW_LABEL, parent_);
            displayTCB.setLabelToolTipText(TTYES2);
        }

        add(displayTCB);
        //displayTCB.setMaximumSize(new Dimension(70, 20));
        displayTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateNode(_index);

            }
        });

        if (isCustom) {
            
            JButton editBtn = new JButton(EDIT_BTN_LBL);
            editBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _parent.editNodeCustVar(_index);
                }
            });
            add(editBtn);
            
            JButton removeBtn = new JButton(REMOVE_BTN_LBL);
            removeBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _parent.removeNodeCustVar(_index);
                }
            });
            add(removeBtn);
        }
        //add(Box.createRigidArea(new Dimension(100,20)));
        add(Box.createHorizontalGlue());
    }

    void setVisibility(Boolean visible_) {
        if (visible_ == null) {
            displayTCB.setState(TristateCheckBox.TRISTATE);
        } else {
            if (visible_.booleanValue()) {
                displayTCB.setState(TristateCheckBox.SELECTED);
            } else {
                displayTCB.setState(TristateCheckBox.UNSELECTED);
            }
        }
    }

    Boolean getVisibility() {
        int state = displayTCB.getState();
        if (state == TristateCheckBox.TRISTATE) {
            return null;
        }
        if (state == TristateCheckBox.SELECTED) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

}
