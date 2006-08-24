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
class EdgeHeaderPanel extends JPanel {

	private static final long serialVersionUID = 1L;

    private int _index;
    private ModulePanel _parent;

    private static final String LABEL = "Show as arcs";
    private static final String TT = "Show relation as arcs";

    private static final String EDIT_BTN_LBL = "Edit var";
	private static final String REMOVE_BTN_LBL = "Remove var";
	
	private TristateCheckBox displayTCB;

    EdgeHeaderPanel(ModulePanel parent_, int index_, boolean isCustom) {
        _parent = parent_;
        _index = index_;

		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

		displayTCB = new TristateCheckBox(LABEL, false, _index, VizResolver.VISIBLE, parent_);
		displayTCB.setLabelToolTipText(TT);
        
		add(displayTCB);
		//displayTCB.setMaximumSize(new Dimension(70, 20));
		displayTCB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				_parent.updateEdge(_index);

			}
		});

        if (isCustom) {        
            
            JButton editBtn = new JButton(EDIT_BTN_LBL);
            editBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _parent.editEdgeCustVar(_index);
                }
            });
            add(editBtn);
            
            JButton removeBtn = new JButton(REMOVE_BTN_LBL);
            removeBtn.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _parent.removeEdgeCustVar(_index);
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
		if (state==TristateCheckBox.TRISTATE) {
			return null;
		}
		if (state==TristateCheckBox.SELECTED) {
			return Boolean.TRUE;
		}
		return Boolean.FALSE;
    }
}
