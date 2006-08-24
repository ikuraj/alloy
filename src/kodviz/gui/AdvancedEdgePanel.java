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
import javax.swing.JPanel;

import kodviz.alloyviz.VizResolver;
import kodviz.util.Dbg;


class AdvancedEdgePanel extends JPanel {

	private static final long serialVersionUID = 1L;

    private static final String RANK_TT = "Align nodes connected by this relation's arcs";
    private static final String RANK_PANEL_LBL = "Align endpoints";

    private static final String ATTR_TT =
        "Additionally display this relation as an attribute on the nodes' labels";
    private static final String ATTR_PANEL_LBL = "Show as attribute";

    private static final String MERGE_TT =
        "Merge opposing arrows between the same nodes as one bidirectional arrow ";
    private static final String MERGE_PANEL_LBL = "Merge arrows";

    private static final String LAYBACK_TT = "Layout graph as if arcs were reversed";
    private static final String LAYBACK_PANEL_LBL = "Layout backwards";

    /*JRadioButton rankingYes, rankingNo, rankingIn;
    JRadioButton attrYes, attrNo, attrIn;
    JRadioButton mergeYes, mergeNo, mergeIn;
    JRadioButton laybackYes, laybackNo, laybackIn;*/
    TristateCheckBox rankingTCB, attrTCB, mergeTCB, laybackTCB;

    private int _index;
    private ModulePanel _caller;

    /**
     * note dialog is non-modal.  Also pass in the ModulePanel that generated this dialog so
     * that it can callback and update the image.
     */
    AdvancedEdgePanel(ModulePanel caller_, int index_) {
        super();
        _index = index_;
        _caller = caller_;

        //mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        // RANKING

        rankingTCB =
            new TristateCheckBox(RANK_PANEL_LBL, false, _index, VizResolver.SAME_RANK, _caller);
        rankingTCB.setMaximumSize(rankingTCB.getPreferredSize());
        rankingTCB.setLabelToolTipText(RANK_TT);
        rankingTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _caller.updateEdge(_index);
            }
        });

        //
        // ATTRIBUTE
        //

        attrTCB =
            new TristateCheckBox(ATTR_PANEL_LBL, false, _index, VizResolver.ATTRIBUTE, _caller);        
        attrTCB.setMaximumSize(attrTCB.getPreferredSize());
        attrTCB.setLabelToolTipText(ATTR_TT);
        attrTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _caller.updateEdge(_index);
            }
        });

        // Merge bidirectional arrows

        mergeTCB = new TristateCheckBox(MERGE_PANEL_LBL, false, _index, VizResolver.MERGE, _caller);
        mergeTCB.setMaximumSize(mergeTCB.getPreferredSize());
        mergeTCB.setLabelToolTipText(MERGE_TT);
        mergeTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _caller.updateEdge(_index);
            }
        });

        // Merge bidirectional arrows

        laybackTCB =
            new TristateCheckBox(
                LAYBACK_PANEL_LBL,
                false,
                _index,
                VizResolver.LAYOUT_BACK,
                _caller);
        laybackTCB.setMaximumSize(laybackTCB.getPreferredSize());
        laybackTCB.setLabelToolTipText(LAYBACK_TT);
        laybackTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _caller.updateEdge(_index);
            }
        });

        this.add(Box.createHorizontalStrut(15));
        this.add(rankingTCB);
        this.add(Box.createHorizontalStrut(15));
        this.add(attrTCB);
        this.add(Box.createHorizontalStrut(15));
        this.add(mergeTCB);
        this.add(Box.createHorizontalStrut(15));
        this.add(laybackTCB);
        this.add(Box.createHorizontalGlue());

    }
    
    private int tcbVal(Boolean b) {
        if (b == null) {
            return TristateCheckBox.TRISTATE;
        }
        if (b.booleanValue()) {
            return TristateCheckBox.SELECTED;
        }
        return TristateCheckBox.UNSELECTED;
    }

    private Boolean boolVal(int tcbVal) {
        Boolean ret = null;
        switch (tcbVal) {
            case TristateCheckBox.SELECTED :
                ret = Boolean.TRUE;
                break;
            case TristateCheckBox.UNSELECTED :
                ret = Boolean.FALSE;
                break;
            case TristateCheckBox.TRISTATE :
                ret = null;
                break;
            default :
                Dbg.fail("unknown argument to boolVal");
        }

        return ret;
    }

    public void setRanking(Boolean ranking_) {
        rankingTCB.setState(tcbVal(ranking_));
    }

    public void setAttribute(Boolean attr_) {
        attrTCB.setState(tcbVal(attr_));
    }

    public void setMerge(Boolean merge_) {
        mergeTCB.setState(tcbVal(merge_));
    }

    public void setLayoutBack(Boolean layback_) {
        laybackTCB.setState(tcbVal(layback_));
    }

    public Boolean getRanking() {
        return boolVal(rankingTCB.getState());
    }

    public Boolean getAttribute() {
        return boolVal(attrTCB.getState());
    }

    public Boolean getMerge() {
        return boolVal(mergeTCB.getState());
    }

    public Boolean getLayoutBack() {
        return boolVal(laybackTCB.getState());
    }
}
