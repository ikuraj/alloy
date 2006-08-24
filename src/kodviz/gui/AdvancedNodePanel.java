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

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import kodviz.alloyviz.VizResolver;
import kodviz.util.Dbg;


class AdvancedNodePanel extends JPanel {

	private static final long serialVersionUID = 1L;
	private static final String RANK_PANEL_TYPE_LBL = "Align by type";
    private static final String RANK_PANEL_SET_LBL = "Align members";

    private static final String RANK_TYPE_TT = "Aligns nodes of this type";
	private static final String RANK_SET_TT = "Aligns members of this set";
    
    private static final String SHOWLBL_PANEL_LBL = "Show as labels";
    private static final String SHOWLBL_TT = "Show membership in set by labeling nodes";
    
    private static final String SHOW_ATTR_PANEL_LBL = "Show in relation attributes";
    private static final String SHOW_ATTR_TT = "Show set membership in relation attributes";
    
    private static final String HIDE_UNCONN_PANEL_LBL = "Hide unconnected nodes";
    private static final String HIDE_UNCONN_TT = "Hide nodes without arcs";
    
    private static final String NUMBER_PANEL_LBL = "Number nodes";
    private static final String NUMBER_TT = "Attach atom number to node label as suffix";
    
    private ModulePanel _caller;
    private int _index;
    private boolean _isType;

    //JRadioButton rankingYes, rankingNo, rankingIn;
    //JRadioButton showLblYes, showLblNo, showLblIn;
    //JRadioButton showAttrYes, showAttrNo, showAttrIn;
    //JRadioButton hideUnconnYes, hideUnconnNo, hideUnconnIn;
    //JRadioButton numberYes, numberNo, numberIn;

    //JComboBox rankingCombo, showLblCombo, showAttrCombo, hideUnconnCombo, numberCombo;
    TristateCheckBox rankingTCB, showLblTCB, showAttrTCB, hideUnconnTCB, numberTCB;

    /**
     * much of the code is recycled from AdvancedNodeDialog
     */
    AdvancedNodePanel(ModulePanel caller_, int index_, boolean isType_) {
        super();
        _isType = isType_;
        _index = index_;
        _caller = caller_;

        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

        if (_isType) {
            rankingTCB =
                new TristateCheckBox(
                    RANK_PANEL_TYPE_LBL,
                    true,
                    _index,
                    VizResolver.SAME_RANK,
                    _caller);

            
            rankingTCB.setMaximumSize(rankingTCB.getPreferredSize());
            rankingTCB.setLabelToolTipText(RANK_TYPE_TT);
        } else {
            rankingTCB =
                new TristateCheckBox(
                    RANK_PANEL_SET_LBL,
                    true,
                    _index,
                    VizResolver.SAME_RANK,
                    _caller);
            rankingTCB.setMaximumSize(rankingTCB.getPreferredSize());
            rankingTCB.setLabelToolTipText(RANK_SET_TT);
        }

        rankingTCB.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _caller.updateNode(_index);
            }
        });

        // ONLY DO SET LABEL AND ATTR STUFF FOR SETS
        if (!_isType) {

            showLblTCB =
                new TristateCheckBox(
                    SHOWLBL_PANEL_LBL,
                    true,
                    _index,
                    VizResolver.SHOW_LABEL,
                    _caller);
            showLblTCB.setMaximumSize(showLblTCB.getPreferredSize());
            showLblTCB.setLabelToolTipText(SHOWLBL_TT);
            showLblTCB.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _caller.updateNode(_index);
                }
            });

            showAttrTCB =
                new TristateCheckBox(
                    SHOW_ATTR_PANEL_LBL,
                    true,
                    _index,
                    VizResolver.ATTRIBUTE,
                    _caller);
            showAttrTCB.setMaximumSize(showAttrTCB.getPreferredSize());
            showAttrTCB.setLabelToolTipText(SHOW_ATTR_TT);
            showAttrTCB.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _caller.updateNode(_index);
                }
            });

            add(Box.createRigidArea(new Dimension(15, 0)));
            add(rankingTCB);
            add(Box.createRigidArea(new Dimension(25, 0)));
            add(showLblTCB);
            add(Box.createRigidArea(new Dimension(25, 0)));
            add(showAttrTCB);
            add(Box.createHorizontalGlue());
        } else {
            // not a set--i.e. a type
            // do hide unconnected stuff

            hideUnconnTCB =
                new TristateCheckBox(
                    HIDE_UNCONN_PANEL_LBL,
                    true,
                    _index,
                    VizResolver.HIDE_UNCONNECTED,
                    _caller);
            hideUnconnTCB.setMaximumSize(hideUnconnTCB.getPreferredSize());
            hideUnconnTCB.setLabelToolTipText(HIDE_UNCONN_TT);
            hideUnconnTCB.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _caller.updateNode(_index);
                }
            });

            // number atoms stuff
            numberTCB =
                new TristateCheckBox(
                    NUMBER_PANEL_LBL,
                    true,
                    _index,
                    VizResolver.NUMBER_ATOMS,
                    _caller);
            numberTCB.setMaximumSize(numberTCB.getPreferredSize());
            numberTCB.setLabelToolTipText(NUMBER_TT);
            numberTCB.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    _caller.updateNode(_index);
                }
            });
            add(Box.createRigidArea(new Dimension(15, 0)));
            add(rankingTCB);
            add(Box.createRigidArea(new Dimension(25, 0)));
            add(hideUnconnTCB);
            add(Box.createRigidArea(new Dimension(25, 0)));
            add(numberTCB);
            add(Box.createHorizontalGlue());

        }

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

    public Boolean getRanking() {
        return boolVal(rankingTCB.getState());
    }

    public void setShowLabel(Boolean showLbl_) {
        if (!_isType) {
            showLblTCB.setState(tcbVal(showLbl_));
        }
    }

    public void setShowInAttr(Boolean showAttr_) {
        if (!_isType) {
            showAttrTCB.setState(tcbVal(showAttr_));
        }
    }

    public void setHideUnconn(Boolean hide_) {
        if (_isType) {
            hideUnconnTCB.setState(tcbVal(hide_));
        }
    }

    public void setNumberAtoms(Boolean number_) {
        if (_isType) {
            numberTCB.setState(tcbVal(number_));
        }
    }

    public Boolean getShowLabel() {
        if (!_isType) {
            return boolVal(showLblTCB.getState());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean getShowInAttr() {
        if (!_isType) {
            return boolVal(showAttrTCB.getState());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean getHideUnconn() {
        if (_isType) {
            return boolVal(hideUnconnTCB.getState());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean getNumberAtoms() {
        if (_isType) {
            return boolVal(numberTCB.getState());
        } else {
            return Boolean.FALSE;
        }
    }
}
