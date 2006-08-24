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
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import kodviz.alloyviz.VizResolver;
import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotPaletteManager;
import kodviz.dotviz.DotStyle;


/**
 * package-protected class, used for internal abstraction purposes
 */
class EdgeOptionsPanel extends JPanel implements OptionsPanel {

	private static final long serialVersionUID = 1L;
	private ModulePanel _parent;
    private JTextField labelText;
    private JComboBox colorCombo, outlineCombo;
    private JSpinner weightSpinner;
    private int _index;
    private Vector colorList;
    private DotPalette _curPal;
    private JPanel topRow;

    //AdvancedEdgeDialog advDialog;
    AdvancedEdgePanel advPanel;

    private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);
    // the main problem is the height...

    /**
     * edgeLabel is the default edgeLabel, index should correspond to the
     * index of this EdgeOptionsPanel in the vizOptions array.  Also,
     * parent_ should be the ModulePanel that creates this EdgeOptionsPanel
     */
    @SuppressWarnings("unchecked")
    EdgeOptionsPanel(String edgeLabel_, ModulePanel parent_, int index_, DotPalette pal_) {
        _curPal = pal_;
        _parent = parent_;
        _index = index_;
        int rendererElt = ModulePanel.RELATION;

        this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        advPanel = new AdvancedEdgePanel(_parent, _index);
        advPanel.setAlignmentX(LEFT_ALIGNMENT);

        topRow = new JPanel();
        topRow.setLayout(new BoxLayout(topRow, BoxLayout.X_AXIS));
        topRow.setAlignmentX(LEFT_ALIGNMENT);

        labelText = new JTextField(edgeLabel_, 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateEdge(_index);
            }
        });
        labelText.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent e) {
                _parent.updateEdge(_index);
            }
        });

        // Edge COLOR
        colorList = new Vector();
        colorList.add(null);
        DotPaletteManager dpm = new DotPaletteManager();
        colorList.addAll(dpm.getEdgeColors(_curPal));
        colorCombo = new JComboBox(colorList);
        colorCombo.setRenderer(new DotPropertyListCellRenderer(rendererElt, _index, VizResolver.COLOR, _parent));
        colorCombo.setMaximumSize(new Dimension(110, 35));
        colorCombo.setBorder(WIDGET_BORDER);
        colorCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateEdge(_index);
            }
        });

        // STYLE (OUTLINE)
        Vector outlineList = new Vector();
        outlineList.add(null);
        outlineList.addAll(DotStyle.getValidValues());
        outlineCombo = new JComboBox(outlineList);
        outlineCombo.setRenderer(new DotPropertyListCellRenderer(rendererElt, _index, VizResolver.STYLE, _parent));
        outlineCombo.setMaximumSize(new Dimension(105, 35));
        outlineCombo.setBorder(WIDGET_BORDER);
        outlineCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateEdge(_index);
            }
        });

        // Edge weight
        weightSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 999, 1));
        weightSpinner.setMaximumSize(weightSpinner.getPreferredSize());
        JPanel weightPanel = putInWestEastPanel("Weight:", weightSpinner);
        weightPanel.setBorder(WIDGET_BORDER);
        weightPanel.setToolTipText(
            "A higher weight will cause the edge to be shorter and straighter.");
        weightSpinner.setToolTipText(
            "A higher weight will cause the edge to be shorter and straighter.");
        weightSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                _parent.updateEdge(_index);
            }
        });

        final Dimension SPACER = new Dimension(15, 0);

        topRow.add(Box.createRigidArea(SPACER));
        topRow.add(labelText);
        topRow.add(Box.createRigidArea(SPACER));
        topRow.add(colorCombo);
        topRow.add(Box.createRigidArea(SPACER));
        topRow.add(outlineCombo);
        topRow.add(Box.createRigidArea(SPACER));
        topRow.add(weightPanel);

        this.add(topRow);
        this.add(advPanel);
        showAdvanced(true);

    }

    public void showAdvanced(boolean visible) {
        if (visible) {
            // need to show the panel
            advPanel.setVisible(true);
        } else {
            // need to hide the panel
            advPanel.setVisible(false);
        }
    }

    /*
     * displays/hides the basic row of options
     */
    public void showBasic(boolean visible) {
        if (visible) {
            topRow.setVisible(true);
        } else {
            topRow.setVisible(false);
        }
    }

    /**
     * changes contents of color combo box if dp is different from _curPal
     * returns the currently selected DotColor if a change has been made.
     * If no good mapping was found or if no change was made null is return.
     */
    @SuppressWarnings("unchecked")
    DotColor changePalette(DotPalette dp) {
        //System.out.println("changepal  called");
        if (!_curPal.equals(dp)) {
            DotPaletteManager dpm = new DotPaletteManager();
            DotColor newColor =
                dpm.findBestEdgeMatch(_curPal, dp, (DotColor)colorCombo.getSelectedItem());
            _curPal = dp;
            colorList.clear();
            colorList.add(null);
            colorList.addAll(dpm.getNodeColors(_curPal));
            colorCombo.setSelectedItem(newColor);
            //System.out.println(newColor);
            return newColor;
        }
        return null;
    }

    void setLabel(String label_) {
        labelText.setText(label_);
    }

    void setColor(DotColor color_) {
        colorCombo.setSelectedItem(color_);
    }

    void setOutline(DotStyle outline_) {
        outlineCombo.setSelectedItem(outline_);
    }

    void setWeight(int weight_) {
        weightSpinner.setValue(new Integer(weight_));
    }

    void setRanking(Boolean ranking_) {
        advPanel.setRanking(ranking_);
    }

    void setAttribute(Boolean attr_) {
        advPanel.setAttribute(attr_);
    }

    void setMerge(Boolean merge_) {
        advPanel.setMerge(merge_);
    }

    void setLayoutBack(Boolean layback_) {
        advPanel.setLayoutBack(layback_);
    }

    public String getLabel() {
        return labelText.getText();
    }

    public DotColor getColor() {
        Object temp = colorCombo.getSelectedItem();
        if (temp != null) {
            return (DotColor)temp;
        } else {
            return null;
        }
    }

    public DotStyle getOutline() {
        Object temp = outlineCombo.getSelectedItem();
        if (temp != null) {
            return (DotStyle)temp;
        } else {
            return null;
        }
    }

    public int getWeight() {
        return ((Integer)weightSpinner.getValue()).intValue();
    }

    Boolean getRanking() {
        return advPanel.getRanking();
    }

    Boolean getAttribute() {
        return advPanel.getAttribute();
    }

    Boolean getMerge() {
        return advPanel.getMerge();
    }

    Boolean getLayoutBack() {
        return advPanel.getLayoutBack();
    }

    private JPanel putInWestEastPanel(String compLabel, JComponent comp) {
        JPanel panel = new JPanel();

        JLabel label = new JLabel(compLabel);

        //comp.setMaximumSize(OPTIONS_MAXSIZE);
        //label.setMaximumSize(OPTIONS_MAXSIZE);
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.add(label);
        panel.add(Box.createRigidArea(new Dimension(10, 0)));
        panel.add(comp);

        return panel;
    }
}
