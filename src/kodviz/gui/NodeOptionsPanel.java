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
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import kodviz.alloyviz.VizResolver;
import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotPaletteManager;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;


/**
 * internal class for creating the node options panel.
 */

class NodeOptionsPanel extends JPanel implements OptionsPanel {

	private static final long serialVersionUID = 1L;

	private ModulePanel _parent;
	private JPanel topRow;
    private JTextField labelText;
    private JComboBox colorCombo, shapeCombo, outlineCombo;
    private int _index;
    private boolean _isType;
    private DotPalette _curPal;
    //private boolean _advVisible;

    //AdvancedNodeDialog advDialog;
    AdvancedNodePanel advPanel;

    private Vector colorList; // to keep track of the vector to modify if necessary

    private Border WIDGET_BORDER = BorderFactory.createEmptyBorder(5, 5, 5, 5);
    // the main problem is the height...

    // CONSTANTS
    //private static final String ADV_LBL = "More...";

    /**
     * nodeName is the default label, index should correspond to the
     * index of this NodeOptionsPanel in the vizOptions array.  Also,
     * parent_ should be the ModulePanel that creates this NodeOptionsPanel
     */

    @SuppressWarnings("unchecked")
    NodeOptionsPanel(
        String nodeName_,
        ModulePanel parent_,
        int index_,
        boolean isType_,
        DotPalette pal_) {
        setBorder(WIDGET_BORDER);
        _curPal = pal_;
        _parent = parent_;
        _index = index_;
        _isType = isType_;
        int rendererElt;
        if (_isType) {
            rendererElt = ModulePanel.TYPE;
        } else {
            rendererElt = ModulePanel.SET;
        }

        advPanel = new AdvancedNodePanel(_parent, _index, _isType);
        advPanel.setAlignmentX(LEFT_ALIGNMENT);

        topRow = new JPanel();

        this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        //this.setAlignmentX(LEFT_ALIGNMENT);

        topRow.setLayout(new BoxLayout(topRow, BoxLayout.X_AXIS));
        topRow.setAlignmentX(LEFT_ALIGNMENT);

        // NODE LABEL
        labelText = new JTextField(nodeName_, 10);
        labelText.setMaximumSize(new Dimension(100, 25));
        labelText.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateNode(_index);
            }
        });
        labelText.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent e) {
                _parent.updateNode(_index);
            }
        });

        // NODE COLOR
        colorList = new Vector();
        colorList.add(null);
        DotPaletteManager dpm = new DotPaletteManager();
        colorList.addAll(dpm.getNodeColors(_curPal));
        colorCombo = new JComboBox(colorList);
        colorCombo.setRenderer(new DotPropertyListCellRenderer(rendererElt, _index, VizResolver.COLOR, _parent));
        //colorCombo.setMinimumSize(new Dimension(colorCombo.getMinimumSize().width, 32));
        //colorCombo.setPreferredSize(new Dimension(colorCombo.getPreferredSize().width, 32));
        colorCombo.setMaximumSize(new Dimension(100, 35));
        colorCombo.setBorder(WIDGET_BORDER);
        colorCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateNode(_index);
            }
        });

        // SHAPE
        Vector shapeList = new Vector();
        shapeList.add(null);
        shapeList.addAll(DotShape.getValidValues());
        shapeCombo = new JComboBox(shapeList);
        shapeCombo.setRenderer(new DotPropertyListCellRenderer(rendererElt, _index, VizResolver.SHAPE, _parent));
        //shapeCombo.setMinimumSize(new Dimension(shapeCombo.getMinimumSize().width, 32));
        //shapeCombo.setPreferredSize(new Dimension(shapeCombo.getPreferredSize().width, 32));
        shapeCombo.setMaximumSize(new Dimension(155, 35));
        shapeCombo.setBorder(WIDGET_BORDER);
        shapeCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateNode(_index);
            }
        });

        // OUTLINE
        Vector outlineList = new Vector();
        outlineList.add(null);
        outlineList.addAll(DotStyle.getValidValues());
        outlineCombo = new JComboBox(outlineList);
        outlineCombo.setRenderer(new DotPropertyListCellRenderer(rendererElt, _index, VizResolver.STYLE, _parent));
        //outlineCombo.setMinimumSize(new Dimension(outlineCombo.getMinimumSize().width, 32));
        //outlineCombo.setPreferredSize(new Dimension(outlineCombo.getPreferredSize().width, 32));
        outlineCombo.setMaximumSize(new Dimension(95, 35));
        outlineCombo.setBorder(WIDGET_BORDER);
        outlineCombo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                _parent.updateNode(_index);
            }
        });

        // ADVANCED OPTIONS
        /*JButton advanced = new JButton(ADV_LBL);
        advanced.setMaximumSize(new Dimension(75, 25));
        advanced.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                advDialog.show();
            }
        });*/

        topRow.add(Box.createRigidArea(new Dimension(15, 0)));
        topRow.add(labelText);
        topRow.add(Box.createRigidArea(new Dimension(15, 0)));
        topRow.add(colorCombo);
        topRow.add(Box.createRigidArea(new Dimension(15, 0)));
        topRow.add(shapeCombo);
        topRow.add(Box.createRigidArea(new Dimension(15, 0)));
        topRow.add(outlineCombo);
        //add(Box.createRigidArea(new Dimension(15, 0)));
        //add(advanced);        
        topRow.add(Box.createHorizontalGlue());

        this.add(topRow);
        this.add(advPanel);
        showAdvanced(true);
    }

    /*
     * displays/hides advanced options panel 
     */
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
     	}
     	else {
     		topRow.setVisible(false);
     	}
     }

    /*private JComboBox createComboBox(Vector items, int maxWidth, int rendererElt)
    {
        JComboBox combo  = new JComboBox(items);
    	combo.setRenderer(new DotPropertyListCellRenderer(rendererElt));
    	Dimension dim = combo.getPreferredSize();
    	dim.height = 32;
    	combo.setMaximumSize(dim);
    	combo.setMaximumSize(new Dimension(maxWidth, 35));
    	combo.setBorder(WIDGET_BORDER);
    	combo.addActionListener(new ActionListener() {
    		public void actionPerformed(ActionEvent e)
    		{
    		    _parent.updateNode(_index);
    		}
        });
        
        return combo;
    }*/

    /**
     * changes contents of color combo box if dp is different from _curPal
     */
     @SuppressWarnings("unchecked")
    DotColor changePalette(DotPalette dp) {
        if (!_curPal.equals(dp)) {
            DotPaletteManager dpm = new DotPaletteManager();
            DotColor newColor =
                dpm.findBestNodeMatch(_curPal, dp, (DotColor)colorCombo.getSelectedItem());
            _curPal = dp;
            colorList.clear();
            colorList.add(null);
            colorList.addAll(dpm.getNodeColors(_curPal));
            colorCombo.setSelectedItem(newColor);
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

    void setShape(DotShape shape_) {
        shapeCombo.setSelectedItem(shape_);
    }

    void setOutline(DotStyle outline_) {
        outlineCombo.setSelectedItem(outline_);
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

    public DotShape getShape() {
        Object temp = shapeCombo.getSelectedItem();
        if (temp != null) {
            return (DotShape)temp;
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

    public void setRanking(Boolean ranking_) {
        advPanel.setRanking(ranking_);
    }

    public void setShowLabel(Boolean showLbl_) {
        advPanel.setShowLabel(showLbl_);
    }

    public void setShowInAttr(Boolean show_) {
        advPanel.setShowInAttr(show_);
    }

    public void setHideUnconn(Boolean hide_) {
        advPanel.setHideUnconn(hide_);
    }

    public void setNumberAtoms(Boolean number_) {
        advPanel.setNumberAtoms(number_);
    }

    public Boolean getRanking() {
        return advPanel.getRanking();
    }

    public Boolean getShowLabel() {
        return advPanel.getShowLabel();
    }

    public Boolean getShowInAttr() {
        return advPanel.getShowInAttr();
    }

    public Boolean getHideUnconn() {
        return advPanel.getHideUnconn();
    }

    public Boolean getNumberAtoms() {
        return advPanel.getNumberAtoms();
    }

}
