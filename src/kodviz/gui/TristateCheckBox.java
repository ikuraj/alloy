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

import java.awt.Graphics;
import java.awt.event.MouseEvent;

import javax.swing.Action;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;

import kodviz.util.Dbg;


/**
 * TristateCheckBox is a subclass of JCheckBox which supports the rendering of a
 * TristateButtonModel.
 *
 */
@SuppressWarnings("serial")
public class TristateCheckBox extends JCheckBox {
    private JCheckBox tristateComponent;

    private int _index;
    private boolean _isNode;

    private ModulePanel _modPanel;
    private String _fieldType;
    private boolean _checkedInTristate;

    private String _labelToolTip = "";

    private String CB_TT_SELECTED = "turned ON";
    private String CB_TT_UNSELECTED = "turned OFF";
    private String CB_TT_INHERIT_SELECTED = "inherited ON";
    private String CB_TT_INHERIT_UNSELECTED = "inherited OFF";

    public static final int SELECTED = 1;
    public static final int UNSELECTED = 2;
    public static final int TRISTATE = 3;

    private static final Icon UNCHECKED_ICON = new ImageIcon(AlloySwingUtilities.loadImage("images/tcb01.gif"));
    private static final Icon CHECKED_ICON = new ImageIcon(AlloySwingUtilities.loadImage("images/tcb02.gif"));
    private static final Icon TRI_UNCHECKED_ICON = new ImageIcon(AlloySwingUtilities.loadImage("images/tcb03.gif"));
    private static final Icon TRI_CHECKED_ICON = new ImageIcon(AlloySwingUtilities.loadImage("images/tcb04.gif"));

    public TristateCheckBox() {
        this(null, null, false, true);
    }

    public TristateCheckBox(Icon icon) {
        this(null, icon, false, true);
    }

    public TristateCheckBox(Icon icon, boolean selected) {
        this(null, icon, selected, true);
    }

    public TristateCheckBox(Icon icon, boolean selected, boolean tristate) {
        this(null, icon, selected, tristate);
    }

    public TristateCheckBox(String text) {
        this(text, null, false, false);
    }

    public TristateCheckBox(Action a) {
        this(null, null, false, false);
        setAction(a);
    }

    public TristateCheckBox(String text, boolean selected) {
        this(text, null, selected, true);
    }

    public TristateCheckBox(String text, boolean selected, boolean tristate) {
        this(text, null, selected, tristate);
    }

    public TristateCheckBox(String text, Icon icon) {
        this(text, icon, false, true);
    }

    public TristateCheckBox(String text, Icon icon, boolean selected) {
        this(text, icon, selected, true);
    }

    public TristateCheckBox(String text, Icon icon, boolean selected, boolean tristate) {
        this(text, icon, selected, tristate, true, 0, "", null);
    }

    public TristateCheckBox(
        String text,
        boolean isNode,
        int index,
        String fieldType,
        ModulePanel modPanel) {
        this(text, null, false, true, isNode, index, fieldType, modPanel);
    }

    public TristateCheckBox(
        String text,
        Icon icon,
        boolean selected,
        boolean tristate,
        boolean isNode,
        int index,
        String fieldType,
        ModulePanel modPanel) {

        super(text, icon, selected);

        setIcon(UNCHECKED_ICON);
        setSelectedIcon(CHECKED_ICON);

        _isNode = isNode;
        _index = index;
        _fieldType = fieldType;
        _modPanel = modPanel;

        setModel(new DefaultTristateButtonModel(getModel()));
        setBorderPainted(false);
        setHorizontalAlignment(LEADING);

        ((TristateButtonModel)getModel()).setTristate(tristate);

        tristateComponent = new JCheckBox();

        tristateComponent.setIcon(TRI_UNCHECKED_ICON);
        tristateComponent.setSelectedIcon(TRI_CHECKED_ICON);

        _checkedInTristate = true;

        super.setToolTipText(""); // enable tooltips

    }

    /**
     * Override paint() to paint the tristate checkbox differently if it's in tristate.
     */
    public void paint(Graphics g) {
        ButtonModel model = getModel();
        if (model instanceof TristateButtonModel && ((TristateButtonModel)model).isTristate()) {

            // if going from tristate to normal, paint checked (the next state)
            if (model.isPressed() && model.isArmed()) {
                setIcon(CHECKED_ICON);
                super.paint(g);
                setIcon(UNCHECKED_ICON);
            } else {
                paintTristate(g);
            }

        } else {

            if (model.isPressed() && model.isArmed()) {
                if (model.isSelected()) {
                    // if going from checked to unchecked, print unchecked
                    setSelectedIcon(UNCHECKED_ICON);
                    super.paint(g);
                    setSelectedIcon(CHECKED_ICON);
                } else {
                    // if going from unchecked to tristate, paint tristate
                    paintTristate(g);
                }
            } else {
                super.paint(g);
            }

        }
    }

    private void paintTristate(Graphics g) {

        tristateComponent.setBackground(getBackground());
        tristateComponent.setForeground(getForeground());
        tristateComponent.setFont(getFont());
        tristateComponent.setText(getText());
        tristateComponent.setEnabled(isEnabled());
        tristateComponent.setBorder(getBorder());
        tristateComponent.setBounds(getBounds());

        ButtonModel model = tristateComponent.getModel();

        // controls whether an arrow is shown in tristate

        if (_modPanel != null) {
            model.setSelected(getInheritedState());

        } else {
            model.setSelected(_checkedInTristate);
        }

        tristateComponent.paint(g);

    }

    private boolean getInheritedState() {
        Boolean val =
            _isNode
                ? (Boolean)_modPanel.getInheritedNodeProperty(_index, _fieldType)
                : (Boolean)_modPanel.getInheritedEdgeProperty(_index, _fieldType);

        if (val != null) {
            return val.booleanValue();
        } else {
            Dbg.fatal("unexpected error while determining projected state");
            return false;
        }
    }

    private void setTristate(boolean b) {
        if (model instanceof TristateButtonModel) {
            ((TristateButtonModel)model).setTristate(b);
        }
    }

    public void setSelected() {
        throw new UnsupportedOperationException("Use setState");
    }

    public void setState(int state) {
        switch (state) {
            case TRISTATE :
                setTristate(true);
                break;
            case SELECTED :
                // first set to tristate, to avoid the unsel->tristate change from triggering
                setTristate(true);
                ((TristateButtonModel)getModel()).setSelected(true);
                break;
            case UNSELECTED :
                setTristate(true);
                ((TristateButtonModel)getModel()).setSelected(false);
                break;
        }
    }

    public int getState() {
        if (isTristate()) {
            return TRISTATE;
        }
        if (isSelected()) {
            return SELECTED;
        }
        return UNSELECTED;
    }

    // for testing purposes only -- gives control to whether or not tristate is
    // displayed as checked in the absence of a _modPanel
    void setCheckedInTristate(boolean b) {
        _checkedInTristate = b;
    }

    boolean getCheckedInTristate() {
        return _checkedInTristate;
    }

    private boolean isTristate() {
        if (model instanceof TristateButtonModel) {
            return ((TristateButtonModel)model).isTristate();
        } else {
            return false;
        }
    }

    public void setToolTipText(String tip) {
        this.setLabelToolTipText(tip);
        super.setToolTipText(tip);
    }

    public void setLabelToolTipText(String tip) {
        _labelToolTip = tip;
    }

    public String getLabelToolTipText() {
        return _labelToolTip;
    }

    public String getToolTipText(MouseEvent e) {
        java.awt.Point p = e.getPoint();
        boolean onCheckBoxPart = p.getX() < (this.getIcon().getIconWidth() + this.getIconTextGap());

        String ret = "";

        if (onCheckBoxPart) {
            // display correct label depending on state

            switch (this.getState()) {
                case TRISTATE :
                    ret = getInheritedState() ? CB_TT_INHERIT_SELECTED : CB_TT_INHERIT_UNSELECTED;
                    break;
                case SELECTED :
                    ret = CB_TT_SELECTED;
                    break;
                case UNSELECTED :
                    ret = CB_TT_UNSELECTED;
                    break;
                default :
                    Dbg.fatal("unknown error while determining tooltip for TristateCheckBox");
                    break;
            }
        } else {
            ret = _labelToolTip;
        }

        return ret;

    }
}
