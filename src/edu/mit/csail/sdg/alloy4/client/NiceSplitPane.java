package edu.mit.csail.sdg.alloy4.client;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.JSplitPane;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import edu.mit.csail.sdg.alloy4.util.Util;

/** Taken from Alloy 3 */

public final class NiceSplitPane extends JSplitPane { // changed by vincent to public for use in viz

    private static final long serialVersionUID = 1L;

    NiceSplitPane(int orientation) {
    super(orientation);
    fixDivider();
    }

    public NiceSplitPane(int orientation, Component leftComp, Component rightComp) {
    super(orientation, leftComp, rightComp);
    fixDivider();
    }

    public void setDividerVisible(boolean b) {
    ((BasicSplitPaneUI)getUI()).getDivider().setVisible(b);
    }

    private void fixDivider() {
    	if (Util.onMac())
    	    ((BasicSplitPaneUI)getUI()).getDivider().setBorder(new NiceSplitPaneDividerBorder(getOrientation()));
        }

    public void setOrientation(int o) {
    super.setOrientation(o);
    fixDivider();
    }


    public static class NiceSplitPaneDividerBorder implements Border {
        private Insets _insets;

        NiceSplitPaneDividerBorder(int orientation) {
        _insets = (orientation == JSplitPane.HORIZONTAL_SPLIT) ?
            new Insets(0, 1, 0, 1) : new Insets(1, 0, 1, 0);
        }

        public Insets getBorderInsets(Component c) {
        return _insets;
        }

        public boolean isBorderOpaque() {
        return true;
        }

        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        g.setColor(Color.lightGray);
        g.fillRect(0, 0, width + _insets.left + _insets.right, _insets.top);
        g.fillRect(0, height - 1, width + _insets.left + _insets.right, _insets.bottom);
        g.fillRect(0, 0, _insets.left, height + _insets.top + _insets.bottom);
        g.fillRect(width - 1, 0, _insets.right, height + _insets.top + _insets.bottom);
        }
    }

}

