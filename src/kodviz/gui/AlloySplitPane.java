/*
 * Alloy Analyzer
 * Copyright (c) 2004 Massachusetts Institute of Technology
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.JSplitPane;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneUI;

public class AlloySplitPane extends JSplitPane { // changed by vincent to public for use in viz

	private static final long serialVersionUID = 1L;

	AlloySplitPane(int orientation) {
	super(orientation);
	fixDivider();
    }

    public AlloySplitPane(int orientation, Component leftComp, Component rightComp) {
	super(orientation, leftComp, rightComp);
	fixDivider();
    }

    public void setDividerVisible(boolean b) {
	((BasicSplitPaneUI)getUI()).getDivider().setVisible(b);
    }

    private void fixDivider() {
	if (AlloySwingUtilities.onMac())
	    ((BasicSplitPaneUI)getUI()).getDivider().setBorder(new AlloySplitPaneDividerBorder(getOrientation()));
    }

    public void setOrientation(int o) {
	super.setOrientation(o);
	fixDivider();
    }
}

class AlloySplitPaneDividerBorder implements Border {
    private Insets _insets;

    AlloySplitPaneDividerBorder(int orientation) {
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
