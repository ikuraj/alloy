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

import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.JList;
import javax.swing.JTextArea;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.border.EmptyBorder;


/** JList whose cells wrap their text */
@SuppressWarnings("serial")
class WrappingList extends JList {
    WrappingList(ListModel model) {
	super(model);
	addComponentListener((ComponentListener)AlloySwingUtilities.listener(new ComponentAdapter() {
		public void componentResized(ComponentEvent e) {
		    setFixedCellWidth(getWidth());
		}
	    }));
	setCellRenderer(new WrappingListCellRenderer());
    }
    
    public boolean getScrollableTracksViewportWidth() {
	return true;
    }
}

@SuppressWarnings("serial")
class WrappingListCellRenderer extends JTextArea implements ListCellRenderer {
    WrappingListCellRenderer() {
	super();
	setOpaque(true);
	setLineWrap(true);
	setWrapStyleWord(true);
    }
    
    public Component getListCellRendererComponent(JList list, Object value, // value to display
						  int index, // cell index
						  boolean isSelected, // is the cell selected
						  boolean cellHasFocus) // the list and the cell have the focus
    {
	if (isSelected && cellHasFocus) {
	    setBackground(list.getSelectionBackground());
	    setForeground(list.getSelectionForeground());
	}
	else {
	    setBackground(list.getBackground());
	    setForeground(list.getForeground());
	}
	String s = value.toString();
	setFont(list.getFont());
	setText(s);
	setBorder(new EmptyBorder(1, 1, 1, 1));
	
        setBounds(0,0,list.getWidth(),0);
	
	return this;
    }
}

