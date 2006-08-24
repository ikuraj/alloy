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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;

/**
 * PalettePanel is a panel that holds a palette of stored customization that can
 * be selected and applied to the current visualization.
 */
@SuppressWarnings("unchecked")
class PalettePanel extends JPanel {

	private static final long serialVersionUID = 1L;

	private static final String TITLE = "Saved Visualizations";
    
    PalettePanel() {
	super();
	setLayout(new BorderLayout());

	JList list = createPaletteList();
	JLabel title = createTitle();
	JPanel midPanel = new JPanel();
	midPanel.setLayout(new BorderLayout());
	midPanel.add(title, BorderLayout.NORTH);
	midPanel.add(list, BorderLayout.CENTER);
	JScrollPane scroll = new JScrollPane(midPanel);
	add(scroll, BorderLayout.CENTER);
    }

    /*
     * Generate a title label for this PalettePanel.
     */
    private JLabel createTitle() {
	JLabel title = new JLabel(TITLE);
	title.setBackground(Color.LIGHT_GRAY);
	title.setBorder(BorderFactory.createEmptyBorder(5, 5, 10, 5));
	
	return title;
    }
    
    /*
     * Generate the list of choices available in the palette.
     */
    private JList createPaletteList() {
	
	/*
	 * Eventually this will have to be replaced by code that actually looks
	 * at the choices that are available and creates a JList based on that,
	 * but this will do for the initial test.
	 */
	Vector elts = new Vector();
	elts.add("Choice 1");
	elts.add("Choice 2");
	elts.add("Choice 3");

	JList list = new JList(elts);
	list.setBackground(Color.LIGHT_GRAY);
	list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	list.setCellRenderer(new PaletteCellRenderer());
	
	return list;
    }
    
    /*
     * The PaletteCellRenderer makes the selections in the palette list appear
     * similar to buttons.
     */
    private class PaletteCellRenderer extends JLabel implements ListCellRenderer {

	private static final long serialVersionUID = 1L;

	PaletteCellRenderer() {
	    super();
	}

	public Component getListCellRendererComponent(JList list, Object value, int index,
						      boolean isSelected, boolean cellHasFocus) {
	    
	    setText(value.toString());
	    Border defaultBorder = BorderFactory.createEmptyBorder(2, 5, 2, 2);	    
	    if (isSelected) {
		setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLoweredBevelBorder(),
							     defaultBorder));
	    }
	    else {
		setBorder(BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
							     defaultBorder));
	    }
	    
	    return this;
	}
    }
    
}
