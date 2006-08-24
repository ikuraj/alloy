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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

class HorizontalRule extends JPanel
{
	private static final long serialVersionUID = 1L;

	HorizontalRule()
	{
		super();
		setBorder(new EmptyBorder(5, 0, 0, 0));
		setAlignmentX(LEFT_ALIGNMENT);
	}
	
	HorizontalRule(String label)
	{
		this();
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		add(new JLabel(label));
	}
	
	public void paintComponent(Graphics g) 
	{
		super.paintComponent(g);
		g.setColor(Color.lightGray);
		Insets insets = getInsets();
		g.drawLine(insets.left, insets.top, getWidth(), insets.top);
	}
}


