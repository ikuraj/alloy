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

import javax.swing.JButton;
import javax.swing.JPanel;

class HeaderPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	protected JButton advanced;

    /**
     * shows visibility (display, hide, etc.) options only
     */

    void showVisibilityOnly() {
	if (advanced!=null) {
	    advanced.setVisible(false);
	}
    }

    /**
     * shows everything
     */
    void showAll() {
	if (advanced != null) {
	    advanced.setVisible(true);
	}
    }

}
