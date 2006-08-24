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

import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import kodviz.util.ParameterEvent;
import kodviz.util.ParameterListener;
import kodviz.util.Params;

class TextWindow extends AlloyWindow {
	private static final long serialVersionUID = 1L;
    private final JTextArea textArea;
    private ParameterListener fontUpdater;

    TextWindow(String title, String text) {
	super(title);
	
	textArea = new JTextArea(text);
	textArea.setEditable(false);
	textArea.setFont(Params.glob.getFontParam("GUI", "textfont"));

	final JScrollPane scrollPane = new JScrollPane(textArea);
	getContentPane().add(scrollPane);

	fontUpdater = new ParameterListener() {
		public void valueChanged(ParameterEvent e) {
		    textArea.setFont(Params.glob.getFontParam("GUI", "textfont"));
		}
	    };
	Params.glob.addParameterListener("GUI","textfont",fontUpdater);
	
	addWindowListener((WindowListener)AlloySwingUtilities.listener(new WindowAdapter() {
                // show top left corner of text when window is first created and opened
                public void windowOpened(WindowEvent e) {
                    scrollPane.getViewport().setViewPosition(new Point(0, 0));
                }
            }));
    }

    public void dispose() {
	Params.glob.removeParameterListener("GUI","textfont",fontUpdater);
	super.dispose();
    }
}

