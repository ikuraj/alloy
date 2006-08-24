/*
 * Alloy Analyzer
 * Copyright (c) 2003 Massachusetts Institute of Technology
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.Action;
import javax.swing.JButton;

import kodviz.util.ParameterEvent;
import kodviz.util.ParameterListener;
import kodviz.util.Params;

// AlloyButton is a JButton that by default displays
// its text below its icon.  All AlloyButtons can be
// make to hide their text using the static method setTextShown
public class AlloyButton extends JButton {

	private static final long serialVersionUID = 1L;
	private String _text;
    private AlloyButton _useForSizing;

    private static Set alloyButtonSet = new HashSet();
    private static boolean textShown = true;

    static {
	Params.glob.addParameterListener
	    ("GUI", "showButtonText",
	     new ParameterListener() {
		 public void valueChanged(ParameterEvent e) {
		     setTextShown(Params.glob.getBoolParam("GUI", "showButtonText"));
		 }
	     });
    }

    public AlloyButton() {
	this(null);
    }

    @SuppressWarnings("unchecked")
    public AlloyButton(Action action) {
	super(action);
	alloyButtonSet.add(this);
	setVerticalTextPosition(BOTTOM);
	setHorizontalTextPosition(CENTER);
	setBorderPainted(false);
	setFont(getFont().deriveFont((float)10));
    }
    
    public void setText(String text_) {
	_text = text_;
	if (textShown)
	    setDisplayedText(_text);
    }

    private void setDisplayedText(String text_) {
	super.setText(text_);
    }
    
    public static void setTextShown(boolean b) {
	textShown = b;
	Iterator it = alloyButtonSet.iterator();
	while (it.hasNext()) {
	    AlloyButton button = (AlloyButton)it.next();
	    if (textShown)
		button.setDisplayedText(button._text);
	    else
		button.setDisplayedText(null);
	}
    }

    // must be careful that this sizing dependence has no cycles
    public void setButtonForSizing(AlloyButton button_) {
	_useForSizing = button_;
    }

    public Dimension getMinimumSize() {
	if (_useForSizing == null)
	    return super.getMinimumSize();
	else
	    return _useForSizing.getMinimumSize();
    }

    public Dimension getPreferredSize() {
	if (_useForSizing == null)
	    return super.getPreferredSize();
	else
	    return _useForSizing.getPreferredSize();
    }

    public Dimension getMaximumSize() {
	if (_useForSizing == null)
	    return super.getMaximumSize();
	else
	    return _useForSizing.getMaximumSize();
    }
}
